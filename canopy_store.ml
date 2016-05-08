open Lwt.Infix
open V1_LWT
open Canopy_config
open Canopy_utils

module Store (C: CONSOLE) (CTX: Irmin_mirage.CONTEXT) (INFL: Git.Inflate.S) = struct

  module Hash = Irmin.Hash.SHA1
  module Mirage_git_memory = Irmin_mirage.Irmin_git.Memory(CTX)(INFL)
  module Store = Mirage_git_memory(Irmin.Contents.String)(Irmin.Ref.String)(Hash)
  module Sync = Irmin.Sync(Store)
  module Topological = Graph.Topological.Make(Store.History)

  let store_config = Irmin_mem.config ()
  let task s = Irmin.Task.create ~date:0L ~owner:"Server" s
  let config = Canopy_config.config ()
  let repo _ = Store.Repo.create store_config
  let new_task _ = repo () >>= Store.master task
  let upstream = Irmin.remote_uri config.remote_uri

  let get_subkeys key =
    new_task () >>= fun t ->
    Store.list (t "Reading posts") key

  let get_key key =
    new_task () >>= fun t ->
    Store.read (t "Read post") key

  let fold t fn acc =
    let acc = ref (Lwt.return acc) in
    let mut = Lwt_mutex.create () in
    Store.iter t (fun k v ->
      Lwt_mutex.with_lock mut
                 (fun _ -> !acc >>= fun acc' -> (acc := (fn k v acc')) |> Lwt.return))
    >>= fun _ -> !acc

  let pull console =
    new_task () >>= fun t ->
    Lwt.return (C.log console "Pulling repository") >>= fun _ ->
    Lwt.catch
      (fun () ->
         Sync.pull_exn (t "Updating") upstream `Update >>= fun _ ->
         Lwt.return (C.log console "Repository pulled"))
      (fun e ->
         let msg = Printf.sprintf "Fail pull %s" (Printexc.to_string e) in
         Lwt.return (C.log console msg))

  let commit_date repo commit_id =
    Store.Repo.task_of_commit_id repo commit_id >|= fun task ->
    Irmin.Task.date task |> Int64.to_float |> Ptime.of_float_s

  let commits repo key =
    Store.master task repo >>= fun t  ->
    Store.history (t "Reading history") >>= fun history ->
    Topological.fold
      (fun id acc ->
         Store.of_commit_id (Irmin.Task.none) id repo >>= fun store ->
         acc >>= fun acc ->
         Store.read (store ()) key >>= function
         | None -> Lwt.return acc
         | Some x -> commit_date repo id >|= function
           | None -> acc
           | Some d -> (d, x) :: acc)
      history (Lwt.return [])

  let find_last_ts c = function
    | [] -> c
    | ((_, newest)::_) as xs ->
      List.fold_left (fun ts (n, data) -> if newest = data then n else ts) c xs

  let date_updated_created head repo key =
    commits repo key >|= fun commits ->
    match List.sort (fun (t1, _) (t2, _) -> compare t1 t2) commits with
    | ((f, _)::_) as xs -> (f, find_last_ts f (List.rev xs))
    | [] -> (head, head)

  let last_commit_date () =
    new_task () >>= fun t  ->
    repo () >>= fun repo ->
    Store.head_exn (t "Finding head") >>= fun head ->
    commit_date repo head >>= function
      | None -> assert false
      | Some x -> Lwt.return x

  let fill_cache article_map =
    let open Canopy_content in
    last_commit_date () >>= fun head_date ->
    repo () >>= fun repo ->
    let fold_fn key value acc =
      value >>= fun content ->
      date_updated_created head_date repo key >>= fun (created, updated) ->
      Printf.printf "article %s created %s updated %s\n%!" (String.concat "/" key) (Ptime.to_rfc3339 created) (Ptime.to_rfc3339 updated) ;
      let uri = String.concat "/" key in
      match of_string ~uri ~content ~created ~updated with
      | Ok article ->
        article_map := KeyMap.add key article !article_map;
        Lwt.return acc
      | Error error ->
        let error_msg = Printf.sprintf "Error while parsing %s: %s" uri error in
        Lwt.return (error_msg::acc)
      | Unknown ->
        let error_msg = Printf.sprintf "%s : Unknown content type" uri in
        Lwt.return (error_msg::acc)
    in
    new_task () >>= fun t ->
    fold (t "Folding through values") fold_fn []

end
