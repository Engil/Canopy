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

  let created_updated_ids commit key =
    repo () >>= fun repo ->
    Store.master task repo >>= fun t  ->
    Store.history (t "Reading history") >>= fun history ->
    let aux commit_id acc =
      Store.of_commit_id (Irmin.Task.none) commit_id repo >>= fun store ->
      acc >>= fun (created, updated, last) ->
      Store.read (store ()) key >|= fun data ->
      match data, last with
      | None, None -> (created, updated, last)
      | None, Some _ -> (created, updated, last)
      | Some x, Some y when x = y -> (created, updated, last)
      | Some _, None -> (commit_id, commit_id, data)
      | Some _, Some _ -> (created, commit_id, data)
    in
    Topological.fold aux history (Lwt.return (commit, commit, None))

  let date_updated_created key =
    new_task () >>= fun t  ->
    repo () >>= fun repo ->
    Store.head_exn (t "Finding head") >>= fun head ->
    created_updated_ids head key >>= fun (created_commit_id, updated_commit_id, _) ->
    let to_ptime task = Irmin.Task.date task |> Int64.to_float |> Ptime.of_float_s in
    Store.Repo.task_of_commit_id repo updated_commit_id >>= fun updated ->
    Store.Repo.task_of_commit_id repo created_commit_id >>= fun created ->
    match to_ptime updated, to_ptime created with
    | Some a, Some b -> Lwt.return (a, b)
    | _ -> raise (Invalid_argument "date_updated_last")

  let fill_cache article_map =
    let open Canopy_content in
    let fold_fn key value acc =
      value () >>= fun content ->
      date_updated_created key >>= fun (updated, created) ->
      let uri = String.concat "/" key in
      match of_string ~uri ~content ~created ~updated with
      | COk article ->
        article_map := KeyMap.add key article !article_map;
        Lwt.return acc
      | CError error ->
        let error_msg = Printf.sprintf "Error while parsing %s: %s" uri error in
        Lwt.return (error_msg::acc)
      | CUnknown ->
        let error_msg = Printf.sprintf "%s : Unknown content type" uri in
        Lwt.return (error_msg::acc)
    in
    new_task () >>= fun t ->
    fold (t "Folding through values") fold_fn []

  let last_commit_date () =
    new_task () >>= fun t  ->
    repo () >>= fun repo ->
    Store.head_exn (t "Finding head") >>= fun head ->
    Store.Repo.task_of_commit_id repo head >>= fun task ->
    let date = Irmin.Task.date task |> Int64.to_float in
    Ptime.of_float_s date |> function
      | Some o -> Lwt.return o
      | None -> raise (Invalid_argument "date_updated_last")
end
