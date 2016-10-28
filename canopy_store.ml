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
  let repo _ = Store.Repo.create store_config

  let new_task _ =
    match remote_branch () with
    | None -> repo () >>= Store.master task
    | Some branch -> repo () >>= Store.of_branch_id task branch

  let upstream = Irmin.remote_uri (remote_uri ())

  let key_type = function
    | x::_ when x = "static" -> `Static
    | x::_ when x = ".config" -> `Config
    | _ -> `Article

  let get_subkeys key =
    new_task () >>= fun t ->
    Store.list (t "Reading posts") key >|= fun keys ->
    List.filter (fun k -> match key_type k with `Article -> true | _ -> false) keys

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

  let base_uuid () =
    get_key [".config" ; "uuid"] >|= function
    | None -> invalid_arg ".config/uuid is required in the remote git repository"
    | Some n -> String.trim n

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
    new_task () >>= fun t ->
    repo () >>= fun repo ->
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

  let fill_cache base_uuid cache =
    let module C = Canopy_content in
    let fold_fn key value acc =
      value () >>= fun content ->
      date_updated_created key >>= fun (updated, created) ->
      match key_type key with
      | `Static ->
        (cache := KeyMap.add key (`Raw content) !cache;
         Lwt.return acc)
      | `Config ->
        (cache := KeyMap.add key (`Config (String.trim content)) !cache;
         Lwt.return acc)
      | `Article ->
        let uri = String.concat "/" key in
        match C.of_string ~base_uuid ~uri ~content ~created ~updated with
        | C.Ok article ->
          cache := KeyMap.add key (`Article article) !cache;
          Lwt.return acc
        | C.Error error ->
          let error_msg = Printf.sprintf "Error while parsing %s: %s" uri error in
          Lwt.return (error_msg::acc)
        | C.Unknown ->
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
