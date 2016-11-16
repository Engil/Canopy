open Lwt.Infix
open Canopy_config
open Canopy_utils


module Store (CTX: Irmin_mirage.CONTEXT) (INFL: Git.Inflate.S) = struct

  module Hash = Irmin.Hash.SHA1
  module Mirage_git_memory = Irmin_mirage.Irmin_git.Memory(CTX)(INFL)
  module Store = Mirage_git_memory(Irmin.Contents.String)(Irmin.Ref.String)(Hash)
  module Sync = Irmin.Sync(Store)
  module Topological = Graph.Topological.Make(Store.History)
  module View = Irmin.View(Store)

  let src = Logs.Src.create "canopy-store" ~doc:"Canopy store logger"
  module Log = (val Logs.src_log src : Logs.LOG)

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

  let pull () =
    new_task () >>= fun t ->
    Log.info (fun f -> f "pulling repository") ;
    Lwt.catch
      (fun () ->
         Sync.pull_exn (t "Updating") upstream `Update >|= fun _ ->
         Log.info (fun f -> f "repository pulled"))
      (fun e -> Lwt.return (Log.warn (fun f -> f "failed pull %s" (Printexc.to_string e))))

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

  let set_diffs repo c1 c2 =
    let view_of_commit repo commit_id =
      Store.of_commit_id task commit_id repo >>= fun t ->
      View.of_path (t "view") []
    in
    view_of_commit repo c1 >>= fun v1 ->
    view_of_commit repo c2 >>= fun v2 ->
    View.diff v1 v2 >|= fun diffs ->
    diffs

  let fill_history_cache () =
    new_task () >>= fun t ->
    repo () >>= fun repo ->
    Store.history (t "Reading history") >>= fun history ->
    let aux commit_id acc =
      acc >>= fun acc ->
      match acc with
      | None, acc_list ->
        (Some commit_id, acc_list) |> Lwt.return
      | Some prev_commit_id, acc_list ->
        Store.Repo.task_of_commit_id repo commit_id >>= fun task ->
        let timestamp = Irmin.Task.date task |> Int64.to_float |> Ptime.of_float_s in
        set_diffs repo prev_commit_id commit_id >|= fun diffs ->
        let c_history = Canopy_content.of_c_history timestamp diffs in
        let acc_list = List.append acc_list c_history in
        (Some commit_id, acc_list)
    in
    Topological.fold aux history (Lwt.return (None, [])) >|= fun (_, diffs) ->
    diffs

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

  let check_redirect content =
    match Astring.String.cut ~sep:"redirect:" content with
    | None -> None
    | Some (_, path) -> Some (Uri.of_string (String.trim path))

  let fill_cache base_uuid =
    let module C = Canopy_content in
    let fn key value cache =
      value () >>= fun content ->
      date_updated_created key >|= fun (updated, created) ->
      match key_type key with
      | `Static -> KeyMap.add key (`Raw (content, updated)) cache
      | `Config -> KeyMap.add key (`Config (String.trim content)) cache
      | `Article ->
        let uri = String.concat "/" key in
        match C.of_string ~base_uuid ~uri ~content ~created ~updated with
        | C.Ok article -> KeyMap.add key (`Article article) cache
        | C.Unknown ->
          Log.warn (fun f -> f "%s : Unknown content type" uri) ;
          cache
        | C.Error error ->
          begin
            match check_redirect content with
            | None ->
              Log.warn (fun f -> f "Error while parsing %s: %s" uri error) ;
              cache
            | Some uri -> KeyMap.add key (`Redirect uri) cache
          end
    in
    new_task () >>= fun t ->
    fold (t "Iterating over values") fn KeyMap.empty

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
