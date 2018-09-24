open Lwt.Infix
open Canopy_config
open Canopy_utils

module Store (C: sig val context : Conduit_mirage.t val resolver : Resolver_lwt.t end) = struct

  module Mirage_git_memory = Irmin_mirage.Git.Mem.KV
  module Store = Mirage_git_memory(Irmin.Contents.String)
  module Sync = Irmin.Sync(Store)
  module Topological = Graph.Topological.Make(Store.History)

  let src = Logs.Src.create "canopy-store" ~doc:"Canopy store logger"
  module Log = (val Logs.src_log src : Logs.LOG)

  let store_config = Irmin_mem.config ()
  let repo _ = Store.Repo.v store_config

  let store () =
    match remote_branch () with
    | None        -> repo () >>= Store.master
    | Some branch -> repo () >>= fun r -> Store.of_branch r branch

  let upstream = Git_mirage.endpoint ~conduit:C.context ~resolver:C.resolver (Uri.of_string (remote_uri ()))
  let upstream = Sync.remote upstream

  let walk t root =
    let todo = ref [] in
    let all = ref [] in
    let rec aux () = match !todo with
      | []      -> Lwt.return_unit
      | k::rest ->
        todo := rest;
        Store.list t k >>= fun childs ->
        Lwt_list.iter_p (fun (s, c) ->
            let k = k @ [s] in
            match c with
            | `Node     -> todo := k :: !todo; Lwt.return_unit
            | `Contents ->
              Store.get t k >|= fun v ->
              all := (k, v) :: !all
          ) childs >>=
        aux
    in
    todo := [root];
    aux () >|= fun () ->
    !all

  let key_type = function
    | x::_ when x = "static" -> `Static
    | x::_ when x = ".config" -> `Config
    | _ -> `Article

  let get_subkeys key =
    store () >>= fun t ->
    walk t key >|= fun keys ->
    List.fold_left (fun acc (k, _) ->
        if key_type k = `Article then k :: acc else acc
      ) [] keys

  let get_key key =
    store () >>= fun t ->
    Store.find t key

  let fold t fn acc =
    let mut = Lwt_mutex.create () in
    walk t [] >>= fun all ->
    Lwt_list.fold_left_s (fun acc (k, v) ->
        Lwt_mutex.with_lock mut (fun () -> acc >|= fn k v)
      ) (Lwt.return acc) all
    >>= fun x -> x

  let base_uuid () =
    get_key [".config" ; "uuid"] >|= function
    | None -> invalid_arg ".config/uuid is required in the remote git repository"
    | Some n -> String.trim n

  let pull () =
    store () >>= fun t ->
    Log.info (fun f -> f "pulling repository") ;
    Lwt.catch
      (fun () ->
         Sync.pull_exn t upstream `Set >|= fun _ ->
         Log.info (fun f -> f "repository pulled"))
      (fun e ->
         Log.warn (fun f -> f "failed pull %a" Fmt.exn e);
         Lwt.return ())

  let created_updated_ids commit key =
    store () >>= fun t ->
    Store.history t >>= fun history ->
    let aux commit_id acc =
      Store.of_commit commit_id >>= fun store ->
      acc >>= fun (created, updated, last) ->
      Store.find store key >|= fun data ->
      match data, last with
      | None  , None -> (created, updated, last)
      | None  , Some _ -> (created, updated, last)
      | Some x, Some y when x = y -> (created, updated, last)
      | Some _, None -> (commit_id, commit_id, data)
      | Some _, Some _ -> (created, commit_id, data)
    in
    Topological.fold aux history (Lwt.return (commit, commit, None))

  let date_updated_created key =
    store () >>= fun t  ->
    Store.Head.get t >>= fun head ->
    created_updated_ids head key >>= fun (created_commit_id, updated_commit_id, _) ->
    let to_ptime info =
      Irmin.Info.date info |> Int64.to_float |> Ptime.of_float_s
    in
    Store.Commit.info updated_commit_id |> fun updated ->
    Store.Commit.info created_commit_id |> fun created ->
    match to_ptime updated, to_ptime created with
    | Some a, Some b -> Lwt.return (a, b)
    | _ -> raise (Invalid_argument "date_updated_last")

  let check_redirect content =
    match Astring.String.cut ~sep:"redirect:" content with
    | None -> None
    | Some (_, path) -> Some (Uri.of_string (String.trim path))

  let fill_cache base_uuid =
    let module C = Canopy_content in
    let fn key content cache =
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
          match check_redirect content with
          | None ->
            Log.warn (fun f -> f "Error while parsing %s: %s" uri error) ;
            cache
          | Some uri -> KeyMap.add key (`Redirect uri) cache
    in
    store () >>= fun t ->
    fold t fn KeyMap.empty

  let last_commit_date () =
    store () >>= fun t  ->
    Store.Head.get t >>= fun head ->
    Store.Commit.info head |> fun info ->
    let date = Irmin.Info.date info |> Int64.to_float in
    Ptime.of_float_s date |> function
    | Some o -> Lwt.return o
    | None -> raise (Invalid_argument "date_updated_last")
end
