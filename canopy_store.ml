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

  let created_commit_id commit keys =
    repo () >>= fun repo ->
    Store.master task repo >>= fun t  ->
    Store.history (t "Reading history") >>= fun history ->
    let rec aux last_commit visited to_visit =
      match to_visit with
      | [] -> Lwt.return last_commit
      | commit::to_visit ->
	 Store.of_commit_id (Irmin.Task.none) commit repo >>= fun store ->
	 Store.read (store ()) keys >>= fun readed_file ->
	 let visited = commit::visited in
	 match readed_file with
	 | Some _ ->
	    let to_visit =
	      ( match Store.History.pred history commit with
		| [] -> to_visit
		| pred::pred2::[] ->
		   let to_visit = if ((List.mem pred visited) = false) then pred::to_visit else to_visit in
		   let to_visit = if ((List.mem pred2 visited) = false) then pred2::to_visit else to_visit in
		   to_visit
		| pred::[] ->
		   let to_visit = if ((List.mem pred visited) = false) then pred::to_visit else to_visit in
		   to_visit
		| q -> List.append (List.rev q) to_visit)
	    in aux commit visited to_visit
	 | None -> Lwt.return last_commit in
    aux commit [] [commit]

let last_updated_commit_id head key =
  repo () >>= fun repo ->
  new_task () >>= fun t  ->
  Store.read_exn (t "Reading file") key >>= fun current_file ->
  Store.history (t "Reading history") >>= fun history ->
  let rec aux ucommit visited to_visit =
    match to_visit with
    | [] -> Lwt.return ucommit
    | commit::to_visit ->
       Store.of_commit_id (Irmin.Task.none) commit repo >>= fun store ->
       Store.read (store ()) key >>= fun readed_file ->
       let visited = commit::visited in
       (match readed_file with
	 | Some readed_file ->
	    let matched = (String.compare readed_file current_file) = 0 in
	    let to_visit =
	      if matched then
		(
		  match Store.History.pred history commit with
		  | [] -> to_visit
		  | pred::pred2::[] ->
		     let to_visit = if ((List.mem pred visited) = false) then pred::to_visit else to_visit in
		     let to_visit = if ((List.mem pred2 visited) = false) then pred2::to_visit else to_visit in
		     to_visit
		  | pred::[] ->
		     let to_visit = if ((List.mem pred visited) = false) then pred::to_visit else to_visit in
		     to_visit
		  | q -> List.append (List.rev q) to_visit
		) else to_visit
	    in
	    if matched then
	      aux commit visited to_visit
	    else
	      aux ucommit visited []
	 | None -> aux ucommit visited to_visit)
  in
  aux head [] [head]

  let date_updated_created key =
    new_task () >>= fun t  ->
    repo () >>= fun repo ->
    Store.head_exn (t "Finding head") >>= fun head ->
    last_updated_commit_id head key >>= fun updated_commit_id ->
    created_commit_id head key >>= fun created_commit_id ->
    Store.Repo.task_of_commit_id repo updated_commit_id >>= fun task ->
    let date = Irmin.Task.date task |> Int64.to_float in
    let updated_date = Ptime.of_float_s date in
    Store.Repo.task_of_commit_id repo created_commit_id >>= fun task ->
    let date = Irmin.Task.date task |> Int64.to_float in
    let created_date = Ptime.of_float_s date in
    match updated_date, created_date with
    | Some a, Some b -> Lwt.return (a, b)
    | _ -> raise (Invalid_argument "date_updated_last")

  let fill_cache article_hashtbl =
    let open Canopy_content in
    let key_to_path key = List.fold_left (fun a b -> a ^ "/" ^ b) "" key in
    let fold_fn key value acc =
      value >>= fun content ->
      date_updated_created key >>= fun (updated, created) ->
      let uri = List.fold_left (fun s a -> s ^ "/" ^ a) "" key in
      match of_string ~uri ~content ~created ~updated with
	| Ok article -> (KeyHashtbl.replace article_hashtbl key article; Lwt.return acc)
	| Error error ->
	   let error_msg = Printf.sprintf "Error while parsing %s: %s" (key_to_path key) error in
	   Lwt.return (error_msg::acc)
	| Unknown ->
	   let error_msg = Printf.sprintf "%s : Unknown content type" (key_to_path key) in
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
