open Lwt.Infix

type store_ops = {
  subkeys : string list -> string list list Lwt.t ;
  value : string list -> string option Lwt.t ;
  update : unit -> string list Lwt.t
}

module Make (S: Cohttp_lwt.Server) (C: V1_LWT.CONSOLE) (Disk: V1_LWT.KV_RO)
= struct

  let read_fs fs path =
    Disk.size fs path
    >>= function
    | `Error (Disk.Unknown_key _) -> Lwt.return_none
    | `Ok size ->
      Disk.read fs path 0 (Int64.to_int size)
      >>= function
      | `Error (Disk.Unknown_key _) -> Lwt.return_none
      | `Ok bufs -> Lwt.return_some (Cstruct.copyv bufs)

  let moved_permanently uri =
    let headers = Cohttp.Header.init_with "location" (Uri.to_string uri) in
    S.respond ~headers ~status:`Moved_permanently ~body:`Empty ()

  let rec dispatcher config headers console disk store atom cache uri =
    let open Canopy_utils in
    let respond_html ~headers ~status ~content ~title =
      store.subkeys [] >>= fun keys ->
      let body = Canopy_templates.main ~config ~content ~title ~keys in
      let headers = Cohttp.Header.init_with "Content-Type" "text/html; charset=UTF-8" in
      S.respond_string ~status ~headers ~body ()
    and respond_update = function
      | [] -> S.respond_string ~headers ~status:`OK ~body:"" ()
      | errors ->
	let body = List.fold_left (fun a b -> a ^ "\n" ^ b) "" errors in
        S.respond_string ~headers ~status:`Bad_request ~body ()
    in
    match Re_str.split (Re_str.regexp "/") (Uri.pct_decode uri) with
    | [] -> dispatcher config headers console disk store atom cache config.Canopy_config.index_page
    | "static"::_ ->
      begin
        read_fs disk uri >>= function
        | None -> S.respond_string ~headers ~status:`Not_found ~body:"Not found" ()
        | Some body -> 
          let headers = Cohttp.Header.init_with "Content-Type" (Magic_mime.lookup uri) in
          S.respond_string ~status:`OK  ~headers ~body ()
      end
    | "atom" :: [] ->
      atom () >>= fun body ->
      let headers =
        Cohttp.Header.add headers
          "Content-Type" "application/atom+xml; charset=UTF-8"
      in
      S.respond_string ~headers ~status:`OK ~body ()
    | uri::[] when uri = config.Canopy_config.push_hook_path ->
      store.update () >>= fun l ->
      respond_update l
    | "tags"::tagname::_ ->
      let aux _ v l =
        if Canopy_content.find_tag tagname v then (v::l) else l
      in
      let content =
	KeyHashtbl.fold aux cache []
          |> List.sort Canopy_content.compare
	  |> List.map Canopy_content.to_tyxml_listing_entry
          |> Canopy_templates.listing
      in
      respond_html ~headers ~status:`OK ~title:config.Canopy_config.blog_name ~content
    | key ->
      begin
        match KeyHashtbl.find_opt cache key with
        | None ->
          store.subkeys key >>= fun keys ->
          if (List.length keys) = 0 then
            S.respond_string ~headers ~status:`Not_found ~body:"Not found" ()
          else
            let articles = List.map (KeyHashtbl.find_opt cache) keys in
            let content =
	      list_reduce_opt articles
                |> List.sort Canopy_content.compare
		|> List.map Canopy_content.to_tyxml_listing_entry
                |> Canopy_templates.listing in
            respond_html ~headers ~status:`OK ~title:config.Canopy_config.blog_name ~content
        | Some article ->
          let title, content = Canopy_content.to_tyxml article in
          respond_html ~headers ~status:`OK ~title ~content
      end

    let create console dispatch =
      let conn_closed (_, conn_id) =
        let cid = Cohttp.Connection.to_string conn_id in
        C.log console (Printf.sprintf "conn %s closed" cid)
      in
      let callback = match dispatch with
        | `Redirect fn ->
          (fun _ request _ ->
             let req = Cohttp.Request.uri request in
             let uri = fn req in
             C.log_s console (Printf.sprintf "redirecting to %s" (Uri.to_string uri)) >>= fun () ->
             moved_permanently uri)
        | `Dispatch (config, headers, disk, store, atom, content) ->
          (fun _ request _ ->
             let uri = Cohttp.Request.uri request in
             C.log_s console (Printf.sprintf "request %s" (Uri.to_string uri)) >>= fun () ->
             dispatcher config headers console disk store atom content (Uri.path uri))
      in
      S.make ~callback ~conn_closed ()


end
