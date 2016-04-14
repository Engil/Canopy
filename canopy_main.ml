open Lwt
open V1_LWT

module Main  (C: CONSOLE) (RES: Resolver_lwt.S) (CON: Conduit_mirage.S) (S:Cohttp_lwt.Server) (Disk: KV_RO)  = struct

  let read_fs fs path =
    Disk.size fs path
    >>= function
    | `Error (Disk.Unknown_key _) -> Lwt.return_none
    | `Ok size ->
      Disk.read fs path 0 (Int64.to_int size)
      >>= function
      | `Error (Disk.Unknown_key _) -> Lwt.return_none
      | `Ok bufs -> Lwt.return_some (Cstruct.copyv bufs)

  let start console res ctx http disk _ =

    let open Canopy_config in
    let open Canopy_utils in
    let config = Canopy_config.config () in
    let module Context =
      ( struct
        let v _ = Lwt.return_some (res, ctx)
      end : Irmin_mirage.CONTEXT)
    in
    let module Store = Canopy_store.Store(C)(Context)(Inflator) in

    let content_hashtbl = KeyHashtbl.create 32 in

    let respond_html ~status ~content ~title =
      Store.get_subkeys [] >>= fun keys ->
      let body = Canopy_templates.main ~config ~content ~title ~keys in
      let headers = Cohttp.Header.init_with "Content-Type" "text/html; charset=UTF-8" in
      S.respond_string ~status ~headers ~body () in

    let respond_update = function
      | [] -> S.respond_string ~status:`OK ~body:"" ()
      | errors ->
	 let body = List.fold_left (fun a b -> a ^ "\n" ^ b) "" errors in
	 S.respond_string ~status:`Bad_request ~body () in

    let update_atom, get_atom =
      let cache = ref None in
      let update_atom () =
        let l = KeyHashtbl.fold (fun key x acc -> x :: acc) content_hashtbl []
                |> List.sort Canopy_content.compare
                |> Canopy_utils.resize 10 in
        let entries = List.map Canopy_content.to_atom l in
        let ns_prefix _ = Some "" in
        Store.last_commit_date ()
        >|= fun updated ->
          Syndic.Atom.feed
            ~id:(Uri.of_string config.blog_name)
            ~title:(Syndic.Atom.Text config.blog_name : Syndic.Atom.text_construct)
            ~updated
            ~links:[Syndic.Atom.link ~rel:Syndic.Atom.Self (Uri.of_string "/atom")]
            entries
        |> fun feed -> Syndic.Atom.to_xml feed |> fun x -> Syndic.XML.to_string ~ns_prefix x
        |> fun body -> cache := Some body; body
      in
      (fun () -> ignore (update_atom ()); Lwt.return ()),
      (fun () -> match !cache with
        | Some body -> Lwt.return body
        | None -> update_atom ())
    in

    Store.pull console >>= fun _ ->
    Store.fill_cache content_hashtbl >>= fun l ->
    update_atom () >>= fun () ->
    Lwt_list.iter_p (C.log_s console) l >>= fun () ->

    let rec dispatcher uri =
      let s_uri = Re_str.split (Re_str.regexp "/") (Uri.pct_decode uri) in
      match s_uri with
      | "static"::_ ->
        begin
          read_fs disk uri >>= function
          | None ->
            S.respond_string ~status:`Not_found ~body:"Not found" ()
          | Some body ->
            let headers = Cohttp.Header.init_with "Content-Type" (Magic_mime.lookup uri) in
            S.respond_string ~status:`OK  ~headers ~body ()
        end

      | "atom" :: [] ->
        get_atom () >>= fun body ->
        let headers = Cohttp.Header.init_with "Content-Type" "application/atom+xml; charset=UTF-8" in
        S.respond_string ~status:`OK ~headers ~body ()
      | [] ->
        dispatcher config.index_page

      | uri::[] when uri = config.push_hook_path ->
	 Store.pull console >>= fun _ ->
	 KeyHashtbl.clear content_hashtbl |> Lwt.return >>= fun _ ->
	 Store.fill_cache content_hashtbl >>= fun l ->
   update_atom () >>= fun () ->
	 respond_update l

      | "tags"::tagname::_ ->
	 let aux _ v l =
	   if Canopy_content.find_tag tagname v then (v::l) else l in
      	 let content =
	   KeyHashtbl.fold aux content_hashtbl []
	   |> List.sort Canopy_content.compare
	   |> List.map Canopy_content.to_tyxml_listing_entry
	   |> Canopy_templates.listing in
	 respond_html ~status:`OK ~title:config.blog_name ~content

      | key ->
        begin
          match KeyHashtbl.find_opt content_hashtbl key with
            | None ->
              Store.get_subkeys key >>= fun keys ->
              if (List.length keys) = 0 then
                S.respond_string ~status:`Not_found ~body:"Not found" ()
              else
                let articles = List.map (KeyHashtbl.find_opt content_hashtbl) keys in
                let content =
		  list_reduce_opt articles
		  |> List.sort Canopy_content.compare
		  |> List.map Canopy_content.to_tyxml_listing_entry
		  |> Canopy_templates.listing in
                respond_html ~status:`OK ~title:config.blog_name ~content
            | Some article ->
              let title, content = Canopy_content.to_tyxml article in
              respond_html ~status:`OK ~title ~content
        end

    in
    let callback _ request _ =
      let uri = Cohttp.Request.uri request in
      dispatcher (Uri.path uri)
    in
    let conn_closed (_,conn_id) =
      let cid = Cohttp.Connection.to_string conn_id in
      C.log console (Printf.sprintf "conn %s closed" cid)
    in
    http (`TCP config.port) (S.make ~conn_closed ~callback ())

end
