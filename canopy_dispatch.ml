open Lwt.Infix


type store_ops = {
  subkeys : string list -> string list list Lwt.t ;
  value : string list -> string option Lwt.t ;
  update : unit -> unit Lwt.t ;
  last_commit : unit -> Ptime.t Lwt.t ;
}

module Make (S: Cohttp_lwt.S.Server) = struct

  let src = Logs.Src.create "canopy-dispatch" ~doc:"Canopy dispatch logger"
  module Log = (val Logs.src_log src : Logs.LOG)

  let moved_permanently uri =
    let headers = Cohttp.Header.init_with "location" (Uri.to_string uri) in
    S.respond ~headers ~status:`Moved_permanently ~body:`Empty ()

  let rec dispatcher headers store atom cache uri etag =
    let open Canopy_utils in
    let respond_not_found () =
      S.respond_string ~headers ~status:`Not_found ~body:"Not found" ()
    in
    let respond_if_modified ~headers ~body ~updated =
      match etag with
      | Some tg when Ptime.to_rfc3339 updated = tg ->
        S.respond ~headers ~status:`Not_modified ~body:`Empty ()
      | _ ->
        S.respond_string ~headers ~status:`OK ~body ()
    in
    let respond_html ~headers ~content ~title ~updated =
      store.subkeys [] >>= fun keys ->
      let site_name = Canopy_config.blog_name !cache in
      let index_uri = Canopy_config.index_page !cache in
      let body = Canopy_templates.main ~cache:(!cache) ~content ~title ~site_name ~index_uri ~keys in
      let headers = html_headers headers updated in
      respond_if_modified ~headers ~body ~updated
    and respond_update () = S.respond_string ~headers ~status:`OK ~body:"" ()
    in
    match Re.Str.split (Re.Str.regexp "/") (Uri.pct_decode uri) with
    | "templates" :: [] -> respond_not_found ()
    | [] ->
      let index_page = Canopy_config.index_page !cache in
      dispatcher headers store atom cache index_page etag
    | "atom" :: [] ->
      atom () >>= fun body ->
      store.last_commit () >>= fun updated ->
      let headers = atom_headers headers updated in
      respond_if_modified ~headers ~body ~updated
    | uri::[] when uri = Canopy_config.push_hook_path () ->
      store.update () >>= fun () ->
      respond_update ()
    | "tags"::[] -> (
        let tags = Canopy_content.tags !cache in
        let json = Canopy_article.tags_to_json tags in
        let content = Canopy_templates.tags ~cache:!cache json in
        store.last_commit () >>= fun updated ->
        let title = Canopy_config.blog_name !cache in
        respond_html ~headers ~title ~content ~updated
      )
    | "tags"::tagname::_ -> (
        let aux _ v l =
          if Canopy_content.find_tag tagname v then (v::l) else l
        in
        let sorted = KeyMap.fold_articles aux !cache [] |> List.sort Canopy_content.compare in
        match sorted with
        | [] -> respond_not_found ()
        | _ ->
          let updated = List.hd (List.rev (List.sort Ptime.compare (List.map Canopy_content.updated sorted))) in
          let content = sorted
                        |> List.map Canopy_content.to_json
                        |> List.map snd
                        |> fun arr -> `O ["articles", `A arr]
                        |> Canopy_templates.articles_listing ~cache:!cache
          in
          let title = Canopy_config.blog_name !cache in
          respond_html ~headers ~title ~content ~updated
      )
    | key ->
      begin
        match KeyMap.find_opt !cache key with
        | None
        | Some (`Template _ )
        | Some (`Config _ ) -> (
            store.subkeys key >>= function
            | [] -> respond_not_found ()
            | keys ->
              let articles = List.map (KeyMap.find_article_opt !cache) keys |> list_reduce_opt in
              match articles with
              | [] -> respond_not_found ()
              | _ -> (
                  let sorted = List.sort Canopy_content.compare articles in
                  let updated = List.hd (List.rev (List.sort Ptime.compare (List.map Canopy_content.updated articles))) in
                  let content = sorted
                                |> List.map Canopy_content.to_json
                                |> List.map snd
                                |> fun arr -> `O ["articles", `A arr]
                                |> Canopy_templates.articles_listing ~cache:!cache
                  in
                  let title = Canopy_config.blog_name !cache in
                  respond_html ~headers ~title ~content ~updated
                ))
        | Some (`Article article) ->
          let title, content = Canopy_content.to_json article in
          let updated = Canopy_content.updated article in
          let content = Canopy_templates.article ~cache:!cache content in
          respond_html ~headers ~title ~content ~updated
        | Some (`Raw (body, updated)) ->
          let headers = static_headers headers uri updated in
          respond_if_modified ~headers ~body ~updated
        | Some (`Redirect uri) ->
          moved_permanently uri
      end

  (* maybe this should be provided elsewhere *)
  let log request response =
    let open Cohttp in
    let sget k = match Header.get request.Request.headers k with
      | None -> "-"
      | Some x -> x
    in
    Log.info (fun f ->
        f "\"%s %s %s\" %d \"%s\" \"%s\""
          (Code.string_of_method request.Request.meth)
          request.Request.resource
          (Code.string_of_version request.Request.version)
          (Code.code_of_status response.Response.status)
          (sget "Referer")
          (sget "User-Agent"))

  let create dispatch =
    let conn_closed (_, conn_id) =
      let cid = Cohttp.Connection.to_string conn_id in
      Log.debug (fun f -> f "conn %s closed" cid)
    in
    let callback = match dispatch with
      | `Redirect fn ->
        (fun _ request _ ->
           let redirect = fn (Cohttp.Request.uri request) in
           moved_permanently redirect >|= fun (res, body) ->
           log request res ;
           (res, body))
      | `Dispatch (headers, store, atom, content) ->
        (fun _ request _ ->
           let uri = Cohttp.Request.uri request in
           let etag = Cohttp.Header.get Cohttp.Request.(request.headers) "if-none-match" in
           dispatcher headers store atom content (Uri.path uri) etag >|= fun (res, body) ->
           log request res ;
           (res, body))
    in
    S.make ~callback ~conn_closed ()
end
