open Lwt.Infix

module Main (_: sig end) (Http : Cohttp_mirage.Server.S) (CLOCK: Mirage_clock.PCLOCK) (KEYS: Mirage_kv.RO) = struct

  module X509 = Tls_mirage.X509 (KEYS) (CLOCK)

  module D  = Canopy_dispatch.Make(Http)

  let src = Logs.Src.create "canopy-main" ~doc:"Canopy main logger"
  module Log = (val Logs.src_log src : Logs.LOG)

  let tls_init kv =
    X509.certificate kv `Default >|= fun cert ->
    Tls.Config.server ~certificates:(`Single cert) ()

  module Store = Canopy_store

  let start ctx http _clock keys =
    Store.pull ~ctx >>= fun () ->
    Store.base_uuid () >>= fun uuid ->
    Store.fill_cache uuid >>= fun new_cache ->
    let cache = ref (new_cache) in
    let update_atom, atom =
      Canopy_syndic.atom uuid Store.last_commit_date cache
    in
    let store_ops = {
      Canopy_dispatch.subkeys = Store.get_subkeys ;
      value = Store.get_key ;
      update =
        (fun () ->
           Store.pull ~ctx >>= fun () ->
           Store.fill_cache uuid >>= fun new_cache ->
           cache := new_cache ;
           update_atom ());
      last_commit = Store.last_commit_date ;
    } in
    update_atom () >>= fun () ->
    let disp hdr = `Dispatch (hdr, store_ops, atom, cache) in
    match Canopy_config.tls_port () with
    | Some tls_port ->
      let http =
        let redir uri =
          let https = Uri.with_scheme uri (Some "https") in
          let port = match tls_port, Uri.port uri with
            | 443, None -> None
            | _ -> Some tls_port
          in
          Uri.with_port https port
        in
        let port = Canopy_config.port () in
        Log.info (fun f -> f "HTTP server listening on port %d, \
                              redirecting to https service on port %d"
                     port tls_port) ;
        http (`TCP port) (D.create (`Redirect redir))
      and https =
        tls_init keys >>= fun tls_conf ->
        let hdr = Cohttp.Header.init_with
            "Strict-Transport-Security" "max-age=31536000" (* in seconds, roughly a year *)
        in
        Log.info (fun f -> f "HTTPS server listening on port %d" tls_port);
        http (`TLS (tls_conf, `TCP tls_port)) (D.create (disp hdr))
      in
      Lwt.join [ http ; https ]
    | None ->
      let hdr = Cohttp.Header.init ()
      and port = Canopy_config.port ()
      in
      Log.info (fun f -> f "HTTP server listening on port %d" port) ;
      http (`TCP port) (D.create (disp hdr))
end
