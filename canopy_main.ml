open Lwt
open V1_LWT

module Main  (C: CONSOLE) (S: STACKV4) (RES: Resolver_lwt.S) (CON: Conduit_mirage.S) (CLOCK: V1.CLOCK) (KEYS: KV_RO) = struct

  module TCP  = S.TCPV4
  module TLS  = Tls_mirage.Make (TCP)
  module X509 = Tls_mirage.X509 (KEYS) (CLOCK)

  module HTTP  = Cohttp_mirage.Server(TCP)
  module HTTPS = Cohttp_mirage.Server(TLS)

  module D  = Canopy_dispatch.Make(HTTP)(C)
  module DS = Canopy_dispatch.Make(HTTPS)(C)

  let log c fmt = Printf.ksprintf (C.log c) fmt

  let with_tls c cfg tcp f =
    let peer, port = TCP.get_dest tcp in
    let log str = log c "[%s:%d] %s" (Ipaddr.V4.to_string peer) port str in
    let with_tls_server k = TLS.server_of_flow cfg tcp >>= k in
    with_tls_server @@ function
    | `Error _ -> log "TLS failed"; TCP.close tcp
    | `Ok tls  -> log "TLS ok"; f tls >>= fun () ->TLS.close tls
    | `Eof     -> log "TLS eof"; TCP.close tcp

  let tls_init kv =
    X509.certificate kv `Default >|= fun cert ->
    Tls.Config.server ~certificates:(`Single cert) ()

  let start console stack resolver conduit _clock keys _ =
    let started = match Ptime.of_float_s (CLOCK.time ()) with
      | None -> invalid_arg ("Ptime.of_float_s")
      | Some t -> t
    in
    let module Context =
      ( struct
        let v _ = Lwt.return_some (resolver, conduit)
      end : Irmin_mirage.CONTEXT)
    in
    let module Store = Canopy_store.Store(C)(Context)(Inflator) in
    let open Canopy_utils in
    let cache = ref (KeyMap.empty) in
    Store.pull console >>= fun () ->
    Store.base_uuid () >>= fun uuid ->
    Store.fill_cache uuid cache >>= fun l ->
    let update_atom, atom =
      Canopy_syndic.atom uuid Store.last_commit_date cache
    in
    let store_ops = {
      Canopy_dispatch.subkeys = Store.get_subkeys ;
      value = Store.get_key ;
      update =
        (fun () ->
           Store.pull console >>= fun () ->
           Store.fill_cache uuid cache >>= fun res ->
           update_atom () >|= fun () ->
           res);
      last_commit = Store.last_commit_date ;
    } in
    update_atom () >>= fun () ->
    Lwt_list.iter_p (C.log_s console) l >>= fun () ->
    let disp hdr =
      `Dispatch (hdr, store_ops, atom, cache, started)
    in
    (match Canopy_config.tls_port () with
     | Some tls_port ->
       let redir uri =
         let https = Uri.with_scheme uri (Some "https") in
         let port = match tls_port, Uri.port uri with
           | 443, None -> None
           | _ -> Some tls_port
         in
         Uri.with_port https port
       in
       let http = HTTP.listen (D.create console (`Redirect redir))
       and port = Canopy_config.port ()
       in
       S.listen_tcpv4 stack ~port http ;
       C.log_s console
         (let redirect =
            let req = Uri.of_string "http://127.0.0.1" in
            (* TODO: use own hostname instead of 127.0.0.1 once we have that *)
            Uri.to_string (redir req)
          in
          Printf.sprintf "HTTP server listening on port %d (redirecting to %s)"
            port redirect
         ) >>= fun () ->
       tls_init keys >>= fun tls_conf ->
       let hdr = Cohttp.Header.init_with
           "Strict-Transport-Security" "max-age=31536000" (* in seconds, roughly a year *)
       in
       let callback = HTTPS.listen (DS.create console (disp hdr)) in
       let https flow = with_tls console tls_conf flow callback in
       S.listen_tcpv4 stack ~port:tls_port https ;
       C.log_s console
         (Printf.sprintf "HTTPS server listening on port %d" tls_port)
     | None ->
       let hdr = Cohttp.Header.init () in
       let http = HTTP.listen (D.create console (disp hdr))
       and port = Canopy_config.port ()
       in
       S.listen_tcpv4 stack ~port http ;
       C.log_s console
         (Printf.sprintf "HTTP server listening on port %d" port)
    ) >>= fun () ->
    S.listen stack
end
