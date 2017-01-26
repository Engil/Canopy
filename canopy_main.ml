open Lwt
open Mirage_types_lwt
open Result

module Main (S: STACKV4) (RES: Resolver_lwt.S) (CON: Conduit_mirage.S) (CLOCK: PCLOCK) (KEYS: KV_RO) = struct

  module TCP  = S.TCPV4
  module TLS  = Tls_mirage.Make (TCP)
  module X509 = Tls_mirage.X509 (KEYS) (CLOCK)

  module HTTP  = Cohttp_mirage.Server(TCP)
  module HTTPS = Cohttp_mirage.Server(TLS)

  module D  = Canopy_dispatch.Make(HTTP)
  module DS = Canopy_dispatch.Make(HTTPS)

  let src = Logs.Src.create "canopy-main" ~doc:"Canopy main logger"
  module Log = (val Logs.src_log src : Logs.LOG)

  let with_tls cfg tcp f =
    let peer, port = TCP.dst tcp in
    TLS.server_of_flow cfg tcp >>= function
    | Error e ->
      Log.warn (fun f -> f "%s:%d TLS failed %a" (Ipaddr.V4.to_string peer) port TLS.pp_write_error e);
      TCP.close tcp
    | Ok tls ->
      Log.info (fun f -> f "%s:%d TLS ok" (Ipaddr.V4.to_string peer) port);
      f tls >>= fun () -> TLS.close tls

  let with_tcp tcp f =
    let peer, port = TCP.dst tcp in
    Log.info (fun f -> f "%s:%d TCP established" (Ipaddr.V4.to_string peer) port);
    f tcp >>= fun () -> TCP.close tcp

  let tls_init kv =
    X509.certificate kv `Default >|= fun cert ->
    Tls.Config.server ~certificates:(`Single cert) ()

  let start stack resolver conduit _clock keys _ =
    let module Context =
      ( struct
        let v _ = Lwt.return_some (resolver, conduit)
      end : Irmin_mirage.CONTEXT)
    in
    let module Store = Canopy_store.Store(Context)(Inflator) in
    Store.pull () >>= fun () ->
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
           Store.pull () >>= fun () ->
           Store.fill_cache uuid >>= fun new_cache ->
           cache := new_cache ;
           update_atom ());
      last_commit = Store.last_commit_date ;
    } in
    update_atom () >>= fun () ->
    let disp hdr = `Dispatch (hdr, store_ops, atom, cache) in
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
       let http_callback = HTTP.listen (D.create (`Redirect redir)) in
       let http flow = with_tcp flow http_callback
       and port = Canopy_config.port ()
       in
       S.listen_tcpv4 stack ~port http ;
       Log.info (fun f -> f "HTTP server listening on port %d, \
                             redirecting to https service on port %d"
                    port tls_port) ;
       tls_init keys >|= fun tls_conf ->
       let hdr = Cohttp.Header.init_with
           "Strict-Transport-Security" "max-age=31536000" (* in seconds, roughly a year *)
       in
       let callback = HTTPS.listen (DS.create (disp hdr)) in
       let https flow = with_tls tls_conf flow callback in
       S.listen_tcpv4 stack ~port:tls_port https ;
       Log.info (fun f -> f "HTTPS server listening on port %d" tls_port)
     | None ->
       let hdr = Cohttp.Header.init () in
       let http_callback = HTTP.listen (D.create (disp hdr)) in
       let http flow = with_tcp flow http_callback
       and port = Canopy_config.port ()
       in
       S.listen_tcpv4 stack ~port http ;
       Log.info (fun f -> f "HTTP server listening on port %d" port) ;
       Lwt.return_unit
    ) >>= fun () ->
    S.listen stack
end
