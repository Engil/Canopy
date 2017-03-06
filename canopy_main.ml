open Lwt
open Mirage_types_lwt
open Result
open X509.Encoding

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

  let appends = function
    | []   -> Cstruct.create 0
    | [cs] -> cs
    | csn  ->
       let cs = Cstruct.(create @@ lenv csn) in
       let _ =
          List.fold_left
            (fun off e ->
             let len = Cstruct.len e in
             ( Cstruct.blit e 0 cs off len ; off + len ))
            0 csn in
       cs

  let readf kv name =
    let open Lwt_result in
    KEYS.size kv name >>= fun size ->
    KEYS.read kv name 0L size >>= fun cdata ->
    return (appends cdata)

  let writef key token =
    Log.info (fun f -> f "%s %s" key token)

  (* XXX. stolen from ocaml-acme source code.
     Maybe woth exposing *)
  let priv_of_pem rsa_pem =
    let rsa_pem = Cstruct.of_string rsa_pem in
    let maybe_rsa = Pem.Private_key.of_pem_cstruct rsa_pem in
    match maybe_rsa with
    | [`RSA key] -> Some key
    | _ -> None

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

  let acme_init kv =
    Log.info (fun f -> f "no TLS keys found. Attempting to create an ACME cert.");
    (* XXX. this whole thing can probably  be compressed a bit
             by somebody knowing Lwt_result.
             More than this, the whole error handling is fucked up: I don't know
             how to handle KEYS.Error properly.
     *)
    readf kv "account.key" >>= function
    | Error e -> fail_with "Cannot find account key"
    | Ok rsa_pem ->
       let rsa_pem = Cstruct.to_string rsa_pem in
       readf kv "csr.pem"  >>=  function
       | Error e -> fail_with "Cannot find certificate request"
       | Ok csr_pem ->
          let csr_pem = Cstruct.to_string csr_pem in
          let directory_url = Acme.letsencrypt_staging_url in
          (* XXX. this is wrong. *)
          Log.info (fun f -> f "%s\n%s" rsa_pem csr_pem) ;
          Acme_client.get_crt directory_url rsa_pem csr_pem writef >>= function
          | Error e -> fail_with e
          | Ok crt ->
             let certs = Cstruct.of_string crt |>
                         Pem.Certificate.of_pem_cstruct in
             match priv_of_pem rsa_pem with
             | None -> fail_with "error parsing private key"
             | Some pk -> return (certs, pk)

  let tls_init cert =
    (* X509.certificate kv `Default >|= fun cert -> *)
    Tls.Config.server ~certificates:(`Single cert) () |> return

  let http_redirect_https tls_port uri =
    let https = Uri.with_scheme uri (Some "https") in
    let port = match tls_port, Uri.port uri with
      | 443, None -> None
      | _ -> Some tls_port
    in
    Uri.with_port https port

  let start_https tls_port stack keys disp =
    let redir = http_redirect_https tls_port in
    let http_callback = HTTP.listen (D.create (`Redirect redir)) in
    let http flow = with_tcp flow http_callback
    and port = Canopy_config.port ()
    in
    S.listen_tcpv4 stack ~port http ;
    Log.info (fun f -> f "HTTP server listening on port %d, \
                          redirecting to https service on port %d"
                         port tls_port) ;
    (* XXX. if the keys are not provided, attempt to create an ACME account *)
    acme_init keys >>= fun cert ->
    (* XXX. if the keys are already present in tls/, just use them *)
    tls_init cert >>= fun tls_conf ->
    let hdr = Cohttp.Header.init_with
                "Strict-Transport-Security" "max-age=31536000" (* in seconds, roughly a year *)
    in
    let callback = HTTPS.listen (DS.create (disp hdr)) in
    let https flow = with_tls tls_conf flow callback in
    S.listen_tcpv4 stack ~port:tls_port https ;
    Log.info (fun f -> f "HTTPS server listening on port %d" tls_port) ;
    Lwt.return_unit

  let start_http stack disp =
    let hdr = Cohttp.Header.init () in
    let http_callback = HTTP.listen (D.create (disp hdr)) in
    let http flow = with_tcp flow http_callback
    and port = Canopy_config.port ()
    in
    S.listen_tcpv4 stack ~port http ;
    Log.info (fun f -> f "HTTP server listening on port %d" port) ;
    Lwt.return_unit


  let start stack resolver conduit _clock keys _ =
    let (module Context) = Irmin_mirage.context (resolver, conduit) in
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
     | Some tls_port -> start_https tls_port stack keys disp
     | None -> start_http stack disp
    ) >>= fun () ->
    S.listen stack
end
