open Mirage

(* Command-line options *)

let push_hook_k =
  let doc = Key.Arg.info ~doc:"GitHub push hook." ["hook"] in
  Key.(create "push_hook" Arg.(opt string "push" doc))

let remote_k =
  let doc = Key.Arg.info ~doc:"Remote repository to fetch content.\
                             \ Use suffix #foo to specify a branch 'foo':\
                             \ https://github.com/user/blog.git#content"
      ["r"; "remote"] in
  Key.(create "remote" Arg.(opt string "https://github.com/Engil/__blog.git" doc))

let port_k =
  let doc = Key.Arg.info ~doc:"Socket port." ["p"; "port"] in
  Key.(create "port" Arg.(opt int 8080 doc))

let tls_port_k =
  let doc = Key.Arg.info ~doc:"Enable TLS (using keys in `tls/`) on given port." ["tls"] in
  Key.(create "tls_port" Arg.(opt (some int) None doc))

(* Dependencies *)

let libraries = [
    "omd" ;
    "ptime";
    "decompress";
    "irmin.mirage";
    "irmin.git";
    "mirage-http";
    "tls.mirage";
    "tyxml";
    "syndic";
    "uuidm";
  ]

let packages = [
    "omd" ;
    "tyxml";
    "ptime";
    "decompress";
    "irmin";
    "mirage-http";
    "mirage-flow";
    "tls";
    "mirage-types-lwt";
    "channel";
    "mirage-git";
    "re";
    "cohttp";
    "syndic";
    "magic-mime";
    "uuidm";
  ]


(* Network stack *)

let stack =
  match get_mode () with
  | `Xen -> generic_stackv4 default_console tap0
  | `Unix | `MacOSX -> socket_stackv4 default_console [Ipaddr.V4.any]

let () =
  let keys = Key.([
      abstract push_hook_k;
      abstract remote_k;
      abstract port_k;
      abstract tls_port_k;
    ])
  in
  register "canopy" [
    foreign
      ~libraries
      ~deps:[abstract nocrypto]
      ~keys
      ~packages
      "Canopy_main.Main"
      (console @-> stackv4 @-> resolver @-> conduit @-> clock @-> kv_ro @-> job)
    $ default_console
    $ stack
    $ resolver_dns stack
    $ conduit_direct ~tls:true stack
    $ default_clock
    $ crunch "tls"
  ]
