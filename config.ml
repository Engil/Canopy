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

let packages = [
  package "omd" ;
  package "tyxml";
  package "ptime";
  package "decompress";
  package "irmin";
  package "irmin-mirage";
  package "mirage-http";
  package "mirage-flow";
  package ~sublibs:["mirage"] "tls";
  package "re";
  package "cohttp";
  package "syndic";
  package "magic-mime";
  package "uuidm";
  package "logs";
]


(* Network stack *)
let stack =
  if_impl Key.is_unix
    (socket_stackv4 [Ipaddr.V4.any])
    (generic_stackv4 default_network)

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
      ~deps:[abstract nocrypto]
      ~keys
      ~packages
      "Canopy_main.Main"
      (stackv4 @-> resolver @-> conduit @-> pclock @-> kv_ro @-> job)
    $ stack
    $ resolver_dns stack
    $ conduit_direct ~tls:true stack
    $ default_posix_clock
    $ crunch "tls"
  ]
