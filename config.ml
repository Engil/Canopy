open Mirage

let disk =
  let fs_key = Key.(value @@ kv_ro ()) in
  let fat_ro dir = generic_kv_ro ~key:fs_key dir in
  fat_ro "./disk"


(* Command-line options *)

let index_k =
  let doc = Key.Arg.info ~doc:"Index file name in remote." ["i"; "index"] in
  Key.(create "index" Arg.(opt string "Index" doc))

let name_k =
  let doc = Key.Arg.info ~doc:"Blog name." ["n"; "name"] in
  Key.(create "name" Arg.(opt string "Canopy" doc))

let port_k =
  let doc = Key.Arg.info ~doc:"Socket port." ["p"; "port"] in
  Key.(create "port" Arg.(opt int 8080 doc))

let push_hook_k =
  let doc = Key.Arg.info ~doc:"GitHub push hook." ["hook"] in
  Key.(create "push_hook" Arg.(opt string "push" doc))

let remote_k =
  let doc = Key.Arg.info ~doc:"Remote repository to fetch content." ["r"; "remote"] in
  Key.(create "remote" Arg.(opt string "https://github.com/Engil/__blog.git" doc))

let mathjax_k =
  let doc = Key.Arg.info ~doc:"Enable mathjax" ["mathjax"; "m"] in
  Key.(create "mathjax" Arg.(flag doc))

(* Dependencies *)

let libraries = [
    "cow";
    "ptime";
    "decompress";
    "irmin.mirage";
    "irmin.git";
    "mirage-http";
    "tls.mirage";
    "tyxml";
    "syndic";
  ]

let packages = [
    "cow";
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
  ]


(* Network stack *)

let stack =
  match get_mode () with
  | `Xen -> generic_stackv4 default_console tap0
  | `Unix | `MacOSX -> socket_stackv4 default_console [Ipaddr.V4.any]

let conduit_d = conduit_direct ~tls:true stack
let http_srv = http_server conduit_d
let res_dns = resolver_dns stack


let main =
  let keys = Key.([
                     abstract index_k;
                     abstract name_k;
                     abstract port_k;
                     abstract push_hook_k;
                     abstract remote_k;
                     abstract mathjax_k
                   ]) in
  foreign
    ~libraries
    ~deps:[abstract nocrypto]
    ~keys
    ~packages
    "Canopy_main.Main" (console @-> resolver @-> conduit @-> http @-> kv_ro @-> job)

let () =
  register "canopy" [
    main $ default_console $ res_dns $ conduit_d $ http_srv $ disk
  ]
