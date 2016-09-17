open Mirage

(* Shell commands to run at configure time *)
type shellconfig = ShellConfig
let shellconfig = Type ShellConfig

let no_assets_k =
  let doc = Key.Arg.info ~doc:"Don't compile assets at configure time" [ "no-assets-compilation" ] in
  Key.(create "no_assets" Arg.(flag ~stage:`Configure doc))

let config_shell = impl @@ object
    inherit base_configurable

    method configure i =
      let open Functoria_app.Cmd in
      let (>>=) = Rresult.(>>=) in
      let dir = Info.root i in

      run "mkdir -p %s" (dir ^ "/disk/static/js") >>= fun () ->
      run "mkdir -p %s" (dir ^ "/disk/static/css") >>= fun () ->
      if Key.get (Info.context i) no_assets_k
      then Rresult.Ok ()
      else
        let npm_query = run "which npm" |> Rresult.R.is_ok in
        let lessc_query = run "which lessc" |> Rresult.R.is_ok in
        let browserify_query = run "which browserify" |> Rresult.R.is_ok in
        if (npm_query && lessc_query && browserify_query) then
          (Printf.printf "npm, browserify and lessc found… fetching and compiling all assets\n";
           run "npm install" >>= fun () ->
           run "browserify assets/js/main.js -o disk/static/js/canopy.js" >>= fun () ->
           run "lessc assets/less/style.less disk/static/css/style.css --source-map-map-inline --strict-imports" >>= fun () ->
           run "cp node_modules/bootstrap/dist/css/bootstrap.min.css disk/static/css/bootstrap.min.css" >>= fun () ->
           run "cp node_modules/highlight.js/styles/grayscale.css disk/static/css/highlight.css" >>= fun () ->
           Printf.printf "Compressing compiled assets to assets/assets_generated.tar.gz…\n";
           run "tar -cf assets/assets_generated.tar.gz disk/")
        else
          (Printf.printf "npm, browserify and lessc not found… decompressing from assets/assets_generated.tar.gz\n";
           run "tar -xf assets/assets_generated.tar.gz")

    method clean i = Functoria_app.Cmd.run "rm -rf node_modules disk"

    method module_name = "Functoria_runtime"
    method name = "shell_config"
    method ty = shellconfig
  end

(* disk device *)

let disk =
  let fs_key = Key.(value @@ kv_ro ()) in
  let fat_ro dir = generic_kv_ro ~key:fs_key dir in
  fat_ro "./disk"

(* Command-line options *)

let root_k =
  let doc = Key.Arg.info ~doc:"Blog URL" ["root"] in
  Key.(create "root" Arg.(opt string "http://localhost" doc))

let uuid_k =
  let doc = Key.Arg.info ~doc:"UUID used as atom feed id." ["u"; "uuid"] in
  Key.(create "uuid" Arg.(required string doc))

let index_k =
  let doc = Key.Arg.info ~doc:"Index file name in remote." ["i"; "index"] in
  Key.(create "index" Arg.(opt string "Index" doc))

let tls_port_k =
  let doc = Key.Arg.info ~doc:"Enable TLS (using keys in `tls/`) on given port." ["t"; "tls"] in
  Key.(create "tls_port" Arg.(opt (some int) None doc))

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
  if_impl Key.is_unix
    (socket_stackv4 [Ipaddr.V4.any])
    (generic_stackv4 tap0)

let () =
  let keys = Key.([
      abstract index_k;
      abstract name_k;
      abstract port_k;
      abstract push_hook_k;
      abstract remote_k;
      abstract tls_port_k;
      abstract no_assets_k;
      abstract uuid_k;
      abstract root_k;
    ])
  in
  register "canopy" [
    foreign
      ~libraries
      ~deps:[abstract nocrypto; abstract config_shell]
      ~keys
      ~packages
      "Canopy_main.Main"
      (console @-> stackv4 @-> resolver @-> conduit @-> kv_ro @-> pclock @-> kv_ro @-> job)
    $ default_console
    $ stack
    $ resolver_dns stack
    $ conduit_direct ~tls:true stack
    $ disk
    $ default_posix_clock
    $ crunch "tls"
  ]
