open Canopy_utils

let decompose_git_url url =
  match String.rindex url '#' with
  | exception Not_found -> (url, None)
  | i ->
    let remote_url = String.sub url 0 i in
    let branch = String.sub url (i + 1) (String.length url - i - 1) in
    (remote_url, Some branch)

let remote_uri () = fst (decompose_git_url (Key_gen.remote ()))
let remote_branch () = snd (decompose_git_url (Key_gen.remote ()))
let port () = Key_gen.port ()
let tls_port () = Key_gen.tls_port ()
let push_hook_path () = Key_gen.push_hook ()

let entry name = [ ".config" ; name ]

let index_page cache =
  match KeyMap.find_opt cache @@ entry "index_page" with
  | Some (`Config p) -> p
  | _ -> "Index"

let blog_name cache =
  match KeyMap.find_opt cache @@ entry "blog_name" with
  | Some (`Config n) -> n
  | _ -> "Canopy"

let root cache =
  match KeyMap.find_opt cache @@ entry "root" with
  | Some (`Config r) -> r
  | _ -> "http://localhost"
