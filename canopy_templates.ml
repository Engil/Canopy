open Canopy_config
open Canopy_utils

let get_template cache name =
  let key = [ ".templates"; name ] in
  match KeyMap.find_opt cache key with
  | Some `Template template -> template
  | _ ->
    Logs.err (fun f -> f "template %s not found" name) ;
    assert false

let render_template_try name t j =
  try
    Mustache.render t j
  with
  | exn ->
    Logs.err (fun f -> f "exn while rendering template %s: %a" name Fmt.exn exn);
    raise exn

let tags ~cache tags =
  let template = get_template cache "tags.mustache" in
  render_template_try "tags" template tags

let article ~cache article =
  let template = get_template cache "article.mustache" in
  render_template_try "article" template article

let articles_listing ~cache articles =
  let template = get_template cache "listing_entry.mustache" in
  render_template_try "articles_listing" template articles 

let main ~cache ~content ~title ~site_name ~index_uri ~keys =
  let template = get_template cache "main.mustache" in
  let pages = List.map (function
    | x::_ -> `O ["name", `String x; "uri", `String ("/" ^ x)]
    | _ -> assert false) keys
  in
  let json =
    `O [
      "title", `String title;
      "content", `String content;
      "site_name", `String site_name;
      "site_index", `String index_uri;
      "pages", `A pages
    ]
  in
  render_template_try "main" template json
