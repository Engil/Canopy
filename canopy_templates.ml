open Canopy_config
open Canopy_utils
open Tyxml.Html

let empty =
  div []

let taglist tags =
  let format_tag tag =
    let taglink = Printf.sprintf "/tags/%s" in
    a ~a:[taglink tag |> a_href; a_class ["tag"]] [pcdata tag] in
  match tags with
  | [] -> empty
  | tags ->
     let tags = List.map format_tag tags in
     div ~a:[a_class ["tags"]] ([pcdata "Classified under: "] ++ tags)

let links keys =
  let paths = List.map (function
			 | x::_ -> x
			 | _ -> assert false
		       ) keys |> List.sort_uniq (Pervasives.compare) in
  let format_link link =
    li [ a ~a:[a_href ("/" ^ link)] [span [pcdata link]]] in
  List.map format_link paths

let main ~cache ~content ~title ~keys =
  let links = links keys in
  let page =
    html
      (head
         (Tyxml.Html.title (pcdata title))
         ([
           meta ~a:[a_charset "UTF-8"] ();
           link ~rel:[`Stylesheet] ~href:"/static/css/bootstrap.min.css" ();
           link ~rel:[`Stylesheet] ~href:"/static/css/style.css" ();
           link ~rel:[`Stylesheet] ~href:"/static/css/highlight.css" ();
           script ~a:[a_src "/static/js/canopy.js"] (pcdata "");
           link ~rel:[`Alternate] ~href:"/atom" ~a:[a_title title; a_mime_type "application/atom+xml"] ();
         ])
      )
      (body
         [
           nav ~a:[a_class ["navbar navbar-default navbar-fixed-top"]] [
             div ~a:[a_class ["container"]] [
               div ~a:[a_class ["navbar-header"]] [
                 button ~a:[a_class ["navbar-toggle collapsed"];
                            a_user_data "toggle" "collapse";
                            a_user_data "target" ".navbar-collapse"
                           ] [
                   span ~a:[a_class ["icon-bar"]][];
                   span ~a:[a_class ["icon-bar"]][];
                   span ~a:[a_class ["icon-bar"]][]
                 ];
                 a ~a:[a_class ["navbar-brand"]; a_href ("/" ^ index_page cache)][pcdata (blog_name cache)]
               ];
               div ~a:[a_class ["collapse navbar-collapse collapse"]] [
                 ul ~a:[a_class ["nav navbar-nav navbar-right"]] links
               ]
             ]
           ];
           main [
             div ~a:[a_class ["flex-container"]] content
           ]
         ]
      )
  in
  let buf = Buffer.create 500 in
  let fmt = Format.formatter_of_buffer buf in
  pp () fmt page ;
  Buffer.contents buf

let listing entries =
  [div ~a:[a_class ["flex-container"]] [
	 div ~a:[a_class ["list-group listing"]] entries
       ]
  ]

let error msg =
  [div ~a:[a_class ["alert alert-danger"]] [pcdata msg]]
