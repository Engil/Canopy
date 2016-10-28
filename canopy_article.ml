open Canopy_utils
open Tyxml.Html

type t = {
  title : string;
  content : string;
  author : string;
  abstract : string option;
  uri : string;
  created: Ptime.t;
  updated: Ptime.t;
  tags: string list;
  uuid: string;
}

let of_string base_uuid meta uri created updated content =
  try
    let split_tags = Re_str.split (Re_str.regexp ",") in
    let content = Omd.to_html (Omd.of_string content) in
    let author = List.assoc "author" meta in
    let title = List.assoc "title" meta in
    let tags = assoc_opt "tags" meta |> map_opt split_tags [] |> List.map String.trim in
    let abstract = assoc_opt "abstract" meta in
    let uuid =
      let open Uuidm in
      let stamp = Ptime.to_rfc3339 created in
      let entry_id = to_string (v5 (create (`V5 (ns_dns, stamp))) base_uuid) in
      Printf.sprintf "urn:uuid:%s" entry_id
    in
    Some {title; content; author; uri; abstract; created; updated; tags; uuid}
  with
  | _ -> None

let to_tyxml article =
  let author = "Written by " ^ article.author in
  let created = ptime_to_pretty_date article.created in
  let updated = ptime_to_pretty_date article.updated in
  let updated = String.concat " "
      [ "Published:" ; created ; "(last updated:" ; updated ^ ")" ]
  in
  let tags = Canopy_templates.taglist article.tags in
  [div ~a:[a_class ["post"]] [
      h2 [pcdata article.title];
      span ~a:[a_class ["author"]] [pcdata author];
      br ();
      tags;
      span ~a:[a_class ["date"]] [pcdata updated];
      br ();
      Tyxml.Html.article [Unsafe.data article.content]
    ]]

let to_tyxml_listing_entry article =
  let author = "Written by " ^ article.author in
  let abstract = match article.abstract with
    | None -> []
    | Some abstract -> [p ~a:[a_class ["list-group-item-text abstract"]] [pcdata abstract]] in
  let created = ptime_to_pretty_date article.created in
  let content = [
    h4 ~a:[a_class ["list-group-item-heading"]] [pcdata article.title];
    span ~a:[a_class ["author"]] [pcdata author];
    pcdata " ";
    pcdata "("; time [pcdata created]; pcdata ")";
    br ();
  ] in
  a ~a:[a_href ("/" ^ article.uri); a_class ["list-group-item"]] (content ++ abstract)

let to_tyxml_tags tags =
  let format_tag tag =
    let taglink = Printf.sprintf "/tags/%s" in
    a ~a:[taglink tag |> a_href; a_class ["list-group-item"]] [pcdata tag] in
  let html = match tags with
    | [] -> div []
    | tags ->
      let tags = List.map format_tag tags in
      p ~a:[a_class ["tags"]] tags
  in
  [div ~a:[a_class ["post"]] [
      h2 [pcdata "Tags"];
      div ~a:[a_class ["list-group listing"]] [html]]]

let to_atom cache ({ title; author; abstract; uri; created; updated; tags; content; uuid}) =
  let text x : Syndic.Atom.text_construct = Syndic.Atom.Text x in
  let summary = match abstract with
    | Some x -> Some (text x)
    | None -> None
  in
  let root = Canopy_config.root cache
  in
  let categories =
    List.map
      (fun x -> Syndic.Atom.category ~scheme:(Uri.of_string (root ^ "/tags/" ^ x)) x)
      tags
  in
  Syndic.Atom.entry
    ~id:(Uri.of_string uuid)
    ~content:(Syndic.Atom.Html (None, content))
    ~authors:(Syndic.Atom.author author, [])
    ~title:(text title)
    ~published:created
    ~updated
    ?summary
    ~categories
    ~links:[Syndic.Atom.link ~rel:Syndic.Atom.Alternate (Uri.of_string uri)]
    ()
