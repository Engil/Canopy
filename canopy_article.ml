open Canopy_utils
open Html5.M

type t = {
  title : string;
  content : string;
  author : string;
  abstract : string option;
  uri : string;
  created: Ptime.t;
  updated: Ptime.t;
  tags: string list;
}

let of_string meta uri created updated content =
  try
    let split_tags = Re_str.split (Re_str.regexp ",") in
    let content = Cow.Markdown.of_string content |> Cow.Html.to_string in
    let author = List.assoc "author" meta in
    let title = List.assoc "title" meta in
    let tags = assoc_opt "tags" meta |> map_opt split_tags [] |> List.map String.trim in
    let abstract = assoc_opt "abstract" meta in
    Some {title; content; author; uri; abstract; created; updated; tags}
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
	 Html5.M.article [Unsafe.data article.content]
       ]]

let to_tyxml_listing_entry article =
  let author = "Written by " ^ article.author in
  let abstract = match article.abstract with
    | None -> []
    | Some abstract -> [p ~a:[a_class ["list-group-item-text abstract"]] [pcdata abstract]] in
  let content = [
      h4 ~a:[a_class ["list-group-item-heading"]] [pcdata article.title];
      span ~a:[a_class ["author"]] [pcdata author];
      br ();
    ] in
  a ~a:[a_href article.uri; a_class ["list-group-item"]] (content ++ abstract)

let to_atom ({ title; author; abstract; uri; created; updated; tags; content; } as article) =
  let text x : Syndic.Atom.text_construct = Syndic.Atom.Text x in
  let summary = match abstract with
    | Some x -> Some (text x)
    | None -> None
  in
  let categories =
    List.map
      (fun x -> Syndic.Atom.category ~scheme:(Uri.of_string ("/tags/" ^ x)) x)
      tags
  in
  let generate_id ?(root = "") { created; uri; _ } =
    let d, m, y = Ptime.to_date created in
    let relatif = Uri.path @@ Uri.of_string uri in
    let ts = Ptime.Span.to_int_s @@ Ptime.to_span created in
    Printf.sprintf "tag:%s,%d-%d-%d:%s/%a" root d m y relatif
      (fun () -> function Some a -> string_of_int a | None -> "") ts
    |> Uri.of_string
  in
  Syndic.Atom.entry
    ~id:(generate_id article)
    ~content:(Syndic.Atom.Html (None, content))
    ~authors:(Syndic.Atom.author author, [])
    ~title:(text title)
    ~published:created
    ~updated
    ?summary
    ~categories
    ~links:[Syndic.Atom.link ~rel:Syndic.Atom.Alternate (Uri.of_string uri)]
    ()
