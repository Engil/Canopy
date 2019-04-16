open Canopy_utils

type t = {
  title : string;
  content : string;
  author : string;
  author_uri : string option;
  abstract : string option;
  uri : string;
  created: Ptime.t;
  updated: Ptime.t;
  tags: string list;
  uuid: string;
}

let of_string base_uuid meta uri created updated content =
  try
    let split_tags = Re.Str.split (Re.Str.regexp ",") in
    let content = Omd.to_html (Omd.of_string content) in
    let author = List.assoc "author" meta in
    let title = List.assoc "title" meta in
    let tags = assoc_opt "tags" meta |> map_opt split_tags [] |> List.map String.trim in
    let abstract = match assoc_opt "abstract" meta with
        | None -> None
        | Some x -> Some (Omd.to_html (Omd.of_string x))
    in
    let author_uri = assoc_opt "author_url" meta in
    let uuid =
      let open Uuidm in
      let stamp = Ptime.to_rfc3339 created in
      let entry_id = to_string (v5 (create (`V5 (ns_dns, stamp))) base_uuid) in
      Printf.sprintf "urn:uuid:%s" entry_id
    in
    Some {title; content; author; author_uri; uri; abstract; created; updated; tags; uuid}
  with
  | _ -> None

let tags_to_json tags =
  let format_tag tag =
    `O [
      "name", `String tag;
      "uri", `String (Printf.sprintf "/tags/%s" tag);
    ]
  in
  `O ["tags", `A (List.map format_tag tags)]

let to_json article =
  let author =  `String article.author in
  let created = `String (ptime_to_pretty_date article.created) in
  let updated = `String (ptime_to_pretty_date article.updated) in
  let author_uri =
    match article.author_uri with
    | Some uri -> `String uri
    | None -> `Bool false
  in
  let title = `String article.title in
  let content = `String article.content in
  let abstract =
    match article.abstract with
    | Some abstract -> `String abstract
    | None -> `Bool false
  in
  `O [
    "author", author;
    "uri", `String article.uri;
    "created", created;
    "updated", updated;
    "author_uri", author_uri;
    "title", title;
    "content", content;
    "abstract", abstract;
    "tags", tags_to_json article.tags;
  ]

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
