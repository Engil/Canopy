open Canopy_utils
open Astring

type content_t =
  | Markdown of Canopy_article.t

type error_t =
    CUnknown
  | CError of string
  | COk of content_t

let meta_assoc str =
  String.cuts ~sep:"\n" str |>
  List.map (fun meta ->
    match String.cut ~sep:":" meta with
    | None -> "", meta
    | Some (k,v) -> k, (String.trim v))

let of_string ~uri ~created ~updated ~content =
  let splitted_content = Re_str.bounded_split (Re_str.regexp "---") content 2 in
  let article_of_meta meta uri created updated content = 
    Canopy_article.of_string meta uri created updated content
    |> map_opt (fun article -> COk (Markdown article)) (CError "Error while parsing article")
  in
  match splitted_content with
  | [raw_meta;raw_content] ->
    begin
      match meta_assoc raw_meta with
      | meta ->
        begin
          match assoc_opt "content" meta with
          | Some "markdown"
          | None -> article_of_meta meta uri created updated raw_content
          | Some _ -> CUnknown
        end
      | exception _ -> CUnknown
    end
  | _ -> article_of_meta [] uri created updated content

let to_tyxml = function
  | Markdown m ->
    let open Canopy_article in
    m.title, to_tyxml m

let to_tyxml_listing_entry = function
  | Markdown m -> Canopy_article.to_tyxml_listing_entry m

let to_atom = function
  | Markdown m -> Canopy_article.to_atom m

let find_tag tagname = function
  | Markdown m ->
    List.exists ((=) tagname) m.Canopy_article.tags

let date = function
  | Markdown m ->
    m.Canopy_article.created

let compare a b = Ptime.compare (date b) (date a)

let updated = function
  | Markdown m ->
    m.Canopy_article.updated

let tags content_map =
  let module S = Set.Make(String) in
  let s = KeyMap.fold (
      fun _k v s -> match v with
        | Markdown m ->
          let s' = S.of_list m.Canopy_article.tags in
          S.union s s')
      content_map S.empty
  in S.elements s
