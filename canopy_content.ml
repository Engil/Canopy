open Canopy_utils

type content_t =
  | Markdown of Canopy_article.t

type error_t =
    Unknown
  | Error of string
  | Ok of content_t

let meta_assoc str =
  Re_str.split (Re_str.regexp "\n") str |>
  List.map (fun meta ->
      let reg = Re_str.regexp "\\(.*\\): \\(.*\\)" in
      let _ = Re_str.string_match reg meta 0 in
      let key = Re_str.matched_group 1 meta in
      let value = Re_str.matched_group 2 meta in
      key, value)

let of_string ~base_uuid ~uri ~created ~updated ~content =
  let splitted_content = Re_str.bounded_split (Re_str.regexp "---") content 2 in
  match splitted_content with
  | [raw_meta;raw_content] ->
    begin
      match meta_assoc raw_meta with
      | meta ->
        begin
          match assoc_opt "content" meta with
          | Some "markdown"
          | None ->
            Canopy_article.of_string base_uuid meta uri created updated raw_content
            |> map_opt (fun article -> Ok (Markdown article)) (Error "Error while parsing article")
          | Some _ -> Unknown
        end
      | exception _ -> Unknown
    end
  | _ -> Error "No header found"

let to_tyxml = function
  | Markdown m ->
    let open Canopy_article in
    m.title, to_tyxml m

let to_tyxml_listing_entry = function
  | Markdown m -> Canopy_article.to_tyxml_listing_entry m

let to_atom cache = function
  | Markdown m -> Canopy_article.to_atom cache m

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
  let s = KeyMap.fold_articles (
      fun _k v s -> match v with
        | Markdown m ->
          let s' = S.of_list m.Canopy_article.tags in
          S.union s s')
      content_map S.empty
  in S.elements s
