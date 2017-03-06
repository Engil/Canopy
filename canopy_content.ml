open Canopy_utils


type content_t =
  | Markdown of Canopy_article.t

type error_t =
    Unknown
  | Error of string
  | Ok of content_t


module Canopy_diff = Simple_diff.Make(String)

let string_of_diff diffs =
  let concat symbol lines =
    let lines = List.map (fun line -> symbol ^ " " ^ line) (Array.to_list lines) in
    String.concat "\n" lines
  in
  let stringify str diff =
    let open Canopy_diff in
    match diff with
    | Added lines   -> str ^ concat "+" lines
    | Deleted lines -> str ^ concat "-" lines
    | Equal lines   -> str
  in
  List.fold_left stringify "" diffs


let of_diffs timestamp diffs =
  let fn (key, diff) =
    let date = match timestamp with
      | None -> ""
      | Some timestamp -> ptime_to_pretty_date timestamp
    in
    let file = String.concat "/" key in
    match diff with
    | `Added value ->
      (Printf.sprintf "%s : created (%s)" file date), value
    | `Removed value ->
      (Printf.sprintf "%s : deleted (%s)" file date), value
    | `Updated (old_value, new_value) ->
      Printf.printf "Date: %s\n" date;
      Printf.printf "File: %s\n" file;
      Printf.printf "Old_value: \n%s\n" old_value;
      Printf.printf "New_value: \n%s\n" new_value;
      let new_lines = Re_str.split (Re_str.regexp "\n") new_value |> Array.of_list in
      let old_lines = Re_str.split (Re_str.regexp "\n") old_value |> Array.of_list in
      let content = Canopy_diff.get_diff old_lines new_lines |> string_of_diff in
      (Printf.sprintf "%s : edited (%s)" file date), content
  in
  List.map fn diffs

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
