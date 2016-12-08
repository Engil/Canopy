open Canopy_utils


type content_t =
  | Markdown of Canopy_article.t

type error_t =
    Unknown
  | Error of string
  | Ok of content_t

type diff_lines_t =
  | Deleted of string array
  | Added of string array
  | Equal of string array


let rec get_diff old_lines new_lines =
  match (old_lines, new_lines) with
  | ([||], [||]) -> []
  | _ ->
    let old_index_map = Hashtbl.create 3000 in

    for i = 0 to Array.length old_lines - 1 do
      let line = old_lines.(i) in
      if Hashtbl.mem old_index_map line then
        let old_val = Hashtbl.find old_index_map line in
        Hashtbl.replace old_index_map line (Array.append old_val [|i|]);
      else
        Hashtbl.add old_index_map line [|i|];
    done;

    let overlap = ref (Hashtbl.create 0) in
    let sub_start_old = ref 0 in
    let sub_start_new = ref 0 in
    let longest_subsequence = ref 0 in

    for new_index = 0 to Array.length new_lines - 1 do
      let new_overlap = Hashtbl.create 3000 in
      let indices = try Hashtbl.find old_index_map new_lines.(new_index) with
        | Not_found -> [||]
      in
      for i = 0 to Array.length indices - 1 do
        let old_index = indices.(i) in
        let new_subsequence = (try Hashtbl.find !overlap (old_index - 1) with
          | Not_found -> 0) + 1
        in
        Hashtbl.add new_overlap old_index new_subsequence;

        if new_subsequence > !longest_subsequence then
          let () = Printf.printf "new_index - %i; old_index - %i; sub_start_new: %i\n" new_index old_index !sub_start_new in
          sub_start_old := old_index - new_subsequence + 1;
          sub_start_new := new_index - new_subsequence + 1;
          longest_subsequence := new_subsequence;
      done;
      overlap := new_overlap;
    done;

    if !longest_subsequence == 0 then
      [Deleted old_lines; Added new_lines]
    else
      let old_lines_length = Array.length old_lines in
      let new_lines_length = Array.length new_lines in
      let () = Printf.printf "old_lines_length: %i\n" old_lines_length in
      let () = Printf.printf "new_lines_length: %i\n" new_lines_length in
      Printf.printf "sub_start_old: %i\n" !sub_start_old;
      Printf.printf "sub_start_new: %i\n" !sub_start_new;
      let old_lines_presubseq = Array.sub old_lines 0 !sub_start_old in
      let new_lines_presubseq = Array.sub new_lines 0 !sub_start_new in
      Printf.printf "old_subsequence: %i\n" (!sub_start_old + !longest_subsequence);
      Printf.printf "new_subsequence: %i\n" (!sub_start_new + !longest_subsequence);
      let old_lines_postsubseq =
        let starting_index = !sub_start_old + !longest_subsequence in
        Array.sub old_lines starting_index (old_lines_length - starting_index)
      in
      Printf.printf "new_lines\n";
      let new_lines_postsubseq =
        let starting_index = !sub_start_new + !longest_subsequence in
        Array.sub new_lines starting_index (new_lines_length - starting_index)
      in
      let unchanged_lines = Array.sub new_lines !sub_start_new !longest_subsequence in
      get_diff old_lines_presubseq new_lines_presubseq @
      [Equal unchanged_lines] @
      get_diff old_lines_postsubseq new_lines_postsubseq


let string_of_diff diffs =
  let concat symbol lines =
    let lines = List.map (fun line -> symbol ^ " " ^ line) (Array.to_list lines) in
    String.concat "\n" lines
  in
  let stringify str diff =
    match diff with
    | Added lines   -> str ^ concat "+" lines
    | Deleted lines   -> str ^ concat "-" lines
    | Equal lines -> str
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
    | `Updated (new_value, old_value) ->
      let new_lines = Re_str.split (Re_str.regexp "\n") new_value |> Array.of_list in
      let old_lines = Re_str.split (Re_str.regexp "\n") old_value |> Array.of_list in
      let content = get_diff old_lines new_lines |> string_of_diff in
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
