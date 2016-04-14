let assoc_opt k l =
  match List.assoc k l with
  | v -> Some v
  | exception Not_found -> None

let map_opt fn default = function
  | None -> default
  | Some v -> fn v

let list_reduce_opt l =
  let rec aux acc = function
    | [] -> acc
    | (Some x)::xs -> aux (x::acc) xs
    | None::xs -> aux acc xs
  in
  aux [] l

let default_opt default = function
  | None -> default
  | Some v -> v

let resize len l =
  List.fold_left
    (fun (len, acc) x ->
      if len > 0
      then (len - 1, x :: acc)
      else (0, acc))
    (len, []) l
  |> fun (_, l) -> List.rev l

let (++) = List.append

let ptime_to_pretty_date t =
  Ptime.to_date t |> fun (y, m, d) ->
    Printf.sprintf "%02d.%02d.%04d" d m y

module KeyHashtbl = struct
    module KeyHash = struct
	type t = string list
	let equal = (=)
	let hash = Hashtbl.hash
      end

    module H = Hashtbl.Make(KeyHash)
    include H

    let find_opt t k =
      try Some (H.find t k) with
      | Not_found -> None
end
