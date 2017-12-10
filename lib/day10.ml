(* let print l =
    List.iter (fun i -> Printf.printf "%i, " i) l;
    Printf.printf "\n"

let print_t l =
    List.iter (fun (i,v) -> Printf.printf "%i: %i, " i v) l;
    Printf.printf "\n";
    l *)

let reverse_part position length l =
    let arr = Array.of_list l in
    let circular_i i = i mod (Array.length arr) in
    let rec select_indices arr position length indices =
        match length with
        | 0 -> indices
        | _ -> 
            let p = circular_i position in
            select_indices arr (position + 1) (length -1) ((p, arr.(p)) :: indices)
    in
    select_indices arr position length []
    |> CCList.split
    |> (fun (indices, values) -> (indices, List.rev values))
    |> (fun (indices, values) -> CCList.combine indices values)
    |> List.iter (fun (i, v) -> Array.set arr i v);
    Array.to_list arr

let hash list_length lengths =
    let list = CCList.range' 0 list_length in
    let rec hash' list lengths position skip_size =
        match lengths with
        | [] -> list
        | length :: tl ->
            let list = reverse_part position length list in
            hash' list tl (position + length + skip_size) (skip_size + 1)
    in
    hash' list lengths 0 0

let multiply = function
    | hd1 :: hd2 :: _ -> hd1 * hd2
    | _ -> min_int