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

let rec hash' list lengths position skip_size =
    match lengths with
    | [] -> (list, position, skip_size)
    | length :: tl ->
        let list = reverse_part position length list in
        hash' list tl (position + length + skip_size) (skip_size + 1)

let hash list_length lengths =
    let list = CCList.range' 0 list_length in
    let (output, _, _) = hash' list lengths 0 0 in
    output

let multiply = function
    | hd1 :: hd2 :: _ -> hd1 * hd2
    | _ -> min_int

let convert_to_input_list input =
    input |> CCString.to_list |> List.map int_of_char |> (fun l -> List.concat [l; [17;31;73;47;23]])

let rec sparse_hash rounds input = 
    let list = CCList.range' 0 256 in
    let rec round r input l position skip_size =
        match r with
        | 0 -> l
        | _ -> 
            let (l, position, skip_size) = hash' l input position skip_size in
            round (r-1) input l position skip_size
    in
    round rounds input list 0 0

let dense_hash sparse =
    CCList.sublists_of_len 16 sparse
    |> List.map (List.fold_left (fun acc i -> acc lxor i) 0)
    |> List.map (Printf.sprintf "%02x")
    |> String.concat ""

let knot_hash input =
    input
    |> convert_to_input_list
    |> sparse_hash 64
    |> dense_hash