let whitespace_to_int input =
    input
    |> Str.split (Str.regexp "[\t| ]")
    |> List.map(int_of_string_opt)
    |> CCOpt.sequence_l

let find_max_index a =
    CCArray.foldi (fun (max,max_i) i number -> if number > max then (number,i) else (max,max_i)) (min_int, 0) a

let rec distribute a (blocks, index) =
    if blocks == 0 then a
    else
        let next_index = (index + 1) mod Array.length a in
        Array.set a next_index (a.(next_index) + 1);
        distribute a (blocks -1, next_index)

module IntList = struct
    type t = int list

    let compare = compare
end
module ListSet = Set.Make(IntList)


let do_distribute input =
    let input = Array.of_list input in
    let (blocks, index) = find_max_index input in
    Array.set input index 0;
    distribute input (blocks, index) |> Array.to_list

let memory_allocation input = 
    let rec find_redistribution_cycles found count input =
        match ListSet.mem input found with
        | true -> count
        | false ->
            let new_memory = do_distribute input in
            find_redistribution_cycles (ListSet.add input found) (count + 1) new_memory
    in
    match whitespace_to_int input with
    | None -> min_int
    | Some l -> find_redistribution_cycles ListSet.empty 0 l