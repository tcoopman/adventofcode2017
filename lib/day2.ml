let parse_input input =
    let to_int l =
        l
        |> List.map(int_of_string_opt)
        |> CCOpt.sequence_l
    in
    input
    |> CCString.lines
    |> List.map(Str.split (Str.regexp "[\t| ]"))
    |> List.map(to_int)
    |> CCOpt.sequence_l

let spreadsheet input = 
    let max l = CCList.fold_left CCInt.max min_int l in
    let min l = CCList.fold_left CCInt.min max_int l in
    CCList.fold_left (fun acc val' -> acc + (max val'-min val')) 0 input

let spreadsheet2 input = 
    let rec rowval l = 
        CCList.find_mapi (fun i number1 -> 
            CCList.foldi (fun acc j number2 -> 
                match acc with
                | Some _ -> acc
                | None when i = j -> acc
                | None ->
                    if (number1 mod number2) = 0 then Some (number1 / number2) else None
            ) None l
        ) l
        |> CCOpt.get_exn
    in
    CCList.fold_left (fun acc val' -> acc + (rowval val')) 0 input