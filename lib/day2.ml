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