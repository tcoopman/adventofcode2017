let jump_outside_length input =
    let parsed_input = 
        input
        |> CCString.lines
        |> List.map(int_of_string_opt)
        |> CCOpt.sequence_l
        |> CCOpt.map Array.of_list
    in
    let rec find_outside_jump position number_of_jumps jump_instructions =
        if position < 0 || position >= Array.length jump_instructions
        then number_of_jumps
        else
            let current_jump_instruction = jump_instructions.(position) in
            Array.set jump_instructions position (current_jump_instruction + 1);
            find_outside_jump (position + current_jump_instruction) (number_of_jumps + 1) jump_instructions
    in
    match (parsed_input) with
    | None -> min_int
    | Some i -> find_outside_jump 0 0 i
