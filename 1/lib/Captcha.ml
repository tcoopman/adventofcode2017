let rec parse_input input =
    input
    |> CCString.to_list
    |> List.map(CCString.of_char)
    |> List.map(int_of_string_opt)
    |> CCOpt.sequence_l


let sum_captcha input =
    let make_circular =
        match input with
        | [] | _::[] -> input
        | _ -> (List.hd (List.rev input)) :: input
    in
    let rec sum l =
        match l with
        | [] | _::[] -> 0
        | hd::snd::tl when hd = snd ->  hd + (sum (snd::tl))
        | hd::snd::tl -> sum (snd::tl)
    in
    sum make_circular