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

let sum_captcha_2 input =
    let input = Array.of_list input in
    let half = Array.length input/2 in
    let matching i = (i + half) mod (Array.length input) in
    CCArray.foldi (fun acc i val' -> 
        match input.(matching i) with
        | x when x = val' -> acc + val'
        | _ -> acc
    ) 0 input
