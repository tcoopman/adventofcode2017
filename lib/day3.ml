let find_square number =
    let is_odd x = x mod 2 = 1 in
    if number <= 1 
    then 0
    else
        number
        |> float_of_int
        |> sqrt
        |> ceil
        |> int_of_float
        |> (fun x -> if is_odd x then x else x + 1)
        |> fun x -> x /2

let manhattan_distance number =
    let max_side_distance = find_square number in
    let side_length = max_side_distance * 2 + 1 in
    if number <= 1 then 0
    else
        let is_square = number mod (side_length - 1) = 1 in
        let side_diff = 
            let center = max_side_distance + 1 in
            let modded = number mod (side_length - 1) |> (fun x ->
                if x = 0 then (side_length -1) else x
            ) in
            center - modded |> abs
        in
        if is_square then max_side_distance + max_side_distance 
        else max_side_distance + side_diff
