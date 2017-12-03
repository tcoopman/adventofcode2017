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


module Position = struct
    type t = int * int * int
    let compare (x0, y0, _) (x1, y1, _) =
        match compare x0 x1 with
        | 0 -> compare y0 y1
        | c -> c

    let next (x, y, row) =
        let rowmin = -1 * row in
        match x with
        | _ when x = y && x = abs x && y = abs y -> (x + 1, y, row + 1) 
        | _ when x = row && y = rowmin -> (x-1, y, row)
        | _ when x = rowmin && y = rowmin -> (x, y+1, row) 
        | _ when x = rowmin && y = row -> (x+1, y, row)
        | _ when x = row && y > rowmin -> (x, y-1, row)
        | _ when y = rowmin && x > rowmin -> (x-1, y, row)
        | _ when x = rowmin && y < row -> (x, y+1, row)
        | _ when y = row && x < row -> (x+1, y, row)
        | _ -> (0, 0, 0)

    let neighbours (x, y, row) =
        [(x-1,y-1);(x ,y-1);(x+1,y-1);
         (x-1,  y);         (x+1,  y);
         (x-1,y+1);(x, y+1);(x+1,y+1)
        ]
        |> List.map (fun (x, y) -> (x, y, max (abs x) (abs y)))

end
module Grid = Map.Make(Position)

let find_first_larger number =
    let rec find grid position number =
        let next = Position.next position in
        let neighbours = Position.neighbours next in
        let sum =
            neighbours
            |> List.map (fun x -> Grid.find_opt x grid)
            |> CCList.filter_map (fun x -> x)
            |> List.fold_left (fun acc v -> acc + v) 0
        in
        if sum > number then sum
        else 
            let new_grid = Grid.add next sum grid in
            find new_grid next number 
    in
    let start_grid = Grid.add (0,0,0) 1 Grid.empty in
    find start_grid (0, 0, 0) number 