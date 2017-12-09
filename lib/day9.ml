type state = Start | Group | Garbage | Ignore

type group = {
    parent: group option;
    score: int;
    children: group list;
    garbage: int;
}

let parse input =
    let chars = CCString.to_list input in
    let rec parse_stream state group chars =
        match (state, group, chars) with 
        | (Start, None, '{':: tl) ->
            let group = Some {parent = None; score = 1; garbage = 0; children = []} in
            parse_stream Group group tl

        | (Group, Some g, '}':: []) ->
            begin match g.parent with
            | None -> Some g
            | Some p -> None
            end

        | (Group, Some g, '{':: tl) ->
            let new_group = {parent = Some g; score = g.score + 1; garbage = 0; children = []} in
            parse_stream Group (Some new_group) tl

        | (Group, Some g, '}':: tl) ->
            begin match g.parent with
            | None -> None
            | Some p -> 
                let parent = {p with children = g :: p.children} in
                parse_stream Group (Some parent) tl
            end

        | (Group, Some g, ','::tl) ->
            parse_stream Group group tl

        | (Group, Some g, '<'::tl) ->
            parse_stream Garbage group tl

        | (Garbage, _, '>'::tl) ->
            parse_stream Group group tl

        | (Garbage, _, '!'::tl) ->
            parse_stream Ignore group tl

        | (Garbage, Some g, _::tl) ->
            let new_group = {g with garbage = g.garbage + 1} in
            parse_stream Garbage (Some new_group) tl

        | (Ignore, _, _::tl) ->
            parse_stream Garbage group tl

        | _ -> None
    in
    parse_stream Start None chars


let rec total_score group =
    group.score + (List.fold_left (fun acc g -> acc + total_score g) 0 group.children) 

let rec total_garbage_score group =
    group.garbage + (List.fold_left (fun acc g -> acc + total_garbage_score g) 0 group.children) 