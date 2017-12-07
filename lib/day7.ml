module StringMap = Map.Make(String)

module Tower = struct
    type t = {
        name: string;
        weight: int;
        children: t list
    }

    type unparsed_t = {
        name: string;
        weight: int;
        children: string list
    }

    let make name weight children : t =
        {
            name;
            weight;
            children
        }
    let parse input = 
        let parse_line line =
            let parse_name_weight str =
                let split_groups = Str.string_match (Str.regexp {|\([a-z]+\) (\([0-9]+\))|}) str 0 in
                let name = Str.matched_group 1 str in
                let weight = Str.matched_group 2 str |> int_of_string in
                match split_groups with
                | true -> (name, weight)
                | false -> ("", 0)
            in
            let split_children = Str.split (Str.regexp " -> ") line in
            match split_children with
            | [] -> None
            | hd :: [] ->
                let (name, weight) = parse_name_weight hd in
                Some { name; weight; children = []}
            | hd :: children :: [] ->  
                let (name, weight) = parse_name_weight hd in
                let children_names = CCString.split ~by:", " children in
                Some { name; weight; children = children_names}
            | _ -> None
        in
        let unparsed =
            input
            |> CCString.lines
            |> List.map parse_line
            |> CCOpt.sequence_l
        in
        let rec try_build_children children build build_children =
            match children with
            | [] -> (Some build_children, build)
            | child_name :: tl ->
                match StringMap.find_opt child_name build with
                | None -> (None, build)
                | Some child -> try_build_children tl (StringMap.remove child_name build) (child::build_children)
        in
        let rec build_tower names_to_build to_build build = 
            match names_to_build with
            | [] ->
                StringMap.find_first (fun _ -> true) build |> snd
            | name :: tl ->
                match StringMap.find_opt name to_build with
                | Some unparsed ->
                    begin match unparsed.children with
                    | [] -> 
                        let new_to_build = StringMap.remove name to_build in
                        let parsed = make unparsed.name unparsed.weight [] in
                        let new_build = StringMap.add name parsed build in
                        build_tower tl new_to_build new_build
                    | _ ->
                        begin match try_build_children unparsed.children build [] with
                        | (None, _) -> build_tower (List.append tl [name]) to_build build
                        | (Some build_children, build) ->
                            let new_to_build = StringMap.remove name to_build in
                            let parsed = make unparsed.name unparsed.weight build_children in
                            let new_build = StringMap.add name parsed build in
                            build_tower tl new_to_build new_build
                        end
                    end
                | None -> StringMap.find_first (fun _ -> true) build |> snd
        in
        match unparsed with
        | Some unparsed -> 
            let names_to_build = unparsed |> List.map (fun u -> u.name) in
            let to_build = List.fold_left (fun map u -> StringMap.add u.name u map) StringMap.empty unparsed in
            build_tower names_to_build to_build StringMap.empty
        | None ->
            {name = "incorrect input"; weight = min_int; children = []}

    let rec sum (t: t) =
        t.weight + (List.fold_left (fun acc program -> acc + (sum program)) 0 t.children)

    let rec find_weight_to_correct (bottom: t) =
        let module IntMap = Map.Make(struct type t = int let compare = compare end) in
        let counted children = List.fold_left (fun acc e -> 
            match IntMap.find_opt (sum e) acc with
            | None -> IntMap.add (sum e) [e] acc
            | Some x -> IntMap.add (sum e) (e::x) acc
        ) IntMap.empty children
        in
        let difference = 
            let counts = IntMap.fold (fun _key el acc -> (List.hd el |> sum) :: acc) (counted bottom.children) [] in
            let min = List.fold_left min max_int counts in
            let max = List.fold_left max min_int counts in
            (max - min)
        in
        let rec incorrect_balanced (item:t) =
            let counted = counted item.children in
            let incorrect_sub_tree = IntMap.find_first_opt (fun key -> 
                (IntMap.find key counted) |> List.length = 1
            ) counted in
            match incorrect_sub_tree with
            | None -> 
                item.weight - difference
            | Some (_, t) -> 
                incorrect_balanced (List.hd t)
        in
        incorrect_balanced bottom
end