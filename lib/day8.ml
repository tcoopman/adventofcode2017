module Instruction = struct
    type action = Inc of int | Dec of int
    type comparison = Eq | Ne | Lt | Le | Ge | Gt
    type t = {
        register: string;
        action: action;
        condition: comparison * string * int
    }

    let parse input : t option = 
        let line_regex = Str.regexp {|\([a-z]+\) \([a-z]+\) \([-0-9]+\) if \([a-z]+\) \([>=<!]+\) \([-0-9]+\)|} in
        let split_groups = Str.string_match line_regex input 0 in
        match split_groups with
        | false -> None
        | true -> 
            let register = Str.matched_group 1 input in
            let matchedAction = Str.matched_group 2 input in
            let matchedActionInt = Str.matched_group 3 input in
            let conditionRegister = Str.matched_group 4 input in
            let comparison = Str.matched_group 5 input in
            let value = Str.matched_group 6 input in
            let action = match matchedAction with
            | "inc" -> Some (Inc (int_of_string matchedActionInt))
            | "dec" -> Some (Dec (int_of_string matchedActionInt))
            | _ -> None
            in
            let condition = match comparison with
            | "==" -> Some (Eq, conditionRegister, (int_of_string value))
            | ">" -> Some (Gt, conditionRegister, (int_of_string value))
            | ">=" -> Some (Ge, conditionRegister, (int_of_string value))
            | "<" -> Some (Lt, conditionRegister, (int_of_string value))
            | "<=" -> Some (Le, conditionRegister, (int_of_string value))
            | "!=" -> Some (Ne, conditionRegister, (int_of_string value))
            | _ -> None
            in
            CCOpt.map2 (fun action condition -> {
                register;
                action;
                condition
            }) action condition

    let test instruction value =
        match instruction.condition with
        | (Eq, _, i) -> value = i
        | (Gt, _, i) -> value > i
        | (Ge, _, i) -> value >= i
        | (Lt, _, i) -> value < i
        | (Le, _, i) -> value <= i
        | (Ne, _, i) -> value <> i

    let takeAction instruction value =
        match instruction.action with
        | Inc i -> value + i
        | Dec i -> value - i
end

let parse input =
    input
    |> CCString.lines
    |> List.map Instruction.parse
    |> CCOpt.sequence_l


module Cpu = struct
    module StringMap = Map.Make(String)
    type t = int StringMap.t


    let findRegisterValue t key =
        match StringMap.find_opt key t with
        | None -> 0
        | Some i -> i

    let executeInstruction cpu instruction =
        match instruction.Instruction.condition with
        | (_, key, v) -> 
            if Instruction.test instruction (findRegisterValue cpu key) then
                Instruction.takeAction instruction (findRegisterValue cpu instruction.register)
                |> (fun v -> StringMap.add instruction.register v cpu)
            else
                cpu

    let execute instructions =
        List.fold_left (fun cpu instruction -> executeInstruction cpu instruction) StringMap.empty instructions

    let largest cpu =
        StringMap.fold (fun _key v max -> if v > max then v else max) cpu min_int 
end
