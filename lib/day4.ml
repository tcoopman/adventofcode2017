module StringSet = Set.Make(String)
let is_valid_passphrase input = 
    let passwords = String.split_on_char ' ' input in
    let unique_passwords = List.fold_left (fun set v -> StringSet.add v set) StringSet.empty passwords in
    (List.length passwords) = (StringSet.elements unique_passwords |> List.length)

let number_valid_passphrases input = 
    input
    |> CCString.lines
    |> List.map is_valid_passphrase
    |> List.filter (fun x -> x)
    |> List.length