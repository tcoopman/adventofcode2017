module StringSet = Set.Make(String)
let is_valid_passphrase input = 
    let passwords = String.split_on_char ' ' input in
    let unique_passwords = List.fold_left (fun set v -> StringSet.add v set) StringSet.empty passwords in
    (List.length passwords) = (StringSet.elements unique_passwords |> List.length)

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let is_valid_passphrase_anagram input = 
    let sorted word = 
        word
        |> explode
        |> List.sort compare
        |> CCString.of_list
    in
    let passwords = String.split_on_char ' ' input in
    let unique_passwords = List.fold_left (fun set v -> StringSet.add (sorted v) set) StringSet.empty passwords in
    (List.length passwords) = (StringSet.elements unique_passwords |> List.length)

let number_valid_passphrases is_valid input = 
    input
    |> CCString.lines
    |> List.map is_valid
    |> List.filter (fun x -> x)
    |> List.length