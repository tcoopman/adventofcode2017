open Lib.Day10

let test_input_lenghts = [3;4;1;5]
let puzzle_input_lengths = [94;84;0;79;2;27;81;1;123;93;218;23;103;255;254;243]

let test_multiple length input multiplied () =
    Alcotest.(check int) "" (hash length input |> multiply) multiplied

let test_set_star1 = [
  "Test multiply", `Quick, test_multiple 5 test_input_lenghts 12;
  "Test multiply", `Quick, test_multiple 256 puzzle_input_lengths 23715;
]

let puzzle_input = "94,84,0,79,2,27,81,1,123,93,218,23,103,255,254,243"

let test_convert_to_input_list () =
    Alcotest.(check (list int)) "" (convert_to_input_list "1,2,3") [49;44;50;44;51;17;31;73;47;23]

let test_convert_to_knop_hash () =
    Alcotest.(check string) "" (knot_hash "") "a2582a3a0e66e6e86e3812dcb672a272"

let test_puzzle () =
    Alcotest.(check string) "" (knot_hash puzzle_input) "541dc3180fd4b72881e39cf925a50253"

let test_set_star2 = [
    "Input list", `Quick, test_convert_to_input_list;
    "Test knot hash", `Quick, test_convert_to_knop_hash;
    "Test puzzle", `Quick, test_puzzle;
]