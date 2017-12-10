open Lib.Day10

let test_input_lenghts = [3;4;1;5]
let puzzle_input_lengths = [94;84;0;79;2;27;81;1;123;93;218;23;103;255;254;243]

let test_multiple length input multiplied () =
    Alcotest.(check int) "" (hash length input |> multiply) multiplied

let test_set_star1 = [
  "Test multiply", `Quick, test_multiple 5 test_input_lenghts 12;
  "Test multiply", `Quick, test_multiple 256 puzzle_input_lengths 23715;
]

let test_set_star2 = [
]