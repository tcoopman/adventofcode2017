open Lib.Day6

let test_input = "0 2 7 0"

let puzzle_input = "5	1	10	0	1	7	13	14	3	12	8	10	7	12	0	6"

let test_memory_allocation () =
    Alcotest.(check int) "" (memory_allocation test_input |> fst) 5;
    Alcotest.(check int) "" (memory_allocation puzzle_input |> fst ) 5042

let test_memory_allocation_2 () =
    Alcotest.(check int) "" (memory_allocation test_input |> snd) 4;
    Alcotest.(check int) "" (memory_allocation puzzle_input |> snd) 1086

let test_set_star1 = [
  "Memory reallocation", `Quick, test_memory_allocation;
]

let test_set_star2 = [
  "How many cycles", `Quick, test_memory_allocation_2;
]