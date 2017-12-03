open Lib.Day3

let test_find_square () =
    Alcotest.(check int) "1" (find_square 1) 0;
    Alcotest.(check int) "2" (find_square 2) 1;
    Alcotest.(check int) "9" (find_square 9) 1;
    Alcotest.(check int) "10" (find_square 10) 2;
    Alcotest.(check int) "25" (find_square 25) 2;
    Alcotest.(check int) "26" (find_square 26) 3

let test_manhattan_distance () =
    Alcotest.(check int) "1" (manhattan_distance 1) 0;
    Alcotest.(check int) "9" (manhattan_distance 9) 2;
    Alcotest.(check int) "5" (manhattan_distance 5) 2;
    Alcotest.(check int) "8" (manhattan_distance 8) 1;
    Alcotest.(check int) "26" (manhattan_distance 26) 5;
    Alcotest.(check int) "28" (manhattan_distance 28) 3;
    Alcotest.(check int) "1024" (manhattan_distance 1024) 31;
    Alcotest.(check int) "361527" (manhattan_distance 361527) 326

let test_set_star1 = [
  "Find square", `Quick, test_find_square;
  "Manhattan distance", `Quick, test_manhattan_distance;
]


let test_postion_next () =
    Alcotest.(check bool) "" ((Position.next (0,0,0)) = (1,0,1)) true;
    Alcotest.(check bool) "" ((Position.next (2,-2,2)) = (1,-2,2)) true;
    Alcotest.(check bool) "" ((Position.next (-1,0,1)) = (-1,1,1)) true;
    Alcotest.(check bool) "" ((Position.next (0,1,1)) = (1,1,1)) true;
    Alcotest.(check bool) "" ((Position.next (1,0,1)) = (1,-1,1)) true
let test_find_larger () =
    Alcotest.(check int) "1" (find_first_larger 1) 2;
    Alcotest.(check int) "361527" (find_first_larger 361527) 363010

let test_set_star2 = [
  "Find position next", `Quick, test_postion_next;
  "Find first larger", `Quick, test_find_larger;
]