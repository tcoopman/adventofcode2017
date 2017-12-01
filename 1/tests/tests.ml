open Lib.Captcha

let test_foo () =
  Alcotest.(check int) "1234 produces 0 because no digit matches the next."  (create_captcha [1;2;3;4]) 0

let test_set = [
  "Foo", `Quick, test_foo;
]

(* Run it *)
let () =
  Alcotest.run "Day 1" [
    "test_1", test_set;
]