let () =
  Alcotest.run "Day 1" [
    "Star 1", Day1.test_set_star1;
    "Star 2", Day1.test_set_star2;
    "Star 3", Day2.test_set_star1;
    "Star 4", Day2.test_set_star2;
    "Star 5", Day3.test_set_star1;
    "Star 6", Day3.test_set_star2;
    "Star 7", Day4.test_set_star1;
]