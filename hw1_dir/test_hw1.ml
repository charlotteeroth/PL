open Hw1

let q1 () =
  let l1 =
    [(990.1, false); (493.82, false); (864.38, true); (711.52, false);
    (322.46, false); (117.88, true); (748.95, true); (8.02, false);
    (239.78, true); (492.77, false); (983.72, true); (714.95, false);
    (994.49, true); (553.23, false); (23.65, false); (649.03, true);
    (339.25, true); (307.02, false); (509.99, true); (74.1, false)] in
  assert (true_product l1 = 2.01015167311184823e+24);
  assert (true_product' l1 = 2.01015167311184823e+24)

let q2 () =
  let t1 = Nil in
  assert (valid_bst t1 = true);

  let t2 = Node (Nil, false, Nil) in
  assert (valid_bst t2 = true);

  let t3 = 
    Node (
      Node (
        Node (
          Node (
            Node (
              Nil, 10, Node (
                Nil, 6, Nil
              )
            ), 8, Node (
              Node (
                Nil, 8, Nil
              ), 9, Nil
            )
          ), 1, Nil
        ), 2, Node (
          Nil, 6, Nil
        )
      ), 7, Nil
    ) in
  assert (valid_bst t3 = false);

  let t4 = 
    Node (
      Node (
        Node (Nil, false, Nil), false, Nil
      ), true, Nil
    ) in
  assert (valid_bst t4 = false);

  let t5 = 
    Node (
      Node (
        Node (
          Nil, 10, Node (
            Nil, 11, Nil
          )
        ), 12, Nil
      ), 15, Nil
    ) in
  assert (valid_bst t5 = true)

let q3 () =
  let t1 = Nil in
  assert (search_tree t1 true = None);

  let t2 = Node (Nil, (10, false), Nil) in
  assert (search_tree t2 10 = Some false);

  let t3 = 
    Node (
      Node (
        Node (
          Node (
            Node (
              Nil, (10, "ten"), Node (
                Nil, (6, "six"), Nil
              )
            ), (8, "eight"), Node (
              Node (
                Nil, (7, "seven"), Nil
              ), (9, "nine"), Nil
            )
          ), (1, "one"), Nil
        ), (2, "two"), Node (
          Nil, (16, "sixteen"), Nil
        )
      ), (17, "seventeen"), Nil
    ) in
  assert (search_tree t3 1 = Some "one");
  assert (search_tree t3 11 = None)

let all_interval_set : interval_set = [Range (Int.min_int, Int.max_int)]

let difference_intervals : interval -> interval -> interval_set = fun i1 i2 ->
  remove_empty_intervals (intersect_interval_set_with_interval (complement_interval i2) i1)

let difference_interval_set_with_interval : interval_set -> interval -> interval_set = fun s i ->
  List.concat_map (Fun.flip difference_intervals i) s

let difference : interval_set -> interval_set -> interval_set = Fun.flip (List.fold_right (Fun.flip difference_interval_set_with_interval)) ;;

let compare_interval (Range (s1, f1)) (Range (s2, f2)) : int = 
  let c = Int.compare s1 s2 in
  if c = 0 then Int.compare f1 f2
  else c
  
let normalize_interval_set s : interval_set = 
  let inverse = difference all_interval_set in
  let simplified = List.sort compare_interval (remove_empty_intervals (inverse (inverse s))) in
  match simplified with
  | [] -> [Range (Int.min_int, Int.min_int)]
  | _ -> simplified
  
let q4 () =
  let r1 = Range (0, 1) in
  let r2 = Range (1, 5) in
  let r3 = Range (0, 5) in
  let r4 = Range (5, 5) in
  let r5 = Range (7, 8) in
  let r6 = Range (-10, 10) in
  let s1 = [r1; r2; r3; r4; r5; r6] in
  let s2 = [r5] in
  let s3 = [r6] in
  let has_failed = ref false in

  begin
    try 
      assert (List.map is_empty_interval s1 = [false; false; false; true; false; false]);
      print_endline "is_empty_interval: PASS";
    with _ -> 
      print_endline "is_empty_interval: FAIL";
      has_failed := true;
  end;  
  begin
    try 
      assert (intersect_intervals r1 r2 = Range (1, 1));
      assert (intersect_intervals r3 r1 = Range (0, 1));
      assert (intersect_intervals r3 r5 = Range (7, 5));
      print_endline "intersect_intervals: PASS";
    with _ -> 
      print_endline "intersect_intervals: FAIL";
      has_failed := true;
  end;
  begin
    try 
      assert (remove_empty_intervals s1 = [Range (0, 1); Range (1, 5); Range (0, 5); Range (7, 8); Range (-10, 10)]);
      assert (remove_empty_intervals s2 = [Range (7, 8)]);
      assert (remove_empty_intervals s3 = [Range (-10, 10)]);
      print_endline "remove_empty_intervals: PASS";
    with _ -> 
      print_endline "remove_empty_intervals: FAIL";
      has_failed := true;
  end;
  begin
    try 
      assert (normalize_interval_set (intersect_interval_set_with_interval s3 r5) = [Range (7, 8)]);
      assert (normalize_interval_set (intersect_interval_set_with_interval s1 r6) = [Range (-10, 10)]);
      assert (normalize_interval_set (intersect_interval_set_with_interval s1 r4) = [Range (-4611686018427387904, -4611686018427387904)]);
      print_endline "intersect_interval_set_with_interval: PASS";
    with _ -> 
      print_endline "intersect_interval_set_with_interval: FAIL";
      has_failed := true;
  end;
  begin
    try 
      assert (complement_interval r1 = [Range (-4611686018427387904, 0); Range (1, 4611686018427387903)]);
      assert (complement_interval r6 = [Range (-4611686018427387904, -10); Range (10, 4611686018427387903)]);
      print_endline "complement_interval: PASS";
    with _ -> 
      print_endline "complement_interval: FAIL";
      has_failed := true;
  end;
  begin
    try 
      assert (normalize_interval_set (intersection s3 s2) = [Range (7, 8)]);
      assert (normalize_interval_set (intersection empty_interval_set s2) = [Range (-4611686018427387904, -4611686018427387904)]);
      assert (normalize_interval_set (intersection s1 s2) = [Range (7, 8)]);
      print_endline "intersection: PASS";
    with _ -> 
      print_endline "intersection: FAIL";
      has_failed := true;
  end;
  begin
    try 
      assert (normalize_interval_set (union s1 s2) = [Range (-10, 10)]);
      assert (normalize_interval_set (union s2 s3) = [Range (-10, 10)]);
      assert (normalize_interval_set (union s1 s3) = [Range (-10, 10)]);
      print_endline "union: PASS";
    with _ -> 
      print_endline "union: FAIL";
      has_failed := true;
  end;

  if !has_failed then
    raise (Failure "Some tests have failed")

let q5 () =
  let has_failed = ref false in

  begin
    try
      assert (valid_word "b1.txt" "DEAF" = Some 4);
      assert (valid_word "b1.txt" "BAD" = Some 3);
      assert (valid_word "b1.txt" "BEEF" = None);
    with _ ->
      print_endline "valid_word with valid boards: FAIL";
      has_failed := true;
  end;

  begin
    try
      ignore (valid_word "b2.txt" "BEEF");
      ignore (valid_word "b2.txt" "beef");
      ignore (valid_word "b3.txt" "BEEF");
      print_endline "valid_word with invalid boards: FAIL";
      has_failed := true;
    with Invalid_board ->
      print_endline "valid_word with invalid boards: PASS";
  end;

  if !has_failed then
    raise (Failure "Some tests have failed")

let () = 
  print_endline "Running tests...";
  begin
    try 
      print_endline "QUESTION 1: Testing true_product and true_product'...";
      q1 ();
      print_endline "QUESTION 1: PASS";
    with _ -> 
      print_endline "QUESTION 1: FAIL";
  end;

  begin
    try 
      print_endline "QUESTION 2: Testing valid_bst...";
      q2 ();
      print_endline "QUESTION 2: PASS";
    with _ -> 
      print_endline "QUESTION 2: FAIL";
  end;

  begin
    try 
      print_endline "QUESTION 3: Testing search_tree...";
      q3 ();
      print_endline "QUESTION 3: PASS";
    with _ -> 
      print_endline "QUESTION 3: FAIL";
  end;

  begin
    try 
      print_endline "QUESTION 4: Testing interval_set functions...";
      q4 ();
      print_endline "QUESTION 4: PASS";
    with _ -> 
      print_endline "QUESTION 4: FAIL";
  end;

  begin
    try 
      print_endline "QUESTION 5: Testing valid_word with valid and invalid boards...";
      q5 ();
      print_endline "QUESTION 5: PASS";
    with _ -> 
      print_endline "QUESTION 5: FAIL";
  end;