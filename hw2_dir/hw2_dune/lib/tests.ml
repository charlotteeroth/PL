open Lang

(***************************************** is_num_val *****************************************)

let%test "is_num_val 0" = is_num_val Zero
let%test "is_num_val (succ 0)" = is_num_val (Succ Zero)
let%test "is_num_val (succ (succ 0))" = is_num_val (Succ (Succ Zero))
let%test "is_num_val true" = not (is_num_val True)
let%test "is_num_val (succ true)" = not (is_num_val (Succ True))
let%test "is_num_val (plus 0 0)" = not (is_num_val (Plus (Zero, Zero)))
let%test "is_num_val (and 0 0)" = not (is_num_val (And (Zero, Zero)))

(***************************************** is_val *****************************************)

let%test "is_val 0" = is_val Zero
let%test "is_val (succ 0)" = is_val (Succ Zero)
let%test "is_val (succ (succ 0))" = is_val (Succ (Succ Zero))
let%test "is_val true" = is_val True
let%test "is_val (succ true)" = not (is_val (Succ True))
let%test "is_val (plus 0 0)" = not (is_val (Plus (Zero, Zero)))
let%test "is_val (and 0 0)" = not (is_val (And (Zero, Zero)))

(***************************************** nextToken *****************************************)

let%test "nextToken (not true)" =
  nextToken (string_to_char_list "(not true)")
  = Some (LParen, string_to_char_list "not true)")

let%test "nextToken not true)" =
  nextToken (string_to_char_list "not true)")
  = Some (TokNot, string_to_char_list " true)")

let%test "nextToken  true)" =
  nextToken (string_to_char_list " true)") = Some (TokTrue, [ ')' ])

let%test "nextToken )" = nextToken [ ')' ] = Some (RParen, [])
let%test "nextToken " = nextToken [] = None

(***************************************** scan *****************************************)

let%test "scan (not true)" =
  scan (string_to_char_list "(not true)") = [ LParen; TokNot; TokTrue; RParen ]

let%test "scan true true 0 false" =
  scan (string_to_char_list "true true true 0 false")
  = [ TokTrue; TokTrue; TokTrue; TokZero; TokFalse ]

let%test "scan " = scan [] = []

(***************************************** nextTerm *****************************************)

let%test "nextTerm true" = nextTerm [ TokTrue ] = Some (True, [])

let%test "nextTerm (not true)" =
  nextTerm [ LParen; TokNot; TokTrue; RParen ] = Some (Not True, [])

let%test "nextTerm (not true" =
  try
    let _ = nextTerm [ LParen; TokNot; TokTrue ] in
    false
  with Parse_exn -> true

let%test "nextTerm (if true 0 true)" =
  nextTerm [ LParen; TokIf; TokTrue; TokZero; TokTrue; RParen ]
  = Some (If (True, Zero, True), [])

let%test "nextTerm (true)" =
  try
    let _ = nextTerm [ LParen; TokTrue; RParen ] in
    false
  with Parse_exn -> true

let%test "nextTerm true false" =
  nextTerm [ TokTrue; TokFalse ] = Some (True, [ TokFalse ])

(***************************************** parse *****************************************)

let%test "parse true" = parse [ TokTrue ] = True

let%test "parse (not true)" =
  parse [ LParen; TokNot; TokTrue; RParen ] = Not True

let%test "parse (not true" =
  try
    let _ = parse [ LParen; TokNot; TokTrue ] in
    false
  with Parse_exn -> true

let%test "parse (if true 0 true)" =
  parse [ LParen; TokIf; TokTrue; TokZero; TokTrue; RParen ]
  = If (True, Zero, True)

let%test "parse (true)" =
  try
    let _ = parse [ LParen; TokTrue; RParen ] in
    false
  with Parse_exn -> true

let%test "parse true false" =
  try
    let _ = parse [ TokTrue; TokFalse ] in
    false
  with Parse_exn -> true

(***************************************** small_step *****************************************)

let%test "small_step 0" = small_step Zero = None
let%test "small_step (succ 0)" = small_step (Succ Zero) = None

let%test "small_step (succ (plus 0 0))" =
  small_step (Succ (Plus (Zero, Zero))) = Some (Succ Zero)

let%test "small_step (plus (if true 0 (succ 0)) (and true false))" =
  small_step (Plus (If (True, Zero, Succ Zero), And (True, False)))
  = Some (Plus (Zero, And (True, False)))

let%test "small_step (plus 0 (and true false))" =
  small_step (Plus (Zero, And (True, False))) = Some (Plus (Zero, False))

let%test "small_step (plus 0 false)" = small_step (Plus (Zero, False)) = None

let%test "small_step (succ (if true 0 (succ 0)))" =
  small_step (Succ (If (True, Zero, Succ Zero))) = Some (Succ Zero)

let%test "small_step (succ (if false 0 (succ 0)))" =
  small_step (Succ (If (False, Zero, Succ Zero))) = Some (Succ (Succ Zero))

let%test "small_step (if (iszero (pred 0)) false true)" =
  small_step (If (IsZero (Pred Zero), False, True))
  = Some (If (IsZero Zero, False, True))

let%test "small_step (if (iszero 0) false true)" =
  small_step (If (IsZero Zero, False, True)) = Some (If (True, False, True))

let%test "small_step (and false 0)" =
  small_step (And (False, Zero)) = Some False

let%test "small_step (and 0 false)" = small_step (And (Zero, False)) = None

let%test "small_step (and (not true) false)" =
  small_step (And (Not True, False)) = Some (And (False, False))

let%test "small_step (and true 0)" = small_step (And (True, Zero)) = Some Zero
let%test "small_step (or false 0)" = small_step (Or (False, Zero)) = Some Zero
let%test "small_step (or 0 false)" = small_step (Or (Zero, False)) = None

let%test "small_step (or (not true) false)" =
  small_step (Or (Not True, False)) = Some (Or (False, False))

let%test "small_step (or true 0)" = small_step (Or (True, Zero)) = Some True

(***************************************** is_normal *****************************************)

let%test "is_normal true" = is_normal True
let%test "is_normal (succ 0)" = is_normal (Succ Zero)
let%test "is_normal (succ false)" = is_normal (Succ False)
let%test "is_normal (not false)" = not (is_normal (Not False))
let%test "is_normal (pred 0)" = not (is_normal (Pred Zero))
let%test "is_normal (and 0 true)" = is_normal (And (Zero, True))

(***************************************** is_stuck *****************************************)

let%test "is_stuck true" = not (is_stuck True)
let%test "is_stuck (succ 0)" = not (is_stuck (Succ Zero))
let%test "is_stuck (succ false)" = is_stuck (Succ False)
let%test "is_stuck (not false)" = not (is_stuck (Not False))
let%test "is_stuck (pred 0)" = not (is_stuck (Pred Zero))
let%test "is_stuck (and 0 true)" = is_stuck (And (Zero, True))

(***************************************** multistep_full *****************************************)

let%test "multistep_full 0" = multistep_full Zero = Zero
let%test "multistep_full (succ 0)" = multistep_full (Succ Zero) = Succ Zero

let%test "multistep_full (succ (plus 0 0))" =
  multistep_full (Succ (Plus (Zero, Zero))) = Succ Zero

let%test "multistep_full (plus (if true 0 (succ 0)) (and true false))" =
  multistep_full (Plus (If (True, Zero, Succ Zero), And (True, False)))
  = Plus (Zero, False)

let%test "multistep_full (plus 0 (and true false))" =
  multistep_full (Plus (Zero, And (True, False))) = Plus (Zero, False)

let%test "multistep_full (plus 0 false)" =
  multistep_full (Plus (Zero, False)) = Plus (Zero, False)

let%test "multistep_full (succ (if true 0 (succ 0)))" =
  multistep_full (Succ (If (True, Zero, Succ Zero))) = Succ Zero

let%test "multistep_full (succ (if false 0 (succ 0)))" =
  multistep_full (Succ (If (False, Zero, Succ Zero))) = Succ (Succ Zero)

let%test "multistep_full (if (iszero (pred 0)) false true)" =
  multistep_full (If (IsZero (Pred Zero), False, True)) = False

let%test "multistep_full (if (iszero 0) false true)" =
  multistep_full (If (IsZero Zero, False, True)) = False

let%test "multistep_full (and false 0)" =
  multistep_full (And (False, Zero)) = False

let%test "multistep_full (and 0 false)" =
  multistep_full (And (Zero, False)) = And (Zero, False)

let%test "multistep_full (and (not true) false)" =
  multistep_full (And (Not True, False)) = False

let%test "multistep_full (and true 0)" =
  multistep_full (And (True, Zero)) = Zero

let%test "multistep_full (or false 0)" =
  multistep_full (Or (False, Zero)) = Zero

let%test "multistep_full (or 0 false)" =
  multistep_full (Or (Zero, False)) = Or (Zero, False)

let%test "multistep_full (or (not true) false)" =
  multistep_full (Or (Not True, False)) = False

let%test "multistep_full (or true 0)" = multistep_full (Or (True, Zero)) = True

(***************************************** multisteps_to_value *****************************************)

let%test "multisteps_to_value 0" = multisteps_to_value Zero
let%test "multisteps_to_value (succ 0)" = multisteps_to_value (Succ Zero)

let%test "multisteps_to_value (succ (plus 0 0))" =
  multisteps_to_value (Succ (Plus (Zero, Zero)))

let%test "multisteps_to_value (plus (if true 0 (succ 0)) (and true false))" =
  not
    (multisteps_to_value (Plus (If (True, Zero, Succ Zero), And (True, False))))

let%test "multisteps_to_value (plus 0 (and true false))" =
  not (multisteps_to_value (Plus (Zero, And (True, False))))

let%test "multisteps_to_value (plus 0 false)" =
  not (multisteps_to_value (Plus (Zero, False)))

let%test "multisteps_to_value (succ (if true 0 (succ 0)))" =
  multisteps_to_value (Succ (If (True, Zero, Succ Zero)))

let%test "multisteps_to_value (succ (if false 0 (succ 0)))" =
  multisteps_to_value (Succ (If (False, Zero, Succ Zero)))

let%test "multisteps_to_value (if (iszero (pred 0)) false true)" =
  multisteps_to_value (If (IsZero (Pred Zero), False, True))

let%test "multisteps_to_value (if (iszero 0) false true)" =
  multisteps_to_value (If (IsZero Zero, False, True))

let%test "multisteps_to_value (and false 0)" =
  multisteps_to_value (And (False, Zero))

let%test "multisteps_to_value (and 0 false)" =
  not (multisteps_to_value (And (Zero, False)))

let%test "multisteps_to_value (and (not true) false)" =
  multisteps_to_value (And (Not True, False))

let%test "multisteps_to_value (and true 0)" =
  multisteps_to_value (And (True, Zero))

let%test "multisteps_to_value (or false 0)" =
  multisteps_to_value (Or (False, Zero))

let%test "multisteps_to_value (or 0 false)" =
  not (multisteps_to_value (Or (Zero, False)))

let%test "multisteps_to_value (or (not true) false)" =
  multisteps_to_value (Or (Not True, False))

let%test "multisteps_to_value (or true 0)" =
  multisteps_to_value (Or (True, Zero))

(***************************************** big_step *****************************************)

let%test "big_step 0" = big_step Zero = Some Zero
let%test "big_step (succ 0)" = big_step (Succ Zero) = Some (Succ Zero)

let%test "big_step (succ (plus 0 0))" =
  big_step (Succ (Plus (Zero, Zero))) = Some (Succ Zero)

let%test "big_step (plus (if true 0 (succ 0)) (and true false))" =
  big_step (Plus (If (True, Zero, Succ Zero), And (True, False))) = None

let%test "big_step (plus 0 (and true false))" =
  big_step (Plus (Zero, And (True, False))) = None

let%test "big_step (plus 0 false)" = big_step (Plus (Zero, False)) = None

let%test "big_step (succ (if true 0 (succ 0)))" =
  big_step (Succ (If (True, Zero, Succ Zero))) = Some (Succ Zero)

let%test "big_step (succ (if false 0 (succ 0)))" =
  big_step (Succ (If (False, Zero, Succ Zero))) = Some (Succ (Succ Zero))

let%test "big_step (if (iszero (pred 0)) false true)" =
  big_step (If (IsZero (Pred Zero), False, True)) = Some False

let%test "big_step (if (iszero 0) false true)" =
  big_step (If (IsZero Zero, False, True)) = Some False

let%test "big_step (and false 0)" = big_step (And (False, Zero)) = Some False
let%test "big_step (and 0 false)" = big_step (And (Zero, False)) = None

let%test "big_step (and (not true) false)" =
  big_step (And (Not True, False)) = Some False

let%test "big_step (and true 0)" = big_step (And (True, Zero)) = Some Zero
let%test "big_step (or false 0)" = big_step (Or (False, Zero)) = Some Zero
let%test "big_step (or 0 false)" = big_step (Or (Zero, False)) = None

let%test "big_step (or (not true) false)" =
  big_step (Or (Not True, False)) = Some False

let%test "big_step (or true 0)" = big_step (Or (True, Zero)) = Some True
