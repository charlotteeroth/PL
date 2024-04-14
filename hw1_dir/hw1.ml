exception No_solution

(* Problem 1: Finding a product *)

(* Given a list of pairs of floats and bools, return the product
	 of all the members whose boolean value is true.
	 Don't use higher order functions (like map) *)

let rec true_product (l : (float * bool) list) : float =
   match l with
   | [] -> 1.0
   | (x, false) :: ls -> true_product ls
   | (x, true) :: ls -> x *. true_product ls


(* Now do the same thing using higher order functions (like map).
   You shouldn't pattern match at all. *)

let rec true_product' (l : (float * bool) list) : float =
  List.fold_left (fun x (f, b) -> if b then f *. x else x) 1.0 l;;


(* Problem 2: Balanced Trees *)

(* Consider the following tree type *)
type 'a tree = Nil | Node of 'a tree * 'a * 'a tree
   (* bse case if null return true,
    find the largest node from left side and smallest node of right side
    check if left is less than, right is greater than

  *)
(* Write a function that tests whether a tree is a valid binary search tree using the built-in '<' operation *)

let rec make_list (t : 'a tree) =
  match t with
  | Nil -> []
  | Node (left, num, right) -> make_list left @ [num] @ make_list right

let rec check_list (ls: 'a list) : bool =
  match ls with
  | [] -> true
  | [_] -> true
  | a :: b :: ls -> if a < b then check_list (b::ls) else false  

let rec valid_bst (t : 'a tree) : bool =
  let bst_lst = make_list t in
  check_list bst_lst


(* Problem 3: Searching a tree *)

(* Now assume we have a tree of key-value pairs.
   Write a function that given a key, returns an associated value.
   If no such value is found, it should return None.

   This tree may not be sorted. *)

let rec search_tree (t : ('a * 'b) tree) (key : 'a) : 'b option =
  match t with
  | Nil -> None
  | Node (left, (x,y), right) -> if x = key then Some y else
      match search_tree left key, search_tree right key with
      | None, None -> None
      | Some l, None -> Some l
      | None, Some r -> Some r
      | Some l, Some r -> Some l

(* Problem 4: Intervals *)

(** `Range' is the constructor that takes a start time and an end time
    and returns an `interval'. *)
type interval = Range of int * int

(** `is_empty_interval i' takes an `i : interval' and returns `true' iff the time interval specified by
    `i' encodes a non-postive amount of time; otherwise, it returns false.
      e.g. is_empty_interval (Range -5 1) => false
           is_empty_interval (Range 5 -1) => true
    *)
let is_empty_interval : interval -> bool =
  function Range (x, y) -> if x >= y then true else false


(** `intersect_intervals i1 i2' takes two `i1 i2 : interval' and returns an `interval'
    that is the largest possible interval included in both intervals given. *)

let intersect_intervals i1 i2 : interval =
  match i1, i2 with
  | Range (a, b), Range (x, y) -> Range (max a x, min b y)

(** An `interval_set' enocodes (possibly overlapping or touching) `interval's.
    It allows us to represent disjoint time intervals. *)
type interval_set = interval list

(* Don't worry about simplifying (i.e. joining) overlapping or touching intervals in the following functions *)

(** `to_interval_set i' turns `i : interval' into a singleton `interval_set' contatining `i'. *)
let to_interval_set i : interval_set = [i]

(** `empty_interval_set' is `interval_set' that specifies no time span. *)
let empty_interval_set : interval_set = []

(** Given an `s : interval_set', `remove_empty_intervals s' returns a new `interval_set'
    that contains all the intervals of `s' except those which are empty. *)
let remove_empty_intervals s : interval_set =
  let ls = List.filter (fun time -> if to_interval_set time = empty_interval_set then false else true) s in ls

(** `intersect_interval_set_with_interval s i' returns the largest interval set that is included in both `s' and `i'. *)
let intersect_interval_set_with_interval (s : interval_set) (i: interval) : interval_set =
  remove_empty_intervals (List.map (intersect_intervals i) s)

(** `complement_interval i` takes an `i : interval` and returns an `interval_set' that does *not* contain `i'.
     The complement of an `interval' must return an `interval_set' because it may result in two disjoint intervals. *)
let complement_interval i : interval_set =
  match i with
  | Range (a,b) -> [Range (min_int, a);  Range (b, max_int)]

(** `intersection s1 s2' returns the largest (meaning specifying the largest possible time span) `interval_set' that is included in both `s1 s2 : interval_set`. *)
let intersection s1 s2 : interval_set =
  remove_empty_intervals (List.concat (List.map (fun var ->
    intersect_interval_set_with_interval s2 var) s1))

(** `union s1 s2` returns all the intervals specified by either s1 or s2. *)
let union s1 s2 : interval_set = s1 @ s2


(* Problem 5: Spelling Bee *)

exception Invalid_board

(*
	This exercise is based on the New York Times Spelling Bee game:
   https://www.nytimes.com/puzzles/spelling-bee

   You should read in a file of the form
	`--
	 ---
	 --`
	where each `-` should be a capital letter. Raise Invalid_board if input does not match this format.
   A valid word is a sequence of at least 3 letters, all of which appear on the board and *at least one*
   of which is the center letter in the board. For instance, given the board

	 EH
   KPC
   IR
   "PERCH" is a valid word, while "RICE" and "CHIMP" are not.

	Return `Some n` where `n` is the length of the input word if the given word is valid,
   otherwise return `None`.
 *)

let check_word (word : string) (middle_character : char) : int option =
  if String.length word >= 3 then
    if String.contains word middle_character 
      then Some (String.length word) else None
  else None

  let valid_word (filename : string) (word : string) : int option =
    let file = open_in filename in
    let rec read_lines () =

      try
        match input_line file with
        | line when String.length line >= 3 ->
          let first_orig = String.sub line 0 2 in
          let sec_orig = String.sub line 2 3 in
          let third_orig = String.sub line 5 2 in

          let first_line = String.uppercase_ascii first_orig in
          let second_line = String.uppercase_ascii sec_orig in
          let third_line = String.uppercase_ascii third_orig in

          if String.length first_line = 2 
            && String.length second_line = 3 
            && String.length third_line = 2
            && first_orig = first_line 
            && sec_orig = second_line 
            && third_orig = third_line 
            then check_word word sec_orig.[1]
          else
            None

        | _ -> raise Invalid_board

      with End_of_file ->
        close_in file;
        None
    in
    read_lines ()

