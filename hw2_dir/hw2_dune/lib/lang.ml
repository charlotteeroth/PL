open List
open Char
open Format

exception Not_implemented
exception Parse_exn

(* Data Definitions *)

type token =
  | LParen
  | RParen
  | TokTrue
  | TokFalse
  | TokZero
  | TokIf
  | TokSucc
  | TokPred
  | TokIsZero
  | TokAnd
  | TokOr
  | TokNot
  | TokPlus

type term =
  | True
  | False
  | Zero
  | If of term * term * term
  | Succ of term
  | Pred of term
  | IsZero of term
  | And of term * term
  | Or of term * term
  | Not of term
  | Plus of term * term

(* Utilities *)

(* Strings in ocaml are not represented as lists of characters. For lexing and parsing purposes, it's easier to work
   with lists of characters. You shouldn't need to touch these functions that convert between the two, but feel free to use them for debugging. *)
let string_to_char_list (s : string) : char list =
  s |> String.to_seq |> List.of_seq

let char_list_to_string (cl : char list) : string =
  cl |> List.to_seq |> String.of_seq

(* Value Judgements *)

(* Returns true if the term is a numeric value, false otherwise. *)
let rec is_num_val (t : term) : bool = 
  match t with
  | Zero -> true
  | Succ(t1) -> is_num_val t1
  | _ -> false

(* Returns true if the term is a value, false otherwise. *)
let is_val (t : term) : bool =
  match t with
  | True -> true
  | False -> true
  | t when is_num_val t -> true
  | _ -> false

(* Lexical Scanner *)

(* nextToken should return the next token from a list of characters, along with the characters thereafter
    - return None if the list of chars is empty
    - skip whitespace characters
    - throw an exception if the characters cannot be tokenized
   Some basic cases have been given for you. *)
let rec nextToken (cs : char list) : (token * char list) option =
  match cs with
  | ' ' :: tl -> nextToken tl
  | [] -> None                                                
  | '(' :: tl -> Some (LParen, tl)
  | ')' :: tl -> Some (RParen, tl)
  | 't' :: 'r' :: 'u' :: 'e' :: tl -> Some (TokTrue, tl)
  | 'f' :: 'a' :: 'l' :: 's' :: 'e' :: tl -> Some (TokFalse, tl)
  | '0' :: tl -> Some (TokZero, tl)
  | 'i' :: 'f' :: tl -> Some (TokIf, tl)
  | 's' :: 'u' :: 'c' :: 'c' :: tl ->Some (TokSucc, tl)
  | 'p' :: 'r' :: 'e' :: 'd' :: tl -> Some (TokPred, tl)
  | 'i' :: 's' :: 'z' :: 'e' :: 'r' :: 'o' :: tl -> Some (TokIsZero, tl)
  | 'a' :: 'n' :: 'd' :: tl -> Some (TokAnd, tl)
  | 'o' :: 'r' :: tl -> Some (TokOr, tl)
  | 'n' :: 'o' :: 't' :: tl -> Some (TokNot, tl)
  | 'p' :: 'l':: 'u':: 's' :: tl -> Some (TokPlus, tl)
  | _ -> raise Parse_exn                                   

(* turn a string of code (like "(pred 0)" into a list of tokens (like [LParen, TokPred, TokZero, RParen]) *)
let rec scan (ls : char list) : token list = 
  match ls with
  | [] -> []
  | _ -> 
    match nextToken ls with
    | None -> []
    | Some (tok, tl) -> tok :: scan tl

(* Parser *)

(* nextTerm should return the next term from a list of tokens, along with the tokens thereafter
   - return None if the list of tokens is empty
   - throw an exception if the characters cannot be tokenized *)
let rec nextTerm (ts : token list) : (term * token list) option =
  match ts with
  | [] -> None
  | TokTrue :: tl -> Some (True, tl)
  | TokFalse :: tl -> Some (False, tl)
  | TokZero :: tl -> Some (Zero, tl)
  | LParen::TokIf :: tl -> 
    (match nextTerm tl with
    | None -> raise Parse_exn
    | Some (a, als) -> 
      (match nextTerm als with
      | None -> raise Parse_exn
      | Some (b, bls) -> 
        (match nextTerm bls with
        | Some (c, RParen::cls) -> Some (If (a, b, c), cls)
        |_-> raise Parse_exn)))
  
  | LParen::TokSucc :: tl ->
    (match nextTerm tl with
    | Some (a, RParen::als) -> Some (Succ (a), als)
    |_-> raise Parse_exn)

  | LParen::TokPred :: tl -> 
    (match nextTerm tl with
    | None -> raise Parse_exn
    | Some (a, RParen::als) -> Some (Pred (a), als)
    |_-> raise Parse_exn)

  | LParen::TokIsZero :: tl -> 
    (match nextTerm tl with
    | Some (a, RParen::als) -> Some (IsZero (a), als)
    |_-> raise Parse_exn)

  | LParen::TokAnd :: tl ->
    (match nextTerm tl with
    | None -> raise Parse_exn
    | Some (a, als) ->
      (match nextTerm als with
      | Some (b, RParen::bls) -> Some (And (a, b), bls)
      |_-> raise Parse_exn))
    
  | LParen::TokOr :: tl -> 
    (match nextTerm tl with 
    | None -> raise Parse_exn
    | Some (a, als) -> 
      (match nextTerm als with
      | Some (b, RParen::bls) -> Some (Or (a, b), bls)
      |_-> raise Parse_exn))

  | LParen::TokNot :: tl ->
    (match nextTerm tl with
    | Some (a, RParen::als) -> Some (Not (a), als)
    |_-> raise Parse_exn)

  | LParen::TokPlus :: tl ->
    (match nextTerm tl with
    | None -> raise Parse_exn
    | Some (a, als) ->
      (match nextTerm als with
      | Some (b, RParen::bls) -> Some (Plus (a, b), bls)
      |_-> raise Parse_exn))

  | _ -> raise Parse_exn


(* turn a list of tokens (like [LParen ,TokPred, TokZero, RParen] into a term (like Pred 0) *)
let rec parse (tokens : token list) : term = 
  match nextTerm tokens with
  | None -> raise Parse_exn
  | Some (tok, tl) -> if tl = [] then tok else 
    match tl with
    | RParen :: _ -> tok
    | _ -> parse tl

(* Small Step evaluator *)

(* Implement the small-step evaluation relation from class.
   For And, Or and Not, you should be able to determine
   appropriate rules by extrapolating from the If rules.
   If a term is not a normal form, take the next possible step
   - i.e., if t -> u, then step(t) should return Some(u)
   if a term is a normal form, return None *)
let rec small_step (t : term) : term option = 
  match t with

  | If (True, t2, _) -> Some t2
  | If (False, _, t3) -> Some t3
  | If (t1, t2, t3) -> 
    (match small_step t1 with
    | Some(t1_prime)-> Some (If (t1_prime, t2, t3))
    | None -> None)

  | Succ (t1) -> 
    (match small_step t1 with
    | Some (t1_prime) -> Some (Succ (t1_prime))
    | None -> None)

  | Pred (Zero) -> Some Zero
  | Pred (Succ t1) -> Some t1
  | Pred (t1) ->
    (match small_step t1 with
    | Some t1_prime -> Some (Pred t1_prime)
    | None -> None)

  | IsZero t1 -> 
    (match t1 with
    | Zero -> Some True
    | Succ _ -> Some False
    | t2 ->
      (match small_step t2 with
      | Some t2_prime -> Some (IsZero t2_prime)
      | None -> None))

  | And (True, t1) -> Some t1
  | And (False, _) -> Some False
  | And (t1, t2) -> 
    (match small_step t1 with
    | Some t1_prime -> Some (And (t1_prime, t2))
    | None -> None)

  | Or (True, _) -> Some True
  | Or (False, t1) -> Some t1
  | Or (t1, t2) -> 
    (match small_step t1 with
    | None -> None
    | Some t1_prime -> Some (Or (t1_prime, t2)))

  | Not True -> Some False
  | Not False -> Some True
  | Not t1 -> 
    (match small_step t1 with
    | None -> None
    | Some t1_prime -> Some (Not t1_prime))

  | Plus (t1, Zero) when is_num_val t1 -> Some t1
  | Plus (Zero, t2) when is_num_val t2 -> Some t2
  | Plus (Succ t1, t2) when is_num_val t1 ->
    (match small_step t2 with
    | Some t2_prime -> Some (Succ(Plus (t1, t2_prime)))
    | None -> None)
  | Plus (t1, t2) -> 
    (match small_step t1 with
    | Some t1_prime -> Some (Plus (t1_prime, t2))
    | None -> 
      ( match small_step t2 with
      | Some t2_prime -> Some (Plus (t1, t2_prime))
      | None -> None))
      
  | _ -> None

(* Returns true if the term is a normal form, false otherwise. *)
let is_normal (t : term) : bool = 
  match small_step t with
  | None -> true
  | _ -> false

(* Returns true if the term is stuck, false otherwise. *)
let is_stuck (t : term) : bool = not (is_val t) && is_normal t


(* Given t, return t' such that t ->* t' and t' is a normal form. *)
let rec multistep_full (t : term) : term = 
  match is_normal t with
  | true -> t
  | false -> 
    match small_step t with
    | None -> if is_stuck t then t else raise Parse_exn
    | Some t_prime -> multistep_full t_prime

(* Returns true if a term steps to a value, and false otherwise. *)
let rec multisteps_to_value (t : term) : bool = is_val (multistep_full t)

(* Big Step evaluator *)

(* Now implement the big-step relation from class.
   Again, return none if the program doesn't (big) step.
   You should be able to infer the big step semantics of
   and, or, not and plus from the small-step ones. *)
let rec big_step (t : term) : term option = 
  match t with
  | Zero -> Some Zero
  | True -> Some True
  | False -> Some False
  | If (t1, t2, t3) ->
    (match big_step t1 with
    | Some True -> big_step t2
    | Some False -> big_step t3
    |_ -> None)
  | Succ(t1) ->
    (match big_step t1 with
    | Some t2 -> Some (Succ t2)
    |_ -> None)
  | Pred(t1) -> 
    (match big_step t1 with
    | Some Zero -> Some Zero
    | Some Succ t2 -> Some t2
    | Some Pred t2 -> Some (Pred (Pred t2))
    | _ -> None)
  | IsZero t1 -> 
    (match big_step t1 with
    | Some Zero -> Some True
    | Some (Succ _) -> Some False
    | _ -> None)
  | And (t1, t2) ->
    (match big_step t1 with
    | Some True -> big_step t2
    | Some False -> Some False
    | _ -> None)
  | Or (t1, t2) ->
    (match big_step t1 with
    | Some True -> Some True
    | Some False -> big_step t2
    | _ -> None)
  | Not (t1) ->
    (match big_step t1 with
    | Some True -> Some False
    | Some False -> Some True
    | _ -> None)
  | Plus (t1, t2) ->
    (match big_step t1, big_step t2 with
    | Some Zero, Some t2_prime when is_num_val t2_prime -> Some t2_prime
    | Some Succ t1_prime, Some t2_prime -> Some (Succ (Plus (t1_prime, t2_prime)))
    | _ -> None)
  

