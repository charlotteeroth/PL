open Hw2.Lang

(* Interpreter *)
(* You should not need to modify this code -- feel free to add statements for debugging,
   but remember to delete them before submitting. *)

let opt_term_to_string (o : term option) : string =
  match o with None -> " " | Some t -> term_to_string t

let interpret (str : string) : unit =
  let chars = string_to_char_list str in
  let tokens = scan chars in
  let ast = parse tokens in
  let ss_term = small_step ast in
  let bs_term = big_step ast in
  let ms_term = multistep_full ast in
  let term_str = term_to_string ast in
  let ss_term_str = opt_term_to_string ss_term in
  let bs_term_str = opt_term_to_string bs_term in
  let ms_term_str = term_to_string ms_term in
  let _ = print_endline "----- Small Step Evaluation -----" in
  let _ = print_endline ("      " ^ term_str) in
  let _ = print_endline ("->    " ^ ss_term_str) in
  let _ = print_endline "" in
  let _ = print_endline "-----------------------------------" in
  let _ = print_endline "" in
  let _ = print_endline "----- Big Step Evaluation -----" in
  let _ = print_endline ("      " ^ term_str) in
  let _ = print_endline ("||    " ^ bs_term_str) in
  let _ = print_endline "" in
  let _ = print_endline "-----------------------------------" in
  let _ = print_endline "" in
  let _ = print_endline "----- Multi Step Full Evaluation ------" in
  let _ = print_endline ("      " ^ term_str) in
  print_endline ("-->*    " ^ ms_term_str)
;;

interpret Sys.argv.(1)
