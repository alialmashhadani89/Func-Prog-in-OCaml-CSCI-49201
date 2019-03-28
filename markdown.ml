open Pervasives
open Printf

let file = "test.html"

let string_of_char c = String.make 1 c
;;

(* Converts a string to a list of chars *)
let explode str =
  let rec explode_inner cur_index chars = 
    if cur_index < String.length str then
      let new_char = str.[cur_index] in
      explode_inner (cur_index + 1) (chars @ [new_char])
    else chars in
  explode_inner 0 []
;;

(* Converts a list of chars to a string *)
let rec implode chars =
  match chars with
    [] -> ""
    | h::t ->  string_of_char h ^ (implode t)
;;

let get_line ic =
  try
    Some (input_line ic)
  with
  | End_of_file -> None
;;

let write_to_file message = 
  let oc = open_out_gen [Open_append; Open_creat] 0o666 file in
  fprintf oc "%s" message;
  close_out oc;
;;

let rec check_b l = 
  match l with
  |'*'::tl -> write_to_file "</strong>"; check_b tl; 
  |hd::tl -> write_to_file (string_of_char(hd));  check_b tl;
  |[] -> write_to_file "\n";
  ;; 


let rec check_i l =
    match l with
    | '*'::tl -> write_to_file "</em>"; check_i tl;
    | hd::tl -> write_to_file (string_of_char(hd));  check_i tl;
    |[] -> write_to_file "\n";
;;

let rec check_i_and_b l =
  match l with
  |'*'::'*'::'*' ::tl -> write_to_file "</em></strong>"; check_i_and_b tl;
  | hd::tl-> write_to_file (string_of_char(hd));  check_i_and_b tl; 
  |[] -> write_to_file "\n";
;;

let rec check_under_line l =
  match l with
  | '_'::'_'::'_'::tl ->  write_to_file "</em></strong>"; check_under_line tl;
  | hd::tl -> write_to_file (string_of_char(hd));  check_under_line tl;
  |[] -> write_to_file "\n"
;;

let rec check_l_s_s l =
  match l with
  |'_'::'*'::'*'::tl -> write_to_file "</em></strong>"; check_l_s_s tl; 
  | hd::tl -> write_to_file (string_of_char(hd));  check_l_s_s tl;
  |[]-> write_to_file "\n";
;;

let rec check_s_l_l l =
  match l with
  | '_'::'_'::'*'::tl -> write_to_file "</em></strong>"; check_s_l_l tl; 
  | hd::tl -> write_to_file (string_of_char(hd));  check_s_l_l tl;
  |[]-> write_to_file "\n";
;;

let check_output l = 
  match explode l with
  | '#'::tl -> write_to_file "<h1>";
               write_to_file (implode tl);
               write_to_file "</h1>";
               write_to_file "\n";
  |'*'::tl ->   
              begin
                match tl with
                |'*'::tl ->  begin
                            match tl with
                              |'*'::tl -> write_to_file "<strong><em>"; check_i_and_b tl;
                              |'_'::tl-> write_to_file "<strong><em>"; check_l_s_s tl;
                              |_ -> write_to_file "<strong>"; check_b tl; 
                            end 
                |'_'::tl-> begin
                              match tl with
                              |'_'::tl -> write_to_file "<strong><em>"; check_s_l_l tl;
                              | _ -> ()
                            end
                |_-> write_to_file "<strong>"; check_b tl;
              end            
  |'_'::tl -> 
              begin
                match tl with
                | '_'::tl ->
                          begin
                            match tl with
                            |'_'::tl -> write_to_file "<strong><em>"; check_under_line tl;
                            |'*'::tl -> write_to_file "<strong><em>";
                            | _ -> ();
                          end

                | _ -> ();
              end

  
  |_ ->  write_to_file "<p>";
         write_to_file  l;
         write_to_file "</p>";
         write_to_file "\n";
;;

let () = 
	
	let argv_list = Array.to_list Sys.argv in
	match argv_list with
	|_::[] -> ()
	|[] -> ()
    |hd::hd2::tl -> 

    let ic = open_in hd2 in 

 	let rec read () =
    	match get_line ic with
    	| Some line -> check_output line;
                    read ()
    	| None -> ()
  	in
  	read ();

  (* close input channel *)
  close_in ic