let rec list_rev_append l1 l2 =
	 match l1 with
	  [] -> l2
	| h::t -> list_rev_append t (h::l2)

let list_append l1 l2 =
	list_rev_append (list_rev_append l1 []) l2

let list_reverse l =
	list_rev_append l []

let encode l =
	if l = [] then []
	else
		let rec loop l2 n lf = 
		match l2 with
		  head::second::tail when head = second -> loop (second::tail) (n + 1) lf
		| head::tail -> loop (tail) (1) (lf @ [(n, head)])
		| _ -> lf
		in loop l 1 []

let get_1_in_t (n,_) = n

let get_2_in_t (_,n) = n

(* let tuples_int_list_to_string li =
	let rec do_the_trick l s =
		match l with
		  h::t -> do_the_trick t ((string_of_int (get_1_in_t h)) ^ (string_of_int (get_2_in_t h)) ^ s)
		| _ -> s
	in do_the_trick li "" *)

let tuples_list_to_simple_list l =
	if l = [] then []
	else
		let rec loop li lf =
			match li with
			  h::t -> loop t ((get_1_in_t h)::(get_2_in_t h)::lf)
			| _ -> lf
		in loop l []

let int_list_to_string l =
	let rec loop li s =
		match li with
		  h::t -> loop t ((string_of_int h) ^ s)
		| _ -> s
	in loop l ""

let sequence n =
	if n < 1 then ""
	else
		let lis =
			let rec loop lt i =
			match i with
			  x when x < 1 -> lt
			| _ -> loop (list_reverse (tuples_list_to_simple_list (encode lt))) (i - 1)
			in loop [1] n
		in int_list_to_string lis

(*****************************************************************************)

let () =
	print_string "Sequence (-1) : "; print_endline (sequence (-1));
	print_string "Sequence (0) : "; print_endline (sequence 0);
	print_string "Sequence (1) : "; print_endline (sequence 1);
	print_string "Sequence (2) : "; print_endline (sequence 2);
	print_string "Sequence (3) : "; print_endline (sequence 3);
	print_string "Sequence (4) : "; print_endline (sequence 4);
	print_string "Sequence (5) : "; print_endline (sequence 5);
	print_string "Sequence (6) : "; print_endline (sequence 6);
	print_string "Sequence (7) : "; print_endline (sequence 7);
	print_string "Sequence (8) : "; print_endline (sequence 8);
	print_string "Sequence (9) : "; print_endline (sequence 9)