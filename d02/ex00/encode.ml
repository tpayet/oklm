let encode l =
	if l = [] then []
	else
		let rec loop l2 n lf = 
		match l2 with
		  head::second::tail when head = second -> loop (second::tail) (n + 1) lf
		| head::tail -> loop (tail) (1) (lf @ [(n, head)])
		| _ -> lf
		in loop l 1 []

(*TESTING*FUNCTIONS***********************************************************)

let rec print_int_list l =
	match l with
	  [] -> ()
	| h::t -> print_int h; print_char ' '; print_int_list t

let rec print_tuple_list l =
	match l with
	  [] -> ()
	| (nb,n)::tail -> print_int nb; print_char ':'; print_int n; print_char ' '; print_tuple_list tail

(*****************************************************************************)

let () =
	let l1 = [1;1;1;1;2;2;2;3;3;3;4] in
	print_int_list l1;print_char '\n' ;print_tuple_list (encode l1)