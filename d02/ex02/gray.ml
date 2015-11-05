let rec list_rev_append l1 l2 =
	 match l1 with
	  [] -> l2
	| h::t -> list_rev_append t (h::l2)

let list_append l1 l2 =
	list_rev_append (list_rev_append l1 []) l2

let list_reverse l =
	list_rev_append l []

let rec list_map f l =
	match l with
	  [] -> []
	| h::t -> (f h)::(list_map f t)

let rec gray n =
	match n with
	  x when x > 1 -> let li = gray (n - 1) in
	  					list_append (list_map (fun s -> "0" ^ s) li) (list_map (fun s -> "1" ^ s) (list_reverse li))
	  | _ -> ["0"; "1"]

(*****************************************************************************)

let () =
	let rec print_list l =
		match l with
		  h::t -> print_string h; print_char ' '; print_list t
		| _ -> ()
	in
	print_string "gray (0) : "; print_list (gray 0);
	print_string "\ngray (1) : "; print_list (gray 1);
	print_string "\ngray (2) : "; print_list (gray 2);
	print_string "\ngray (3) : "; print_list (gray 3);
	print_string "\ngray (4) : "; print_list (gray 4); print_char '\n'