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
	| h::t -> (f h)::(list_map f tail)

let gray n =
	if n < 1 then print_endline "Error"
	else
		let rec loop n li =
			match n with
			  1 -> loop (n - 1) (["0";"1"]::li)
			| x when x > 1 -> loop (n - 1) (list_append li (list_reverse li))
			| _ -> li
		in loop n []