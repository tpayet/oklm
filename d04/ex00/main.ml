let() =
	print_endline "Testing toString and toStringVerbose on Color.all : ";
  	let rec loop l =
  		match l with
			  h::t -> print_endline ((Color.toString h) ^ " : " ^ (Color.toStringVerbose h)); loop t
			| []         -> ()
  in loop Color.all