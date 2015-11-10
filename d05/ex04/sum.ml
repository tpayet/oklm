let sum x y =
	x +. y

let () =
	print_endline ("Sum 3.02 39.4 = " ^ string_of_float (sum 3.02 39.4))