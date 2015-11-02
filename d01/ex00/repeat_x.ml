let rec repeat_x n =
	if n < 0 then
		"Error"
	else if n = 0 then
	 	""
	else
		"x" ^ repeat_x (n - 1)

let test_repeat_x n =
	print_string "\nTest with n = ";
	print_int n;
	print_string " : \"";
	print_string (repeat_x n);
	print_endline "\""

let main () =
	test_repeat_x (-1);
	test_repeat_x 0;
	test_repeat_x 1;
	test_repeat_x 2;
	test_repeat_x 5

(*****************************************************************************)

let () = main ()	