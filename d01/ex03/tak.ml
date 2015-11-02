let rec tak x y z =
	if y < x then
		tak (tak (x - 1) y z) (tak (y - 1) z x) (tak (z - 1) x y)
	else
		z

let test_tak x y z =
	print_string "tak(";
	print_int x;
	print_string ", ";
	print_int y;
	print_string ", ";		
	print_int z;
	print_string ") = ";
	print_int (tak x y z);
	print_char '\n'

let main () =
	test_tak 1 2 3;
	test_tak 5 23 7;
	test_tak 9 1 0;
	test_tak 1 1 1;
	test_tak 0 42 0;
	test_tak 23498 98734 98776

(*****************************************************************************)

let () = main ()