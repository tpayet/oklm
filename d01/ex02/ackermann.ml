let rec ackermann m n =
	if (m < 0) || (n < 0) then
		(-1)
	else if m = 0 then
		n + 1
	else if n = 0 then
		ackermann (m - 1) 1
	else
		ackermann (m - 1) (ackermann m (n - 1))

let test_ackermann m n =
	print_string "A(";
	print_int m;
	print_string ", ";
	print_int n;
	print_string ") = ";
	print_int (ackermann m n);
	print_char '\n'

let main () =
	test_ackermann (-1) 7;
	test_ackermann 0 0;
	test_ackermann 2 3;
	test_ackermann 4 1

(*****************************************************************************)

let () = main ()