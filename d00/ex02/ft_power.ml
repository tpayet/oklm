let rec ft_power x n =
	if n = 0 then 1
	else x * ft_power x (n - 1)

let test_ft_power x n =
	print_string "Test with [";
	print_int x;
	print_string "] and [";
	print_int n;
	print_endline "]: ";
	print_string "Result: ";
	print_int (ft_power x n);
	print_char '\n'

let main () =
	test_ft_power 2 4;
	test_ft_power 3 0;
	test_ft_power 0 5

(*****************************************************************************)

let () = main ()