let ft_test_sign x =
	if x < 0 then
		print_endline "negative"
	else
		print_endline "positive"

let test_ft_test_sign x =
	print_string "Test with [";
	print_int x;
	print_string "]: ";
	ft_test_sign x

let main () = 
	test_ft_test_sign 42;
	test_ft_test_sign 0;
	test_ft_test_sign (-42)

(*****************************************************************************)

let () = main ()