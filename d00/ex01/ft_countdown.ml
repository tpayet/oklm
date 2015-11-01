let rec ft_countdown x =
	print_int x; print_char '\n';
	if x > 0 then ft_countdown ( x - 1 )


let test_ft_countdown x =
	print_string "Test with [";
	print_int x;
	print_endline "]: ";
	ft_countdown x

let main () = 
	test_ft_countdown 9;
	test_ft_countdown 0;
	test_ft_countdown (-1)

(*****************************************************************************)

let () = main ()