let ft_rot_n n str =
	let m = (n mod 26) in
	let rotate c =
		let i = int_of_char c in
		if (i >= int_of_char 'a' && i <= int_of_char 'z') || (i >= int_of_char 'A' && i <= int_of_char 'Z') then
			if ((i + m > int_of_char 'Z') && ((i + m) < int_of_char 'a')) || ((i + m) > int_of_char 'z') then
				char_of_int (i + m - 26)
			else
				char_of_int (i + m)
		else
			c
	in String.map rotate str

let test_ft_rot_n n str =
	print_string "Test with n = ";
	print_int n;
	print_string " and str = \"";
	print_string str;
	print_endline "\":";
	print_endline (ft_rot_n n str);
	print_char '\n'

let main () =
	test_ft_rot_n 1 "abcdefghijklmnopqrstuvwxyz";
	test_ft_rot_n 13 "abcdefghikklmnopqrstuvwxyz";
	test_ft_rot_n 42 "0123456789";
	test_ft_rot_n 2 "OI2EAS67B9";
	test_ft_rot_n 0 "Damned !";
	test_ft_rot_n 42 "";
	test_ft_rot_n 1 "NBzlk qnbjr !"

(*****************************************************************************)

let () = main ()