let rec repeat_x n =
	if n < 0 then
		"Error"
	else
		let rec loop str n =
			if n = 0 then 
				str
			else
				let arg = str ^ "x" in
					loop arg (n - 1)
		in loop "" n

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

let () = main ()	