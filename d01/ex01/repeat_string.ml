let rec repeat_string ?(str="x") n =
	if n = 0 then
		""
	else if n < 0 then
		"Error"
	else
		str ^ repeat_string ~str:str (n - 1)

let test_repeat_string ?(str="x") n =
	print_string "\nTest with str = \"";
	print_string str;
	print_string "\" and n = ";
	print_int n;
	print_string "; Result: \"";
	print_string (repeat_string ~str:str n);
	print_endline "\""

let main () =
	test_repeat_string 3;
	test_repeat_string 0;
	test_repeat_string (-1);
	test_repeat_string ~str:"Hello world ! " 3;
	test_repeat_string ~str:"Hello world ! " 0;
	test_repeat_string ~str:"Hello world ! " (-1)

(*****************************************************************************)

let () = main ()