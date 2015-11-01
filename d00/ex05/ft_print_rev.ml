let ft_print_rev str =
	let rec loop n stri =
		if n >= 0 then 
		let c = String.get stri n in
			print_char c; loop (n - 1) stri
	in loop ((String.length str) - 1) str;
	print_char '\n'

let test_ft_print_rev str =
	print_string "Test with \"";
	print_string str;
	print_endline "\":";
	ft_print_rev str

let main () =
	test_ft_print_rev "Hello world !";
	test_ft_print_rev ""

(*****************************************************************************)

let () = main ()