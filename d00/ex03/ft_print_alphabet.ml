let ft_print_alphabet () =
	let int_of_start = int_of_char 'a' in
	let int_of_stop = int_of_char 'z' in
	let rec loop int_of_current_letter =
		if int_of_current_letter <= int_of_stop then
		let char_of_current_letter = char_of_int int_of_current_letter in 
			print_char char_of_current_letter;
			loop (int_of_current_letter + 1)
	in loop int_of_start;
	print_char '\n'

let main () =
	ft_print_alphabet ()

(*****************************************************************************)

let () = main ()