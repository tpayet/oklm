let ft_print_comb2 () =
	let rec loop a b = 
		if a < 10 then
			print_char '0';
		print_int a;
		print_char ' ';
		if b < 10 then
			print_char '0';
		print_int b;
		if a <> 98 then 
			begin
				print_char ','; print_char ' ';
				if b = 99 then
					loop (a + 1) (a + 2)
				else
					loop a (b + 1)
			end
	in loop 0 1;
	print_char '\n'

let main () =
	ft_print_comb2 ()

(*****************************************************************************)

let () = main ()