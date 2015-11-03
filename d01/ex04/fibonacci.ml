let rec fibonacci n =
	if n < 0 then
		(-1)
	else
		let rec loop m acc old =
			if m = 0 then
				acc
			else
				loop (m - 1) old (acc + old)
		in loop n 0 1

let () =
	print_string "Fibonacci (-1) = "; print_int (fibonacci (-1));
	print_string "\nFibonacci (0) = "; print_int (fibonacci 0);
	print_string "\nFibonacci (1) = "; print_int (fibonacci 1);
	print_string "\nFibonacci (2) = "; print_int (fibonacci 2);
	print_string "\nFibonacci (3) = "; print_int (fibonacci 3);
	print_string "\nFibonacci (4) = "; print_int (fibonacci 4);
	print_string "\nFibonacci (5) = "; print_int (fibonacci 5);
	print_string "\nFibonacci (6) = "; print_int (fibonacci 6);
	print_string "\nFibonacci (7) = "; print_int (fibonacci 7); print_char '\n'