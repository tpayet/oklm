let ft_sum f start stop =
	if stop - start < 0 then nan
	else let rec loop n acc =
		if n < 0 then acc
		else loop (n - 1) (acc +. (f (start + n)))
	in loop (stop - start) 0.

let () = 
	print_float (ft_sum (fun i -> float_of_int (i * i)) 1 10);
	print_float (ft_sum (fun i -> float_of_int (i * i)) 10 10);
	print_float (ft_sum (fun i -> float_of_int (i * i)) 10 1)
