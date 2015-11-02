let rec fibonnaci n =
	if n < 0 then
		(-1)
	else
		let rec loop m acc old =
			if m < 2 then
				n
			else
				loop (n - 1) old (acc + old)
		in loop n 0 1