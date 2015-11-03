let leibniz_pi d =
	if d < 0. then (-1)
	else let rec loop acc n =
		let diff =
			if acc -. atan 1. > 0. then 4. *. (acc -. atan 1.) else 4. *. (atan 1. -. acc)
		in 
(* 		print_string "pi: "; print_float (4. *. acc); print_endline "";
     	print_string "diff: "; print_float (diff); print_endline ""; *)
(* MERCI MCANAL *)
		if diff <= d then n
		else
			loop (((-1.)**(float_of_int n) /. (2. *. (float_of_int n) +. 1.)) +. acc) (n + 1)
	in loop 0. 0

let () =
	print_endline "testing delta = -42.: ";
   print_int (leibniz_pi (-42.));
   print_endline "\ntesting delta = 0.1: ";
   print_int (leibniz_pi 0.1);
   print_endline "\ntesting delta = 0.01: ";
   print_int (leibniz_pi 0.01);
   print_endline "\ntesting delta = 0.001: ";
   print_int (leibniz_pi 0.001);
   print_endline ""