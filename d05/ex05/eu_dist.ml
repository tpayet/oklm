let eu_dist a_1 a_2 = 
	let rec loop acc n =
	match n with
		  x when x > 0 -> loop (((a_1.(n) -. a_2.(n)) ** 2.) +. acc) (n - 1)
		| _ -> acc 
	in sqrt (loop 0. ((Array.length a_1) - 1))

let () =
	let a1 = [| 1.; 2.; 3. |] in
	let a2 = [| 1.; 2.; 1. |] in
	let a3 = [| 1.; 2.; 6. |] in
	print_endline ("Testing eu_dist 1;2;3 1;2;1 :\n"
				 ^ (string_of_float (eu_dist a1 a2)));
	print_endline ("\nTesting eu_dist 1;2;3 1;2;6 :\n"
				 ^ (string_of_float (eu_dist a2 a3)))