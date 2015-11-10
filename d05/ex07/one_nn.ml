let eu_dist a_1 a_2 = 
	let rec loop acc n =
	match n with
		  x when x >= 0 -> loop (((a_1.(n) -. a_2.(n)) ** 2.) +. acc) (n - 1)
		| _ -> acc 
	in sqrt (loop 0. ((Array.length a_1) - 1))

let count_occ c str = 
	let rec loop acc n =
		if n < 0 then acc
		else if str.[n] = c then
			loop (acc + 1) (n - 1)
		else
			loop acc (n - 1)
	in loop 0 ((String.length str) - 1)

let parse_line str = 
	let n = count_occ ',' str in
	let arr = Array.make n 0. in
	let rec loop i start stop =
		Array.set arr i (float_of_string (String.sub str start (stop - start)));
		if i = (n - 1) then
			(arr, (String.sub str (stop + 1) (String.length str - stop - 1)))
		else
			loop (i + 1) (stop + 1) (String.index_from str (stop + 1) ',')
	in loop 0 0 (String.index str ',')

let examples_of_file path =
	let ic = open_in path in
		let rec read_loop l = 
				try
					read_loop ((parse_line (input_line ic))::l)
				with
			  End_of_file -> close_in ic; List.rev l
			| _ -> failwith "Unknown"
		in read_loop []

let one_nn r_l r =
	let name = ref "" in
	let rec loop n l =
		match l with
		  h::t -> let act_n = eu_dist (fst h) (fst r) in
		  				if act_n <= n then (name.contents <- snd h; loop act_n t)
		  				else loop n t
		| [] -> name.contents
	in loop (eu_dist (fst (List.hd r_l)) (fst r)) r_l

let () =
  print_endline "testing with csv:";
  print_endline (one_nn 
				   (examples_of_file "../ex06/ionosphere.test.csv")
				   ([| 1.;0.;0.74916;0.02549;0.98994;0.09792;0.75855;0.12877;0.74313;-0.09188;0.95842;0.02482;0.97921;-0.00469;0.96110;0.10195;0.91482;0.03756;0.71026;0.02683;0.81221;-0.08048;1.;0.;0.71764;-0.01207;0.82271;0.02552;0.72435;-0.01073;0.90409;0.11066;0.72837;0.02750|], "g"));

  print_endline "\ntesting with hardcoded values:";
  print_endline (one_nn
				   ([ ([|1.; 1.; 1.; 1.; 1.|], "1");
					  ([|2.; 2.; 2.; 2.; 2.|], "2");
					  ([|3.; 3.; 3.; 3.; 3.|], "3");
					  ([|4.; 4.; 4.; 4.; 4.|], "4");
					  ([|5.; 5.; 5.; 5.; 5.|], "5") ])
				   ([|1.; 2.; 3.; 4.; 5.|], "0");)