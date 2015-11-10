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

let () = 
  let rec test = function
	  []         -> ()
	| head::tail ->  match head with
					   (x, y) -> 
					   for i = 0 to Array.length x -1 do
						 print_float x.(i);
						 print_string ","
					   done;
					   print_endline y;
					   test tail
  in test (examples_of_file "ionosphere.test.csv")