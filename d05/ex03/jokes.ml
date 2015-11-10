let count_data str =
	let r = ref 0 in
		for i = 0 to (String.length str) - 1 do
			if str.[i] = ';' then incr r
		done;
	!r

let do_the_joke path =
	let ic = open_in path in
	let str = input_line ic in
	close_in ic;
	let len = (count_data str) + 1 in
	let arr = (Array.make len "") in
	let start = ref 0 in
	if len = 1 then (Array.set arr 0 str; print_endline arr.(0))
	else
		let stop = ref (String.index str ';') in
		for i = 0 to (len - 1) do
			Array.set arr i (String.sub str !start (!stop - !start));
			start := !stop + 1;
			if i >= (len - 2) then
				stop:= (String.length str)
			else
				stop:= (String.index_from str !start ';');
		done;
		print_endline (arr.(Random.int len))

let () =
	Random.self_init();
	if Array.length Sys.argv = 2 then do_the_joke (Array.get Sys.argv 1)