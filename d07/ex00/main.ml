let () =
	let p = new People.people "John Doe" in
	print_endline p#to_string;
	p#talk;
	p#die