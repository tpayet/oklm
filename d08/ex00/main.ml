let () =
	let a = new Atom.hydrogen in
	print_endline (a#to_string);

	let a = new Atom.carbon in
	print_endline (a#to_string);

	let a = new Atom.oxygen in
	print_endline (a#to_string);

	let a = new Atom.aluminium in
	print_endline (a#to_string);

	let a = new Atom.nitrogen in
	print_endline (a#to_string);

	let a = new Atom.helium in
	print_endline (a#to_string);

	let a = new Atom.lithium in
	print_endline (a#to_string);

	let a = new Atom.potassium in
	print_endline (a#to_string);

	let a = new Atom.calcium in
	print_endline (a#to_string);

	let a = new Atom.zinc in
	print_endline (a#to_string);

	let a = new Atom.copper in
	print_endline (a#to_string);

	let a = new Atom.hydrogen in
	let b = new Atom.oxygen in
	let c = new Atom.oxygen in
	print_endline ("\nTesting equals on Hydrogen && Oxygen: " ^ (string_of_bool (a#equals b)));
	print_endline ("Testing equals on Oxygen && Oxygen: " ^ (string_of_bool (c#equals b)))
