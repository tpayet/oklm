let () =
	let doc = new Doctor.doctor "What" 28 "Who" in
	print_endline"\n\nTesting doc#to_string";
	print_endline doc#to_string;
	print_endline"\n\nTesting doc#use_sonic_screwdriver";
	doc#use_sonic_screwdriver;
	print_endline"\n\nTesting doc#travel_in_time";
	doc#travel_in_time;
	print_endline"\n\nTesting doc#test_regeneration";	
	doc#test_regeneration