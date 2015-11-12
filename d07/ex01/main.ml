let () =
	let doc = new Doctor.doctor "What" 28 "Who" in
	print_endline"\n\nTesting doc#to_string";
	print_endline doc#to_string;
	print_endline"\n\nTesting doc#use_sonic_screwdriver";
	doc#use_sonic_screwdriver;
	print_endline"\n\nTesting doc#travel_in_time from 2015 to 1992";
	doc#travel_in_time 2015 1992;
	print_endline"\n\nTesting doc#test_regeneration";	
	doc#test_regeneration