let () =
	Random.self_init ();
	let doc = new Doctor.doctor "Who" 987 "Jack sparrow" in
	let human = new People.people "HUMAN A" in
	let dalek = new Dalek.dalek () in
	human#talk;
	dalek#talk;
	dalek#exterminate human;
	doc#use_sonic_screwdriver;
	ignore (dalek#die)
