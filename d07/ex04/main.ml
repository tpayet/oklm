let () =
	Random.self_init ();
	Random.self_init ();
  	let ga = new Galifrey.galifrey 
			   [new Dalek.dalek ();
				new Dalek.dalek ();
				new Dalek.dalek ()] 
			   [new Doctor.doctor "$!*$@" 909 "Amy"] 
			   [new People.people "Vastra";
				new People.people "Rose";
				new People.people "Clara";
				new People.people "Strax"] in
  	ga#do_time_war