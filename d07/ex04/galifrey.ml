class galifrey (l_d:Dalek.dalek list) (l_doc:Doctor.doctor list) (l_p:People.people list) =
	object

	val _l_d = l_d

	val _l_doc = l_doc

	val _l_p = l_p

	method do_time_war =
		print_endline "Beginning of the war !\n";
		let rec war_loop (bim, bam, boum) =
			match (bim, bam, boum) with
				  ([], _, []) -> "Arg! They are all dead..."
			 	| (_, _, []) -> "The daleks won the war!"
			 	| ([], _, _) -> "The doctor won the war!"
			 	| (hda::tda, dr, hpe::tpe) -> let doc = List.hd dr in
			 				doc#talk;
			 				doc#use_sonic_screwdriver;
			 				hpe#talk;
			 				hda#talk; hda#exterminate hpe;
			 				hda#die;
			 				print_char '\n';
			 				war_loop (tda, dr, tpe)
		in print_endline (war_loop (_l_d, _l_doc, _l_p))

	end