class dalek ?(hp=100) () =
	object

		val _hp = hp

		val _name = "Dalek" 	^ String.make 1 (char_of_int ((Random.int 26) + 65))
									^ String.make 1 (char_of_int ((Random.int 26) + 97))
									^ String.make 1 (char_of_int ((Random.int 26) + 97))

		val mutable _shield = true

		method talk =
			let aux i =
				match i with 
				  0 -> "Explain! Explain!"
				| 1 -> "Exterminate! Exterminate!"
				| 2 -> "I obey!"
				| _ -> "You are the Doctor! You are the enemy of the Daleks!"
			in print_endline (aux (Random.int 3))

		method exterminate (p:People.people) =
			_shield <- not _shield;
			p#die

		method die =
			print_endline "Emergency Temporal Shift!"
	end