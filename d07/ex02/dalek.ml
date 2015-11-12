class dalek ?(hp=100) =
	object (self)

		val _hp = hp


		val _name:string = Dalek 	^ String.make 1 (char_of_int ((Random.int 26) + 65))
									^ String.make 1 (char_of_int ((Random.int 26) + 97))
									^ String.make 1 (char_of_int ((Random.int 26) + 97))

		val mutable _shield = true

		method talk =
			let aux i =
				match i with 
				  0 -> "Explain! Explain!"
				| 1 -> "Exterminate! Exterminate!"
				| 2 -> "I obey!"
				| 3 -> "You are the Doctor! You are the enemy of the Daleks!"
			in print_endline (aux Random.int 3)

		method exterminate (p:People.people) =
	end