class people name =
	object

		initializer print_endline "A people object had been initialized."

		val _name = name

		val _hp = 100

		method to_string = 
			"My name is " ^ _name ^ " and I have " ^ string_of_int _hp ^ "hp."

		method talk =
			print_endline ("I'm " ^ _name ^ "! Do you know the Doctor?")

		method die =
			print_endline "Aaaarghh!"
	end