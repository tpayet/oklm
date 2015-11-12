class doctor name age ?(hp=100) sidekick_name =
	object (self)

		initializer print_endline "A doctor object is born!"

		val _name:string = name

		val _age:int = age

		val _sidekick:(People.people) = new People.people sidekick_name

		val _hp = hp

		method to_string =
			"My name is " ^ _name ^ ", I am " ^ string_of_int _age ^ ", I have " ^ string_of_int _hp ^ "hp and my sidekick is " ^ _sidekick#get_name

		method talk =
			print_endline "HI! I'am the doctor!"

		method use_sonic_screwdriver =
			print_endline "Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii"

		method private regenerate = new doctor _name _age _sidekick#get_name

		method call_regenerate = self#regenerate

		method get_hurt = new doctor _name _age ~hp:(_hp - 50) _sidekick#get_name

		method how_i_am =
			print_endline ("My current hp is " ^ string_of_int _hp ^ ", shoud I feel good ?")

		method travel_in_time start arrival =
			print_endline ("\n		  _
         /-\
    _____|#|_____
   |_____________|
  |_______________|
|||_POLICE_##_BOX_|||
 | |¯|¯|¯|||¯|¯|¯| |
 | |-|-|-|||-|-|-| |
 | |_|_|_|||_|_|_| |
 | ||~~~| | |¯¯¯|| |
 | ||~~~|!|!| O || |
 | ||~~~| |.|___|| |
 | ||¯¯¯| | |¯¯¯|| |
 | ||   | | |   || |
 | ||___| | |___|| |
 | ||¯¯¯| | |¯¯¯|| |
 | ||   | | |   || |
 | ||___| | |___|| |
|¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯|
 ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯" ^ "\nTraveling from " ^ string_of_int start ^ " to " ^ string_of_int arrival)

		method test_regeneration =
			self#how_i_am; (self#get_hurt)#how_i_am; ((self#get_hurt)#call_regenerate)#how_i_am

	end