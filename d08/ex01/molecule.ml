class virtual molecule name (a_list: Atom.atom list) =
	object

		val _name = name

		val _formula = let rec loop n str = function
			  [] -> str
	  		| hd::nt::tl when hd#name = nt#name -> loop (n+1) str (nt::tl)
	  		| hd::nt::tl -> loop 1 (str ^ hd#symbol ^ (
	  							if n > 1
	  							then (string_of_int n) 
								else ""))
	  						(nt::tl)
	  		| hd::tl -> str ^ hd#symbol ^
	  							(if n > 1 
								then (string_of_int n) 
								else "")
			in loop 1 "" (List.sort 
					 (fun x y -> match (x#symbol, y#symbol) with
								   (a, b) when a = b -> 0
								 | (_, "C")          -> 1
								 | ("C", _)          -> -1
								 | (_, "H")          -> 1
								 | ("H", _)          -> -1
								 | (a, b)            -> compare a b) a_list)

		method name =
			_name

		method formula =
			_formula
		
		method to_string =
			"Molecule: " ^ _name ^ ", formula: " ^ _formula

		

	end