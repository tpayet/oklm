class virtual atom name symbol atomic_number =
	object

		val _name = name

		val _symbol = symbol

		val _atomic_number = atomic_number

		method name =
			_name

		method symbol = 
			_symbol

		method atomic_number =
			_atomic_number

		method to_string =
			"Atom: " ^ _name ^ ", symbol: " ^ _symbol ^ ", atomic_number: " ^ string_of_int _atomic_number

		method equals (a:atom) =
			a#atomic_number = _atomic_number

	end

class hydrogen =
	object
		inherit atom "Hydrogen" "H" 1
	end

class carbon =
	object
		inherit atom "Carbon" "C" 6
	end

class oxygen =
	object
		inherit atom "Oxygen" "O" 8
	end

class aluminium =
	object
		inherit atom "Aluminium" "Al" 13
	end

class nitrogen =
	object
		inherit atom "Nitrogen" "N" 7
	end

class helium =
	object
		inherit atom "Helium" "He" 2
	end

class lithium =
	object
		inherit atom "Lithium" "Li" 3
	end

class potassium =
	object
		inherit atom "Potassium" "K" 19
	end

class calcium =
	object
		inherit atom "Calcium" "Ca" 20
	end

class zinc =
	object
		inherit atom "Zinc" "Zn" 30
	end

class copper =
	object
		inherit atom "Copper" "Cu" 29
	end