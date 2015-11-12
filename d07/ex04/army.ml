class ['a] army (l: 'a list) =
	object

		val _l = l

		method add el = new army (el::_l)

		method delete = new army (List.tl _l)

		method get_l =
			_l

	end