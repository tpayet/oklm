module type FIXED = sig
	type t
	val of_float : float -> t
	val of_int : int -> t
	val to_float : t -> float
	val to_int : t -> int
	val to_string : t -> string
	val zero : t
	val one : t
	val succ : t -> t
	val pred : t -> t
	val min : t -> t -> t
	val max : t -> t -> t
	val gth : t -> t -> bool
	val lth : t -> t -> bool
	val gte : t -> t -> bool
	val lte : t -> t -> bool
	val eqp : t -> t -> bool (** physical equality *)
	val eqs : t -> t -> bool (** structural equality *)
	val add : t -> t -> t
	val sub : t -> t -> t
	val mul : t -> t -> t
	val div : t -> t -> t
	val foreach : t -> t -> (t -> unit) -> unit
end

(*****************************************************************************)

module type FRACTIONNAL_BITS =
	sig
		val bits: int
	end

module type MAKE =
	functor (Fb:FRACTIONNAL_BITS) -> FIXED

module Make : MAKE =
	functor (Fb:FRACTIONNAL_BITS) ->
	struct

		type t = int

		let of_float ft =
			int_of_float (floor (ft *. float_of_int (1 lsl Fb.bits) +. 0.5))

		let of_int i =
			i lsl Fb.bits

		let to_float fb =
			float_of_int  fb /. float_of_int (1 lsl Fb.bits)

		let to_int fb =
			fb lsr Fb.bits

		let to_string fb =
			string_of_float (to_float fb)

		let zero = 0

		let one = 1 lsl Fb.bits

		let succ fb = fb + 1

	    let pred fb = fb - 1

	    let min fb1 fb2 = if fb1 <= fb2 then fb1 else fb2

	    let max fb1 fb2 = if fb1 >= fb2 then fb1 else fb2

	    let gth fb1 fb2 = fb1 > fb2

	    let lth fb1 fb2 = fb1 < fb2

	    let gte fb1 fb2 = fb1 >= fb2

	    let lte fb1 fb2 = fb1 <= fb2

	    let eqp fb1 fb2 = fb1 == fb2

	    let eqs fb1 fb2 = fb1 = fb2

	    let add fb1 fb2 = fb1 + fb2

	    let sub fb1 fb2 = fb1 - fb2

	    let mul fb1 fb2 = (fb1 * fb2 + (1 lsl Fb.bits - 1)) lsr (1 lsl Fb.bits)

	    let div fb1 fb2 = 
			let tmp = (fb1 lsl Fb.bits) in
			if (tmp >= 0 && fb2 >= 0) || (tmp < 0 && fb2 < 0)
			then (tmp + fb2 / 2) / fb2 else (tmp - fb2 / 2) / fb2

	    let foreach fb1 fb2 f =
			let rec loop x =
			if x <= fb2 then (f x; loop (x + 1))
			in loop fb1

	end

(*****************************************************************************)

module Fixed4 : FIXED = Make (struct let bits = 4 end)

module Fixed8 : FIXED = Make (struct let bits = 8 end)

let () =
	let x8 = Fixed8.of_float 21.10 in
	let y8 = Fixed8.of_float 21.32 in
	let r8 = Fixed8.add x8 y8 in
	print_endline (Fixed8.to_string r8);
	Fixed4.foreach (Fixed4.zero) (Fixed4.one) (fun f -> print_endline (Fixed4.to_string f));
	print_endline "\nTesting of_int/to_int 42:";
	print_endline (string_of_int (Fixed8.to_int (Fixed8.of_int 42)));
	print_endline "\nTesting of_float/to_float 42.42:";
	print_endline (string_of_float (Fixed8.to_float (Fixed8.of_float 42.42)));
	print_endline "\nTesting to_string/of_float 42.42:";
	print_endline (Fixed8.to_string (Fixed8.of_float 42.42));
	print_endline "\nTesting zero:";
	print_endline (Fixed8.to_string Fixed8.zero);
	print_endline "\nTesting one:";
	print_endline (Fixed8.to_string Fixed8.one);
	print_endline "\nTesting succ one:";
	print_endline (Fixed8.to_string (Fixed8.succ Fixed8.one));
	print_endline "\nTesting pred one:";
	print_endline (Fixed8.to_string (Fixed8.pred Fixed8.one));
	print_endline "\nTesting min zero one:";
	print_endline (Fixed8.to_string (Fixed8.min Fixed8.zero Fixed8.one));
	print_endline "\nTesting gth zero one:";
	print_endline (string_of_bool (Fixed8.gth Fixed8.zero Fixed8.one));
	print_endline "\nTesting lth zero one:";
	print_endline (string_of_bool (Fixed8.lth Fixed8.zero Fixed8.one));
	print_endline "\nTesting gte zero one:";
	print_endline (string_of_bool (Fixed8.gte Fixed8.zero Fixed8.one));
	print_endline "\nTesting lte zero one:";
	print_endline (string_of_bool (Fixed8.lte Fixed8.zero Fixed8.one));
	print_endline "\nTesting eqp zero one:";
	print_endline (string_of_bool (Fixed8.eqp Fixed8.zero Fixed8.one));
	print_endline "\nTesting eqs zero one:";
	print_endline (string_of_bool (Fixed8.eqs Fixed8.zero Fixed8.one));
	print_endline "\nTesting add zero one:";
	print_endline (Fixed8.to_string (Fixed8.add Fixed8.zero Fixed8.one));
	print_endline "\nTesting sub zero one:";
	print_endline (Fixed8.to_string (Fixed8.sub Fixed8.zero Fixed8.one));
	print_endline "\nTesting mul zero one:";
	print_endline (Fixed8.to_string (Fixed8.mul Fixed8.zero Fixed8.one));
	print_endline "\nTesting div zero one:";
	print_endline (Fixed8.to_string (Fixed8.div Fixed8.zero Fixed8.one));