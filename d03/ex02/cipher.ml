let rec caesar n str =
	if n < 0 then caesar (n + 128) str
	else
		let rot c =
			char_of_int ((int_of_char c + n) mod 128)
		in String.map rot str

let rot42 str =
	caesar 42 str

let xor key str =
	let aux c =
		char_of_int(key lxor (int_of_char c))
	in String.map aux str

let rec ft_crypt (str:string) f_l =
	match f_l with
	  h::t -> ft_crypt (h str) t
	| [] -> str