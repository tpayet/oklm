let uncaesar n str =
	Cipher.caesar (-n) str

let unrot42 str =
	uncaesar 42 str

let ft_uncrypt str f_l =
	let rec aux l1 l2 =
		match l1 with
		  h::t -> aux t (h::l2)
		| _ -> l2
	in Cipher.ft_crypt str (aux f_l [])

let() =
  let s = "Ushhh... this is a secret string! :O" in
  print_endline ("String to crypt: '" ^ s ^ "'");
  print_endline ("rot42:   '" ^ 
				   (Cipher.rot42 s) ^ "'");
  print_endline ("unrot42: '" ^ 
				   (unrot42 (Cipher.rot42 s)) ^ "'");
  print_endline ("caesar 12:   '" ^ 
				   (Cipher.caesar 12 s) ^ "'");
  print_endline ("uncaesar 12: '" ^ 
				   (uncaesar 12 (Cipher.caesar 12 s)) ^ "'");
  print_endline ("caesar 4242:   '" ^ 
				   (Cipher.caesar 4242 s) ^ "'");
  print_endline ("uncaesar 4242: '" ^ 
				   (uncaesar 4242 (Cipher.caesar 4242 s)) ^ "'");
  print_endline ("caesar -4242:   '" ^ (Cipher.caesar (-4242) s) ^ "'");
  print_endline ("uncaesar -4242: '" ^ 
				   (uncaesar (-4242) (Cipher.caesar (-4242) s)) ^ "'");
  print_endline ("xor 42:     '" ^ 
				   (Cipher.xor 42 s) ^ "'");
  print_endline ("xor 42: '" ^ 
				   (Cipher.xor 42 (Cipher.xor 42 s)) ^ "'");
  print_endline ("ft_crypt [xor 42; caesar 42]:     '" ^ 
				   (Cipher.ft_crypt s [(Cipher.xor 42); (Cipher.caesar 42)]) ^ "'");
  print_endline ("ft_uncrypt [xor 42; uncaesar 42]: '" ^ 
				   (ft_uncrypt 
					  (Cipher.ft_crypt s [(Cipher.xor 42); (Cipher.caesar 42)])
					  [(Cipher.xor 42); (uncaesar 42)]) ^ "'")