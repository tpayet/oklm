let ft_is_palindrome str =
	if String.length str > 0 then
		let rec loop str index =
			if index <= ((String.length str) / 2) then
				if String.get str index = String.get str (((String.length str) - 1) - index) then
					true && loop str (index + 1)
				else
					false
			else
				true
		in loop str 0
	else
		true

let test_ft_is_palindrome str =
	print_string "Test with \"";
	print_string str;
	print_string "\": ";
	print_endline (string_of_bool (ft_is_palindrome str))


let main () =
	test_ft_is_palindrome "radar";
	test_ft_is_palindrome "madam";
	test_ft_is_palindrome "car";
	test_ft_is_palindrome ""

(*****************************************************************************)

let () = main ()