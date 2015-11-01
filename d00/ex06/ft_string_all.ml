let ft_string_all predicate string_to_test =
	if (String.length string_to_test) > 0 then
	begin
		let rec test_each_char string_tested pred_function index =
			if index >= 0 then
			begin
				if pred_function (String.get string_tested index) then
					true && test_each_char string_tested pred_function (index - 1)
				else
					false
			end	
			else
				true
		in test_each_char string_to_test predicate ((String.length string_to_test) - 1)
	end
	else
		true

let is_digit c =
	c >= '0' && c<= '9'

let is_space c =
	c = ' '

let is_alpha c =
	(c >= 'a' && c <= 'z') || (c >= 'A' && c<= 'Z')

let is_alnum c =
	is_digit c || is_alpha c

let test_ft_string_all string_to_test =
	print_string "Tested string: \"";
	print_string string_to_test;
	print_string "\"\n\nWith is_digit: ";
	print_endline (string_of_bool (ft_string_all is_digit string_to_test));
	print_string "With is_space: ";
	print_endline (string_of_bool (ft_string_all is_space string_to_test));
	print_string "With is_alpha: ";
	print_endline (string_of_bool (ft_string_all is_alpha string_to_test));
	print_string "With is_alnum: ";
	print_endline (string_of_bool (ft_string_all is_alnum string_to_test));
	print_endline "\n**********************************************************\n"

let main () = 
	test_ft_string_all "123";
	test_ft_string_all "Hello world !";
	test_ft_string_all "abcd";
	test_ft_string_all "   ";
	test_ft_string_all "abcd123"

(*****************************************************************************)

let () = main ()