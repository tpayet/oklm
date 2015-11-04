let crossover l1 l2 =
	if (l1 = []) || (l2 = []) then []
	else
		let rec search x li =
			match li with
			  h::t -> if h = x then true else search x t
			| _ -> false
		in
		let rec loop l2 lf =
			match l2 with
			  h::t ->	if (search h l1) then
			  				loop t ([h] @lf)
			  			else
			  				loop t lf
 			| _ -> lf
		in loop l2 []

(*****************************************************************************)

let() =
  let rec print_list_int = function
	  []		-> ()
	| hd::tl	-> print_int hd; print_string " "; print_list_int tl

  in let test l1 l2 =
	   print_string "testing "; print_list_int l1; print_string "and ";
	   print_list_int l2; print_endline ":";
	   print_list_int (crossover l1 l2); print_endline "";

	 in let l0 = []
		and l1 = [1]
		and l2 = [1; 2; 3; 4; 5]
		and l3 = [2; 5; 21; 3; 42; 0; 21]
		and l4 = [1; 2; 2; 3; 3; 3; 21; 3] 
		and l5 = [1; 2; 3; 4; 3; 2; 42]

		in test l0 l0;
		   test l0 l1;	
		   test l1 l2;
		   test l2 l3;
		   test l4 l5