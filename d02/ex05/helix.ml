let rec list_rev_append l1 l2 =
	 match l1 with
	  [] -> l2
	| h::t -> list_rev_append t (h::l2)

let list_reverse l =
	list_rev_append l []

type phosphate = string

type deoxyribose = string

type nucleobase = A | T | C | G | None

type nucleotide = {p: phosphate; d: deoxyribose; n: nucleobase}

let generate_nucleotide c =
	{	p = "phosphate";
		d = "deoxyribose";
		n = match c with
			  'A' -> A
			| 'T' -> T
			| 'C' -> C
			| 'G' -> G
			| _ -> None
	}

type helix = nucleotide list

let generate_helix n =
	if n < 1 then []
	else
		let rec loop n_l i =
			if i < 0 then
				n_l
			else
				loop (generate_nucleotide begin
											match (Random.int 4) with
											  0 -> 'A'
											| 1 -> 'T'
											| 2 -> 'C'
											| 3 -> 'G'
											| _ -> 'Z'

											end
				::n_l) (i - 1)
		in loop [] n

let helix_to_string l =
	if l = [] then ""
	else
		let rec loop li s =
			match li with
			  h::t -> loop t (begin
			  						match h.n with
			  						  A -> "A"
			  						| T -> "T"
			  						| C -> "C"
			  						| G -> "G"
			  						| None -> "-"
			  					end
			  					 ^ s)
			| _ -> s
		in loop l ""

let complementary_helix h =
	if h = [] then []
	else
		let rec loop c_h o_h =
			match o_h with 
				  t::q -> loop (generate_nucleotide begin
				  					match t.n with
				  						  A -> 'T'
				  						| T -> 'A'
				  						| C -> 'G'
				  						| G -> 'C'
				  						| None -> 'Z'
				  				end
				  				::c_h) q
				| _ -> c_h
		in list_reverse (loop [] h)


(*****************************************************************************)

let() =
	Random.self_init ();
  	for i = -1 to 8 do 
		let h = generate_helix i in
		print_endline ("\ntesting "^(string_of_int i)^":");
		print_endline ("       -normal: "^(helix_to_string h)); 
		print_endline ("-complementary: "^(helix_to_string (complementary_helix h)))
  	done