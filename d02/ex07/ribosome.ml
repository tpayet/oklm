let rec list_rev_append l1 l2 =
	 match l1 with
	  [] -> l2
	| h::t -> list_rev_append t (h::l2)

let list_reverse l =
	list_rev_append l []

type phosphate = string

type deoxyribose = string

type nucleobase = A | T | C | G | U | None

type nucleotide = {p: phosphate; d: deoxyribose; n: nucleobase}

let generate_nucleotide c =
	{	p = "phosphate";
		d = "deoxyribose";
		n = match c with
			  'A' -> A
			| 'T' -> T
			| 'C' -> C
			| 'G' -> G
			| 'U' -> U
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
			  						| U -> "U"
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

type rna = nucleotide list

let generate_rna h =
	if h = [] then []
	else
		let rec loop rna o_h =
			match o_h with
			  t::q ->loop (generate_nucleotide begin
			  					match t.n with
			  						  A-> 'U'
			  						| T -> 'A'
			  						| C -> 'G'
				  					| G -> 'C'
				  					| None -> 'Z'
			  				end
			  				::rna) q
			| _ -> rna
		in list_reverse (loop [] h)

type aminoacid = Ala | Arg | Asn | Asp | Cys
				| Gln | Glu | Gly | His | Ile
				| Leu | Lys | Met | Phe | Pro
				| Ser | Thr | Trp | Tyr | Val
				| Stop

type protein = aminoacid list

let generate_base_triplets rna =
	let rec loop l t =
		match l with
			  un::deux::trois::suite -> loop suite ((un, deux, trois)::t)
			| _ -> t
	in list_reverse (loop rna [])

let decode_arn rna =
	let rec loop t p =
		match t with
			  ((U, A, A) | (U, A, G) | (U, G, A))::tail -> list_rev (Stop::p)
			| ((G, C, A) | (G, C, C) | (G, C, G) | (G, C, U))::tail -> list_rev (Ala::p)
			| ((A, G, A) | (A, G, G) | (C, G, A) | (C, G, C) | (C, G, G) | (C, G, U))::tail -> list_rev (Arg::p)
			| ((A, A, C) | (A, A, U))::tail -> list_rev (Asn::p)



