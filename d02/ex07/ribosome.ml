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
				  						| _ -> 'Z'
				  				end
				  				::c_h) q
				| _ -> c_h
		in list_reverse (loop [] h)

type rna = nucleobase list

let generate_rna h =
	if h = [] then []
	else
		let rec loop rna o_h =
			match o_h with
			  t::q ->loop (begin
			  					match t.n with
			  						  A -> U
			  						| T -> A
			  						| C -> G
				  					| G -> C
				  					| _ -> None
			  				end
			  				::rna) q
			| _ -> rna
		in list_reverse (loop [] h)

(*****************************************************************************)

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

(* let decode_arn rna =
	let rec loop t p =
		match t with
			  ((U, A, A) | (U, A, G) | (U, G, A))::tail -> list_reverse (Stop::p)
			| ((G, C, A) | (G, C, C) | (G, C, G) | (G, C, U))::tail -> loop t (Ala::p)
			| ((A, G, A) | (A, G, G) | (C, G, A) | (C, G, C) | (C, G, G) | (C, G, U))::tail -> loop t (Arg::p)
			| ((A, A, C) | (A, A, U))::tail -> loop t (Asn::p)
			| ((G, A, C) | (G, A, U))::tail -> loop t (Asp::p)
    		| ((U, G, C) | (U, G, U))::tail -> loop t (Cys::p)
    		| ((C, A, A) | (C, A, G))::tail -> loop t (Gln::p)
    		| ((G, A, A) | (G, A, G))::tail -> loop t (Glu::p)
    		| ((G, G, A) | (G, G, C) | (G, G, G) | (G, G, U))::tail -> loop t (Gly::p)
    		| ((C, A, C) | (C, A, U))::tail -> loop t (His::p)
    		| ((A, U, A) | (A, U, C) | (A, U, U))::tail -> loop t (Ile::p)
    		| ((C, U, A) | (C, U, C) | (C, U, G) | (C, U, U) | (U, U, A) | (U, U, G))::tail -> loop t (Leu::p)
		    | ((A, A, A) | (A, A, G))::tail -> loop t (Lys::p)
		    | ((A, U, G))::t -> loop t (Met::p)
		    | ((U, U, C) | (U, U, U))::t -> loop t (Phe::p)
		   	| ((C, C, C) | (C, C, A) | (C, C, G) | (C, C, U))::t -> loop t (Pro::p)
		    | ((U, C, A) | (U, C, C) | (U, C, G) | (U, C, U) | (A, G, U) | (A, G, C))::t -> loop t (Ser::p)
		    | ((A, C, A) | (A, C, C) | (A, C, G) | (A, C, U))::t -> loop t (Thr::p)
		    | ((U, G, G))::t -> loop t (Trp::p)
		    | ((U, A, C) | (U, A, U))::t -> loop t (Tyr::p)
		    | ((G, U, A) | (G, U, C) | (G, U, G) | (G, U, U))::t -> loop t (Val::p)
			| _ -> []
	in loop (generate_base_triplets rna) [] *)

let decode_arn rna =
  let rec loop tri pro =
	match tri with
	  ((U, A, A) | (U, A, G) | (U, G, A))::t                                     -> list_reverse (Stop::pro)
    | ((G, C, A) | (G, C, C) | (G, C, G) | (G, C, U))::t                         -> loop t (Ala::pro)
    | ((A, G, A) | (A, G, G) | (C, G, A) | (C, G, C) | (C, G, G) | (C, G, U))::t -> loop t (Arg::pro)
    | ((A, A, C) | (A, A, U))::t                                                 -> loop t (Asn::pro)
    | ((G, A, C) | (G, A, U))::t                                                 -> loop t (Asp::pro)
    | ((U, G, C) | (U, G, U))::t                                                 -> loop t (Cys::pro)
    | ((C, A, A) | (C, A, G))::t                                                 -> loop t (Gln::pro)
    | ((G, A, A) | (G, A, G))::t                                                 -> loop t (Glu::pro)
    | ((G, G, A) | (G, G, C) | (G, G, G) | (G, G, U))::t                         -> loop t (Gly::pro)
    | ((C, A, C) | (C, A, U))::t                                                 -> loop t (His::pro)
    | ((A, U, A) | (A, U, C) | (A, U, U))::t                                     -> loop t (Ile::pro)
    | ((C, U, A) | (C, U, C) | (C, U, G) | (C, U, U) | (U, U, A) | (U, U, G))::t -> loop t (Leu::pro)
    | ((A, A, A) | (A, A, G))::t                                                 -> loop t (Lys::pro)
    | ((A, U, G))::t                                                             -> loop t (Met::pro)
    | ((U, U, C) | (U, U, U))::t                                                 -> loop t (Phe::pro)
    | ((C, C, C) | (C, C, A) | (C, C, G) | (C, C, U))::t                         -> loop t (Pro::pro)
    | ((U, C, A) | (U, C, C) | (U, C, G) | (U, C, U) | (A, G, U) | (A, G, C))::t -> loop t (Ser::pro)
    | ((A, C, A) | (A, C, C) | (A, C, G) | (A, C, U))::t                         -> loop t (Thr::pro)
    | ((U, G, G))::t                                                             -> loop t (Trp::pro)
    | ((U, A, C) | (U, A, U))::t                                                 -> loop t (Tyr::pro)
    | ((G, U, A) | (G, U, C) | (G, U, G) | (G, U, U))::t                         -> loop t (Val::pro)
	| _ -> []
  in loop (generate_base_triplets rna) []

let string_of_protein pro =
    let rec loop l str =
      match l with
        []       -> str
      | hd :: tl -> loop tl (str ^ begin match hd with
                                            Ala -> "Alanine"
										  | Arg -> "Arginine"
										  | Asn -> "Asparagine"
										  | Asp -> "Aspartique"
										  | Cys -> "Cysteine"
										  | Gln -> "Glutamine"
										  | Glu -> "Glutamique"
										  | Gly -> "Glycine"
										  | His -> "Histidine"
										  | Ile -> "Isoleucine"
										  | Leu -> "Leucine"
										  | Lys -> "Lysine"
										  | Met -> "Methionine"
										  | Phe -> "Phenylalanine"
										  | Pro -> "Proline"
										  | Ser -> "Serine"
										  | Thr -> "Threonine"
										  | Trp -> "Tryptophane"
										  | Tyr -> "Tyrosine"
										  | Val -> "Valine"
										  | Stop -> "EOP"
                                    end ^ "-")
    in loop pro ""

(*****************************************************************************)

let() =
	Random.self_init ();
	let string_of_rna rna =
		let rec loop r str =
	  	match r with
		[]       -> str
	  | hd :: tl -> loop tl (str ^ begin match hd with
											A    -> "A"
										  | T    -> "T"
										  | C    -> "C"
										  | G    -> "G"
										  | U    -> "U"
										  | _ -> "None"
									end)	  
	in loop rna ""
  in let h = generate_helix 1000 in
	 print_endline ("Testing "^(string_of_int 1000)^":");
	 print_endline ("\nHelix :\n"^(helix_to_string h)); 
	 print_endline ("\nRNA :\n"^(string_of_rna (generate_rna h)));
	 print_endline ("\nProtein :\n"^(string_of_protein (decode_arn (generate_rna h))))