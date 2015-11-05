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

(***************************TEST, THANKS TO MCANAL****************************)

let() =
  let print_nucleotide nuc =
	print_string "p="; print_string nuc.p;
	print_string ", d="; print_string nuc.d;
	print_string ", b="; print_endline begin match nuc.n with
												A -> "A"
											  | T -> "T"
											  | C -> "C"
											  | G -> "G"
											  | None -> "None"
										end in 
  print_endline "testing A:"; print_nucleotide (generate_nucleotide 'A');
  print_endline "\ntesting T:"; print_nucleotide (generate_nucleotide 'T');
  print_endline "\ntesting C:"; print_nucleotide (generate_nucleotide 'C');
  print_endline "\ntesting G:"; print_nucleotide (generate_nucleotide 'G');
  print_endline "\ntesting Z:"; print_nucleotide (generate_nucleotide 'Z')
