module StringHash =
	struct

		type t = string

		let equal s_1 s_2 =
			s_1 = s_2

		let hash str = 
			let rec loop n acc =
				if n < 0 then acc
				else loop (n - 1) (acc * 1000 + int_of_char (str.[n]))
			in loop ((String.length str) - 1) 0
			
	end

module StringHashtbl = Hashtbl.Make(StringHash)

(*****************************************************************************)

let () =
	let ht = StringHashtbl.create 5 in
	let values = [ "Hello"; "world"; "42"; "Ocaml"; "H" ] in
	let pairs = List.map (fun s -> (s, String.length s)) values in
		List.iter (fun (k,v) -> StringHashtbl.add ht k v) pairs;
		StringHashtbl.iter (fun k v -> Printf.printf "k = \"%s\", v = %d\n" k v) ht