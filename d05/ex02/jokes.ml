let () =
	let jokes = [|"C'est un schtroumpf,\nil court,\nil tombe,\nil se fait un bleu.";
			"C'est l'histoire d'une blague vaseuse\nMets tes bottes.";
			"C'est l'histoire d'un poil, avant il etait bien, maintenant il est pubien.";
			"C'est l'histoire d'un zoophile, il rentre dans un bar.";
			"C'est quoi le plus dur a mixer dans un légume ?\nLe fauteil."|]
	in Random.self_init (); print_endline (jokes.(Random.int 5))
