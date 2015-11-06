type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

let draw_square x y size =
	Graphics.moveto (x - (size / 2)) (y - (size / 2));
	Graphics.lineto (x - (size / 2)) (y + (size / 2));
	Graphics.lineto (x + (size / 2)) (y + (size / 2));
	Graphics.lineto (x + (size / 2)) (y - (size / 2));
	Graphics.lineto (x - (size / 2)) (y - (size / 2))

let draw_tree_node t_n =
	match t_n with
	  Nil -> draw_square 400 300 50; Graphics.moveto 400 300; Graphics.draw_string "Nil"
	| Node (v,_,_) -> draw_square 400 300 50; Graphics.moveto 400 300; Graphics.draw_string v;
						draw_square 500 200 50; Graphics.moveto 500 200; Graphics.draw_string "Nil";
						draw_square 500 400 50; Graphics.moveto 500 400; Graphics.draw_string "Nil";
						Graphics.moveto 425 275; Graphics.lineto 475 225;
						Graphics.moveto 425 325; Graphics.lineto 475 375

(*****************************************************************************)

let rec size tree =
	match tree with
		  Nil -> 0
		| Node(_, x, y) -> (size x) + (size y) + 1

let rec height tree =
	match tree with
		  Nil -> 0
		| Node(_, x, y) -> if (height x) > (height y) then
								((height x) + 1)
							else
								((height y) + 1)

let draw_tree t =
	let size = 200 in
	let rec loop tree x y n =
		match tree with
			  Nil -> draw_square x y (size / n); Graphics.moveto x y; Graphics.draw_string "Nil"
			| Node(v, a, b) -> draw_square x y (size/n); Graphics.moveto x y; Graphics.draw_string v;
													Graphics.moveto (x+(size/(2*n))) y;
													Graphics.lineto ((x + (size/n)) - (size/(4 * n))) (y + size/(2*n));
													Graphics.moveto (x+(size/(2*n))) y;
													Graphics.lineto ((x + (size/n)) - (size/(4 * n))) (y - size/(2*n));
													loop a (x + (size/n)) (y + size/(2*n)) (n * 2);
													loop b (x + (size/n)) (y - size/(2*n)) (n * 2)
			(* | Node(v, a, Nil)  *)
			(* | Node(v, Nil, a) -> draw_square x y size; loop a (x + size) (y) *)
	in loop t 200 300 1

let () =
	Graphics.open_graph " 800x600";
	(* draw_tree (Node("Hello", Nil, Nil)); *)
	draw_tree (Node("Head", Node("R", Node("R-r", Nil, Nil), Node("R-l", Nil, Nil)),  Node("L", Node("L-r", Nil, Nil), Node("L-l", Nil, Nil))));
	ignore (Graphics.read_key())
	(* draw_tree Nil *)