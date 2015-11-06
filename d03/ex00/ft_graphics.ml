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


let main () =
	Graphics.open_graph " 800x600";
	(* Graphics.moveto 400 300; *)
	(* Graphics.draw_string "42"; *)
	(* draw_square 400 300 100; *)
	draw_tree_node (Node("value", Nil, Nil));
	ignore (Graphics.read_key())

let () = main ()