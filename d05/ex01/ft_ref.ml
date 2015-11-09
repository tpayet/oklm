type 'a ft_ref = { mutable value : 'a }

let return x =
 { value = x }

let get r =
  r.value

let set r x = 
  r.value <- x
  
let bind r f:('b ft_ref) =
  f r.value

let () =
  let r = return 1 in
  print_endline ("Testing return 42:\n" ^ (string_of_int (get r)));
  set r 42; 
  print_endline ("\nTesting set 41 / get:\n" ^ (string_of_int (get r)));
  print_endline ("\nTesting bind r + 1:\n"
				 ^ (string_of_int (get (bind r (fun x -> return (x + 1))))))