let my_sleep () = Unix.sleep 1

let () =

	if Array.length Sys.argv = 2 then
	begin
		try 
			begin
				let u = int_of_string (Sys.argv.(1)) in
				for i = 0 to (u - 1) do
					my_sleep ()
				done
			end
		with Failure e -> print_endline ("Catching exception: "^ e)
	end
	else exit 1