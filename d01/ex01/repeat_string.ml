let repeat_string ?(str="") n =
	if n < 0 then
		"Error"
	else if str = "" then
	begin
		let rec loop str n =
			if n = 0 then 
				str
			else
				let arg = str ^ "x" in
					loop arg (n - 1)
		in loop "" n
	end
	else
	begin
		let rec loop2 stri m =
			if m = 0 then
				stri
			else
				let argu = str ^ stri in
					loop2 argu (m - 1)
	end