let () =
	let dr = new Doctor.doctor "Who" 987 "Jack sparrow" in
	let pe = new People.people "HUMAN A" in
	let da = new Dalek.dalek () in
	let dr_army = new Army.army [dr] in
	let da_army = new Army.army [da] in
	let pe_army = new Army.army [pe] in

	let dr_army = dr_army#add dr in
	let da_army = da_army#add da in
	let pe_army = pe_army#add pe in

	if dr_army#get_l != [] && da_army#get_l != [] && pe_army#get_l != []
	then print_endline "Army created";

	let dr_army = dr_army#delete in
	let dr_army = dr_army#delete in
	let da_army = da_army#delete in
	let da_army = da_army#delete in
	let pe_army = pe_army#delete in
	let pe_army = pe_army#delete in

	if dr_army#get_l = [] && da_army#get_l = [] && pe_army#get_l = []
	then print_endline "Army destroyed"