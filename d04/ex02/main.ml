let() =
  print_endline "testing allSpades:";
  List.iter (fun x -> print_string ((Card.toString x) ^ "; ")) Card.allSpades;
  print_endline "\n\ntesting allHearts:";
  List.iter (fun x -> print_string ((Card.toString x) ^ "; ")) Card.allHearts;
  print_endline "\n\ntesting allDiamonds:";
  List.iter (fun x -> print_string ((Card.toString x) ^ "; ")) Card.allDiamonds;
  print_endline "\n\ntesting allClubs:";
  List.iter (fun x -> print_string ((Card.toString x) ^ "; ")) Card.allClubs;
  print_endline "\n\ntesting all (verbose):";
  List.iter (fun x -> print_string ((Card.toStringVerbose x) ^ "; ")) Card.all;
  print_endline "\n\ntesting new card As Spade isSpade:";
  print_endline (string_of_bool (Card.isSpade (Card.newCard Card.Value.As Card.Color.Spade)));
  print_endline "\ntesting new card As Heart isHeart:";
  print_endline (string_of_bool (Card.isHeart (Card.newCard Card.Value.As Card.Color.Heart)));
  print_endline "\ntesting new card As Diamond isDiamond:";
  print_endline (string_of_bool (Card.isDiamond (Card.newCard Card.Value.As Card.Color.Diamond)));
  print_endline "\ntesting new card As Club isClub:";
  print_endline (string_of_bool (Card.isClub (Card.newCard Card.Value.As Card.Color.Club)));
  print_endline "\ntesting max new card As Club new card T2 Club:";
  print_endline (Card.toString (max (Card.newCard Card.Value.As Card.Color.Club)
									(Card.newCard Card.Value.T2 Card.Color.Club)));
  print_endline "\ntesting min new card As Club new card T2 Club:";
  print_endline (Card.toString (min (Card.newCard Card.Value.As Card.Color.Club)
									(Card.newCard Card.Value.T2 Card.Color.Club)));
  print_endline "\ntesting Card.compare and compare new card As Club new card T2 Club:";
  print_int (Card.compare (Card.newCard Card.Value.As Card.Color.Club)
					 (Card.newCard Card.Value.T2 Card.Color.Club));
  print_int (compare (Card.newCard Card.Value.As Card.Color.Club)
					 (Card.newCard Card.Value.T2 Card.Color.Club));
  print_endline "\n\ntesting best of allSpades";
  print_endline (Card.toString (Card.best Card.allSpades));