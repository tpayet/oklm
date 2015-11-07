module Color =
	struct

		type t = Spade | Heart | Diamond | Club

		let all = [Spade; Heart; Diamond; Club]

		let toString t =
			match t with
			  Spade -> "S"
			| Heart -> "H"
			| Diamond -> "D"
			| Club -> "C"

		let toStringVerbose t =
			match t with
			  Spade -> "Spade"
			| Heart -> "Heart"
			| Diamond -> "Diamond"
			| Club -> "Club"

	end

module Value =
	struct

		type t = T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen | King | As

		let all = [T2; T3; T4; T5; T6; T7; T8; T9; T10; Jack; Queen; King; As]

		let toInt t =
			match t with
			  T2 -> 2
			| T3 -> 3
			| T4 -> 4
			| T5 -> 5
			| T6 -> 6
			| T7 -> 7
			| T8 -> 8
			| T9 -> 9
			| T10 -> 10
			| Jack -> 11
			| Queen -> 12
			| King -> 13
			| As -> 14

		let toString t =
			match t with
			  T2 -> "2"
			| T3 -> "3"
			| T4 -> "4"
			| T5 -> "5"
			| T6 -> "6"
			| T7 -> "7"
			| T8 -> "8"
			| T9 -> "9"
			| T10 -> "10"
			| Jack -> "J"
			| Queen -> "Q"
			| King -> "K"
			| As -> "A"

		let toStringVerbose t =
			match t with
			  T2 -> "2"
			| T3 -> "3"
			| T4 -> "4"
			| T5 -> "5"
			| T6 -> "6"
			| T7 -> "7"
			| T8 -> "8"
			| T9 -> "9"
			| T10 -> "10"
			| Jack -> "Jack"
			| Queen -> "Queen"
			| King -> "King"
			| As -> "As"

		let next t = 
			match t with
			  T2 -> T3
			| T3 -> T4
			| T4 -> T5
			| T5 -> T6
			| T6 -> T7
			| T7 -> T8
			| T8 -> T9
			| T9 -> T10
			| T10 -> Jack
			| Jack -> Queen
			| Queen -> King
			| King -> As
			| As -> invalid_arg "As was called in Value.next"

		let previous t = 
			match t with
			  T2 -> invalid_arg "T2 was called in Value.previous"
			| T3 -> T2
			| T4 -> T3
			| T5 -> T4
			| T6 -> T5
			| T7 -> T6
			| T8 -> T7
			| T9 -> T8
			| T10 -> T9
			| Jack -> T10
			| Queen -> Jack
			| King -> Queen
			| As -> King
	end


type t = (Value.t * Color.t)

let newCard va co =
  (va, co)

let allSpades = List.map (fun x -> newCard x Color.Spade) Value.all

let allHearts = List.map (fun x -> newCard x Color.Heart) Value.all

let allDiamonds = List.map (fun x -> newCard x Color.Diamond) Value.all

let allClubs = List.map (fun x -> newCard x Color.Club) Value.all

let all = allSpades @ allHearts @ allDiamonds @ allClubs

let getValue = function
	(va, _) -> va

let getColor = function 
	(_, co) -> co

let toString = function
	(va, co) -> (Value.toString va) ^ (Color.toString co)

let toStringVerbose = function
	(va, co) -> "Card(" ^ (Value.toStringVerbose va)
			  ^ "," ^ (Color.toStringVerbose co) ^ ")"

let compare (va1, _) (va2, _) =
  match (Value.toInt va1) - (Value.toInt va2) with
	z when z < 0 -> -1
  |	z when z > 0 -> 1
  | _            -> 0

let max ca1 ca2 =
  if ca1 >= ca2 then ca1 else ca2

let min ca1 ca2 =
  if ca1 <= ca2 then ca1 else ca2

let best = function
	[]         -> invalid_arg "best: empty list"
  | head::tail -> List.fold_left max head tail

let isOf (_, co1) co2 =
	co1 = co2

let isSpade ca =
  isOf ca Color.Spade

let isHeart ca =
  isOf ca Color.Heart

let isDiamond ca =
  isOf ca Color.Diamond

let isClub ca =
  isOf ca Color.Club