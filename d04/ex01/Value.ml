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