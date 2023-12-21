(* suit
      0 - Spades
      1 - Clubs
      2 - Hearts
      3 - Diamonds*)

exception EmptyDeck
exception UnknownSuit

type card = { suit : int; rank : int }
type deck = card list
type dealt = { card : card; ddeck : deck }

let get_suit c = c.suit
let get_rank c = c.rank

let rec pair_deck deck =
  match deck with [] -> [] | h :: t -> (h, Random.bits ()) :: pair_deck t

let rec pair_deck deck =
  match deck with [] -> [] | h :: t -> (h, Random.bits ()) :: pair_deck t

let randomize pdeck = List.sort (fun (x, y) (z, w) -> compare y w) pdeck

let rec rm_r pdeck =
  match pdeck with [] -> [] | h :: t -> ( match h with x, y -> x :: rm_r t)

let shuffle deck = deck |> pair_deck |> randomize |> rm_r

let rank_string = function
  | 11 -> "Jack"
  | 12 -> "Queen"
  | 13 -> "King"
  | 1 -> "Ace"
  | x -> string_of_int x

let suit_string = function
  | 0 -> "Spades"
  | 1 -> "Clubs"
  | 2 -> "Hearts"
  | 3 -> "Diamonds"
  | _ -> raise UnknownSuit

let print_card card = rank_string card.rank ^ " of " ^ suit_string card.suit

let rec deck_helper num acc =
  if num = 0 then acc
  else
    deck_helper (num - 1) ({ suit = num mod 4; rank = (num mod 13) + 1 } :: acc)

let make_deck () = deck_helper 52 []

let deal deck =
  match deck with [] -> raise EmptyDeck | h :: t -> { card = h; ddeck = t }

let rec empty_the_deck deck = match deck with [] -> [] | h :: t -> []
