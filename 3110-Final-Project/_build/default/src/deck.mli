(** Representation of the deck and operations on the deck.

       This module represents the deck and its operations. *)

type card = { suit : int; rank : int }
type deck = card list
type dealt = { card : card; ddeck : deck }

exception EmptyDeck

(* get_suit provides the suit (in integer form - Spades (0), Clubs (1),
   Hearts (2), Diamonds (3)) of a given card *)
val get_suit : card -> int

(* get_rank provides the rank (in integer form - Ace (1), 2-10, Jack (11),
   Queen (12), King (13)) of a given card *)
val get_rank : card -> int

(* make_deck creates a standard 52 card deck
   of 4 suits with thirteen cards each *)
val make_deck : unit -> deck

(* deal takes in a deck and returns a dealt, which consists
   of the top card and the remaining deck *)
val deal : deck -> dealt

(* shuffle performs the shuffling operation on a deck and returns a new deck *)
val shuffle : deck -> deck

(* print_card takes a card and prints the written version of the card
   Ex. {suit = 1; rank = 1} -> "Ace of Clubs"
*)
val print_card : card -> string
