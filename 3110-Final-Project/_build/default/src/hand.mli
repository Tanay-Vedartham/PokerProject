open Deck
open Board

(** Representation of a player's card hand. 

    This module represents the hand of player, more specifically the combinations
     they can create with their hand and the cards on the board. *)

type hand

(* best_hand determines what the best possible hand a player can form from their cards
   and the board *)
val best_hand : card * card -> card list -> hand

(* compare_hands takes in two hands and compares them based on which form the best
   combination with the board *)
val compare_hands : card * card -> card list -> card * card -> card list -> int

(* string_of_hand converts a hand to the string representation of it *)
val string_of_hand : hand -> string
