open Board
open Deck

(** Representation of the critical actions. 

    This module represents the different actions that will take place during a game, 
    including dealing the cards, flop, turn, and river. *)

(* hand has two cards *)
type hand = card list

(* deal_player deals a player's hand from a shuffled deck. *)
val deal_player : deck -> hand

(* burn returns deck with its top card *)
val burn : deck -> deck

(* deal_flop takes a deck and deals the top three cards *)
val deal_flop : deck -> card list

(* deal_turn takes a deck and deals the turn *)
val deal_turn : deck -> card

(* deal_river takes a deck and deals the river *)
val deal_river : deck -> card
