open Deck
(** Representation of the visible game features.

    This module represents the board including the players' cards, the pot, and the board. *)

type board = card list

(* make_board creates a board representing the cards on table *)
val make_board : unit -> board
