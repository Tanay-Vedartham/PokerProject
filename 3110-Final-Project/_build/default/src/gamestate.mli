open Deck
open Prompt
open Player
open Board

(** Representation of dynamic game state.

    This module represents the state of a board as it is being played,
    including the players' cards, the pot, and the board. *)

type round = Pflop | Flop | Turn | River | Showdown | Folded of bool | End

type gamestate = {
  mutable boardst : board;
  mutable deck : deck;
  mutable pot : int;
  mutable round : round;
  player : player;
  bot : player;
  mutable p_turn : bool;
  mutable raise_val : int;
  mutable end_round : bool;
  mutable dealer : bool;
  bb : int;
  sb : int;
}

val init_gs : gamestate

(* game initaties the poker game from an initial gamestate *)
val game : gamestate -> unit