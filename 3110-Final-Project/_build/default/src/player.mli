open Deck

(** Representation of a player and their attributes.

    This module represents a player (either active or CPU) and includes details such as, 
    their balance, cards, betting amounts and action. *)

type player = {
  mutable balance : int;
  mutable cards : card list;
  mutable fold : bool;
  mutable raised : int;
  mutable bet : int;
}

type hand = card list
type action = Raise of int | Check | Fold | Call | Shove

(* make_player initializes a player *)
val make_player : unit -> player

(* getBalance returns the balance of the desired player*)
val getBalance : player -> int

(* getCards shows which cards a given player has *)
val getCards : player -> hand

(* isFold returns true when a player elects to fold and false otherwise *)
val isFold : player -> bool

(* getRaised returns the amount a player bets when they raise*)
val getRaised : player -> int

(* raise represents a player raising the bet by some amount*)
val raise : player -> int -> unit

(* fold represents the player folding and forfeiting the round*)
val fold : player -> unit
