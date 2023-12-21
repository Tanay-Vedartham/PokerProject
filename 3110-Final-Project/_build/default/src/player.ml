open Deck

type hand = card list

type player = {
  mutable balance : int;
  mutable cards : card list;
  mutable fold : bool;
  mutable raised : int;
  mutable bet : int;
}

type action = Raise of int | Check | Fold | Call | Shove

let make_player () =
  { balance = 0; cards = []; fold = false; raised = 0; bet = 0 }

let getBalance p = p.balance
let getCards p = p.cards
let isFold p = p.fold
let getRaised p = p.raised
let get_bet p = p.bet

let raise p v =
  p.raised <- v;
  p.balance <- p.balance - v

let fold pl = pl.fold <- true
let isRaise action = String.lowercase_ascii action = "raise"

type p = { mutable bet : int }

let person = { bet = 0 }

let action_prompt action =
  if isRaise action then (
    print_newline ();
    print_endline "Amount:";
    let bet =
      print_newline ();
      input_line stdin
    in
    person.bet <- int_of_string bet;
    print_newline ();
    print_endline ("Balance is: " ^ string_of_int person.bet))
  else print_endline ""
