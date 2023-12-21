open Deck

type pot = { main_pot : int; side_pot : int }
type board = card list

let make_board () = []
(* let make_pot = { main_pot = 0; side_pot = 0 }
   let get_main_pot cpot = cpot.main_pot
   let get_flop bd = bd.flop
   let get_turn bd = bd.turn
   let get_river bd = bd.river

   let rec cards_str_helper = function
     | [] -> ""
     | h :: t -> print_card h ^ "|" ^ cards_str_helper t

   let cards_str bd f t r =
     if r then
       "Flop : " ^ cards_str_helper bd.flop ^ " || Turn : "
       ^ cards_str_helper bd.turn ^ " || River : " ^ cards_str_helper bd.river
     else if t then
       "Flop : " ^ cards_str_helper bd.flop ^ " || Turn : "
       ^ cards_str_helper bd.turn
     else if f then "Flop : " ^ cards_str_helper bd.flop
     else "Empty Board"

   let print_board curr_bd =
     if curr_bd.flop = [] then "Empty board"
     else if curr_bd.turn = [] then cards_str curr_bd true false false
     else if curr_bd.river = [] then cards_str curr_bd true true false
     else cards_str curr_bd true true true

   let setPot p b side sb = assert false *)
