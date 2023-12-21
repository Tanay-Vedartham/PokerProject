open Deck
open Board

type card = Deck.card
type hand = card list

let deal_player = function c1 :: c2 :: t -> [ c1; c2 ] | _ -> []
let burn = function _ :: t -> t | _ -> []
let deal_flop = function c1 :: c2 :: c3 :: t -> [ c1; c2; c3 ] | _ -> []
let deal_turn = function h :: _ -> h | _ -> assert false
let deal_river d = deal_turn d
