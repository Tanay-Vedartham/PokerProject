open Deck
open Prompt
open Player
open Board
open Hand

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
  mutable dealer : bool; (* true if player is dealer, false else*)
  bb : int;
  sb : int;
}

let delay () = Unix.sleepf 0.5

let enter s () =
  print_endline ("ENTER to " ^ s);
  print_endline (input_line stdin)

let table_money gs () =
  print_endline ("player balance is " ^ string_of_int gs.player.balance);
  delay ();
  print_endline ("bot balance is " ^ string_of_int gs.bot.balance);
  delay ();
  print_endline ("pot " ^ string_of_int gs.pot);
  delay ();
  print_newline ()

let if_broke m = m <= 0
let bot_amt gs = gs.pot / 5
let starting_balance = 1000 (*CHANGE THIS*)
let bb = 100
let sb = 50
let checkable gs p = gs.raise_val <= p.raised
let rand_num () = Random.int 10
let checkable gs p = gs.raise_val <= p.raised

let cpu_choice gs cpu =
  let num = rand_num () in
  match num with
  | 0 | 1 -> if checkable gs cpu then "CHECK" else "CALL" (*fold later*)
  | 2 | 3 | 4 | 5 | 6 -> if checkable gs cpu then "CHECK" else "CALL"
  | _ -> "RAISE"

let init_gs =
  {
    boardst = make_board ();
    deck =
      (make_deck ()
      |>
      let _ = Random.self_init () in
      shuffle);
    pot = bb + sb;
    round = Pflop;
    player = { (make_player ()) with balance = starting_balance };
    bot = { (make_player ()) with balance = starting_balance };
    p_turn = false;
    raise_val = bb;
    end_round = false;
    dealer = true;
    bb;
    sb;
  }

let colorify c =
  match String.split_on_char ' ' c with
  | _ :: _ :: [ t ] ->
      if t = "Hearts" || t = "Diamonds" then [ ANSITerminal.red ]
      else if t = "Clubs" || t = "Spades" then [ ANSITerminal.black ]
      else [ ANSITerminal.blue ] (*CHANGE THIS*)
  | _ -> [ ANSITerminal.black ]

let rec print_board cards =
  match cards with
  | h :: t ->
      ANSITerminal.print_string (colorify (print_card h)) (print_card h);
      print_newline ();
      delay ();
      print_board t
  | _ -> print_newline ()

let gsdeal player deck gs =
  player.cards <- (deal deck).card :: player.cards;
  gs.deck <- (deal deck).ddeck

let finish_round gs p =
  gs.boardst <- make_board ();
  gs.deck <-
    (make_deck ()
    |>
    let _ = Random.self_init () in
    shuffle);
  p.balance <- p.balance + gs.pot;
  gs.pot <- bb + sb;
  gs.raise_val <- bb;
  gs.dealer <- not gs.dealer;
  if gs.dealer then gs.p_turn <- false else gs.p_turn <- true;
  gs.round <- Pflop;
  gs.player.cards <- [];
  gs.bot.cards <- []

(* PREFLOP ACTIONS*)

let rec bot_raise_prod p gs p_in =
  if gs.player.balance = 0 then
    if gs.bot.raised >= gs.raise_val then (
      cpu_action "CHECK" gs;
      (* gs.p_turn <- not gs.p_turn; *)
      gs.end_round <- true)
    else (
      cpu_action "CALL" gs;

      gs.end_round <- true)
  else
    try
      let amt = int_of_string p_in in
      if amt <= 0 then ask_action_prompt gs
      else if p.balance < amt + (gs.raise_val - p.raised) then (
        print_endline "Insufficient funds";
        delay ();
        ask_action_prompt gs)
      else (
        gs.pot <- gs.pot + amt + gs.raise_val - p.raised;
        p.balance <- p.balance - (gs.raise_val - p.raised) - amt;
        p.raised <- gs.raise_val + amt;
        gs.raise_val <- p.raised;
        gs.p_turn <- not gs.p_turn)
    with exn ->
      print_endline "Enter valid number";
      delay ();
      ask_action_prompt gs

and raise_prod p gs p_in =
  if gs.bot.balance = 0 then (
    print_endline "Invalid action";
    ask_action_prompt gs)
  else
    try
      let amt = int_of_string p_in in
      if amt <= 0 then (
        print_endline "Enter positive integer";
        ask_action_prompt gs)
      else if p.balance < amt + (gs.raise_val - p.raised) then (
        print_endline "Insufficient funds";
        delay ();
        ask_action_prompt gs)
      else (
        gs.pot <- gs.pot + amt + gs.raise_val - p.raised;
        p.balance <- p.balance - (gs.raise_val - p.raised) - amt;
        p.raised <- gs.raise_val + amt;
        gs.raise_val <- p.raised;
        gs.p_turn <- not gs.p_turn)
    with exn ->
      print_endline "Enter valid number";
      delay ();
      ask_action_prompt gs

and ask_action_prompt gs =
  print_endline
    "Enter an action : Raise, Check, Fold, Call, or View (to see cards and $)";
  ask_action gs (input_line stdin)

and ask_action gs p_in =
  match modify_action_input p_in with
  | "FOLD" ->
      gs.end_round <- true;
      gs.round <- Folded true
  | "CHECK" ->
      print_newline ();
      if not (checkable gs gs.player) then (
        print_endline "Invalid action";
        ask_action gs (input_line stdin))
      else if gs.dealer then gs.end_round <- true
      else gs.p_turn <- not gs.p_turn
  | "CALL" ->
      if gs.player.raised >= gs.raise_val then (
        print_endline "Invalid action";
        ask_action_prompt gs)
      else if gs.raise_val - gs.player.raised > gs.player.balance then (
        gs.player.raised <- gs.player.raised + gs.player.balance;
        gs.pot <- gs.pot + gs.player.balance - (gs.raise_val - gs.player.raised);
        gs.raise_val <- gs.player.raised;
        gs.bot.balance <- gs.bot.balance + (gs.raise_val - gs.player.raised);
        gs.player.balance <- 0;
        gs.end_round <- true)
      else (
        gs.pot <- gs.pot + gs.raise_val - gs.player.raised;
        gs.player.balance <- gs.player.balance - gs.raise_val + gs.player.raised;
        gs.player.raised <- gs.raise_val;
        gs.end_round <- true)
  | "RAISE" ->
      print_endline "Enter raise amount:";
      raise_prod gs.player gs (input_line stdin)
  | "VIEW" ->
      print_board gs.player.cards;
      table_money gs ();
      ask_action_prompt gs
  | _ ->
      print_endline "Illegal action";
      ask_action_prompt gs

and cpu_action s gs =
  match modify_action_input s with
  | "FOLD" ->
      print_newline ();
      print_endline "Your opponent has folded.";
      gs.end_round <- true;
      gs.round <- Folded false
  | "CHECK" ->
      print_endline "Your opponent has checked.";
      if not gs.dealer then (
        gs.end_round <- true;
        gs.p_turn <- not gs.p_turn)
      else gs.p_turn <- not gs.p_turn
  | "CALL" ->
      if gs.raise_val - gs.bot.raised > gs.bot.balance then (
        (*CPU calls but can't match player's raise *)
        print_endline (string_of_int gs.raise_val);
        print_endline "Your opponent has called.";
        gs.pot <- gs.pot + gs.bot.balance + gs.raise_val - gs.bot.raised;
        gs.bot.balance <- 0;
        gs.bot.raised <- gs.bot.raised + gs.bot.balance;
        gs.player.balance <- gs.player.balance + (gs.raise_val - gs.bot.raised);
        gs.raise_val <- gs.bot.balance;

        if gs.raise_val <= bb && gs.round = Pflop then
          gs.p_turn <- not gs.p_turn
        else gs.end_round <- true)
      else (
        print_endline "Your opponent has called.";
        gs.pot <- gs.pot + (gs.raise_val - gs.bot.raised);
        gs.bot.balance <- gs.bot.balance - gs.raise_val + gs.bot.raised;
        gs.bot.raised <- gs.raise_val);
      if gs.raise_val <= bb && gs.round = Pflop then gs.p_turn <- not gs.p_turn
      else gs.end_round <- true
  | "RAISE" ->
      let amt = bot_amt gs in

      if gs.bot.balance < amt + (gs.raise_val - gs.bot.raised) then
        bot_raise_prod gs.bot gs (string_of_int gs.bot.balance)
      else if amt + gs.raise_val <= gs.bot.balance then (
        bot_raise_prod gs.bot gs (string_of_int amt);
        print_endline ("Your opponent has raised by " ^ string_of_int amt))
      else if gs.raise_val = gs.bot.balance then cpu_action "CALL" gs
      else (
        print_endline
          ("Your opponent has raised by " ^ string_of_int gs.bot.balance);
        bot_raise_prod gs.bot gs (string_of_int gs.bot.balance))
  | _ -> print_endline "Gateway Error 505"

let round_1 gs =
  if gs.dealer then gs.p_turn <- false else gs.p_turn <- true;
  gs.end_round <- false;
  (*Just for r1*)
  let p, b = (gs.player, gs.bot) in
  if gs.dealer then (
    p.balance <- p.balance - gs.bb;
    b.balance <- b.balance - gs.sb;
    p.raised <- bb;
    b.raised <- sb)
  else (
    p.balance <- p.balance - gs.sb;
    b.balance <- b.balance - gs.bb;
    p.raised <- sb;
    b.raised <- bb);
  if gs.bot.balance <= 0 then gs.bot.balance <- gs.bot.balance + 1000;
  if gs.player.balance <= 0 then gs.player.balance <- gs.player.balance + 1000;
  gsdeal p gs.deck gs;
  gsdeal p gs.deck gs;

  enter "to see your cards" ();

  print_board p.cards;

  gsdeal b gs.deck gs;
  gsdeal b gs.deck gs;

  (* enter "to see bots cards" (); *)
  delay ();

  print_board b.cards;

  enter "to see money on the table" ();

  table_money gs ();
  (* enter "to start your turn" (); *)
  delay ();

  while not gs.end_round do
    if gs.p_turn then ask_action_prompt gs
    else (
      delay ();
      cpu_action (cpu_choice gs gs.bot) gs;
      delay ()
      (* enter "to see money on the table" ();
         table_money gs () *)
      (* print_endline ("player balance is " ^ string_of_int gs.player.balance);
         print_endline ("bot balance is " ^ string_of_int gs.bot.balance);
         print_endline ("pot " ^ string_of_int gs.pot) *))
  done;
  gs.player.raised <- 0;
  gs.bot.raised <- 0;
  gs.raise_val <- 0;
  match gs.round with Folded _ -> () | _ -> gs.round <- Flop

(* Round 2 functions *)

let round_2 gs =
  if gs.dealer then gs.p_turn <- false else gs.p_turn <- true;
  gs.end_round <- false;
  enter "to see flop" ();
  gs.boardst <- (gs.deck |> deal).card :: gs.boardst;
  gs.deck <- (gs.deck |> deal).ddeck;
  gs.boardst <- (gs.deck |> deal).card :: gs.boardst;
  gs.deck <- (gs.deck |> deal).ddeck;
  gs.boardst <- (gs.deck |> deal).card :: gs.boardst;
  gs.deck <- (gs.deck |> deal).ddeck;
  print_board gs.boardst;
  (* table_money gs (); *)
  (* print_endline ("player balance is " ^ string_of_int gs.player.balance);
     print_endline ("bot balance is " ^ string_of_int gs.bot.balance);
     print_endline ("pot " ^ string_of_int gs.pot); *)
  enter "to start your turn" ();
  while not gs.end_round do
    if gs.p_turn then ask_action_prompt gs
    else cpu_action (cpu_choice gs gs.bot) gs;
    (* enter "to see money on the table" (); *)
    delay ()
    (* table_money gs () *)
    (* print_endline ("player balance is " ^ string_of_int gs.player.balance);
       print_endline ("bot balance is " ^ string_of_int gs.bot.balance) *)
  done;
  gs.player.raised <- 0;
  gs.bot.raised <- 0;
  gs.raise_val <- 0;
  match gs.round with Folded _ -> () | _ -> gs.round <- Turn

let round_3 gs =
  (* turn *)
  if gs.dealer then gs.p_turn <- false else gs.p_turn <- true;
  gs.end_round <- false;
  enter "to see the turn" ();
  gs.boardst <- (gs.deck |> deal).card :: gs.boardst;
  gs.deck <- (gs.deck |> deal).ddeck;
  print_board gs.boardst;
  (* enter "to see money on the table" (); *)
  delay ();
  (* table_money gs (); *)
  (* print_endline ("player balance is " ^ string_of_int gs.player.balance);
     print_endline ("bot balance is " ^ string_of_int gs.bot.balance);
     print_endline ("pot " ^ string_of_int gs.pot); *)
  while not gs.end_round do
    if gs.p_turn then ask_action_prompt gs
    else cpu_action (cpu_choice gs gs.bot) gs
      (* enter "to see money on the table" () *)
      (* table_money gs () *)
      (* print_endline ("player balance is " ^ string_of_int gs.player.balance);
         print_endline ("bot balance is " ^ string_of_int gs.bot.balance) *)
  done;
  gs.player.raised <- 0;
  gs.bot.raised <- 0;
  gs.raise_val <- 0;
  match gs.round with Folded _ -> () | _ -> gs.round <- River

let round_4 gs =
  (* river *)
  if gs.dealer then gs.p_turn <- false else gs.p_turn <- true;
  gs.end_round <- false;
  enter "to see the river" ();
  gs.boardst <- (gs.deck |> deal).card :: gs.boardst;
  gs.deck <- (gs.deck |> deal).ddeck;
  print_board gs.boardst;
  (* enter "to see money on the table" (); *)
  (* table_money gs (); *)
  while not gs.end_round do
    if gs.p_turn then ask_action_prompt gs
    else cpu_action (cpu_choice gs gs.bot) gs
      (* enter "to see money on the table" ();
         table_money gs () *)
  done;
  gs.player.raised <- 0;
  gs.bot.raised <- 0;
  gs.raise_val <- 0;
  match gs.round with Folded _ -> () | _ -> gs.round <- Showdown

let list_to_card_tuple c =
  match c with c1 :: c2 :: t -> (c1, c2) | _ -> assert false

let round_5 gs =
  (* showdown *)
  (* cp v player *)
  enter "to see winner" ();

  let winner =
    compare_hands
      (list_to_card_tuple gs.bot.cards)
      gs.boardst
      (list_to_card_tuple gs.player.cards)
      gs.boardst
  in

  print_endline
    (string_of_hand (best_hand (list_to_card_tuple gs.player.cards) gs.boardst));
  delay ();
  print_endline
    (string_of_hand (best_hand (list_to_card_tuple gs.bot.cards) gs.boardst));
  delay ();
  if winner = 1 then (
    print_endline "CPU Wins";
    finish_round gs gs.bot)
  else (
    print_endline "You Win";
    print_endline "Ctrl+D to quit";
    delay ();
    enter "to start new game" ();
    finish_round gs gs.player)

let rec game gs =
  match gs.round with
  | Pflop ->
      round_1 gs;
      game gs
  | Flop ->
      round_2 gs;
      game gs
  | Turn ->
      round_3 gs;
      game gs
  | River ->
      round_4 gs;
      game gs
  | Showdown ->
      round_5 gs;
      game gs
  | Folded b ->
      if b then finish_round gs gs.bot else finish_round gs gs.player;
      game gs
  | End -> print_endline "game over end"
