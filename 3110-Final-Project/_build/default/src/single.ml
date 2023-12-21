open Deck
open Prompt
open Gamestate

type bet = { mutable balance : int; mutable bet : int }

let starting_balance = 1000 (*CHANGE THIS*)
let buyin = 100
let player_bet = { balance = starting_balance; bet = 0 }

type deck_for_round = { mutable current_deck : deck }

let deck_for_round =
  let _ = Random.self_init () in
  { current_deck = make_deck () |> shuffle }

let cards () =
  let dealt = deck_for_round.current_deck |> deal in
  deck_for_round.current_deck <- dealt.ddeck;
  let rem_deck = deck_for_round.current_deck in
  let snd = rem_deck |> deal in
  deck_for_round.current_deck <- snd.ddeck;
  (dealt.card |> print_card, snd.card |> print_card)

let burn =
  deck_for_round.current_deck <- (deck_for_round.current_deck |> deal).ddeck

let flop () =
  let card_and_deck = deck_for_round.current_deck |> deal in
  deck_for_round.current_deck <- card_and_deck.ddeck;
  card_and_deck.card |> print_card

let colorify c =
  match String.split_on_char ' ' c with
  | _ :: _ :: [ t ] ->
      if t = "Hearts" || t = "Diamonds" then [ ANSITerminal.red ]
      else if t = "Clubs" || t = "Spades" then [ ANSITerminal.black ]
      else [ ANSITerminal.blue ] (*CHANGE THIS*)
  | _ -> [ ANSITerminal.black ]
(*CHANGE THIS*)

let river i =
  match i with
  | "" ->
      burn;
      (deck_for_round.current_deck |> deal).card |> print_card
  | _ -> "CHANGE THIS"

let print_balance () =
  "Your remaining balance is " ^ string_of_int player_bet.balance ^ "."

let print_actions_with_balance () =
  "What would you like to do: FOLD, CHECK, RAISE" ^ " (Balance: "
  ^ string_of_int player_bet.balance
  ^ ")"

type game_board = {
  mutable player_cards : string list;
  mutable board : string list;
}

let gboard = { player_cards = []; board = [] }

let rec print_board b =
  match b with
  | h :: t ->
      ANSITerminal.print_string (colorify h) h;
      print_newline ();
      print_board t
  | _ -> print_newline ()

let winner i =
  match i with
  | "" -> print_endline ("You won " ^ string_of_int player_bet.bet ^ " dollars.")
  | _ -> print_endline "Invalid Input"

let rec final_actions a =
  match modify_action_input a with
  | "FOLD" ->
      print_newline ();
      print_endline "Thanks for playing."
  | "CHECK" ->
      print_newline ();
      print_endline "Press ENTER + RETURN to reveal winner";
      winner (input_line stdin)
  | "RAISE" ->
      print_newline ();
      print_endline "Enter your bet: ";
      print_newline ();
      let bet = input_line stdin in
      let bet_int = int_of_string bet in
      player_bet.balance <- player_bet.balance - bet_int;
      player_bet.bet <- player_bet.bet + bet_int;
      print_newline ();
      print_endline
        (print_balance () ^ " The pot is " ^ string_of_int player_bet.bet ^ ".");
      print_endline "Press ENTER + RETURN to reveal winner";
      winner (input_line stdin)
  | _ -> print_endline "Invalid Action"

and final_round user_input =
  match user_input with
  | "" ->
      print_endline (print_actions_with_balance ());
      print_newline ();
      final_actions (input_line stdin)
  | _ -> print_endline "Invalid Input"

and show_board_final i =
  match i with
  | "" ->
      print_newline ();
      print_endline (print_actions_with_balance ());
      final_round (input_line stdin)
  | "/" ->
      print_newline ();
      print_endline "The Board:";
      print_board gboard.board;
      print_newline ();
      print_endline "Your Cards:";
      print_board gboard.player_cards;
      print_endline "Press ENTER or RETURN to continue.";
      final_round (input_line stdin)
  | _ -> print_endline "Invalid Input"

let riverify2 () =
  let river_card2 = river (input_line stdin) in
  ANSITerminal.print_string (colorify river_card2) river_card2;
  gboard.board <- gboard.board @ [ river_card2 ];
  print_newline ();
  print_newline ();
  print_endline
    "Press / + ENTER for full board. Press ENTER or RETURN to continue.";
  show_board_final (input_line stdin)

let rec third_actions a =
  match modify_action_input a with
  | "FOLD" ->
      print_newline ();
      print_endline "Thanks for playing."
  | "CHECK" ->
      print_newline ();
      print_endline "Press ENTER or RETURN to see the final card.";
      riverify2 ()
  | "RAISE" ->
      print_newline ();
      print_endline "Enter your bet: ";
      print_newline ();
      let bet = input_line stdin in
      let bet_int = int_of_string bet in
      player_bet.balance <- player_bet.balance - bet_int;
      player_bet.bet <- player_bet.bet + bet_int;
      print_newline ();
      print_endline
        (print_balance () ^ " The pot is " ^ string_of_int player_bet.bet ^ ".");
      riverify2 ()
  | _ -> print_endline "Invalid Action"

and third_round user_input =
  match user_input with
  | "" ->
      print_endline (print_actions_with_balance ());
      print_newline ();
      third_actions (input_line stdin)
  | _ -> print_endline "Invalid Input"

and show_board_snd i =
  match i with
  | "" -> third_round (input_line stdin)
  | "/" ->
      print_newline ();
      print_endline "The Board:";
      print_board gboard.board;
      print_newline ();
      print_endline "Your Cards:";
      print_board gboard.player_cards;
      print_endline "Press ENTER or RETURN to continue.";
      third_round (input_line stdin)
  | _ -> print_endline "Invalid Input"

let riverify () =
  let river_card = river (input_line stdin) in
  ANSITerminal.print_string (colorify river_card) river_card;
  gboard.board <- gboard.board @ [ river_card ];
  print_newline ();
  print_newline ();
  print_endline
    "Press / + ENTER for full board. Press ENTER or RETURN to continue.";
  show_board_snd (input_line stdin)

let rec show_board i =
  match i with
  | "" -> second_round (input_line stdin)
  | "/" ->
      print_newline ();
      print_endline "The Board:";
      print_board gboard.board;
      print_newline ();
      print_endline "Your Cards:";
      print_board gboard.player_cards;
      print_endline "Press ENTER or RETURN to continue.";
      second_round (input_line stdin)
  | _ -> print_endline "Invalid Input"

and snd_actions a =
  match modify_action_input a with
  | "FOLD" ->
      print_newline ();
      print_endline "Thanks for playing."
  | "CHECK" ->
      print_newline ();
      print_endline "Press ENTER or RETURN to see the next card.";
      riverify ()
  | "RAISE" ->
      print_newline ();
      print_endline "Enter your bet: ";
      print_newline ();
      let bet = input_line stdin in
      let bet_int = int_of_string bet in
      player_bet.balance <- player_bet.balance - bet_int;
      player_bet.bet <- player_bet.bet + bet_int;
      print_newline ();
      print_endline
        (print_balance () ^ " The pot is " ^ string_of_int player_bet.bet ^ ".");
      print_endline "Press ENTER + RETURN to see the next card";
      riverify ()
  | _ -> print_endline "Invalid Action"

and second_round user_input =
  match user_input with
  | "" ->
      print_endline (print_actions_with_balance ());
      print_newline ();
      snd_actions (input_line stdin)
  | _ -> print_endline "Invalid Input"

let start_flop user_input =
  match user_input with
  | _ ->
      burn;
      let flop1 = flop () in
      ANSITerminal.print_string (colorify flop1) flop1;
      gboard.board <- gboard.board @ [ flop1 ];
      print_newline ();

      let flop2 = flop () in
      ANSITerminal.print_string (colorify flop2) flop2;
      gboard.board <- gboard.board @ [ flop2 ];
      print_newline ();

      let flop3 = flop () in
      ANSITerminal.print_string (colorify flop3) flop3;
      gboard.board <- gboard.board @ [ flop3 ];
      print_newline ();

      print_newline ();
      print_endline
        "Press / + ENTER for full board. Press ENTER or RETURN to continue.";
      show_board (input_line stdin)

let raise_bet i =
  player_bet.bet <- player_bet.bet + int_of_string i;
  player_bet.balance <- player_bet.balance - int_of_string i;
  print_newline ();
  print_endline "Press ENTER or RETURN to see the flop";
  start_flop (input_line stdin)

let raise_procedure () =
  print_newline ();
  (* print_endline ("Your current balance is: " ^ string_of_int player_bet.balance); *)
  print_endline "Enter your bet (No commas): ";
  print_newline ();
  raise_bet (input_line stdin)

let rec deal_cards i =
  match i with
  | "" ->
      let hand = cards () in
      ANSITerminal.print_string (colorify (fst hand)) (fst hand);
      gboard.player_cards <- gboard.player_cards @ [ fst hand ];
      print_newline ();
      ANSITerminal.print_string (colorify (snd hand)) (snd hand);
      gboard.player_cards <- gboard.player_cards @ [ snd hand ];
      print_newline ();
      print_newline ();
      print_endline
        ("What would you like to do: FOLD, CHECK, RAISE" ^ " (Buy in: "
       ^ string_of_int buyin ^ ", Balance: "
        ^ string_of_int player_bet.balance
        ^ ")");
      print_newline ();
      actions (input_line stdin)
  | _ -> print_endline "CHANGE THIS"

and actions a =
  match modify_action_input a with
  | "FOLD" ->
      print_newline ();
      print_endline "Thanks for playing."
  | "CHECK" ->
      print_newline ();
      player_bet.balance <- player_bet.balance - buyin;
      player_bet.bet <- player_bet.bet + buyin;
      print_endline
        ("Your remaining balance is "
        ^ string_of_int player_bet.balance
        ^ ". " ^ "Press ENTER or RETURN to see the flop.");
      start_flop (input_line stdin)
  | "RAISE" -> raise_procedure ()
  | _ -> print_endline "Invalid Action"

and start_round user_input =
  match user_input with
  | "" ->
      print_endline "Press ENTER or RETURN to see your cards.";
      deal_cards (input_line stdin)
  | _ -> print_endline "Invalid Input"