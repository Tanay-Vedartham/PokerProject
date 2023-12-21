open Game
open Deck
open Prompt
open Gamestate
open Single

let main () =
  print_endline
    "Would you like to play single player or against CPU? (1 for single, 2 for \
     CPU)";
  let choice = input_line stdin in
  if choice = "1" then (
    print_newline ();
    print_endline "Welcome to Poker. Press ENTER or RETURN to begin the round.";
    start_round (input_line stdin))
  else
    let _ = Random.self_init () in
    let gs = init_gs in
    game gs

let () = main ()