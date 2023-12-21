(* TESTING PLAN

   We tested nearly every function at least once. Since many functions were modifiers
   dependent on other functions, we wrote tests that measured how some characteristics
   would differ after an action. For a simple example, testing shuffle involved observing
   that a shuffled deck differs from a deck, but should be the same length. We omitted
   testing the gamestate function, because it would not be feasible nor informative, as the
   function serves as a kind of shifter between the different rounds. We tested that manually,
   but all other modules were tested in OUnit. We believe our testing approach
   demonstrates the correctness of the system, because we performed black box testing by
   writing the mli files and then failing test cases (on purpose), then performed
   white box testing (specifically in hand) to determine if different branches would be taken,
    and although we did not use QCheck, we included randomization by simulating poker matches
   online and using their results to test. With this balanced approach, we covered aspects of
   correctness, but the program is not perfect and we can continue to test further.
*)

open OUnit2
open Game
open Deck
open Action
open Player
open Hand
open Board

let rec deal_til_empty deck = deal_til_empty (deal deck).ddeck
let check_deck c d = assert (List.length (List.filter (fun x -> x = c) d) = 0)

let action_tests =
  [
    "test suite for deal_player"
    >::: [
           (let deck = make_deck () |> shuffle in
            "deal_player deals a hand (or a card list with len 2)" >:: fun _ ->
            assert_equal 2 (List.length (deal_player deck)));
         ];
    "test suite for burn"
    >::: [
           (let deck = make_deck () in
            "a burned deck is not the same as original deck" >:: fun _ ->
            assert (deck <> burn deck));
         ];
    "test suite for deal_flop"
    >::: [
           (let deck = make_deck () in
            "deal_flop deals a flop (or a card list with len 3" >:: fun _ ->
            assert_equal 3 (List.length (deal_flop deck)));
         ];
    "test suite for deal_turn"
    >::: [
           (let deck = make_deck () in
            "deal_turn deals one card that is different from the card dealt \
             from the burned deck"
            >:: fun _ -> assert (deal_turn (burn deck) <> deal_turn deck));
         ];
    "test suite for deal_river"
    >::: [
           (let deck = make_deck () in
            "deal_river deals one card that is not in rest of deck" >:: fun _ ->
            check_deck (deal_river deck) (burn deck));
         ];
  ]

let deck_tests =
  [
    "test suite for get_suit"
    >::: [
           (let ace_of_spades = { suit = 0; rank = 1 } in
            "suit of ace of spades is spades" >:: fun _ ->
            assert_equal 0 (get_suit ace_of_spades));
         ];
    "test suite for get_rank"
    >::: [
           (let ace_of_spades = { suit = 0; rank = 1 } in
            "rank of ace of spades is 1 (or an ace)" >:: fun _ ->
            assert_equal 1 (get_rank ace_of_spades));
         ];
    "test suite for make_deck"
    >::: [
           ( "make_deck creates a standard deck (or a card list of len 52)"
           >:: fun _ -> assert_equal 52 (List.length (make_deck ())) );
         ];
    "test suite for deal"
    >::: [
           (let deck = make_deck () in
            "trying to deal from an empty deck raises EmptyDeck " >:: fun _ ->
            assert_raises EmptyDeck (fun _ -> deal_til_empty deck));
           (let first = make_deck () |> deal in
            "The first card dealt is different than the second card dealt"
            >:: fun _ -> assert (first.card <> (first.ddeck |> deal).card));
           (let deck = make_deck () |> shuffle in
            "The post-deal deck is shorter than the pre-deal deck" >:: fun _ ->
            assert (List.length (deal deck).ddeck < List.length deck));
         ];
    (let deck = make_deck () in
     "test suite for shuffle"
     >::: [
            ( "a shuffled deck is different than its original deck" >:: fun _ ->
              assert (deck <> shuffle deck) );
            (let shuffle_deck = deck |> shuffle in
             "a shuffled deck is different than a shuffled deck from it"
             >:: fun _ -> assert (shuffle_deck <> shuffle (shuffle deck)));
          ]);
    (let ace_of_clubs = { suit = 1; rank = 1 } in
     "test suite for print_card"
     >::: [
            ( "suit - 1, rank - 1" >:: fun _ ->
              assert_equal "Ace of Clubs" (print_card ace_of_clubs) );
          ]);
  ]

let board_tests =
  [
    "test suite for make_board"
    >::: [
           (let deck = make_deck () in
            "adding the flop to the board should yield three cards (or a list \
             of len 3"
            >:: fun _ ->
            assert_equal 3 (List.length (make_board () @ deal_flop deck)));
         ];
  ]

let rfp = ({ suit = 2; rank = 1 }, { suit = 2; rank = 10 })

let royal_flush =
  [ { suit = 2; rank = 13 }; { suit = 2; rank = 11 }; { suit = 2; rank = 12 } ]

let sfp = ({ suit = 2; rank = 1 }, { suit = 2; rank = 5 })

let straight_flush =
  [ { suit = 2; rank = 3 }; { suit = 2; rank = 4 }; { suit = 2; rank = 2 } ]

let sfp2 = ({ suit = 1; rank = 6 }, { suit = 1; rank = 7 })

let straight_flush2 =
  [ { suit = 1; rank = 8 }; { suit = 1; rank = 9 }; { suit = 1; rank = 10 } ]

let fkp = ({ suit = 3; rank = 8 }, { suit = 3; rank = 8 })

let four_kind =
  [ { suit = 3; rank = 8 }; { suit = 1; rank = 8 }; { suit = 0; rank = 7 } ]

let fhp = ({ suit = 0; rank = 6 }, { suit = 2; rank = 6 })

let full_house =
  [ { suit = 1; rank = 6 }; { suit = 1; rank = 11 }; { suit = 2; rank = 11 } ]

let fhp2 = ({ suit = 0; rank = 1 }, { suit = 0; rank = 6 })

let full_house2 =
  [ { suit = 3; rank = 6 }; { suit = 3; rank = 6 }; { suit = 1; rank = 1 } ]

let fp = ({ suit = 0; rank = 10 }, { suit = 0; rank = 10 })

let flush =
  [ { suit = 0; rank = 11 }; { suit = 0; rank = 11 }; { suit = 0; rank = 12 } ]

let sp = ({ suit = 1; rank = 7 }, { suit = 1; rank = 8 })

let straight =
  [ { suit = 1; rank = 9 }; { suit = 3; rank = 10 }; { suit = 3; rank = 11 } ]

let tkp = ({ suit = 2; rank = 4 }, { suit = 3; rank = 10 })

let three_kind =
  [ { suit = 2; rank = 12 }; { suit = 3; rank = 12 }; { suit = 3; rank = 12 } ]

let tkp2 = ({ suit = 2; rank = 13 }, { suit = 2; rank = 13 })

let three_kind2 =
  [ { suit = 0; rank = 13 }; { suit = 1; rank = 2 }; { suit = 3; rank = 8 } ]

let tpp = ({ suit = 1; rank = 1 }, { suit = 0; rank = 1 })

let two_pair =
  [ { suit = 3; rank = 13 }; { suit = 0; rank = 9 }; { suit = 1; rank = 9 } ]

let pp = ({ suit = 0; rank = 12 }, { suit = 0; rank = 12 })

let pair =
  [ { suit = 0; rank = 1 }; { suit = 2; rank = 2 }; { suit = 3; rank = 3 } ]

let hcp = ({ suit = 0; rank = 5 }, { suit = 0; rank = 10 })

let high_card =
  [ { suit = 3; rank = 13 }; { suit = 0; rank = 11 }; { suit = 0; rank = 12 } ]

let hcp2 = ({ suit = 0; rank = 13 }, { suit = 0; rank = 4 })

let high_card2 =
  [ { suit = 3; rank = 12 }; { suit = 2; rank = 3 }; { suit = 2; rank = 8 } ]

(*Random In-Game Simulations*)

let scene1 =
  "Scenario 1 - (Hand: 9D, AD; Board: 7D, 8C, AS, 6H, 8H) -> Best Hand: (8C, \
   8H), (AD, AS)"

let hand1 = ({ suit = 3; rank = 9 }, { suit = 3; rank = 1 })

let board1 =
  [
    { suit = 3; rank = 7 };
    { suit = 2; rank = 8 };
    { suit = 0; rank = 1 };
    { suit = 2; rank = 6 };
    { suit = 2; rank = 8 };
  ]

let scene2 =
  "Scenario 2 - (Hand: KS, 3S; Board: 3H, 3C, KH, 2S, 8D) -> Full House"

let ans2 = "Full House"
let hand2 = ({ suit = 0; rank = 13 }, { suit = 0; rank = 3 })

let board2 =
  [
    { suit = 2; rank = 3 };
    { suit = 1; rank = 3 };
    { suit = 2; rank = 13 };
    { suit = 0; rank = 2 };
    { suit = 3; rank = 8 };
  ]

let scene3 =
  "Scenario 3 - (Hand: QC, 8C; Board: 8H, JD, 8D, 6D, 10C) -> Three of a Kind"

let ans3 = "Three of a Kind"
let hand3 = ({ suit = 1; rank = 12 }, { suit = 1; rank = 8 })

let board3 =
  [
    { suit = 2; rank = 8 };
    { suit = 3; rank = 11 };
    { suit = 3; rank = 8 };
    { suit = 3; rank = 6 };
    { suit = 1; rank = 10 };
  ]

let scene4 = "Scenario 4 - Hand: 6S, 5C; Board: 5S, 6C, 7S"
let ans4 = "Two Pair"
let hand4 = ({ suit = 0; rank = 6 }, { suit = 1; rank = 5 })

let board4 =
  [ { suit = 0; rank = 5 }; { suit = 1; rank = 6 }; { suit = 0; rank = 7 } ]

let scene5 = "Scenario 5 - Hand: QH, 10S; Board: 4S, 4C, QD, 3H, 10D"
let ans5 = "Two Pair"
let hand5 = ({ suit = 2; rank = 12 }, { suit = 0; rank = 10 })

let board5 =
  [
    { suit = 0; rank = 4 };
    { suit = 1; rank = 4 };
    { suit = 2; rank = 12 };
    { suit = 2; rank = 3 };
    { suit = 3; rank = 10 };
  ]

let scene6 = "Scenario 6 - Hand: 3S, 2S ; Board: 4C, 2C, 5D, KC, 6D"
let ans6 = "Straight"
let hand6 = ({ suit = 0; rank = 3 }, { suit = 0; rank = 2 })

let board6 =
  [
    { suit = 1; rank = 4 };
    { suit = 1; rank = 2 };
    { suit = 3; rank = 5 };
    { suit = 1; rank = 13 };
    { suit = 3; rank = 6 };
  ]

let scene7 = "Scenario 7 - Hand: KD, 10S ; Board: QD, JD, 6H, 4D, 10D"
let ans7 = "Flush"
let hand7 = ({ suit = 3; rank = 13 }, { suit = 0; rank = 10 })

let board7 =
  [
    { suit = 3; rank = 12 };
    { suit = 3; rank = 11 };
    { suit = 2; rank = 6 };
    { suit = 3; rank = 4 };
    { suit = 3; rank = 10 };
  ]

let scene8 = "Scenario 8 - Hand: 7S, AS ; Board: 9S, 3C, 7C, 7H, 4S"
let ans8 = "Three of a Kind"
let hand8 = ({ suit = 0; rank = 7 }, { suit = 0; rank = 1 })

let board8 =
  [
    { suit = 0; rank = 9 };
    { suit = 1; rank = 3 };
    { suit = 1; rank = 7 };
    { suit = 2; rank = 7 };
    { suit = 0; rank = 4 };
  ]

let scene9 = "Scenario 9 - Hand: 10D, QH; Board: KC, JD, 7S, 6H, AS "
let ans9 = "Straight"
let hand9 = ({ suit = 3; rank = 10 }, { suit = 2; rank = 12 })

let board9 =
  [
    { suit = 1; rank = 13 };
    { suit = 3; rank = 11 };
    { suit = 0; rank = 7 };
    { suit = 2; rank = 6 };
    { suit = 0; rank = 1 };
  ]

let scene10 = "Scenario 10 - Hand: 9C, 8D ; Board: 10D, QD, 5S, 4D, JH"
let ans10 = "Straight"
let hand10 = ({ suit = 1; rank = 9 }, { suit = 3; rank = 8 })

let board10 =
  [
    { suit = 3; rank = 10 };
    { suit = 3; rank = 12 };
    { suit = 0; rank = 5 };
    { suit = 3; rank = 4 };
    { suit = 2; rank = 11 };
  ]

let scene11 = "Scenario 11 - Board: 2D, JH, 9C, 4S, 10D; Hand: 9D, AH -> Pair"
let ans11 = "Pair"
let hand11 = ({ suit = 3; rank = 9 }, { suit = 2; rank = 1 })

let board11 =
  [
    { suit = 3; rank = 2 };
    { suit = 2; rank = 11 };
    { suit = 1; rank = 9 };
    { suit = 0; rank = 4 };
    { suit = 3; rank = 10 };
  ]

let scene12 =
  "Scenario 12 - Board: 4C, 10H, 8H, 2H, KD ; Hand: 8C, 10D  -> Two Pair"

let ans12 = "Two Pair"
let hand12 = ({ suit = 1; rank = 8 }, { suit = 3; rank = 10 })

let board12 =
  [
    { suit = 1; rank = 4 };
    { suit = 2; rank = 10 };
    { suit = 2; rank = 8 };
    { suit = 2; rank = 2 };
    { suit = 3; rank = 13 };
  ]

let scene13 =
  "Scenario 13 - Board: 6H, JS, 5H, 4S, 6D ; Hand: QC, QD  -> Two Pair"

let ans13 = "Two Pair"
let hand13 = ({ suit = 1; rank = 12 }, { suit = 3; rank = 12 })

let board13 =
  [
    { suit = 2; rank = 6 };
    { suit = 0; rank = 11 };
    { suit = 2; rank = 5 };
    { suit = 0; rank = 4 };
    { suit = 3; rank = 6 };
  ]

let scene14 =
  "Scenario 14 - Board: 10C, 10H, JS, KD, 8H ; Hand: 8C, JH   -> Two Pair "

let ans14 = "Two Pair"
let hand14 = ({ suit = 1; rank = 8 }, { suit = 2; rank = 11 })

let board14 =
  [
    { suit = 1; rank = 10 };
    { suit = 2; rank = 10 };
    { suit = 0; rank = 11 };
    { suit = 3; rank = 13 };
    { suit = 2; rank = 8 };
  ]

let scene15 =
  "Scenario 15 - Board: 2C, 9C, 6D, 3D, 7C ; Hand: 6S, 6H   -> Three of a Kind "

let ans15 = "Three of a Kind"
let hand15 = ({ suit = 0; rank = 6 }, { suit = 2; rank = 6 })

let board15 =
  [
    { suit = 1; rank = 2 };
    { suit = 1; rank = 9 };
    { suit = 3; rank = 6 };
    { suit = 3; rank = 3 };
    { suit = 1; rank = 7 };
  ]
(*Comparator Functions*)

let create_hand_test name p lst output =
  name >:: fun _ ->
  assert_equal output (string_of_hand (best_hand p lst)) ~printer:(fun x -> x)

let create_hand_comparison_test name c r c2 r2 output =
  name >:: fun _ -> assert_equal output (compare_hands c r c2 r2)

let hand_tests =
  [
    (*Base tests*)
    create_hand_test "Royal Flush" rfp royal_flush "Royal Flush";
    create_hand_test "Straight Flush" sfp straight_flush "Straight Flush";
    create_hand_test "Four of a kind" fkp four_kind "Four of a Kind";
    create_hand_test "Full House" fhp full_house "Full House";
    create_hand_test "Flush" fp flush "Flush";
    create_hand_test "Straight" sp straight "Straight";
    create_hand_test "Three of a kind" tkp three_kind "Three of a Kind";
    create_hand_test "Two pair" tpp two_pair "Two Pair";
    create_hand_test "Pair" pp pair "Pair";
    create_hand_test "High card" hcp high_card "High Card";
    (*Random In-Game Sim Tests*)
    create_hand_test scene1 hand1 board1 "Two Pair";
    create_hand_test scene2 hand2 board2 ans2;
    create_hand_test scene3 hand3 board3 ans3;
    create_hand_test scene4 hand4 board4 ans4;
    create_hand_test scene5 hand5 board5 ans5;
    create_hand_test scene6 hand6 board6 ans6;
    create_hand_test scene7 hand7 board7 ans7;
    create_hand_test scene8 hand8 board8 ans8;
    create_hand_test scene9 hand9 board9 ans9;
    create_hand_test scene10 hand10 board10 ans10;
    create_hand_test scene11 hand11 board11 ans11;
    create_hand_test scene12 hand12 board12 ans12;
    create_hand_test scene13 hand13 board13 ans13;
    create_hand_test scene14 hand14 board14 ans14;
    create_hand_test scene15 hand15 board15 ans15;
    (*Comparison tests*)
    create_hand_comparison_test "Royal flush over four of a kind" rfp
      royal_flush fkp four_kind 1;
    create_hand_comparison_test "Full house over a three of a kind" fhp
      full_house tkp three_kind 1;
    create_hand_comparison_test "Straight over pair" sp straight pp pair 1;
    create_hand_comparison_test "Pair over high card" pp pair hcp high_card 1;
    create_hand_comparison_test "Two pair worse than a flush" tpp two_pair fp
      flush ~-1;
    create_hand_comparison_test "Three of a kind worse than straight flush" tkp
      three_kind sfp straight_flush ~-1;
    create_hand_comparison_test "High card worse than pair" hcp high_card pp
      pair ~-1;
    create_hand_comparison_test "Same hands are equal with three kind" tkp
      three_kind tkp three_kind 0;
    create_hand_comparison_test "One straight flush higher than another" sfp2
      straight_flush2 sfp straight_flush 1;
    create_hand_comparison_test "One full house has higher two pair than other"
      fhp full_house fhp2 full_house2 ~-1;
    create_hand_comparison_test "Higher three kind than other" tkp2 three_kind2
      tkp three_kind 1;
    create_hand_comparison_test "high card better than other" hcp high_card hcp2
      high_card2 1;
  ]

let suite =
  "test suite for Poker"
  >::: List.flatten [ action_tests; board_tests; deck_tests; hand_tests ]

let _ = run_test_tt_main suite
