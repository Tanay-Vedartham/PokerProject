open Deck

type straight_info = { special : bool; high : int }
type full_house_info = { three : int; two : int }
type two_pair_info = { pair_one : int; pair_two : int }

type hand =
  | RoyalFlush
  | StraightFlush of straight_info
  | FourKind of int
  | FullHouse of full_house_info
  | Flush
  | Straight of straight_info
  | ThreeKind of int
  | TwoPair of two_pair_info
  | Pair of int
  | HighCard

let rec lst_contains lst suit rank =
  match lst with
  | [] -> false
  | h :: t ->
      if get_suit h = suit && get_rank h = rank then true
      else lst_contains t suit rank

let rec lst_contains_rank lst rank =
  match lst with
  | [] -> false
  | h :: t -> if get_rank h = rank then true else lst_contains_rank t rank

let is_royal_flush card_list =
  lst_contains card_list 2 1
  && lst_contains card_list 2 10
  && lst_contains card_list 2 11
  && lst_contains card_list 2 12
  && lst_contains card_list 2 13

let special_straight card_list =
  lst_contains_rank card_list 1
  && lst_contains_rank card_list 13
  && lst_contains_rank card_list 12
  && lst_contains_rank card_list 11
  && lst_contains_rank card_list 10

let sort_hand lst =
  List.sort (fun c1 c2 -> compare (get_rank c1) (get_rank c2)) lst

let get_diff card_list =
  let sorted = sort_hand card_list in
  get_rank (List.hd (List.rev sorted)) - get_rank (List.hd sorted)

let five_consec lst =
  match lst with
  | h :: t -> List.map (fun x -> x + h) [ 0; 1; 2; 3; 4 ] = lst
  | _ -> false

let is_straight card_list =
  let sorted = sort_hand card_list in
  match sorted with
  | [ c1; c2; c3; c4; c5 ] ->
      five_consec [ c1.rank; c2.rank; c3.rank; c4.rank; c5.rank ]
      || special_straight card_list
  | [ c1; c2; c3; c4; c5; c6; c7 ] ->
      if
        five_consec [ c1.rank; c2.rank; c3.rank; c4.rank; c5.rank ]
        || special_straight card_list
      then true
      else if
        five_consec [ c2.rank; c3.rank; c4.rank; c5.rank; c6.rank ]
        || special_straight card_list
      then true
      else if
        five_consec [ c3.rank; c4.rank; c5.rank; c6.rank; c7.rank ]
        || special_straight card_list
      then true
      else false
  | _ -> false

let is_flush_aid f card_list = List.length (List.filter f card_list) > 4

let is_flush card_list =
  is_flush_aid (fun x -> get_suit x = get_suit (List.hd card_list)) card_list
  || is_flush_aid
       (fun x -> get_suit x = get_suit (List.hd (List.tl card_list)))
       card_list
  || is_flush_aid
       (fun x -> get_suit x = get_suit (List.hd (List.tl (List.tl card_list))))
       card_list

let is_straight_flush card_list = is_straight card_list && is_flush card_list

let rec compute_counts card_list counts =
  match card_list with
  | [] -> ()
  | h :: t ->
      counts.(get_rank h - 1) <- counts.(get_rank h - 1) + 1;
      compute_counts t counts

let is_4_of_a_kind card_list =
  let counts = [| 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 |] in
  compute_counts card_list counts;
  Array.exists (fun x -> x = 4) counts

let is_three_of_a_kind card_list =
  let counts = [| 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 |] in
  compute_counts card_list counts;
  Array.exists (fun x -> x = 3) counts

let is_two_of_a_kind card_list =
  let counts = [| 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 |] in
  compute_counts card_list counts;
  Array.exists (fun x -> x = 2) counts

let is_full_house card_list =
  is_three_of_a_kind card_list && is_two_of_a_kind card_list

let is_two_pair card_list =
  let counts = [| 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 |] in
  compute_counts card_list counts;
  let _ = Array.sort compare counts in
  let pairs = ref 0 in
  Array.fold_left
    (fun b x ->
      if !pairs = 0 && x = 2 then pairs := 1
      else if !pairs = 1 && x = 2 then pairs := 2
      else ())
    () counts;
  !pairs = 2

let high_comparison i i2 =
  if i = i2 then 0 else if i = 1 then 1 else if i2 = 1 then -1 else compare i i2

let rec compute_high_card card_list max_rank comp =
  match card_list with
  | [] -> max_rank
  | h :: t -> compute_high_card t (comp (get_rank h) max_rank) comp

let compute_four_kind_type lst =
  let sorted = sort_hand lst in
  get_rank (List.nth sorted 1)

let compute_full_house lst =
  let sorted = sort_hand lst in
  let b1 = get_rank (List.nth sorted 1) in
  let b2 = get_rank (List.nth sorted 2) in
  if b1 = b2 then { three = b1; two = get_rank (List.nth sorted 3) }
  else { three = b2; two = b1 }

let compute_three_kind lst =
  let sorted = sort_hand lst in
  get_rank (List.nth sorted 2)

let compute_two_pair lst =
  let sorted = sort_hand lst in
  {
    pair_one = get_rank (List.nth sorted 1);
    pair_two = get_rank (List.nth sorted 3);
  }

let rec compute_pair lst =
  if List.length lst = 2 then get_rank (List.nth lst 0)
  else
    match lst with
    | h :: t :: tl ->
        let v1 = get_rank h in
        let v2 = get_rank t in
        if v1 = v2 then v1 else compute_pair (t :: tl)
    | _ -> raise (Failure "Compute Pair Failed")

let best_hand c r =
  match c with
  | c1, c2 ->
      let lst = c1 :: c2 :: r in
      if is_royal_flush lst then RoyalFlush
      else if is_straight_flush lst then
        StraightFlush
          { special = special_straight lst; high = compute_high_card lst 0 max }
      else if is_4_of_a_kind lst then FourKind (compute_four_kind_type lst)
      else if is_full_house lst then FullHouse (compute_full_house lst)
      else if is_flush lst then Flush
      else if is_straight lst then
        Straight
          { special = special_straight lst; high = compute_high_card lst 0 max }
      else if is_three_of_a_kind lst then ThreeKind (compute_three_kind lst)
      else if is_two_pair lst then TwoPair (compute_two_pair lst)
      else if is_two_of_a_kind lst then Pair (compute_pair lst)
      else HighCard

let string_of_hand h =
  match h with
  | RoyalFlush -> "Royal Flush"
  | StraightFlush _ -> "Straight Flush"
  | FourKind _ -> "Four of a Kind"
  | FullHouse _ -> "Full House"
  | Flush -> "Flush"
  | Straight _ -> "Straight"
  | ThreeKind _ -> "Three of a Kind"
  | TwoPair _ -> "Two Pair"
  | Pair _ -> "Pair"
  | HighCard -> "High Card"

let int_of_hand h =
  match h with
  | RoyalFlush -> 10
  | StraightFlush _ -> 9
  | FourKind _ -> 8
  | FullHouse _ -> 7
  | Flush -> 6
  | Straight _ -> 5
  | ThreeKind _ -> 4
  | TwoPair _ -> 3
  | Pair _ -> 2
  | HighCard -> 1

let rec compare_high_card_two_hands lst1 lst2 =
  match (lst1, lst2) with
  | [], [] -> 0
  | h :: t, h2 :: t2 ->
      let r1 = get_rank h in
      let r2 = get_rank h2 in
      let comp = high_comparison r1 r2 in
      if comp = 0 then compare_high_card_two_hands t t2 else comp
  | _ -> raise (Failure "Card Lists Different Lengths")

let compute_higher_straight lst1 lst2 s1 s2 h1 h2 =
  if s1 && s2 then 0
  else if s1 && not s2 then 1
  else if (not s1) && s2 then -1
  else if h1 = h2 then 0
  else if h1 > h2 then 1
  else -1

let mod_max i i2 = if i = 1 || i2 = 1 then 1 else max i i2

let handle_equal_hands lst1 lst2 h1 h2 =
  let sorted1 = sort_hand lst1 in
  let sorted2 = sort_hand lst2 in
  match (h1, h2) with
  | RoyalFlush, RoyalFlush -> 0
  | ( StraightFlush { special; high },
      StraightFlush { special = special2; high = high2 } )
  | Straight { special; high }, Straight { special = special2; high = high2 } ->
      compute_higher_straight sorted1 sorted2 special special2 high high2
  | FourKind i, FourKind i2 | ThreeKind i, ThreeKind i2 | Pair i, Pair i2 ->
      if i = i2 then compare_high_card_two_hands sorted1 sorted2
      else high_comparison i i2
  | FullHouse { three; two }, FullHouse { three = three2; two = two2 } ->
      let comp1 = high_comparison three three2 in
      let comp2 = high_comparison two two2 in
      if comp1 = 0 then comp2 else comp1
  | Flush, Flush -> compare_high_card_two_hands sorted1 sorted2
  | TwoPair { pair_one; pair_two }, TwoPair { pair_one = p1; pair_two = p2 } ->
      let c1 = high_comparison pair_one p1 in
      let c2 = high_comparison pair_two p2 in
      if c1 = 0 && c2 = 0 then compare_high_card_two_hands sorted1 sorted2
      else if c1 = 0 && c2 != 0 then c2
      else if c1 != 0 && c2 = 0 then c1
      else
        let val1 = mod_max pair_one pair_two in
        let val2 = mod_max p1 p2 in
        high_comparison val1 val2
  | HighCard, HighCard -> compare_high_card_two_hands sorted1 sorted2
  | _, _ -> raise (Failure "Incorrect use of Handle Equal Hands")

let compare_hands c r c2 r2 =
  match (c, c2) with
  | (card1, card2), (card3, card4) ->
      let lst1 = card1 :: card2 :: r in

      let lst2 = card3 :: card4 :: r2 in
      let h1 = best_hand c r in
      let h2 = best_hand c2 r2 in
      let hand1 = int_of_hand h1 in
      let hand2 = int_of_hand h2 in
      if hand1 = hand2 then handle_equal_hands lst1 lst2 h1 h2
      else if hand1 > hand2 then 1
      else -1
