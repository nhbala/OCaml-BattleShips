open State

(* [pinput_to_int str] returns the integer represented by a letter-number pair.
 * For example, A1 maps to 0, A2 maps to 1, B1 maps to 10, and so on. The input
 * is case-insensitive.
 * Raises: exception if the string is not of length 2, or if one of the characters
 * is not a valid place on the board
 * Requires: [str] is a string. *)
let pinput_to_int str =
  if String.length str = 3 then
    let letter = str.[0] in
    let code = Char.code letter in
    let multipler = 10 * (code - 97) in
    if code < 97 || code > 106 then failwith "out of bounds"
    else
      let snd_char = int_of_string_opt (String.sub str 1 2) in
      match snd_char with
      | Some i ->
        if i = 10 then multipler + 9
        else failwith "int out of bounds"
      | None -> failwith "second character is not an integer"
  else if String.length str = 2 then
    let fst_char = String.get str 0 in
    let code = Char.code fst_char in
    let multipler = 10 * (code - 97) in
    if code < 97 || code > 106 then failwith "out of bounds bro"
    else
      let snd_char = int_of_string_opt (String.sub str 1 1) in
      match snd_char with
      | Some i ->
        if i < 1 || i > 10 then failwith "int out of bounds"
        else multipler + i - 1
      | None -> failwith "second character is not an integer"
  else failwith "not a valid string"

(* [new_lst] is a list of coordinates with no ships. *)
let new_lst =
  List.init 100 (fun a -> {id = a; hit = false; ship = None})

(* [make_coor_lst lst ship] is a list of coordinates corresponding to [ship]. *)
let rec make_coor_lst lst ship =
  match lst with
  | [] -> []
  | h::t -> {id = h; hit = false; ship = Some ship}::(make_coor_lst t ship)

(* [make_ship_lst ship lst] is a list of coordinates storing the presence of
 * the ship [ship].
 * Requires: [lst] is an int list. *)
let rec make_ship_lst ship lst =
  match ship with
  | Carrier ->
    if List.length lst != 5 then failwith "not a vaild configuration"
    else make_coor_lst lst ship
  | Battleship ->
    if List.length lst != 4 then failwith "not a valid configuration"
    else make_coor_lst lst ship
  | Cruiser ->
    if List.length lst != 3 then failwith "not a valid configuration"
    else make_coor_lst lst ship
  | Submarine ->
    if List.length lst != 3 then failwith "not a valid configuration"
    else make_coor_lst lst ship
  | Destroyer ->
    if List.length lst != 2 then failwith "not a valid configuration"
    else make_coor_lst lst ship

(* [helper_checking_placement lst value] returns true if each consecutive
 * element of [lst] is incremented by [value]. *)
let rec helper_checking_placement lst value =
  match lst with
  | [] -> true
  | h1::h2::t->
    if h2 - h1 = value
    then helper_checking_placement (h2::t) value
    else false
  | h1::t -> true

(* [checking_coor_placement lst] returns true if [lst] is a list of coordinates
 * that are in a consecutive horizontal or vertical placement.
 * [lst] must be a sorted int list. *)
let checking_coor_placement lst =
  match lst with
  | [] -> true
  | h1::h2::t ->
    if h2 - h1 != 1 && h2 - h1 != 10 then false
    else if h2 - h1 = 1 then helper_checking_placement (h2::t) (1)
    else helper_checking_placement (h2::t) (10)
  | _ -> failwith "impossible "

(* [trim_strings lst] is [lst] with each element trimmed. *)
let rec trim_strings lst =
  List.map String.trim lst

(* [filter_empty_strings lst] is [lst] with empty strings removed. *)
let rec filter_empty_strings lst =
  match lst with
  | [] -> []
  | h::t -> if h = "" then filter_empty_strings t else h::filter_empty_strings t

(* [parse str ship] returns a coordinate list representing the coordinates of [ship]. *)
let parse_setup str ship =
  let lstr = String.lowercase_ascii str in
  let coor_lst = filter_empty_strings (trim_strings (String.split_on_char ';' lstr)) in
  let correct_lst = List.map pinput_to_int coor_lst in
  let sorted_ints = List.sort compare correct_lst in
  if checking_coor_placement sorted_ints
  then make_ship_lst ship sorted_ints
  else failwith "invalid coordinate placement"

(* [replace c coor] replaces a single coordinate in coor that matches c. *)
let rec replace c coor =
  match coor with
  | [] -> []
  | h::t -> if c.id = h.id then c::t else h::(replace c t)

(* [replace_helper ship coor] places the coordinates of the ships onto
   a player's board. *)
let rec replace_helper ship coor =
  match ship with
  | [] -> coor
  | h::t -> replace_helper t (replace h coor)

(* [check_dup ls men] checks whether all the coordinates in ls is unique.
Returns true if all the elements are unique, false otherwise. *)
let rec check_dup ls men =
  match ls with
  | [] -> true
  | h::t -> if List.mem h.id men = true then false else check_dup t (h.id::men)

exception Invalid_Setup of string

(* [player_coor ls] takes in a list of tuple of string and Ship and returns
   a coordinate list with all the ships placed.
   Requires: ls is of length 5.
   Raises: exception if the coordinates of the ships overlapped. *)
let player_coor ls =
  let first_ship = parse_setup (fst (List.nth ls 0)) (snd (List.nth ls 0)) in
  let second_ship = parse_setup (fst (List.nth ls 1)) (snd (List.nth ls 1)) in
  let third_ship = parse_setup (fst (List.nth ls 2)) (snd (List.nth ls 2)) in
  let fourth_ship = parse_setup (fst (List.nth ls 3)) (snd (List.nth ls 3)) in
  let fifth_ship = parse_setup (fst (List.nth ls 4)) (snd (List.nth ls 4)) in
  let all_coor = first_ship@second_ship@third_ship@fourth_ship@fifth_ship in
  if check_dup all_coor [] = false then raise (Invalid_Setup "coordinates overlap")
  else
  let ls1 = replace_helper first_ship new_lst in
  let ls2 = replace_helper second_ship ls1 in
  let ls3 = replace_helper third_ship ls2 in
  let ls4 = replace_helper fourth_ship ls3 in
  replace_helper fifth_ship ls4

(* [quit_middle_setup str] quits the game if [str] is "quit". *)
let quit_middle_setup str =
  if str = "quit" then (print_endline("Goodbye!"); exit 0;)

(* [setup_single_ship str len] handles the player setup of the ship represented
 * by [str] of length [len] and returns the string inputted by the player. *)
let setup_single_ship str len =
  let coordstr =
  print_string ("Enter coordinates for " ^
                str^ " " ^ (string_of_int len) ^ " coordinates in the format A1; A2; ...");
  print_endline "";
  print_string "> ";
  read_line () in quit_middle_setup coordstr; coordstr

(* [setup_ships str] handles the setup of all the ships for a player and returns
 * a coordinate list with all the ships placed. *)
let rec setup_ships str =
  try
  print_endline str;
  let carrier_str = setup_single_ship "Carrier" 5 in
  let battleship_str = setup_single_ship "Battleship" 4 in
  let cruiser_str = setup_single_ship "Cruiser" 3 in
  let sub_str = setup_single_ship "Submarine" 3 in
  let destr_str = setup_single_ship "Destroyer" 2 in
  let parse_setup_lst = [(carrier_str, Carrier); (battleship_str, Battleship);
                         (cruiser_str, Cruiser); (sub_str, Submarine); (destr_str, Destroyer)]
  in player_coor parse_setup_lst
  with _ ->
    let () = print_endline "A problem was encountered with setting up. Please try again!" in
    setup_ships str
