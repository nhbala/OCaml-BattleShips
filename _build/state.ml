open Command

type ship = Carrier | Battleship | Cruiser | Submarine | Destroyer

type coordinate = {
  id: int;
  hit: bool;
  ship: ship option
}

type player = {
  coordinates: coordinate list;
  sunken_ships: (ship * bool) list;
}

type state = player * player * string

(* [ship_coor_list s coor_lst] takes in a Ship and a coordinate list and
   returns a list of coordinates that the ship rests on. *)
let rec ship_coor_list (s:ship) (coor_lst) =
  match coor_lst with
  | [] -> []
  | h::t -> if h.ship = Some s then h::(ship_coor_list s t) else
      ship_coor_list s t

(* [create_ship_coordinates coor_ls] returns a list of all the ships and
   the coordinates of each ship. *)
let rec create_ship_coordinates coor_ls =
  let carriercoor = ship_coor_list Carrier coor_ls in
  let battleshipcoor = ship_coor_list Battleship coor_ls in
  let cruisercoor = ship_coor_list Cruiser coor_ls in
  let submarinecoor = ship_coor_list Submarine coor_ls in
  let destroyercoor = ship_coor_list Destroyer coor_ls in
  (Carrier, carriercoor)::(Battleship, battleshipcoor)
  ::(Cruiser, cruisercoor)::(Submarine, submarinecoor)
  ::(Destroyer, destroyercoor)::[]

(* [sort_lst_ship_coordinate lst] is the same list but with each coordinate list
 * sorted by increasing id. *)
let rec sort_lst_ship_coordinate lst =
  match lst with
  | [] -> []
  | h::t ->
    let shp = (fst h) in
    let coords = (snd h) in
    (shp, (List.sort (fun c1 c2 -> if c1.id = c2.id
                       then 0 else if c1.id < c2.id
                       then -1 else 1) coords)) :: sort_lst_ship_coordinate t

(* [opp_ships s] is a list of all the computer's ships and their locations on
   its board. *)
let opp_ships (s:state) =
  match s with
  | (_,p2,_) -> create_ship_coordinates p2.coordinates

(* [player_ships s] is a list of all the player's ships and their locations on
   the player's board. *)
let player_ships (s:state) =
  match s with
  | (p1,_,_) -> create_ship_coordinates p1.coordinates

(* [ship_hit s coor_ls] returns the number of coordinates that are hit
   for a ship. *)
let rec ship_hit (s:ship) (coor_ls) =
  match coor_ls with
  | [] -> 0
  | h::t -> if h.ship = Some s then if h.hit = true then 1 else ship_hit s t
    else ship_hit s t

(* [calc_player_hits coor_ls] returns the number of ships that are hit. *)
let rec calc_player_hits coor_ls =
  let carrierhit = ship_hit Carrier coor_ls in
  let battleshiphit = ship_hit Battleship coor_ls in
  let cruiserhit = ship_hit Cruiser coor_ls in
  let submarinehit = ship_hit Submarine coor_ls in
  let destroyerhit = ship_hit Destroyer coor_ls in
  carrierhit + battleshiphit + cruiserhit + submarinehit + destroyerhit

(* [p_hits ls] returns the number of ships that are hit. *)
let rec p_hits ls =
  match ls with
  | [] -> 0
  | h::t -> if (snd h) = true then 1 + p_hits t else p_hits t

(* [opp_hits s] is the number of ships the second player has hit. *)
let opp_hits state =
  match state with
  | (p1,_,_) -> p_hits p1.sunken_ships

(* [player_hits s] is the number of ships the first player has hit. *)
let player_hits state =
  match state with
  | (_,p2,_) -> p_hits p2.sunken_ships

(* [not_hit_lst coorlst] returns a list of all the coordinates that are
   not yet hit, *)
let rec not_hit_lst coorlst =
  match coorlst with
  | [] -> []
  | h::t -> if h.hit = false then h::(not_hit_lst t) else not_hit_lst t

(* [player_not_hit s] is the list of the coordinates that have not been hit
   on the player's board. *)
let player_not_hit state =
  match state with
  | (p1,_,_) -> not_hit_lst p1.coordinates

(* [opp_not_hit s] is the list of the coordinates that have not been hit
   on the computer's board. *)
let opp_not_hit state =
  match state with
  | (_,p2,_) -> not_hit_lst p2.coordinates

(* [not_ship_lst coorlst] returns a list of coordinates that do no have
   any ships. *)
let rec not_ship_lst coorlst =
  match coorlst with
  | [] -> []
  | h::t -> if h.ship = None then h::(not_ship_lst t) else not_ship_lst t

(* [opp_not_ship s] is the list of the coordinates that do not have ships
   on the computer's board. *)
let opp_not_ship state =
  match state with
  | (_,p2,_) -> not_ship_lst p2.coordinates

(* [first] returns the first element in a tuple. *)
let first (x,_,_) = x

(* [sec] returns the second element in a tuple. *)
let sec (_,y,_) = y

(* [thrd] returns the third element in a tuple. *)
let thrd (_,_,z) = z

(* [player_ships_sunk s] is the list of ships that the player has sunk. *)
let player_ships_sunk s =
  let o = sec s in
  let lst = o.sunken_ships in
  let ships = List.filter (fun e -> snd e = true) lst in
  fst (List.split ships)

(* [opp_ships_sunk s] is the list of ships of the player that are sunk. *)
let opp_ships_sunk s =
  let p = first s in
  let lst = p.sunken_ships in
  let ships = List.filter (fun e -> snd e = true) lst in
  fst (List.split ships)

(* [count_hits coords accum] returns the total number of hits needed for
   the player to win. *)
let rec count_hits coords accum =
  match coords with
  | [] -> accum
  | h::t ->
    if h.ship <> None && h.hit = false
    then count_hits t (accum+1)
    else count_hits t accum

(* [opp_hits_left s] is the number of hits needed for opponent to win. *)
let opp_hits_left s =
  let pl1 = first s in
  let plcoords = pl1.coordinates in
  count_hits plcoords 0

(* [player_hits_left s] is the number of hits needed for player to win. *)
let player_hits_left s =
  let pl2 = sec s in
  let plcoords = pl2.coordinates in
  count_hits plcoords 0

exception Coordinate_Not_Found of string

(* [find_cool c l] returns the corrdinate in l that matches the ID of c.
   Raises: exception Coordinate_Not_Found if there is no coordinate in l with an
  Id that matches the ID of c. *)
let rec find_coor c l =
  match l with
  | [] -> raise (Coordinate_Not_Found "c")
  | h::t -> if h.id = c then h else find_coor c t

(* [player_attack c s] is whether player has already attacked coordinate c of
   the opponent's board. *)
let player_attack (c:coordinate) (s:state) =
  let o_coor = (sec s).coordinates in
  let c' = find_coor c.id o_coor in
  c'.hit

(* [opp_attack c s] is whether the computer has already attacked coordinate c
   of the player's board. *)
let opp_attack c s =
  let p_coor = (first s).coordinates in
  let c' = find_coor c.id p_coor in
  c'.hit

(* [extract op] returns the type Option without the Some.
    Requires: op is not None. *)
let extract op =
  match op with
  | Some x -> x
  | None -> failwith ("Not possible")

(* [parse_command_s c] returns a string representation of the command. *)
let parse_command_s (c:command) =
  if c.firing_coordinate <> None then extract c.firing_coordinate
  else "unchanged"

(* [index_help c] helps index in converting an ID in a string form to
   an int. *)
let index_help c =
  if String.length c = 3 then 9
  else match c.[1] with
    | '1' -> 0
    | '2' -> 1
    | '3' -> 2
    | '4' -> 3
    | '5' -> 4
    | '6' -> 5
    | '7' -> 6
    | '8' -> 7
    | '9' -> 8
    | _ -> failwith ("Not valid")

(* [index c] converts an index in string form to an int.
   Example: [index "a1"] = 0.
   Requires: c is a valid index, meaning it its letter is between a...j and
   its number is between 1...10. *)
let index c =
  match c.[0] with
  | 'a' -> 0 + index_help c
  | 'b' -> 10 + index_help c
  | 'c' -> 20 + index_help c
  | 'd' -> 30 + index_help c
  | 'e' -> 40 + index_help c
  | 'f' -> 50 + index_help c
  | 'g' -> 60 + index_help c
  | 'h' -> 70 + index_help c
  | 'i' -> 80 + index_help c
  | 'j' -> 90 + index_help c
  | _ -> failwith ("Not valid")

exception Invalid_Command of string
exception Invalid_Hit of string

(* [update_helper_1 x p] checks through the coordinates of a ship to
   determine whether the ship has sunk or not. *)
let update_helper_1 x p =
  let coor = snd x in
  let lst = List.map (fun a -> (find_coor a.id p.coordinates).hit) coor in
  List.for_all (fun b -> b = true) lst

(* [update_helper_2 x p] returns a tuple representing the status of a ship. *)
let update_helper_2 x p =
  if update_helper_1 x p = true then (fst x,true) else (fst x,false)

(* [update_ships s] updates each player's sunken_ships after the new hit.
   Returns : a new state with the coordinates of the players unchanged,
but the players' sunken_ships fields are updated. *)
let update_ships s =
  let p1 = first s in
  let p2 = sec s in
  let p1_ships = player_ships s in
  let p2_ships = opp_ships s in
  let p1_updated = List.map (fun x -> update_helper_2 x p1) p1_ships in
  let p2_updated = List.map (fun x -> update_helper_2 x p2) p2_ships in
  let new_p1 = {coordinates = p1.coordinates; sunken_ships = p1_updated} in
  let new_p2 = {coordinates = p2.coordinates; sunken_ships = p2_updated} in
  (new_p1, new_p2, thrd s)

(* [replace c clist] replaces the coordinate in clist with the same ID as c
   with c. *)
let rec replace c clist =
  match clist with
  | [] -> []
  | h::t -> if c.id = h.id then c::t else h::(replace c t)

(* [next_state c s] updates the state after taking in a command.
   Returns: an unchanged state if the command is invalid.
*)
let next_state c s =
  if c = {
      firing_coordinate = None;
      check_ships = false;
      hits_left = false;
      quit = false
    } then let () = print_endline "Not a valid command, enter again: " in s
  else let command = parse_command_s c in
    if command = "unchanged" then s
    else if thrd s = "player1" then
      if player_attack {id = index command; hit = true; ship = None} s = true
      then let () = print_endline
              ("Already attacked this coordinate. Choose a different one.") in s
      else let x = find_coor (index command) (sec s).coordinates in
        if x.hit = true
        then let () = print_endline
              ("Already attacked this coordinate. Choose a different one.") in s
        else let i = index command in
          let coor = List.nth (sec s).coordinates i in
          let coor1 = {id = coor.id; hit = true; ship = coor.ship} in
          let new_c = replace coor1 (sec s).coordinates in
          update_ships (first s,
        {coordinates = new_c; sunken_ships = (sec s).sunken_ships}, "player2")
    else
    if opp_attack {id = index command; hit = true; ship = None} s = true
    then let () = print_endline ("Invalid hit: Choose a different coordinate") in s
    else let x = find_coor (index command) (first s).coordinates in
      if x.hit = true
      then let () = print_endline
            ("Already attacked this coordinate. Choose a different one.") in s
      else let i = index command in
        let coor = List.nth (first s).coordinates i in
        let coor1 = {id = coor.id; hit = true; ship = coor.ship} in
        let new_c = replace coor1 (first s).coordinates in
        update_ships ({coordinates = new_c;
                      sunken_ships = (first s).sunken_ships}, sec s, "player1")

(* [game_over s] returns whether or not a player has sunken all of its
   opponent's ships, and which player it is. *)
let game_over s =
  let p1_ships = (first s).sunken_ships in
  let p2_ships = (sec s).sunken_ships in
  if List.for_all (fun x -> snd x) p1_ships = true then (true,"player2")
  else if List.for_all (fun x -> snd x) p2_ships = true then (true,"player1")
  else (false,"none")

(* [id_to_string i] is the conversion from [i] to the string representing
   its coordinate *)
let id_to_string i =
  let second = string_of_int (i mod 10 + 1) in
  match i/10 with
  | 0 -> "A" ^ second
  | 1 -> "B" ^ second
  | 2 -> "C" ^ second
  | 3 -> "D" ^ second
  | 4 -> "E" ^ second
  | 5 -> "F" ^ second
  | 6 -> "G" ^ second
  | 7 -> "H" ^ second
  | 8 -> "I" ^ second
  | 9 -> "J" ^ second
  | _ -> failwith "can't happen"
