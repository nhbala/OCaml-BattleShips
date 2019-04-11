open State

(* [default_cinfo] is a default coordinate list with no hits and no ships.
 * The coordinate list is in sorted order by id. *)
let default_cinfo = List.init 100 (fun a -> {id = a; hit = false; ship = None})

(* [merge_coords shipcoords lst] is lst with ships added. [lst] should not have
 * coordinate ids with a ship if [shipcoords] also has that coordinate id. *)
let merge_coords shipcoords lst =
  let ship_ids = List.map (fun c -> c.id) shipcoords in
  let rec merge lst =
    match lst with
    | [] -> lst
    | h::t ->
    if List.mem h.id ship_ids
    then let coord_same = List.find (fun c -> c.id = h.id) shipcoords
      in coord_same::(merge t)
    else h::(merge t)
  in merge lst

(* [create_coordinates ship len start side dir final_list] adds to the coordinate list
 * [final_list] by randomly placing [ship] and adding its corresponding coordinates. *)
let rec create_coordinates ship len start side dir final_list =
  if len = 0 then final_list else
    let accum = if dir = 0 then 1 else 10 in
    let newcoor = {id = start + side; hit = false; ship = Some ship} in
    final_list @ create_coordinates ship (len-1) (start+accum) side dir [newcoor]

(* [check_overlap c1 c2] returns false if there are no repeated ids in c1 and c2
 * and true otherwise. *)
let check_overlap c1 c2 =
  let first_ids = List.map (fun c -> c.id) c1 in
  let second_ids = List.map (fun c -> c.id) c2 in
  let first_sorted = List.sort compare first_ids in
  let second_sorted = List.sort compare second_ids in
  let combined = first_sorted @ second_sorted in
  let origlen = List.length combined in
  let uniq_sorted = List.sort_uniq compare combined in
  if origlen = List.length uniq_sorted then false else true

(* [init_ship ship len] is a coordinate list representing the random placement of
   a ship. A random int 0 means we start on the left side of the board,
   and a random int 0 means the ship is placed horizontal. *)
let init_ship ship len =
  let x = Random.int (10 - len + 1) in
  let y = Random.int (10 - len + 1) in
  let idpos = (x)+ 10*(y+1) in
  let side = Random.int 2 in
  let dir = Random.int 2 in
  create_coordinates ship len idpos side dir []

(* [no_dup_helper c all_ship_coor] is true if there are no duplicate coordinates
 * in [all_ship_coor]. *)
let rec no_dup_helper c all_ship_coor =
  match all_ship_coor with
  | [] -> true
  | h::t -> if h.id = c.id then false else no_dup_helper c t

(* [no_dup ship all_ship_coor] checks whether all the coordinates in ship
   are unique, meaning there will not be an overlap with other ships.
   Returns: true if the coordinates in ship do not have overlap. *)
let rec no_dup ship all_ship_coor =
  match ship with
  | [] -> true
  | h::t -> (no_dup_helper h all_ship_coor) && (no_dup t all_ship_coor)

(* [merge_helper old_coords new_coords old_board ship len] returns a list of
 * coordinates adding [ship] to [old_board] so that there are no overlaps with
 * any ships. *)
let rec merge_helper ship_coor board all_ship_coor ship len =
  if no_dup ship_coor all_ship_coor = true then
    ((merge_coords ship_coor board),ship_coor) else
    merge_helper (init_ship ship len) board all_ship_coor ship len

(* [computer_init_board] initializes the computer's board with no overlapped
   ships. *)
let computer_init_board = fun () ->
  let destr_coords = init_ship Destroyer 2 in
  let destr_fullboard = merge_helper destr_coords default_cinfo [] Destroyer 2 in
  let board_0 = fst destr_fullboard in
  let all_ship_0 = (snd destr_fullboard)@[] in
  let sub_coords = init_ship Submarine 3 in
  let sub_fullboard = merge_helper sub_coords board_0 all_ship_0 Submarine 3 in
  let board_1 = fst sub_fullboard in
  let all_ship_1 = (snd sub_fullboard)@all_ship_0 in
  let cruiser_coords = init_ship Cruiser 3 in
  let cruiser_fullboard = merge_helper cruiser_coords board_1 all_ship_1 Cruiser 3 in
  let board_2 = fst cruiser_fullboard in
  let all_ship_2 = (snd cruiser_fullboard)@all_ship_1 in
  let battleship_coords = init_ship Battleship 4 in
  let battleship_fullboard = merge_helper battleship_coords board_2 all_ship_2 Battleship 4 in
  let board_3 = fst battleship_fullboard in
  let all_ship_3 = (snd battleship_fullboard)@all_ship_2 in
  let carrier_coords = init_ship Carrier 5 in
  fst (merge_helper carrier_coords board_3 all_ship_3 Carrier 5)

type hit_lst = {
  mutable hitlst: coordinate list
}

let target_hit_list = {
  hitlst = []
}

exception Coordinate_Not_Found of string

(* returns the first component of the tuple*)
let first (x,_,_) = x


(* [find_cool c l] returns a single corrdinate in l that matches c. *)
let rec find_coor c l =
  match l with
  | [] -> raise (Coordinate_Not_Found "c")
  | h::t -> if h.id = c then h else find_coor c t


(* [computer_turn_easy] takes in a state and returns a coordinate to be attacked
*)
let rec computer_turn_easy state =
 let hit_lst = player_not_hit state in
  let pointer = Random.int (List.length hit_lst) in
  List.nth hit_lst pointer


(* [return_points_target] takes in a coordinate and outputs a lst
   of ints that represent which coordinates should be attacked next
*)
let return_points_target coor =
  match coor.id with
  | 0 -> [1;10]
  | 1 -> [2;11]
  | 2 -> [1;3;12]
  | 3 -> [2;4;13]
  | 4 -> [3;5;14]
  | 5 -> [4;6;15]
  | 6 -> [5;7;16]
  | 7 -> [6;8;17]
  | 8 -> [7;9;18]
  | 9 -> [8;19]
  | 90 -> [80;91]
  | 91 -> [90;92;81]
  | 92 -> [91;93;82]
  | 93 -> [92;94;83]
  | 94 -> [93;95;84]
  | 95 -> [94;96;85]
  | 96 -> [95;97;86]
  | 97 -> [96;98;87]
  | 98 -> [97;99;88]
  | 99 -> [89;98]
  | 10 -> [0;11;20]
  | 20 -> [10;21;30]
  | 30 -> [20;31;40]
  | 40 -> [30;41;50]
  | 50 -> [40;51;60]
  | 60 -> [50;61;70]
  | 70 -> [60;71;80]
  | 80 -> [90;81;90]
  | 19 -> [9;18;29]
  | 29 -> [19;28;39]
  | 39 -> [29;38;49]
  | 49 -> [39;48;59]
  | 59 -> [49;58;69]
  | 69 -> [59;68;79]
  | 79 -> [69;78;89]
  | 89 -> [79;88;99]
  | _ -> [coor.id - 10; coor.id + 10 ; coor.id -1 ; coor.id + 1]


(* [create_coor_target_helper] takes in an int lst and a state and
   creates a list of coordinates that coorespond with those ints
*)
let rec create_coor_target_helper intlst state =
  match intlst with
  | [] -> []
  | h::t -> (find_coor h ((first state).coordinates)):: (create_coor_target_helper t state)


(* [coorlst] takes in a coordinate list and filters out ones that have already
   been hit
*)
let rec final_target_lst coorlst =
  match coorlst with
  | [] -> []
  | h::t -> if h.hit = false then h::(final_target_lst t) else
      final_target_lst t



(* [medium_turn] takes in a state and outputs a coordinate for the computer
   to attack
*)
let medium_turn state =
  if target_hit_list.hitlst != [] then let point1 = List.nth target_hit_list.hitlst 0 in
    match point1.ship with
    | None -> target_hit_list.hitlst <- List.tl target_hit_list.hitlst; point1
    | Some _ -> let basic = return_points_target point1 in
      let next = create_coor_target_helper basic state in
      let coorlst = final_target_lst next in
      let currentlst =  List.tl target_hit_list.hitlst in
      target_hit_list.hitlst <- coorlst@currentlst; point1
  else
  let hit_lst = player_not_hit state in
  let pointer = Random.int (List.length hit_lst) in
  let point = List.nth hit_lst pointer in
  let pointship = point.ship in
  match pointship with
  | None -> point
  | Some _ -> let basic = return_points_target point in
    let next = create_coor_target_helper basic state in
    let coorlst = final_target_lst next in
    let currentlst = target_hit_list.hitlst in
    target_hit_list.hitlst <- currentlst@coorlst;
    point

(* [computer_turn] takes in a level represented by a string and a
   state and outputs a point to attack
*)
let computer_turn level state =
  let lower_case_level = String.lowercase_ascii level in
  if lower_case_level = "easy" then computer_turn_easy state else if
    lower_case_level = "medium" then medium_turn state
  else failwith "not a valid level"
