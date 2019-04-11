open State

(* [parse s] takes in a string containing information on the locations of
   the ships and parses through the string to return a type p_info to
   initialize the state. *)
val parse_setup : string -> ship -> coordinate list

(* [player_coor ls] takes in a list of tuple of string and Ship and returns
   a coordinate list with all the ships placed.
   Requires: ls is of length 5.
   Raises: exception if the coordinates of the ships overlapped. *)
val player_coor : (string * ship) list -> coordinate list

(* [setup_ships str] handles the setup of all the ships for a player and returns
 * a coordinate list with all the ships placed. *)
val setup_ships: string -> coordinate list

(* [make_ship_lst ship lst] is a list of coordinates storing the presence of
 * the ship [ship].
 * Requires: [lst] is an int list. *)
val make_ship_lst : ship -> int list -> coordinate list

(* [checking_coor_placement lst] is true if [lst] is a list of coordinates
 * that represent a possible placement of a ship. *)
val checking_coor_placement : int list -> bool

(* [pinput_to_int str] converts a coordinate string to the corresponding integer.*)
val pinput_to_int : string -> int
