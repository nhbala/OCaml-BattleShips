open Command

(* [ship] is a variant that contains all the ships of the game. *)
type ship = Carrier | Battleship | Cruiser | Submarine | Destroyer

(* [coordinate] is an abstract type representing a coordinate of
   the battleship board. *)
type coordinate = {
  id: int;
  hit: bool;
  ship: ship option
}

(* [player] is an abstract type representing a player in the game. *)
type player = {
  coordinates: coordinate list;
  sunken_ships: (ship * bool) list;
}

(* [state] is an abstract type representing the state of the game. *)
type state = player * player * string

(* [player_ships s] is a list of all the player's ships and their locations on
   the player's board. *)
val player_ships : state -> (ship * coordinate list) list

(* [sort_lst_ship_coordinate lst] is the same list but with each coordinate list
 * sorted by increasing id. *)
val sort_lst_ship_coordinate : (ship * coordinate list) list -> (ship * coordinate list) list

(* [opp_ships s] is a list of all the computer's ships and their locations on
   its board. *)
val opp_ships : state -> (ship * coordinate list) list

(* [player_hits s] is the number of ships the first player has hit. *)
val player_hits : state -> int

(* [opp_hits s] is the number of ships the second player has hit. *)
val opp_hits : state -> int

(* [player_not_hit s] is the list of the coordinates that have not been hit
   on the player's board. *)
val player_not_hit : state -> coordinate list

(* [opp_not_hit s] is the list of the coordinates that have not been hit
   on the computer's board. *)
val opp_not_hit : state -> coordinate list

(* [opp_not_ship s] is the list of the coordinates that do not have ships
   on the computer's board. *)
val opp_not_ship : state -> coordinate list

(* [player_ships_sunk s] is the list of ships that the player has sunk. *)
val player_ships_sunk : state -> ship list

(* [opp_ships_sunk s] is the list of ships of the player that are sunk. *)
val opp_ships_sunk : state -> ship list

(* [player_hits_left s] is the number of hits needed for player to win. *)
val player_hits_left : state -> int

(* [opp_hits_left s] is the number of hits needed for opponent to win. *)
val opp_hits_left : state -> int

(* [player_attack c s] is whether player has already attacked coordinate c of
   the opponent's board. *)
val player_attack : coordinate -> state -> bool

(* [opp_attack c s] is whether the computer has already attacked coordinate c
   of the player's board. *)
val opp_attack : coordinate -> state -> bool

(* [next_state c s] returns the new state after taking in a command. *)
val next_state : Command.command -> state -> state

(* [game_over s] returns whether or not a player has sunken all of its
   opponent's ships, and which player it is. *)
val game_over : state -> bool*string

(* [id_to_string i] is the conversion from [i] to the string representing
   its coordinate *)
val id_to_string : int -> string
