(* [command] represents an abstract type which is a command inputed by
   the player *)
type command = {
  firing_coordinate: string option;
  check_ships: bool;
  hits_left: bool;
  quit: bool
}

(* [parse str] takes in a player input represented by a string and
   returns a command of type command
   requires: [str] which is the string form of one of the commands
*)
val parse_command: string -> command
