(* [command] represents a command input by a player. *)
type command = {
  firing_coordinate: string option;
  check_ships: bool;
  hits_left: bool;
  quit: bool
}

(* [default_command] represents a null command with all fields taking
 * a None value or false value. *)
let default_command = {
  firing_coordinate = None;
  check_ships = false;
  hits_left = false;
  quit = false
}

(* [parse_coordinate str] returns the default command if the coordinate is not
 * valid, and returns the default_command with the firing_coordniate field
 * updated if the coordinate falls within the board. *)
let parse_coordinate str =
  if String.length str = 3 then
    let letter = str.[0] in
    let numb = int_of_string (String.sub str 1 2) in
    if numb = 10 && (Char.code letter >= 97 && Char.code letter <= 106)
    then {default_command with firing_coordinate = Some str}
    else default_command
  else if String.length str > 3 || String.length str < 2 then default_command
  else
    let letter = str.[0] in
    let numb = int_of_string (String.sub str 1 1) in
    if numb >= 1 && numb <= 10
       && (Char.code letter >= 97 && Char.code letter <= 106) then
      {default_command with firing_coordinate = Some str} else default_command

(* [parse_command str] returns a command that corresponds with the player input in
 * [str]. If the command is not valid, we just return the default command. *)
let parse_command str =
  match (String.trim (String.lowercase_ascii str)) with
  | "quit" -> {default_command with quit = true}
  | "ships" -> {default_command with check_ships = true}
  | "hits" -> {default_command with hits_left = true}
  | _ -> parse_coordinate (String.lowercase_ascii str)
