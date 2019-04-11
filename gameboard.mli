open State



(* [draw_gameboard] draws the mepty gameboards on the graphics window*)
val draw_gameboard : unit -> unit

(* [drawstrike] takes in a ship and coordinate and draws a box indicating a
   strike on the board, returns a red or black box depending if it was
   a successful hit or not
*)
val drawstrike : ship option -> string -> unit


(*[drawship] takes in two coordinates and draws a line representing
  the ship on the graphics module
*)
val drawship : string -> string -> unit

(* [drawstrike_self] same as drawstrike but for the user board*)
val drawstrike_self : ship option-> string -> unit

(*[drawallships] takes in a list of ship coordinates and draws all the ships
  on the graphics window
*)
val drawallships : (ship * coordinate list) list -> unit

(* [draw_p1_window] takes in a state and draws the player one window with
   all the information stored in state*)
val draw_p1_window : state -> unit

(* [draw_p2_window] same as draw_p1_window but for player 2*)

val draw_p2_window : state -> unit
