open State

(* [computer_turn s] is the coordinate that the computer chooses to hit
 * when the game is at state [s]. *)
val computer_turn: string -> state -> coordinate

(* [computer_init_board] is the state of the computer side's initial gameboard. *)
val computer_init_board: unit -> coordinate list
