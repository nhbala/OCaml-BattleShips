open State
open Command
open Setup
open Gameboard
open Graphics
open Ai

(* [thrd] returns the third element in a tuple. *)
let thrd (_,_,z) = z

(* [repl_oneplayer state level] runs one iteration of the game loop starting at
 * [state] and in difficulty [level] for the one player game. *)
let rec repl_oneplayer state level =
  try
    let status = game_over state in
    if (fst status) then
      let winner = snd status in
      if winner = "player1" then (print_endline "You Win!!")
      else (print_endline "Computer Wins!")
    else
      let player = thrd state in
      if player = "player1"
      then
        begin
          let initial_ships_sunk = player_hits state in
          draw_p1_window state;
          let str = print_string ("Player 1 command: "); read_line() in
          let command = parse_command str in
          if command.quit
          then (print_endline("Goodbye!"); exit 0;)
          else if command.check_ships
          then let () = print_int (player_hits state);
                 print_newline () in repl_oneplayer state level
          else if command.hits_left
          then let () = print_int (player_hits_left state);
                 print_newline () in repl_oneplayer state level
          else let nextstate = next_state command state in
            if player_hits nextstate <> initial_ships_sunk
            then let () = print_endline "Ship sunk!" in
              repl_oneplayer nextstate level
            else repl_oneplayer nextstate level
        end
      else begin
        let computer_attack = id_to_string (computer_turn level state).id in
        let command = parse_command computer_attack in
        repl_oneplayer (next_state command state) level
      end
  with _ -> let () = print_endline "Invalid command!" in repl_oneplayer state level

(* [repl_twoplayer state] runs one iteration of the game loop for the two player
 * option. *)
let rec repl_twoplayer state =
  try
    let status = game_over state in
    if (fst status) then
      let winner = snd status in
      if winner = "player1" then (print_endline "Player 1 Wins!")
      else (print_endline "Player 2 Wins!")
    else
      let player = thrd state in
      if player = "player2"
      then begin
        let initial_ships_sunk = opp_hits state in
        draw_p2_window state;
        let str = print_string ("Player 2 command: "); read_line() in
        let command = parse_command str in
        if command.quit
        then (print_endline("Goodbye!"); exit 0;)
        else if command.check_ships then
          let () = print_int (opp_hits state); print_newline () in repl_twoplayer state
        else if command.hits_left then
          let () = print_int (opp_hits_left state); print_newline () in repl_twoplayer state
        else let nextstate = next_state command state in
          if player_hits nextstate <> initial_ships_sunk
          then let () = print_endline "Ship sunk!" in
            repl_twoplayer nextstate
          else repl_twoplayer nextstate
      end
      else begin
        let initial_ships_sunk = player_hits state in
        draw_p1_window state;
        let str = print_string ("Player 1 command: "); read_line() in
        let command = parse_command str in
        if command.quit
        then (print_endline("Goodbye!"); exit 0;)
        else if command.check_ships
        then let () = print_int (player_hits state);
               print_newline () in repl_twoplayer state
        else if command.hits_left
        then let () = print_int (player_hits_left state);
               print_newline () in repl_twoplayer state
        else let nextstate = next_state command state in
          if player_hits nextstate <> initial_ships_sunk
          then let () = print_endline "Ship sunk!" in
            repl_twoplayer nextstate
          else repl_twoplayer nextstate
      end
  with _ -> let () = print_endline "Invalid command!" in repl_twoplayer state

(* [main_two_player ()] controls the game sequence for a two player game, including
 * setup, graphics, and the actual game. *)
let rec main_two_player () =
  try
    let fst_pinfo = setup_ships "First Player Setup:" in
    let () = print_newline();print_newline();print_newline();print_newline();
      print_newline();print_newline();print_newline();print_newline();
      print_newline();print_newline();print_newline();print_newline();print_newline();
      print_newline();print_newline();print_newline();print_newline();print_newline();
      print_newline();print_newline();print_newline();print_newline();print_newline() in
    let snd_pinfo = setup_ships "Second Player Setup:" in
    let () = print_newline();print_newline();print_newline();print_newline();
      print_newline();print_newline();print_newline();print_newline();
      print_newline();print_newline();print_newline();print_newline();print_newline();
      print_newline();print_newline();print_newline();print_newline();print_newline();
      print_newline();print_newline();print_newline();print_newline();print_newline() in
    let init_state =
      ({coordinates = fst_pinfo;
        sunken_ships = [(Carrier, false); (Battleship, false); (Cruiser, false);
                        (Submarine, false);(Destroyer, false)]},
       {coordinates = snd_pinfo;
        sunken_ships = [(Carrier, false); (Battleship, false); (Cruiser, false);
                        (Submarine, false);(Destroyer, false)]}, "player1") in
    draw_p1_window init_state;
    let () = print_newline(); print_string ("DIRECTIONS:\n
    Enter a coordinate (ex. \"G3\") to hit,\n
    \"quit\" to quit the game,\n
    \"ships\" to check how many ships you have sunk,\n
    \"hits\" to check how many hits you need to win.\n\n") in
    repl_twoplayer init_state
  with _ -> print_endline ("Try again!"); main_two_player ()

(* [main_one_player ()] controls the game for the one player game option, including
 * setting up the board for both the user and computer, as well as running the
 * game loop and handling the correct difficulty. *)
let rec main_one_player () =
  try
    let fst_pinfo = setup_ships "First Player Setup:" in
    let c_info = computer_init_board() in
    let init_state =
      ({coordinates = fst_pinfo;
        sunken_ships = [(Carrier, false); (Battleship, false); (Cruiser, false);
                        (Submarine, false);(Destroyer, false)]},
       {coordinates = c_info;
        sunken_ships = [(Carrier, false); (Battleship, false); (Cruiser, false);
                        (Submarine, false);(Destroyer, false)]}, "player1") in
    draw_p1_window init_state;
    let level =
      print_string "Please enter \"easy\" or \"hard\": ";
      read_line () in
    if level = "quit"
    then (print_endline("Goodbye!"); exit 0;) else
      let () = print_newline(); print_string ("DIRECTIONS:\n
    Enter a coordinate (ex. \"G3\") to hit,\n
    \"quit\" to quit the game,\n
    \"ships\" to check how many ships you have sunk,\n
    \"hits\" to check how many hits you need to win.\n\n") in
      if String.trim (String.lowercase_ascii level) = "easy"
      then repl_oneplayer init_state "easy"
      else if String.lowercase_ascii level = "hard"
      then repl_oneplayer init_state "medium"
      else failwith "Not a valid level"
  with _ -> print_endline ("Try again!"); main_one_player()

(* [game_setting ()] handles the type of game, one or two player, as well as
 * calling the correct game loop based on the user's input. *)
let rec game_setting () =
  try
    let typegame =
      print_endline "Please enter \"one player\" or \"two player\": ";
      read_line () in let l_type = String.lowercase_ascii typegame in
    if l_type = "one player" then main_one_player ()
    else if l_type = "two player" then main_two_player ()
    else if l_type = "quit" then (print_endline("Goodbye!"); exit 0;)
    else failwith "Not a valid player configuration"
  with _ -> game_setting ()

(* [main ()] sets up the initial battleship game, opens the graphics, and
 * runs the entire game loop based on user inputs. *)
let main () =
  ANSITerminal.(print_string [black] "\n\nWelcome to Battleship! You can enter \"quit\" at anytime.\n");
  open_graph " 1100x500";
  let () = draw_gameboard() in
  game_setting ()


let () = main ()
