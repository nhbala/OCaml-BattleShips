open OUnit2
open State
open Command
open Setup
open Ai

let a1 = {id = 0; hit = false; ship = None}
let a2 = {id = 1; hit = false; ship = Some Destroyer}
let a3 = {id = 2; hit = false; ship = Some Destroyer}
let a4 = {id = 10; hit = false; ship = None}
let a5 = {id = 11; hit = false; ship = None}
let a6 = {id = 12; hit = false; ship = None}
let a7 = {id = 20; hit = false; ship = None}
let a8 = {id = 21; hit = false; ship = None}
let a9 = {id = 22; hit = false; ship = None}

let b1 = {id = 0; hit = false; ship = Some Cruiser}
let b2 = {id = 1; hit = false; ship = Some Cruiser}
let b3 = {id = 2; hit = false; ship = Some Cruiser}
let b4 = {id = 10; hit = false; ship = None}
let b5 = {id = 11; hit = false; ship = None}
let b6 = {id = 12; hit = false; ship = None}
let b7 = {id = 20; hit = false; ship = None}
let b8 = {id = 21; hit = false; ship = None}
let b9 = {id = 22; hit = false; ship = None}

let c1 = [a1;a2;a3;a4;a5;a6;a7;a8;a9]
let c2 = [b1;b2;b3;b4;b5;b6;b7;b8;b9]

let j1 = [(Carrier,false);(Battleship,false);(Cruiser,false);(Submarine,false);(Destroyer,false)]
let j2 = [(Carrier,false);(Battleship,false);(Cruiser,false);(Submarine,false);(Destroyer,false)]

let player1 = {coordinates = c1; sunken_ships = j1}
let player2 = {coordinates = c2; sunken_ships = j2}

let s = (player1, player2, "player1")

let o1 = [(Carrier, []); (Battleship, []); (Cruiser, []); (Submarine, []);
          (Destroyer,
           [{id = 1; hit = false; ship = Some Destroyer};
            {id = 2; hit = false; ship = Some Destroyer}])]

let o2 = [(Carrier, []); (Battleship, []);
          (Cruiser,
           [{id = 0; hit = false; ship = Some Cruiser};
            {id = 1; hit = false; ship = Some Cruiser};
            {id = 2; hit = false; ship = Some Cruiser}]);
          (Submarine, []); (Destroyer, [])]

let o3 = [{id = 0; hit = false; ship = None};
          {id = 1; hit = false; ship = Some Destroyer};
          {id = 2; hit = false; ship = Some Destroyer};
          {id = 10; hit = false; ship = None}; {id = 11; hit = false; ship = None};
          {id = 12; hit = false; ship = None}; {id = 20; hit = false; ship = None};
          {id = 21; hit = false; ship = None}; {id = 22; hit = false; ship = None}]

let o4 = [{id = 0; hit = false; ship = Some Cruiser};
          {id = 1; hit = false; ship = Some Cruiser};
          {id = 2; hit = false; ship = Some Cruiser};
          {id = 10; hit = false; ship = None}; {id = 11; hit = false; ship = None};
          {id = 12; hit = false; ship = None}; {id = 20; hit = false; ship = None};
          {id = 21; hit = false; ship = None}; {id = 22; hit = false; ship = None}]

let command_a1 = {
  firing_coordinate = Some "a1";
  check_ships = false;
  hits_left = false;
  quit = false
}

let command_a2 = {
  firing_coordinate = Some "a2";
  check_ships = false;
  hits_left = false;
  quit = false
}

let command_a3 = {
  firing_coordinate = Some "a3";
  check_ships = false;
  hits_left = false;
  quit = false
}

let s1 = next_state command_a1 s

let o5 = [(Carrier, []); (Battleship, []);
          (Cruiser,
           [{id = 0; hit = true; ship = Some Cruiser};
            {id = 1; hit = false; ship = Some Cruiser};
            {id = 2; hit = false; ship = Some Cruiser}]);
          (Submarine, []); (Destroyer, [])]

let first (x,_,_) = x
let sec (_,y,_) = y
let thrd (_,_,z) = z

let o6 = [{id = 0; hit = true; ship = Some Cruiser};
          {id = 1; hit = false; ship = Some Cruiser};
          {id = 2; hit = false; ship = Some Cruiser};
          {id = 10; hit = false; ship = None}; {id = 11; hit = false; ship = None};
          {id = 12; hit = false; ship = None}; {id = 20; hit = false; ship = None};
          {id = 21; hit = false; ship = None}; {id = 22; hit = false; ship = None}]

let s2 = next_state command_a2 s1
let s3 = next_state command_a2 s2
let s4 = next_state command_a3 s3

let o7 = [(Carrier, true); (Battleship, true); (Cruiser, true); (Submarine, true);
          (Destroyer, true)]

let o8 = [Carrier; Battleship; Cruiser; Submarine; Destroyer]

let quit_command =
  {
    firing_coordinate = None;
    check_ships = false;
    hits_left = false;
    quit = true
  }

let check_command_ships =
  {
    firing_coordinate = None;
    check_ships = true;
    hits_left = false;
    quit = false
  }

let check_command =
  {
    firing_coordinate = None;
    check_ships = true;
    hits_left = false;
    quit = false
  }

let a1_command =
  {
    firing_coordinate = Some "a1";
    check_ships = false;
    hits_left = false;
    quit = false
  }


let def_command =
  {
    firing_coordinate = None;
    check_ships = false;
    hits_left = false;
    quit = false
  }

let destr_coor = [{id = 0; hit = false; ship = Some Destroyer};
                  {id = 1; hit = false; ship = Some Destroyer}]

let player = player_coor [("a1;a2",Destroyer);("b1;b2;b3",Submarine);("a3;a4;a5",Cruiser)
                         ;("b4;b5;b6;b7",Battleship);("c1;c2;c3;c4;c5",Carrier)]


let com_board = computer_init_board ()

let rec ship_cells lst =
  begin
  match lst with
  | [] -> 0
  | h::t -> begin
      match h.ship with
      | None -> 0 + ship_cells t
      | Some x -> 1 + ship_cells t
    end
end

let st = ({coordinates = player; sunken_ships = j1},{coordinates = com_board;
                            sunken_ships = j2},"player1")

let tests =
  [
    (* Sweep case 0: tests the functionality of player_ships. *)
    "player_ships" >:: (fun _ -> assert_equal o1 (s |> player_ships));

    (* Sweep case 1: tests the functionality of opp_ships. *)
    "opp_ships" >:: (fun _ -> assert_equal o2 (s |> opp_ships));

    (* Sweep case 2: this test case tests the functionality of player_hits. *)
    "player_hits" >:: (fun _ -> assert_equal 0 (s |> player_hits));

    (* Sweep case 3: this test case tests the functionality of opp_hits. *)
    "opp_hits" >:: (fun _ -> assert_equal 0 (s |> opp_hits));

    (* Sweep case 4: this test case tests the functionality of player_not_hit.*)
    "p_n_h" >:: (fun _ -> assert_equal o3 (s |> player_not_hit));

    (* Sweep case 5: this test case tests the functionality of opp_not_hit. *)
    "o_n_h" >:: (fun _ -> assert_equal o4 (s |> opp_not_hit));

    (* Sweep case 6: this test case tests the functionality of player_ships_sunk. *)
    "p_s_s" >:: (fun _ -> assert_equal [] (s |> player_ships_sunk ));

    (* Sweep case 7: this test case tests the functionality of opp_ships_sunk. *)
    "o_s_s" >:: (fun _ -> assert_equal [] (s |> opp_ships_sunk));

    (* Sweep case 8: this test case tests whether next_state updates state
       correctly when hitting a valid coordinate. This test case
    tests the list of ships of the opponent. *)
    "opp_ships0" >:: (fun _ -> assert_equal o5 (s1 |> opp_ships));

    (* Sweep case 9: this test case also tests next_state, but it returns
       the coordinate list of the opponent. *)
    "opp_coor" >:: (fun _ -> assert_equal o6 ((sec s1).coordinates));

    (* Sweep case 10: this test case also test next_state, but it specifically
       tests whether the player changes. *)
    "new_player" >:: (fun _ -> assert_equal "player2" (thrd s1));

    (* Sweep case 11: this test case tests whether game_over returns the
       correct information. In this case, since neither player has sunken all
       the ships, it should returns false. *)
    "game_over" >:: (fun _ -> assert_equal (false,"none") (s1 |> game_over));

    (* Sweep case 12: this test case tests whether sunken_ships updates
       correctly when the opposing player has sunken all the ships of the
       other player. *)
    "player_ships" >:: (fun _ -> assert_equal o7 (first s4).sunken_ships);

    (* Sweep case 13: tests whether opp_ships_sunk will return a list with
       all the ships. *)
    "o_s_s0" >:: (fun _ -> assert_equal o8 (s4 |> opp_ships_sunk));

    (* Sweep case 14: tests whether game_over returns true when player 2
       has sunken all of player 1's ships. *)
    "game_over0" >:: (fun _ -> assert_equal (true,"player2") (s4 |> game_over));

    (* Sweep case 15: tests whether parse_command parses the quit request
       correctly. Also tests whether parse_command handles capitalization and
    white spaces. *)
    "command_quit" >:: (fun _ -> assert_equal quit_command (parse_command " qUit "));

    (* Sweep case 16: tests whether parse_command parses the request for
       checking the number of hits left for the ships correctly. *)
    "command_ships" >:: (fun _ -> assert_equal check_command_ships (parse_command "  shIps"));

    (* Sweep case 17: tests whether parse_command parses a valid coordinate
       correctly. Since "a1" is in the range, it should returns a command
       where firing_coordinate is Some "a1". *)
    "coor_a1" >:: (fun _ -> assert_equal a1_command (parse_command "a1"));

    (* Sweep case 18: this test case also tests for firing a coordinate.
       However, this coordinate is invalid since it is out of the range of
       the alphabet a...j, It should return the default command. *)
    "coor_invalid_alphabet" >:: (fun _ -> assert_equal def_command (parse_command "z8"));

    (* Sweep case 19: as above, this test case tests for invalid coordinate,
       but the invalidity comes from the number, not the alphabet.
       Should return the default command.*)
    "coor_invalid_number" >:: (fun _ -> assert_equal def_command (parse_command "a20"));

    (* Sweep case 20: as above, this test case tests for an invalid command.
       The command itself is not a valid command, so parse_command Should
       return the default command. *)
    "invalid_command_parse" >:: (fun _ -> assert_equal def_command (parse_command "jfdkaslfdjsa"));

    (* Sweep case 21: tests that next_state does not update but rather raises
       an exception when the command is not valid. This test case catches
       the exception and returns the original, unchanged state. *)
    "invalid_command" >:: (fun _ -> assert_equal s
                              (next_state (parse_command "fdjksal") s)) ;

    (* Sweep case 22: tests that pinput_to_int returns the correct integer
       representation of a cell correctly when the cell is valid. This test
       case tests for the conversion of "a1", the leftmost cell of the
       board, it should should 0. *)
    "0_input_to_int" >:: (fun _ -> assert_equal 0 (pinput_to_int "a1"));

    (* Sweep case 23: tests that pinput_to_int handles the last cell,
       j10, correctly. It should return 99. *)
    "99_input_to_int" >:: (fun _ -> assert_equal 99 (pinput_to_int "j10"));

    (* Sweep case 24: tests that pinput_to_int fails when the input is
       invalid. In this case, the first character is out of bounds. *)
    "invalid_input" >:: (fun _ -> assert_equal 100
                            (try (pinput_to_int "z8") with
                               Failure n -> 100));

    (* Sweep case 25: tests the functionality of make_ship_lst when lst has
       the correct length for that specific ship. *)
    "destroyer_coor" >:: (fun _ -> assert_equal destr_coor (make_ship_lst Destroyer [0;1]));

    (* Sweep case 26: tests that make_ship_lst fails when the length of lst
       does not match up with the lenght of the ship. *)
    "invalid_make_ship" >:: (fun _ -> assert_equal []
                                (try make_ship_lst Destroyer [1;2;3;4;5] with Failure n -> []));

    (* Sweep case 27: tests that checking_coor_placement returns true
       when the input list has horizontal consecutive values. *)
    "horizontal_coor_valid" >:: (fun _ -> assert_equal true (checking_coor_placement [1;2;3;4;5]));

    (* Sweep case 28: tests that checking_coor_placement returns true when
       the values in the list are consecutive vertically. *)
    "vertical_coor_valid" >:: (fun _ -> assert_equal true (checking_coor_placement [11;21;31;41]));

    (* Sweep case 29: tests that checking_coor_placement returns false when
      the coordinates are not consecutive horizontally. *)
    "horizontal_coor_invalid" >:: (fun _ -> assert_equal false (checking_coor_placement [4;6;7]));

    (* Sweep case 30: tests that checking_coor_placement returns false when
       the coordiantes are not consecutive vertically. *)
    "vertical_coor_valid" >:: (fun _ -> assert_equal false (checking_coor_placement [11;21;41]));

    (* Sweep case 31: this test case tests for the functionality of parse_setup
       when the input str is correct. *)
    "parse_setup_destroyer" >:: (fun _ -> assert_equal destr_coor (parse_setup "a1;a2 " Destroyer));

    (* Sweep case 32: tests whether player_coor creates a list with 100 elements. *)
    "coor_length" >:: (fun _ -> assert_equal 100 (List.length player));

    (* Sweep case 33: this test case also tests whether player_coor creates
       the desired list by checking the coordinates with ship. *)
    "player_coordinate" >:: (fun _ -> assert_equal
                                {id = 0; hit = false; ship = Some Destroyer} (List.nth player 0));

    (* Sweep case 34: tests the initialization of the computer's board. THe
       lenght of the board should be 100. *)
    "com_board_length" >:: (fun _ -> assert_equal 100 (List.length com_board));

    (* Sweep case 35: tests that the intialization of the computer's board
       correctly places all the ships. There should be 17 cells with ship. *)
    "com_board_ships" >:: (fun _ -> assert_equal 17 (ship_cells com_board));

    (* Sweep case 36: tests that computer_turn returns a coordinate that
       has not yet been hit when the level is easy. *)
    "com_turn_easy" >:: (fun _ -> assert_equal false (computer_turn "easy" st).hit);

    (* Sweep case 37: tests that computer_turn returns a coordinate that
       has not yet been hit when the level is medium. *)
    "com_turn_medium" >:: (fun _ -> assert_equal false (computer_turn "medium" st).hit);


]

let suite =
  "Battleship test suite"
  >::: tests

let _ = run_test_tt_main suite
