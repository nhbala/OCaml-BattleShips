open Graphics
open State

(* [draw_square] draws a square of the specified height and width on the
graphics module*)
let draw_square currentx currenty =
  lineto currentx (currenty + 40);
  lineto (currentx + 40) (currenty + 40);
  lineto (currentx + 40) (currenty);
  lineto (currentx) (currenty)

(* [draw_gameboard_columns] draws the columns
   of the gameboard on the graphics module
   graphics module*)
let rec draw_gameboard_columns startingx startingy =
  if startingy <= 420 then
    (draw_square startingx startingy;
     moveto startingx (startingy + 40);
     draw_gameboard_columns startingx (startingy+40))

(* [write_letter] displays the letters on the side of the board*)
let rec write_letter charcode height =
  if charcode > 64 && charcode < 75 then
    let letter = Char.chr charcode in
    moveto 18 height;
    draw_string (String.make 1 letter);
    write_letter (charcode + 1) (height - 40)


(* write_number displays the numbers on the top of the board*)
let rec write_number charcode length =
  if charcode > 48 && charcode < 58 then
    let number = Char.chr charcode in
    moveto length 470;
    draw_string (String.make 1 number);
    write_number (charcode + 1) (length + 40)

(* [draw_gameboard] draws the mepty gameboards on the graphics window*)
let draw_gameboard = fun () ->
  set_color black;
  moveto 40 60;
  draw_gameboard_columns 40 60;
  moveto 80 60;
  draw_gameboard_columns 80 60;
  moveto 120 60;
  draw_gameboard_columns 120 60;
  moveto 160 60;
  draw_gameboard_columns 160 60;
  moveto 200 60;
  draw_gameboard_columns 200 60;
  moveto 240 60;
  draw_gameboard_columns 240 60;
  moveto 280 60;
  draw_gameboard_columns 280 60;
  moveto 320 60;
  draw_gameboard_columns 320 60;
  moveto 360 60;
  draw_gameboard_columns 360 60;
  moveto 400 60;
  draw_gameboard_columns 400 60;
  moveto 448 440;
  Graphics.set_font "-*-fixed-medium-r-semicondensed--20-*-*-*-*-*-iso8859-1";
  draw_string "3110 Battleship";
  moveto 445 400;
  Graphics.set_font "-*-fixed-medium-r-semicondensed--11-*-*-*-*-*-iso8859-1";
  draw_string "Enter commands in terminal";
  Graphics.set_font "-*-fixed-medium-r-semicondensed--12-*-*-*-*-*-iso8859-1";
  moveto 450 360;
  draw_string "Created by:";
  moveto 450 340;
  draw_string "Angela Zhang";
  moveto 450 330;
  draw_string  "Nathan Bala";
  moveto 450 320;
  draw_string "Vivan Fan";
  moveto 600 60;
  draw_gameboard_columns 600 60;
  moveto 640 60;
  draw_gameboard_columns 640 60;
  moveto 680 60;
  draw_gameboard_columns 680 60;
  moveto 720 60;
  draw_gameboard_columns 720 60;
  moveto 760 60;
  draw_gameboard_columns 760 60;
  moveto 800 60;
  draw_gameboard_columns 800 60;
  moveto 840 60;
  draw_gameboard_columns 840 60;
  moveto 880 60;
  draw_gameboard_columns 880 60;
  moveto 920 60;
  draw_gameboard_columns 920 60;
  moveto 960 60;
  draw_gameboard_columns 960 60;
  moveto 1000 440;
  moveto 40 40;
  Graphics.set_font "-*-fixed-medium-r-semicondensed--15-*-*-*-*-*-iso8859-1";
  draw_string "Opponent's Board";
  moveto 600 40;
  draw_string "Your Board";
  write_letter 65 430;
  Graphics.set_font "-*-fixed-medium-r-semicondensed--15-*-*-*-*-*-iso8859-1";
  write_number 49 54;
  moveto 417 470;
  draw_string "10"


(* [input_to_coor_letter] takes in a str representing the strike coordinate
   and returns the graphics module dimension of the y axis of where it should
   hit.
*)
let input_to_coor_letter str =
  let charc = str.[0] in
  let letter = Char.lowercase_ascii charc in
  match letter with
  | 'a' -> 1050/5*2
  | 'b' -> 950/5*2
  | 'c' -> 850/5*2
  | 'd' -> 750/5*2
  | 'e' -> 650/5*2
  | 'f' -> 550/5*2
  | 'g' -> 450/5*2
  | 'h' -> 350/5*2
  | 'i' -> 250/5*2
  | 'j' -> 150/5*2
  | _ -> failwith "not a valid char"


(* [input_to_coor_number_opp] takes in a str and shows were on the opposing
   board the dimensions of the x axis where the hit should be drawn
*)
let input_to_coor_number_opp str =
  if String.length str = 3 then (1000/5*2) else
    let num = str.[1] in
    match num with
    | '1' -> 100/5*2
    | '2' -> 200/5*2
    | '3' -> 300/5*2
    | '4' -> 400/5*2
    | '5' -> 500/5*2
    | '6' -> 600/5*2
    | '7' -> 700/5*2
    | '8' -> 800/5*2
    | '9' -> 900/5*2
    | _ -> failwith "not a vaild num"


(*[input_to_coor_number_self] same as input_to_coor_number_opp except for the
  current player's board
*)
let input_to_coor_number_self str =
  if String.length str = 3 then (2450/5*2) else
    let num = str.[1] in
    match num with
    | '1' -> 1550/5*2
    | '2' -> 1650/5*2
    | '3' -> 1750/5*2
    | '4' -> 1850/5*2
    | '5' -> 1950/5*2
    | '6' -> 2050/5*2
    | '7' -> 2150/5*2
    | '8' -> 2250/5*2
    | '9' -> 2350/5*2
    | _ -> failwith "not a vaild num"


(*[input_to_coor_number_hit] takes in a str representing where the target
  coordinate is and returns a x dimension showing where it is on the board*)
let input_to_coor_number_hit str =
  if String.length str = 3 then (2400/5*2) else
    let num = str.[1] in
    match num with
    | '1' -> 1500/5*2
    | '2' -> 1600/5*2
    | '3' -> 1700/5*2
    | '4' -> 1800/5*2
    | '5' -> 1900/5*2
    | '6' -> 2000/5*2
    | '7' -> 2100/5*2
    | '8' -> 2200/5*2
    | '9' -> 2300/5*2
    | _ -> failwith "not a vaild num"


(*[input_to_coor_draw] takes in a str representing which coordinate a boat is on
  and returns the y dimension to show where it should be drawn*)
let input_to_coor_draw str =
  let charc = str.[0] in
  let letter = Char.lowercase_ascii charc in
  match letter with
  | 'a' -> 1100/5*2
  | 'b' -> 1000/5*2
  | 'c' -> 900/5*2
  | 'd' -> 800/5*2
  | 'e' -> 700/5*2
  | 'f' -> 600/5*2
  | 'g' -> 500/5*2
  | 'h' -> 400/5*2
  | 'i' -> 300/5*2
  | 'j' -> 200/5*2
  | _ -> failwith "not a valid char"


(* [drawstrike] takes in a ship and coordinate and draws a box indicating a
   strike on the board, returns a red or black box depending if it was
   a successful hit or not
*)
let drawstrike ship coor =
  match ship with
  | None -> set_color black;   let y = input_to_coor_letter coor in
    let x = input_to_coor_number_opp coor in
    fill_rect x y 40 40
  | Some _ -> set_color red; let y = input_to_coor_letter coor in
    let x = input_to_coor_number_opp coor in
    fill_rect x y 40 40

(*[drawship] takes in two coordinates and draws a line representing
the ship on the graphics module
*)
let drawship startcoor endcoor =
  set_color black;
  let x0 = input_to_coor_number_self startcoor in
  let y0 = input_to_coor_draw startcoor in
  let x1 = input_to_coor_number_self endcoor in
  let y1 = input_to_coor_draw endcoor in
  moveto x0 y0;
  lineto x1 y1


(* [drawstrike_self] same as drawstrike but for the user board*)
let drawstrike_self ship coor =
  match ship with
  | None -> set_color black; let y = input_to_coor_letter coor in
    let x = input_to_coor_number_hit coor in
    fill_rect x y 40 40
  | Some _ -> set_color red; let y = input_to_coor_letter coor in
    let x = input_to_coor_number_hit coor in
    fill_rect x y 40 40


(*[convert_int_to_str] takes in a num representing a coordinate and returns
  the letter number string equivalent
*)
let convert_int_to_str num =
  if num >= 0 && num < 10 then
    let number = 1 + num in  "A" ^ string_of_int number else
    let value = num / 10 in
    match value with
    | 1 -> let added = (num mod 10) + 1 in "B" ^ string_of_int added
    | 2 -> let added = (num mod 10) + 1 in "C" ^ string_of_int added
    | 3 -> let added = (num mod 10) + 1 in "D" ^ string_of_int added
    | 4 -> let added = (num mod 10) + 1 in "E" ^ string_of_int added
    | 5 -> let added = (num mod 10) + 1 in "F" ^ string_of_int added
    | 6 -> let added = (num mod 10) + 1 in "G" ^ string_of_int added
    | 7 -> let added = (num mod 10) + 1 in "H" ^ string_of_int added
    | 8 -> let added = (num mod 10) + 1 in "I" ^ string_of_int added
    | 9 -> let added = (num mod 10) + 1 in "J" ^ string_of_int added
    | _ -> failwith "not vaild"


(*[drawallships] takes in a list of ship coordinates and draws all the ships
  on the graphics window
*)
let rec drawallships shipcoorlst =
  let sortedlst = sort_lst_ship_coordinate (shipcoorlst) in
  match sortedlst with
  | [] -> ()
  | h::t -> let front = List.nth (snd h) 0 in
    let back = List.nth (snd h) ((List.length (snd h)) -1) in
    let frontstr = convert_int_to_str front.id in
    let backstr = convert_int_to_str back.id in
    drawship frontstr backstr;
    drawallships t


(*[drawallhitsself] takes in a coordinate list and draws on the graphic window
  all the hits that have been mad by the opposing player
*)
let rec drawallhitsself coorlst =
  match coorlst with
  | [] -> ()
  | h::t -> if h.hit = true then drawstrike_self (h.ship) (convert_int_to_str h.id); drawallhitsself t

(*[drawallhitsopp] same as drawallhitsself except for the Opponent*)
let rec drawallhitsopp coorlst =
  match coorlst with
  | [] -> ()
  | h::t -> if h.hit = true then drawstrike (h.ship) (convert_int_to_str h.id);
    drawallhitsopp t

(* [first] returns the first element in a tuple. *)
let first (x,_,_) = x


(* [sec] returns the second element in a tuple. *)
let sec (_,y,_) = y

(* [draw_p1_window] takes in a state and draws the player one window with
all the information stored in state*)
let draw_p1_window state =
  clear_graph();
  draw_gameboard();
  let info = first state in
  let enminfo = sec state in
  let playerships = player_ships state in
  drawallships playerships;
  let coorlst = info.coordinates in
  drawallhitsself coorlst;
  let enmcoorlst = enminfo.coordinates in
  drawallhitsopp enmcoorlst


(* [draw_p2_window] same as draw_p1_window but for player 2*)
let draw_p2_window state =
  clear_graph();
  draw_gameboard();
  let info = sec state in
  let enminfo = first state in
  let playerships = opp_ships state in
  drawallships playerships;
  let coorlst = info.coordinates in
  drawallhitsself coorlst;
  let enmcoorlst = enminfo.coordinates in
  drawallhitsopp enmcoorlst
