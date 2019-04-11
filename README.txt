# System Requirements
Our game requires the use of Ocaml's Graphics module.
On MacOS, download xQuartz from https://www.xquartz.org first,
and then reinstall Ocaml with "opam switch reinstall 4.06.0"

# How to play?
Enter this command to begin playing: "make play"

A GUI will pop up once "make play" is entered.
If the terminal returns an error such as this: Sys_error(".../Battleship/main.byte: No such file or directory"),
run "rm *byte" and try entering "make play" again.
The error should disappear and the GUI should appear.
Follow the instructions in the REPL to proceed with the game.

Other commands that you can call:
"make" or "make test" to run the test suite
"make clean" to clean the build folder and delete the byte files
