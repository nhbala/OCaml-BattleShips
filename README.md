# Battleship
Nathan Bala <br />
Angela Zhang <br />
Vivian Fan

# What is it?
It's the classic game of Battleship, but without the fuss of losing all the little pieces! Battle it out with another friend or challenge the computer. You can even choose the level of difficulty if you are playing by yourself. The first player to sink all of the other player's ships win!

# System Requirements
Our game requires the use of Ocaml's Graphics module. <br />
On MacOS, download xQuartz from https://www.xquartz.org first, and then reinstall Ocaml with ```opam switch reinstall 4.06.0```

# How to play?
Call any of these commands to begin: <br />
```make``` or ```make test``` to run the test suite <br />
```make clean``` to clean the build folder and delete the byte files <br />
```make play``` to begin the game <br />

A GUI will pop up once ```make play``` is entered. If the terminal returns an error such as this:  ```Sys_error(".../Battleship/main.byte: No such file or directory")```, run ```rm *byte``` and try entering ```make play``` again. The error should disappear and the GUI should appear. Follow the instructions in the REPL to proceed with the game. Have fun!
