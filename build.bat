64tass -a ./src/game.asm -l ./target/game.lbl -L ./target/game.lst -o ./target/game

if not exist ./target/ataripac128.d64 c1541 -format "ataripac128,sh" d64 ./target/ataripac128.d64

c1541 -attach ./target/ataripac128.d64 -delete "game"

c1541 -attach ./target/ataripac128.d64 -write ./target/game game

