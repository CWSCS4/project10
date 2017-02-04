# project10
Parser for Jack

## Instructions for running
1. Compile `Main.hs` with the flag `-XFlexibleInstances`. (e.g. `stack ghc -- -XFlexibleInstances Main.hs -o Main`)
2. Run the executable, passing it the Jack file to be parsed. (e.g. `./Main ArrayTest/Main.jack`)
3. It will create an XML file containing the result of the parse. (e.g. `ArrayTest/Main.cs.xml`)

Alternatively, you can just run `./test.sh` and it will compile the program and run it on all the Jack files.