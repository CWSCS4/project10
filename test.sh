#!/bin/bash

ghc Main.hs -Wall -o Main

for jackFile in */*.jack; do
	./Main $jackFile > $jackFile.cs.xml
done