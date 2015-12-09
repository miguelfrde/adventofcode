all: part1 part2

part1: part1.hs
	ghc -o part1 part1.hs

part2: part2.hs
	ghc -o part2 part2.hs

clean:
	rm -f part1 part2 *.o *.hi
