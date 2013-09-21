CC=ghc 
FLAGS=-Wall

all:	build

build:	runserver.hs
	$(CC) $(FLAGS) -o limonad runserver.hs

run:	build
	./limonad
	clean

clean:
	rm *.hi *.o
