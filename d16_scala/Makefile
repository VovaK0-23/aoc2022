.PHONY: test clean

test: build/Main.class input.txt
	scala -cp build Main input.txt

build/Main.class: build Main.scala
	scalac -d build Main.scala

build:
	mkdir build

clean: build
	rm -r build
