.PHONY: all test

all: run

run:
	lein run

clean:
	rm log-players.txt
	rm log-state.txt
	rm test-jumps.txt
