build:
	stack build

repl:
	stack repl

test-repl:
	stack repl ballast:tests

watch:
	stack build --file-watch --fast

test:
	stack test
