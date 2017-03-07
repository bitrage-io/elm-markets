export PATH  := $(PWD)/node_modules/.bin:$(PATH)
export SHELL := /bin/bash


build-example:
	elm-make example/Main.elm --output example/index.html

serve-example:
	cd example && browser-sync start --server -f index.html

watch-example:
	nodemon -C -e elm -w src -w example --exec "make build-example || exit 1"

example: build-example
	concurrently --kill-others "make serve-example" "make watch-example"

.PHONY: build-example serve-example watch-example example
