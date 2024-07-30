BUILD=build
TARGET=${BUILD}/extension.js
SRC=src

all: ${TARGET} caml-build

node_modules: package.json
	npm install

${TARGET}: ${SRC}/extension.ts
	npm run compile

.PHONY: caml-build
caml-build: FORCE
	cd server; dune build

FORCE: ;

.PHONY: clean
clean:
	rm ${TARGET}
	cd server; dune clean
