all: extension.js caml-build

node_modules: package.json
	npm install

extension.js: extension.ts
	npm run compile

.PHONY: caml-build
caml-build: FORCE
	cd server; dune build

FORCE: ;

.PHONY:
clean:
	rm extension.js
	cd server; dune clean
