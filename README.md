# Catala VSCode extension and LSP server

## Disclaimer

Be advised that this is a work in progress repository and is not yet
considered as a fully functional VSCode extension.

## Features

Client side:

- Syntax highlighting

LSP-server:

- Highlighting of miscellaneous type of errors
- Auto-completion

## Getting started

### Prerequisites

In order to build and use this extension, you will need to
install:

- [OPAM](https://github.com/ocaml/opam), the OCaml package manager
- [npm](https://www.npmjs.com/), the JavaScript package manager

### Installation

```bash
# 1. Clone this repository
git clone https://github.com/CatalaLang/catala-language-server/
# 2. Go to the directory
cd catala-language-server/
# 3. Install the JS dependencies
npm install
# 4. Install the OCaml dependencies
cd server ; opam install . --deps-only ; cd ..
# 5. Build the extension
npm run compile
```

### Testing

1. Open VSCode at the repository's root (e.g., in a terminal `$ code .`)
2. Go to "Run and Debug" in the VSCode sidebar (invoked by command "View: Show Run and Debug")
3. Choose "Launch Client" and click on the play button
4. A new VSCode window should open with the extension running
   - you can press CTRL+R to reload the extension window to apply changes

### Developing

When developing on the "front" (typescript side) of the extension, you may invoke
`npm run watch` to continuously build the extension.

# License

All the code contained in this repository is released under the Apache
license (version 2) unless another license is explicited for a
sub-directory.
