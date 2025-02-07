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

#### VSCode Marketplace

The simplest way to install this extension is through the [VSCode
marketplace](https://marketplace.visualstudio.com/items?itemName=catalalang.catala)
which is directly accessible in VScode. However, the extension still
requires the Catala LSP server which, for the time being, need to be
manually installed through OPAM using this command:

```bash
opam pin catala-lsp git+https://github.com/CatalaLang/catala-language-server.git
```

-- and, optionally, you may also install the code formatter ([see
below](#code-formatting)).

#### Build from sources

It is also possible to build the extension from the sources through
these commands:

```bash
# 1. Clone this repository
git clone https://github.com/CatalaLang/catala-language-server/
# 2. Enter the freshly created directory
cd catala-language-server/
# 3. Install the JS dependencies
npm install
# 4. Install the OCaml dependencies
opam install . --deps-only
# 5. Build the extension
npm run compile
```

From this point on, you may test the extension by skipping to the next
section. Otherwise, you can package and install the extension in
VSCode by following these extra steps:

```bash
# 1. Install VSCode extension CLI tool: e.g.,
sudo npm install -g @vscode/vsce
# 2. Package the extension
vsce package
# 3. Install the generated .vsix extension through the VSCode GUI or
#    by invoking:
code --install-extension catala-0.20.0.vsix
```

#### Testing

If you wish to test the extension without installing it, you may:

1. Open VSCode at the repository's root (e.g., in a terminal `$ code
.`)
2. Go to "Run and Debug" in the VSCode sidebar (invoked by command "View: Show Run and Debug")
3. Choose "Launch Client" and click on the play button
4. A new VSCode window should open with the extension running
   - you can press CTRL+R to reload the extension window to apply changes

### Code formatting

[catala-format](https://github.com/CatalaLang/catala-format) is an
automated code formatting tool based on
[topiary](https://github.com/tweag/topiary/). To install
`catala-format`, use this command:

```
$ opam install catala-format
```

N.b. the installation may be lengthy as it needs to install `topiary`.

Once this is done, you may start (or reload) the extension and use the
format document command: `Ctrl-p` and type 'Format Document'. You may
also bind it to a handy keyboard shortcut. Beware: if the catala
program you are trying to format cannot be properly parsed, the
formatter call will fail.

`catala-format` can also be used as a standalone tool or plugged in
your favorite IDE. Type `catala-format --help` in a terminal for more
details.

### Using the Custom Test Case Editor

The custom (visual) test case editor usage is controlled by a configuration setting. To enable or disable the custom editor:

1. Open VSCode Settings (File > Preferences > Settings)
2. Search for "Catala"
3. Find the "Catala: Enable Custom Test Case Editor" setting
4. Check or uncheck the box to enable or disable the custom test case editor
5. Reload the VSCode window for the changes to take effect (you can do this by closing and reopening VSCode, or by running the "Developer: Reload Window" command from the Command Palette)

When the custom editor is disabled, test files will open in the default text editor.

Note: After changing the setting, you must reload the VSCode window for the changes to take effect. This is because the custom editor registration happens when the extension is activated, and changing the setting doesn't automatically re-register or unregister the editor.

### Developing

When developing on the "front" (typescript side) of the extension, you may invoke
`npm run watch` to continuously build the extension.

# License

All the code contained in this repository is released under the Apache
license (version 2) unless another license is explicited for a
sub-directory.
