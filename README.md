# DATX02-DIT561

[![Haskell CI](https://github.com/PUGzera/DATX02-DIT561/actions/workflows/haskell.yaml/badge.svg?branch=develop)](https://github.com/PUGzera/DATX02-DIT561/actions/workflows/haskell.yaml)

## Daison Frontend

This project is the result of a Bachelor's thesis in Computer science and engineering (DATX02 / DIT561), given at Chalmers University of Technology and University of Gothenborg.

The purpose of the project is to study how the compiler GHC can be used as a library to build a front-end for [Daison](https://github.com/krangelov/daison). The project focuses in particular on how GHC as a library can be used to build interactive Haskell environments.

## Project structure

- [`Setup.hs`](Setup.hs) - Default setup for building executables.
- [`src/`](src) - Haskell sourcecode
  - [`Main.hs`](src/Main.hs) - Start the program
  - [`Frontend/`](src/Frontend) - Contains the Haskell sourcecode.
    - [`Base.hs`](src/Frontend/Base.hs) - Session/State handling
    - [`Context.hs`](src/Frontend/Context.hs) - Extensions/Flag handling
    - [`Eval.hs`](src/Frontend/Eval.hs) - Evaluate expressions sent through the CLI
    - [`Format.hs`](src/Frontend/Format.hs) - Format output from Daison queries
    - [`GHCInterface.hs`](src/Frontend/GHCInterface.hs) - No own implementations here, purely for imports.
    - [`Run.hs`](src/Frontend/Run.hs) - Loop of the program and definitions of commands that can be run
    - [`Typecheck.hs`](src/Frontend/Typecheck.hs) - Typecheck user input to decide how to handle it.

- [`test/`](test) - Contains automated tests for the program
  - [`Tests.hs`](test/Tests.hs) - Functional tests run with [QuickCheck](https://hackage.haskell.org/package/QuickCheck)
  - [`UnitTests.hs`](test/UnitTests.hs) - Unit tests run with [HUnit](https://hackage.haskell.org/package/HUnit)

## Running the application
We used Cabal to automate dependency installations, building as well as testing the project. Run the following commands from the source folder:

### Run:
`cabal run daison-frontend`

### Test:
`cabal test`

### Build executable
`cabal build executable:daison-frontend`

## Example usage
*Todo*
### Available commands
| Command            | Description                                                            |
| ------------------ | ---------------------------------------------------------------------- |
| `<statement>`      | evaluate/run `<statement>`                                             |
| :dbs               | print the list of databases that are currently open                    |
| :?, :help          | display this list of commands                                          |
| :log path          | display the log file's path                                            |
| :log show          | display the log file's contents                                        |
| :log toggle        | enable/disable logging                                                 |
| :log wipe          | attempt to wipe the log file's contents                                |
| :type `<expr>`     | show the type of `<expr>`                                              |
| :q, :quit          | quit the program                                                       |
| :close `<name>`    | close database with `<name>` if opened                                 |
| :open `<name>`     | open database with `<name>` or set focus to `<name>` if already opened |
| :cd `<dir>`        | set the current directory to `<dir>` (relative to current location)    |
| :module `<module>` | import `<module>` if it exists and is in scope                         |
| :load `<filepath>` | load a haskell file from `<filepath>`                                  |
| :set `<option>`    | set `<option>`                                                         |
## Authors

| Name                      | GitHub-handle                                     |
| ------------------------- | ------------------------------------------------- |
| Christoffer Kaltenbrunner | [kaltenbrunner](https://github.com/kaltenbrunner) |
| Alexander Neldefors       | [PUGzera](https://github.com/PUGzera)             |
| Hugo Stegrell             | [steget](https://github.com/steget)               |
| Philip Vedin              | [pvedin](https://github.com/pvedin)               |
