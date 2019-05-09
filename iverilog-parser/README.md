# Verilog -> Iodine IR

Transforms Verilog into Iodine intermediate representation. The implementation
is based on [Icarus Verilog](https://github.com/steveicarus/iverilog).

## Dependencies

  * g++
  * make
  * flex
  * bison
  * zsh

Tested on Ubuntu 18.10, *might work* on a Mac.

## Instructions

```sh
$> make
$> ./ivl.sh [args to ivl ... ] <verilog file>
```
