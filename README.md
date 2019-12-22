# Iodine

## Installation

### Dependencies

- [stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)
- [z3 v4.8.1](https://github.com/Z3Prover/z3/releases/tag/z3-4.8.1)

### Building

```sh
git clone --recursive https://github.com/gokhankici/iodine.git
cd iodine
make -C iverilog-parser
stack build
```

## Usage

When installed, just run `./iodine`. For the test suite, run `./t`.

### Command Line Options

```
iodine v2.0, (C) Rami Gokhan Kici 2019

iodine [OPTIONS] ITEM ITEM

Common flags:
     --iverilog-dir=DIR  path of the iverilog-parser directory
     --print-ir          just run the verilog parser
     --vcgen             just generate the .fq file
     --no-save           do not save the fq file
     --no-fp-output      disable the output from fixpoint
     --trace             disable the debug trace
     --abduction         run abduction algorithm
     --verbose           enable verbose output
  -h --help              Display help message
  -V --version           Print version information
     --numeric-version   Print just the version number

Verifies whether the given Verilog file runs in constant time.

First argument is the path the to the verilog file.
Second argument is a JSON file that contains the annotations.
```

### Example

```sh
./iodine -- examples/verilog/stall.v examples/verilog/annot-stall.json
```
