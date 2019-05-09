# Iodine

## Instructions

To build, you need to install
[stack](https://docs.haskellstack.org/en/stable/README/#how-to-install). When
installed, just run `./iodine`. For the test suite, run `./t`.

## Command Line Options

```
iodine v1.0, (C) Rami Gokhan Kici 2019

iodine [OPTIONS] FILE MODULE

Common flags:
     --iverilog-dir=DIR        path of the iverilog-parser directory
     --ir                      just generate the IR file
  -v --vcgen                   just generate the .fq file
  -m --minimize                run delta-debugging of fixpoint
     --no-save --nosave        do not save the fq file
  -a --abduction               run abduction algorithm
  -t --time                    print the runtime
     --no-output --nofpoutput  disable the output from fixpoint
  -h --help                    Display help message
  -V --version                 Print version information
     --numeric-version         Print just the version number

Checks whether the given Verilog file runs in constant time.

First argument is the path the to the verilog file.
Second argument is the name of the root Verilog module in that file.
```

### Example

```sh
./iodine -- examples/verilog/stall.v stalling_cpu
```
