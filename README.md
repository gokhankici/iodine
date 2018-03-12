# verylog

## Instructions

First copy the `configuration-skeleton.sh` file into `configuration.sh`, and
fill the missing parts.

To build, you need to install
[stack](https://docs.haskellstack.org/en/stable/README/#how-to-install). When
installed, just run `stack install`.

## Command Line Options

``` sh
verylog [options] <verilog file>

verylog options:
  --just-ir
      Just generate the IR file and print it
  --just-vcgen
      Just generate the file that will be fed to qarmc and print it
  --emacs
      Opens the output (IR or VC) in emacs
  -h, --help
      Prints this help

parser options:
  -M <toplevel module name>
      When the verilog file contains multiple modules, this is used to denote the toplevel one
```

### Example

```sh
./verylog -M stalling_cpu examples/verilog/stall.v
```
