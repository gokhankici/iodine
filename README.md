# verylog

## Instructions

First copy the `configuration-skeleton.sh` file into `configuration.sh`, and
fill the missing parts.

## Command Line Options

``` sh
verylog [options] <verilog file>

verylog options:
  --just-ir
      Just generate the IR file and print it
  --just-vcgen
      Just generate the file that will be fed to qarmc and print it
  --no-interpreter
      Build the sicstus program rather than interpreting it
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
