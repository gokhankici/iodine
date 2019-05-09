# The XCrypto reference hardware implementation

*This project contains an area-optimised implementation of the
[XCrypto ISE](https://github.com/scarv/xcrypto),
a general-purpose cryptographic instruction set
extension for the [RISC-V](https://riscv.org/)
architecture. The design can be attatched as a co-processor
to existing RISC-V CPU designs.*

**Contents**

- [Getting Started](#Getting-Started)
  - [Project Organisation](#Project-Organisation)
  - [Setting Up binutils](#Setting-Up-binutils)
  - [Running Simulations](#Running-Simulations)
- [Formal testbench](#Formal-testbench)
- [Implementation Statistics](#Implementation-Statistics)
- [Benchmark Results](#Benchmark-Results)


## Getting Started

**The best place to start** getting a feel for what XCrypto does is to look
at the specification documents in the 
[xcrypto-spec](https://github.com/scarv/xcrypto-spec) repository.
These explain both the ISE itself, the instructions and how to program it.
They also describe the reference implementation we have constructed for
evaluating the ISE with existing RISC-V cores.

**If you want to start playing with it** then you can setup the project
workspace as follows:

```sh
$> export RISCV=/path/to/riscv-tools/installation
$> export VERILATOR_ROOT=/path/to/verilator/build
$> git clone git@github.com:scarv/xcrypto.git
$> cd xcrypto
$> git submodule update --init --recursive
$> source ./bin/source.me.sh
```

Note that one must run the `git submodule update` command in order to
pull the external repositories (like PicoRV32) required for the integration
testbench.

Build the documentation by running `make docs`

- Documentation on the *reference implementation* of the ISE willl be found
  in `${XC_HOME}/docs/implementation.pdf`.
- Pre-built documentation for the reference implementation can
  be found on the [releases](https://github.com/scarv/xcrypto/releases) page.

Depending on how Yosys is installed, one should also set the `YS_INSTALL`
environment variable such that `${YS_INSTALL}/yosys` is a valid path to the
`yosys` executable.

### Project Organisation

```
├── bin                     - Tool/environment setup scripts
├── docs                    - Project documentation
│   ├── diagrams
│   └── implementation      - Reference implementation document
├── examples
│   ├── common              - Shared files between examples
│   ├── ...                 - Multiple cryptographic algorithm examples
│   └── integration-test    - "Hello World" example integration program
├── external                - External repositories
│   ├── libscarv            - Reference software implementation examples
│   ├── picorv32            - Reference to the picorv32 core repo
│   └── riscv-opcodes       - Reference to the offical riscv-opcodes repo
├── flow                    - Hardware simulation/implementation flow
│   ├── gtkwave             - Wave views
│   ├── benchmarks          - Performance benchmarking flow
│   ├── verilator           - Subsystem simulator flow
│   ├── icarus              - Simulation flow
│   └── yosys               - Formal SMT2 generation and synthesis
├── rtl
│   ├── coprocessor         - XCrypto ISE example implementation
│   └── integration         - XCrypto/PicoRV32 integration example
├── verif
│   ├── formal              - Formal verification checks
│   ├── tb                  - Simulation/integration/formal testbenches
│   └── unit                - Simulation sanity tests
└── work                    - Working directory for generated artifacts
```

### Setting Up A Toolchain

You will need our customised version of the RISC-V toolchain in order
to build programs using XCrypto.
Currently, we support only writing in assembly code.

The toolchain itself can be obtained from
[scarv/riscv-tools](https://github.com/scarv/riscv-tools),
and the installation guide there will be enough to get you up and
running.

It is essential that your `$RISCV` environment variable is set to
point at the XCrypto augmented toolchain installation linked too
above. Not at an existing, standard RISC-V toolchain you might
already have installed.

### Running Simulations

There are two simulation testbenches for the design. The integration
testbench acts as a general ISA simulator, which allows one to write
and run C code on a PicoRV32 CPU attatched to the reference XCrypto
implementation. The second is only for testing during design and
implementaton of the RTL, and will be of little interest to those wanting
to write code for the ISE.

**Integration Tests**

These tests work as part of an example integration of the COP with the
[picorv32](https://github.com/cliffordwolf/picorv32) core.
The integrated design subsystem is found in `rtl/integration`.
It can be used to write experimental / development code without having to
have an actual hardware platform on which to implement the reference
design.

Information on how to build and use the simulation binary is found
in [$XC_HOME/flow/verilator/README.md](./flow/verilator/README.md).

Example code to run in the integration testbench is found in 
`examples/integration-test`

**Benchmarking**

The integration testbench described in 
[$XC_HOME/flow/verilator/README.md](./flow/verilator/README.md)
is also used to run the benchmarking programs.
More information on the benchmarking flow can be found in
[$XC_HOME/flow/benchmarks/README.md](./flow/benchmarks/README.md).


**Unit Tests**

- These use icarus verilog, and the modified binutils to run the tests
  found in `verif/unit/`.
- These are *not* correctness/compliance tests. They are used as simple 
  sanity checks during the design cycle.
- Checking for correctness should be done using the formal flow.

Building the testbench:

```sh
$> make unit_tests      # Build the unit tests
$> make icarus_build    # Build the unit test simulation testbench
```

Running the tests:

```sh
$> make icarus_run SIM_UNIT_TEST=<file> RTL_TIMEOUT=1000
$> make icarus_run_all  # Run all unit tests as a regression
```

The `<file>` path should point at a unit-test hex file, present in
`${XC_HOME}/work/unit/*.hex`. Using `${XC_HOME}` as part of an absolute path
to the hex file is advised.

## Formal testbench

Formal checks are run using Yosys and Boolector. Checks are listed in
`verif/formal/*`. A more complete description of the formal flow can
be found toward the end of the implementation document.

To run a single check, use:

```sh
$> make yosys_formal FML_CHECK_NAME=<name>
```

where `<name>` corresponds too some file called 
`verif/formal/fml_check_<name>.v`.

To run a regression of formal checks:

```sh
$> make -j 4 yosys_formal
```

will run 4 proofs in parallel. Change this number to match the available
compute resources you have. One can also run a specfic subset of the
available formal checks by delimiting the check names with spaces, and using
a backslash to escape the spaces: `FML_CHECK_NAME=check1\ check2\ check3`.

## Implementation Statistics

This section lists implementation statistics for the reference implementation
on the 
[Sakura-X](http://satoh.cs.uec.ac.jp/SAKURA/hardware/SAKURA-X.html)
(also known as the Sasebo GIII) Side-channel evaluation platform using a 
Xilinx `xc7k160tfbg676` FPGA.
Xilinx Vivado 2018.1 was used to produce the following results.

A complete system, integrated with an AXI interconnect, on-chip
BRAMS, GPIO and UART peripherals can be implemented at 100MHz on said
FPGA.

The critical path for the system lies in the multi-precision arithmetic
multiplier.
This and other paths with little timing slack can be trivially pipelined
in more high performance implementations of XCrypto.

Our reference implementation is integrated with a PicoRV32 CPU to provide
a complete subsystem.
We break down the resource usage between the XCrypto implementation and the
PicoRV32 for comparison.

Component             | LUTS  | FFs  | BRAMS | DSP Slices
----------------------|-------|------|-------|----------------------
PicoRV32 + XCrypto    | 8710  | 1621 |   0   | 8
PicoRV32              | 5603  | 893  |   0   | 4
XCrypto               | 3101  | 726  |   0   | 4
XCrypto Register File | 1504  | 512  |   0   | 0

Clearly, the size of the XCrypto reference implementation is dominated by the
16x32 register file.
Overall the XCrypto reference implementation is slightly smaller than the
PicoRV32, though this must be weighed against the PicoRV32 needing to
implement instruction fetch logic and other functions which the XCrypto block
need not.

## Benchmark Results

**Note:** These numbers change frequently and are not always up to
date. Running the benchmarks yourself is the best way to check.

Here we report several software benchmark results. These are Cryptographic
primitives or algorithms which have been accelerated using the XCrypto
ISE.

We use the Verilator simulator to obtain our benchmark results, using the
PicoRV32+XCrypto system described above.
The Verilator simulation environment is a cycle accurate model of the
XCrypto reference implementation integrated with the PicoRV32 CPU.
We simulate a single cycle memory access latency for data and instruction
accesses.

Note that the PicoRV32 uses the same memory interface for instructions and
data, while the XCrypto reference implementation adds a dedicted memory
interface. This means that memory accesses using only XCrypto instructions
are faster than the same operations running the basic RISC-V load/store
instructions. While this skews the benchmarks somewhat in favour of
XCrypto, the results here are still representative, especially for compute
bound workloads.

We compare C code compiled with `-O2` and/or `-O3` optimisation flags,
hand-coded RISC-V assembly, and hand-coded RISC-V+XCrypto assembly code.

For fairness, when hand-writing assembly code, we mimic the loop
unrolling policy of the compiler. This means we may only write un-rolled
assembly if that is what the compiler generates for the corresponding
C code.

### Keccak (SHA3)

We include C code compiled with both `-O2` and `-O3`. The principle
difference being that at `-O3`, GCC will un-roll the entire Keccak
round function, hence the dramatically larger code sizes.

KeccakP-400   | Cycles    | Instructions Executed | Code Size
--------------|-----------|-----------------------|--------------
C -O2         | 9280      |  1359                 | 488b
C -O3         | 3423      |  626                  | 2712b
RISC-V        | 7289      |  1238                 | 
XCrypto       | 3609      |  807                  | 292b

KeccakP-1600  | Cycles    | Instructions Executed | Code Size
--------------|-----------|-----------------------|--------------
C -O2         | 13969     | 2283                  | 768b
C -O3         | 6152      | 1066                  | 4240b
RISC-V        | 12717     | 2027                  | 
XCrypto       | 5558      | 1222                  | 496b

### SHA256

Here we report cycle counts and instructions executed for a SHA256
hash computation of 32-words of data. Only the core hash computation
function is modified. Padding and memory management is included in
the measurements but not accelerated.
Code size is given for the core `SHA256` hash computation function only,
not auxiliary functions.

SHA256        | Cycles    | Instructions Executed | Code Size
--------------|-----------|-----------------------|--------------
C -O2         | 22106     |  4773                 | 2568b
C -O3         | 20906     |  4587                 | 3732b
XCrypto       | 14239     |  3112                 | 1156b

### AES Block Cipher

Several implementation styles of AES are evaluated, along side standard C
code, RISC-V assembly, RISC-V + XCrypto assembly and RISC-V + XCrypto
assembly using dedicated lightweight AES accelerator instructions.
These are one of two classes of algorithm specific instructions in XCrypto,
the other being used for Keccak.

#### Encryption

AES-128                          | Cycles | Instructions Executed | Code Size
---------------------------------|--------|-----------------------|--------------
Split Unpacked C -O2             | 17965  | 3094                  |     
Split Packed C -O2               | 10993  | 1999                  |     
Split Packed Assembly            | 8724   | 1800                  | 
Split Packed Assembly + XCrypto  | 3210   | 620                   | 
Split Unpacked ASM + XCrypto AES | 2278   | 409                   | 
Packed   Assembly                | 7969   | 1676                  | 
Packed   Assembly + XCrypto      | 2014   | 407                   | 
Unpacked Assembly + XCrypto AES  | 1171   | 216                   | 

#### Decryption

AES-128                          | Cycles | Instructions Executed | Code Size
---------------------------------|--------|-----------------------|--------------
Split Unpacked C -O2             | 24715  | 4498                  |     
Split Packed C -O2               | 12361  | 2341                  |     
Split Packed Assembly            | 10524  | 2250                  | 
Split Packed Assembly + XCrypto  | 3822   | 746                   | 
Split Unpacked ASM + XCrypto AES | 2278   | 409                   | 
Packed   Assembly                | 9771   | 2127                  | 
Packed   Assembly + XCrypto      | 2633   | 534                   | 
Unpacked Assembly + XCrypto AES  | 1175   | 217                   | 

### Prince Block Cipher

Prince        | Cycles    | Instructions Executed | Code Size
--------------|-----------|-----------------------|--------------
RISC-V        | 45353     | 8545                  |
XCrypto       | 39269     | 9865                  |     

Note that because Prince is an involution function, there is no difference
between the core encryption and decryption functions.

### Multi-precision Integer Arithmetic

Results are for 256 bit unsigned integers.

Add/Sub       | Cycles    | Instructions Executed | Code Size
--------------|-----------|-----------------------|--------------
RISC-V        | 619       | 124                   |
XCrypto       | 572       | 124                   | 

For multiply, both XCrypto and the PicoRV32 use single-cycle
hardware multipliers.

Multiply      | Cycles    | Instructions Executed | Code Size
--------------|-----------|-----------------------|--------------
RISC-V        | 6600      | 1322                  |
XCrypto       | 4328      | 890                   | 
