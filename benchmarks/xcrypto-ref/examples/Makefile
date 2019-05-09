

export CC       = $(RISCV)/bin/riscv32-unknown-elf-gcc
export AS       = $(RISCV)/bin/riscv32-unknown-elf-as
export LD       = $(RISCV)/bin/riscv32-unknown-elf-ld
export OBJDUMP  = $(RISCV)/bin/riscv32-unknown-elf-objdump
export OBJCOPY  = $(RISCV)/bin/riscv32-unknown-elf-objcopy

export INC_DIR  = -I$(XC_HOME)/examples/common \
                  -I$(LIBSCARV)/build/$(LIBSCARV_ARCH)/include \
                  -I$(LIBSCARV)/build/$(LIBSCARV_ARCH)/include/scarv

export ASFLAGS  += -march=rv32imax -mabi=ilp32 

export CFLAGS   += -mabi=ilp32 -march=rv32im -O2 -Wall $(INC_DIR) \
                  -nostartfiles -nostdlib

export LDSCRIPT ?= $(XC_HOME)/examples/common/linker.ld

export LIBSCARV_A = $(LIBSCARV)/build/$(LIBSCARV_ARCH)/lib/libscarv.a

export LDFLAGS  += -T$(LDSCRIPT)

all:
	$(MAKE) -C integration-test all
	$(MAKE) -C helloworld all
	$(MAKE) -C mpn all
	$(MAKE) -C aes-test all
	$(MAKE) -C keccakp-400 all
	$(MAKE) -C keccakp-1600 all
	$(MAKE) -C prince all
	$(MAKE) -C sha2 all

clean:
	$(MAKE) -C integration-test clean
	$(MAKE) -C helloworld clean
	$(MAKE) -C mpn clean
	$(MAKE) -C keccakp-400 clean
	$(MAKE) -C keccakp-1600 clean
	$(MAKE) -C prince clean
	$(MAKE) -C sha2 clean
