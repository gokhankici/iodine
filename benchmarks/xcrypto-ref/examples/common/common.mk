
TGT_DIR  = $(XC_WORK)/examples

EXE      ?= $(TGT_DIR)/$(TEST_NAME).elf

HEADERS  = ../common/common.h ../common/benchmark.h

all: $(EXE) $(EXE:%.elf=%.dis) $(EXE:%.elf=%.hex) $(EXE:%.elf=%.srec)

$(TGT_DIR)/%.o : %.x.S
	-mkdir -p $(TGT_DIR)
	$(AS) $(ASFLAGS) $(INC_DIRS) -c -o $@ $^

$(TGT_DIR)/%.o : ../common/%.S
	-mkdir -p $(TGT_DIR)
	$(AS) $(ASFLAGS) $(INC_DIRS) -c -o $@ $^

$(TGT_DIR)/%.o: ../common/%.c
	-mkdir -p $(TGT_DIR)
	$(CC) $(CFLAGS) -c -o $@ $^ $(LDFLAGS)

$(TGT_DIR)/%.dis : $(TGT_DIR)/%.elf
	-mkdir -p $(TGT_DIR)
	$(OBJDUMP) -j.text -j.data -dt $< > $@

$(TGT_DIR)/%.srec : $(TGT_DIR)/%.elf
	-mkdir -p $(TGT_DIR)
	$(OBJCOPY) -O srec --srec-forceS3 $< $@

$(TGT_DIR)/$(TEST_NAME).o: $(TEST_NAME).c $(HEADERS)
	-mkdir -p $(TGT_DIR)
	$(CC) $(CFLAGS) -c -o $@ $< $(LDFLAGS)

$(TGT_DIR)/$(TEST_NAME).elf : $(OBJECTS)
	$(CC)  $(LDFLAGS) $(CFLAGS) -o $@ $^

$(TGT_DIR)/%.hex : $(TGT_DIR)/%.elf
	$(OBJDUMP) -D -j.text $< | grep -P ":\t" > $@
	sed -i 's/     .*$///' $@
	sed -i 's/^.*:\t//' $@

.PHONY: clean
clean:
	rm -f $(OUT_OBJ) $(OUT_DIS)
