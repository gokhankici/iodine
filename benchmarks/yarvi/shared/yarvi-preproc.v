// -----------------------------------------------------------------------
//
//   Copyright 2016 Tommy Thorn - All Rights Reserved
//
//   This program is free software; you can redistribute it and/or modify
//   it under the terms of the GNU General Public License as published by
//   the Free Software Foundation, Inc., 53 Temple Place Ste 330,
//   Bostom MA 02111-1307, USA; either version 2 of the License, or
//   (at your option) any later version; incorporated herein by reference.
//
// -----------------------------------------------------------------------

/*************************************************************************

This is an attempt at making a nearly as simple as possible implementation
of RISC-V RV32I.  The present subset is a simple one-hot state machine.
The desire to use block ram for memories *and* register file dictates the
need for at least three stages:

                        Unpipelined State Machine
                              IF -> DE -> EX
                               ^----------/

This guarantees a CPI of 3 as no hazards can occur (we don't currently
deal with memory waits).  Since branches only depend on registers, we can
trivially lower this to 2 cycles by issuing branches from DE and thus
overlap EX with IF for the next instruction (a fairly standard trick):

                    Partically Pipelined State Machine

                              IF -> DE -> EX
                               ^----/

There are two obvious directions from here:

1) Have two hardware threads interleave the pipeline, getting 100%
   utilization.

2) Overlap IF/EX with DE.  This requires solving two problems:

   a) Back-to-back dependencies will require forwarding the result
      from EX to DE.

      Adds to the critical path through EX

   b) We have to assume or "predict" the outcome of branches and
      compensate for mistakes.

      The compensation implies interlocks (stalling) and/or
      restarts.  All of this is likely to add to a critical path.



 Below we focus on the Unpipelined State Machine

 CONVENTION:
   var = array[r], where r is a [subfield of] a register assigned with <=
   r <= y, only if y is a [subfield of] a name (register or wire)
   All memory reads must be in the form:

                                   wire q = mem[y];

   Names reflect the stage/state from which they are outputs, eg. if_inst
   is the instruction fetched in IF, de_rs1_val the value of rs1 fetched
   by DE, etc.

 Conceptually: (where <= is a synchronous assignment and = an alias)

   if_pc <= if_pc + 4
   if_inst = code[if_pc]

   de_pc <= if_pc
   de_inst <= if_inst
   de_rs1_val = regs[de_inst`rs1]
   de_rs2_val = regs[de_inst`rs2]
   ex_load_addr = de_rs1_val + de_inst`off

   ex_pc <= de_pc
   ex_inst <= de_inst
   ex_rs1_val <= de_rs1_val
   ex_rs2_val <= de_rs2_val
   ex_load_addr <= de_load_addr

   ex_res = ex_inst`opcode == ADD ? ex_rs1_val + ex_rs2_val
                                  : ex_rs1_val - ex_rs2_val

   ex_loaded = memory[ex_load_addr]

   regs[ex_inst`rd] <= ex_inst`opcode == LOAD ? ex_loaded : ex_res


   With bypass:

      de_rs1_val = de_inst`rs1 == ex_inst`rd ? ex_res : regs[de_inst`rs1]
      de_rs2_val = de_inst`rs2 == ex_inst`rd ? ex_res : regs[de_inst`rs2]

*************************************************************************/


`timescale 1ns/10ps

// -----------------------------------------------------------------------
//
//   Copyright 2016 Tommy Thorn - All Rights Reserved
//
//   This program is free software; you can redistribute it and/or modify
//   it under the terms of the GNU General Public License as published by
//   the Free Software Foundation, Inc., 53 Temple Place Ste 330,
//   Bostom MA 02111-1307, USA; either version 2 of the License, or
//   (at your option) any later version; incorporated herein by reference.
//
// -----------------------------------------------------------------------







































































/**  Control and Status Registers  **/

// User-level, floating-point




// User-level, counter/timers







// Machine-level





  
  
  
  
  
  
  
  
  
  
  
  
  














  








// User-level, counter/timers














// Trap causes






















module yarvi( input  wire        clock

            , output reg  [29:0] address
            , output reg         writeenable = 0
            , output reg  [31:0] writedata
            , output reg  [ 3:0] byteena
            , output reg         readenable = 0
            , input  wire [31:0] readdata // XXX Must be ready next cycle!

            , output wire        bus_req_ready
            , input  wire        bus_req_read
            , input  wire        bus_req_write
            , input  wire [31:0] bus_req_address
            , input  wire [31:0] bus_req_data
            , output reg         bus_res_valid
            , output reg  [31:0] bus_res_data

            , input  wire [11:0] bus_csr_no
            , input  wire        bus_csr_read_enable
            , input  wire        bus_csr_write_enable
            , input  wire [31:0] bus_csr_writedata
            , output reg  [31:0] bus_csr_readdata
            , output wire        bus_csr_readdata_valid
            );

   // ===============================================================
   // ANNOTATIONS
   // ===============================================================
   // ---------------------------------------------------------------
   // source & sink
   // ---------------------------------------------------------------
   // @annot{taint_source(de_inst)}
   // @annot{taint_sink(wb_wbv)}
   // ---------------------------------------------------------------

   // ---------------------------------------------------------------
   // module inputs 
   // ---------------------------------------------------------------
   //  @annot{sanitize_glob(readdata)}
   //  @annot{sanitize_glob(bus_req_read)}
   //  @annot{sanitize_glob(bus_req_write)}
   //  @annot{sanitize_glob(bus_req_address)}
   //  @annot{sanitize_glob(bus_req_data)}
   //  @annot{sanitize_glob(bus_csr_no)}
   //  @annot{sanitize_glob(bus_csr_read_enable)}
   //  @annot{sanitize_glob(bus_csr_write_enable)}
   //  @annot{sanitize_glob(bus_csr_writedata)}
   // ---------------------------------------------------------------

   // ---------------------------------------------------------------
   // extra annotations
   // ---------------------------------------------------------------
   // @annot{sanitize_glob(if_inst)}       fetched insts have same val & tag
   // @annot{sanitize_glob(if_pc)}         program counters have same val & tag
   // @annot{sanitize_glob(interrupt)}     interrupts have same val & tag
   // @annot{taint_eq_mod(bram_tdp, mem)}  values read from the memory have the same tag

   // @annot{sanitize_glob(de_branch_taken)}  branches are taken in both runs
   // @annot{sanitize_glob(ex_ld_condition)}  explained below

   // whether the instruction reads or writes to the memory
   // @annot{sanitize_glob(b_wr0)}             
   // @annot{sanitize_glob(b_wr1)}
   // @annot{sanitize_glob(b_wr2)}
   // @annot{sanitize_glob(b_wr3)}

   // @annot{sanitize_glob(de_illegal_csr_access)}
   // @annot{taint_eq(de_csr_val)}
   // ---------------------------------------------------------------
   // ===============================================================

   wire bus_req_read_go  = bus_req_ready & bus_req_read;
   wire bus_req_write_go = bus_req_ready & bus_req_write;
   wire bus_req_rw_go    = bus_req_read_go | bus_req_write_go;















   /* Global state */

   /* CPU state.  These are a special-case of pipeline registers.  As
      they are only (and must only be) written by the EX stage, we
      needn't keeps per-stage versions of these.

      Note, a pipelined implementation will necessarily have access to
      the up-to-date version of the state, thus care must be taken to
      forward the correct valid where possible and restart whenever
      the use of an out-of-date value is detected.  In the cases where
      the update is rare it's likely better to unconditionally restart
      the pipeline whenever the update occurs (eg. writes of
      csr_ptbr).

      Exceptions to this are CSR cycle and time which are
      generally updated independent of what happens in the
      pipeline. */

   reg  [31:0] regs[0:31];

   // All CSRs can be accessed by csrXX instructions, but some are
   // used directly by the pipeline, as annotated below.

   // URW
   reg  [ 4:0] csr_fflags       = 0;               // @annot{sanitize(csr_fflags)}
   reg  [ 2:0] csr_frm          = 0;               // @annot{sanitize(csr_frm)}
   // URO
   reg  [63:0] csr_cycle        = 0;               // @annot{sanitize(csr_cycle)}
   reg  [63:0] csr_time         = 0;               // @annot{sanitize(csr_time)}
   reg  [63:0] csr_instret      = 0;               // @annot{sanitize(csr_instret)}

   // MRO, Machine Information Registers
   reg  [31:0] csr_mcpuid       = 1 << 8; // RV32I // @annot{sanitize(csr_mcpuid)}
   reg  [31:0] csr_mimpid       = 'h5454; // 'TT'  // @annot{sanitize(csr_mimpid)}
   reg  [31:0] csr_mhartid      = 0;               // @annot{sanitize(csr_mhartid)}                 
   // MRW, Machine Trap Setup
   reg  [31:0] csr_mstatus      = {2'd 3, 1'd 0};  // @annot{sanitize(csr_mstatus)}
   reg  [31:0] csr_mtvec        = 'h 100;          // @annot{sanitize(csr_mtvec)}
   reg  [31:0] csr_mtdeleg      = 0;               // @annot{sanitize(csr_mtdeleg)}
   reg  [ 7:0] csr_mie          = 0;               // @annot{sanitize(csr_mie)}
   reg  [31:0] csr_mtimecmp     = 0;               // @annot{sanitize(csr_mtimecmp)}
   // MRW, Machine Time and Counters
   reg  [31:0] csr_mtime        = 0;               // @annot{sanitize(csr_mtime)}
   // MRW, Machine Trap Handling
   reg  [31:0] csr_mscratch     = 0;               // @annot{sanitize(csr_mscratch)}
   reg  [31:0] csr_mepc         = 0;               // @annot{sanitize(csr_mepc)}
   reg  [31:0] csr_mcause       = 0;               // @annot{sanitize(csr_mcause)}
   reg  [31:0] csr_mbadaddr     = 0;               // @annot{sanitize(csr_mbadaddr)}
   reg  [ 7:0] csr_mip          = 0;               // @annot{sanitize(csr_mip)}
   // MRW, Machine Host-Target Interface (Non-Standard Berkeley Extension)
   reg  [31:0] csr_mtohost      = 0;               // @annot{sanitize(csr_mtohost)}
   reg  [31:0] csr_mfromhost    = 0;               // @annot{sanitize(csr_mfromhost)}


   wire       interrupt      = (csr_mie & csr_mip) != 0 && csr_mstatus[0];
   wire [7:0] interrupt_mask = csr_mie & csr_mip;
   reg  [2:0] interrupt_cause; // @annot{sanitize(interrupt_cause)}

   always @(*)
     if      (interrupt_mask[0]) interrupt_cause = 0;
     else if (interrupt_mask[1]) interrupt_cause = 1;
     else if (interrupt_mask[2]) interrupt_cause = 2;
     else if (interrupt_mask[3]) interrupt_cause = 3;
     else if (interrupt_mask[4]) interrupt_cause = 4;
     else if (interrupt_mask[5]) interrupt_cause = 5;
     else if (interrupt_mask[6]) interrupt_cause = 6;
     else interrupt_cause = 7;

   /* Forward declarations */
   reg        ex_restart          = 1;         // @annot{sanitize(ex_restart)}
   reg [31:0] ex_next_pc          = 32'h00_0200;  // @annot{sanitize(ex_next_pc)}
   reg        ex_valid_           = 0;         // @annot{sanitize(ex_valid_)}
   wire       ex_valid            = ex_valid_;
   reg [31:0] ex_inst;                         // @annot{sanitize(ex_inst)}
   reg        ex_wben;                         // @annot{sanitize(ex_wben)}
   reg [31:0] ex_wbv;                          // @annot{sanitize(ex_wbv)}
   
   reg        wb_valid            = 0;         // @annot{sanitize(wb_valid)}
   reg [31:0] wb_inst;                         // @annot{sanitize(wb_inst)}
   reg        wb_wben;                         // @annot{sanitize(wb_wben)}
   reg [31:0] wb_wbv;                          // @annot{sanitize(wb_wbv)}

//// INSTRUCTION FETCH ////

   reg         if_valid_           = 0;                        // @annot{sanitize(if_valid_)}
   wire        if_valid            = if_valid_ && !ex_restart;
   reg  [31:0] if_pc               = 0;                        // @annot{sanitize(if_pc)}

   always @(posedge clock) begin
      if_valid_ <= if_valid || ex_restart;
      if_pc     <= ex_next_pc;
   end

   wire [31:0] if_inst;

//// DECODE AND REGISTER FETCH ////

   /* If the previous cycle restarted the pipeline then this
      invalidates this stage, eg.

       IF       DE       EX
       8:sw     4:br     0:br 20  --> restart from 20
       20:add   8:-      4:-
       ...      20:add   8:-
       ...      ...      20:add

    */

   reg         de_valid_ = 0; // @annot{sanitize(de_valid_)}
   wire        de_valid = de_valid_ && !ex_restart;
   reg  [31:0] de_pc;         // @annot{sanitize(de_pc)}
   reg  [31:0] de_inst;       // @annot{sanitize(de_inst)}
   wire        de_illegal_csr_access = 
               de_valid && 
               de_inst[6 : 2] == 28 && 
               de_inst[14:12] != 0 &&
               (csr_mstatus[2:1] < de_inst[29:28] ||
                de_inst[31:30] == 3 
                && (de_inst[14:12] != 2 || 
                    de_inst[19:15] != 0));

   reg  [31:0] de_csr_val;    // @annot{sanitize(de_csr_val)}

   always @(posedge clock) begin
      de_valid_ <= if_valid;
      de_pc     <= if_pc;
      de_inst   <= if_inst;
   end

   reg  [31:0] de_rs1_val_r;    // @annot{sanitize(de_rs1_val_r)}
   reg  [31:0] de_rs2_val_r;    // @annot{sanitize(de_rs2_val_r)}

   wire        de_rs1_forward_ex = de_inst[19:15] == ex_inst[11: 7] && ex_wben;
   wire        de_rs2_forward_ex = de_inst[24:20] == ex_inst[11: 7] && ex_wben;
   wire        de_rs1_forward_wb = de_inst[19:15] == wb_inst[11: 7] && wb_wben;
   wire        de_rs2_forward_wb = de_inst[24:20] == wb_inst[11: 7] && wb_wben;

   wire [31:0] de_rs1_val      = de_rs1_forward_ex ? ex_wbv :
                                 de_rs1_forward_wb ? wb_wbv : de_rs1_val_r;
   wire [31:0] de_rs2_val      = de_rs2_forward_ex ? ex_wbv :
                                 de_rs2_forward_wb ? wb_wbv : de_rs2_val_r;

   wire [31:0] de_rs1_val_cmp  = (~de_inst[13] << 31) ^ de_rs1_val;
   wire [31:0] de_rs2_val_cmp  = (~de_inst[13] << 31) ^ de_rs2_val;
   wire        de_cmp_eq       = de_rs1_val     == de_rs2_val;
   wire        de_cmp_lt       = de_rs1_val_cmp  < de_rs2_val_cmp;
   wire        de_branch_taken = (de_inst[14] ? de_cmp_lt : de_cmp_eq) ^ de_inst[12];

   wire        de_sign         = de_inst[31];
   wire [19:0] de_sign20       = {20{de_sign}};
   wire [11:0] de_sign12       = {12{de_sign}};

   // I-type
   wire [31:0] de_i_imm        = {de_sign20, de_inst[31:25], de_inst[24:20]};

   // S-type
   wire [31:0] de_s_imm        = {de_sign20, de_inst[31:25], de_inst[11: 7]};
   wire [31:0] de_sb_imm       = {de_sign20, de_inst[7], de_inst[30:25], de_inst[11:8], 1'd0};

   // U-type
   wire [31:0] de_uj_imm       = {de_sign12, de_inst[19:12], de_inst[20], de_inst[30:21], 1'd0};

   wire [31:0] de_rs2_val_imm  = de_inst[6 : 2] == 4 ? de_i_imm : de_rs2_val;

   wire [31:0] de_load_addr    = de_rs1_val + de_i_imm;
   wire [31:0] de_store_addr   = de_rs1_val + de_s_imm;
   wire [14-1:0] de_load_wa  = de_load_addr[14+1:2];
   wire [14-1:0] de_store_wa = de_store_addr[14+1:2];
   wire [ 3:0] de_bytemask     = de_inst[14:12] == 0 ? 4'd 1 : de_inst[14:12] == 1 ? 4'd 3 : 4'd 15;
   wire [ 3:0] de_load_byteena = de_bytemask << de_load_addr[1:0];
   wire [ 3:0] de_store_byteena = de_bytemask << de_store_addr[1:0];
   wire        de_store        = de_valid && de_inst[6 : 2] == 8;
   wire        de_store_local  = de_store && de_store_addr[31:14+2] == (32'h00_0000 >> (14 + 2));
   wire        de_load        = de_valid && de_inst[6 : 2] == 0;
   wire [31:0] de_rs2_val_shl  = de_rs2_val << (de_store_addr[1:0]*8);

   reg [11:0] de_csrd; // @annot{sanitize(de_csrd)}

   always @(*)
     case (de_inst[14:12])
     2:  de_csrd = de_inst[19:15] ? de_inst[31:20] : 0;
     3:  de_csrd = de_inst[31:20];
     1:  de_csrd = de_inst[31:20];
     6: de_csrd = de_inst[31:20];
     7: de_csrd = de_inst[31:20];
     5: de_csrd = de_inst[31:20];
     default: de_csrd = 0;
     endcase

   always @(*)
     case (de_inst[31:20])
     'h   1:       de_csr_val = csr_fflags;
     'h   2:          de_csr_val = csr_frm;
     'h   3:         de_csr_val = {csr_frm, csr_fflags};

     'h C00:        de_csr_val = csr_cycle;
     'h C01:         de_csr_val = csr_time;
     'h C02:      de_csr_val = csr_instret;
     'h C80:       de_csr_val = csr_cycle[63:32];
     'h C81:        de_csr_val = csr_time[63:32];
     'h C82:     de_csr_val = csr_instret[63:32];

     'h F00:       de_csr_val = csr_mcpuid;
     'h F01:       de_csr_val = csr_mimpid;
     'h F10:      de_csr_val = csr_mhartid;

     'h 300:      de_csr_val = csr_mstatus;
     'h 301:        de_csr_val = csr_mtvec;
     'h 302:      de_csr_val = csr_mtdeleg;
     'h 304:          de_csr_val = csr_mie;
     'h 321:     de_csr_val = csr_mtimecmp;

     'h 340:     de_csr_val = csr_mscratch;
     'h 341:         de_csr_val = csr_mepc;
     'h 342:       de_csr_val = csr_mcause;
     'h 343:     de_csr_val = csr_mbadaddr;
     'h 344:          de_csr_val = csr_mip;

     'h 380:        de_csr_val = 0;
     'h 381:       de_csr_val = 0;
     'h 382:       de_csr_val = 0;
     'h 383:      de_csr_val = 0;
     'h 384:       de_csr_val = 0;
     'h 385:      de_csr_val = 0;

     'h B01:       de_csr_val = 0; // XXX It's not clear
     'h B81:      de_csr_val = 0;

     'h 780:      de_csr_val = csr_mtohost;
     'h 781:    de_csr_val = csr_mfromhost;

     default:           de_csr_val = 'h X;
     endcase

   // XXX Yeah, this code duplication isn't real clever
   always @(*)
     case (bus_csr_no)
     'h   1:       bus_csr_readdata = csr_fflags;
     'h   2:          bus_csr_readdata = csr_frm;
     'h   3:         bus_csr_readdata = {csr_frm, csr_fflags};

     'h C00:        bus_csr_readdata = csr_cycle;
     'h C01:         bus_csr_readdata = csr_time;
     'h C02:      bus_csr_readdata = csr_instret;
     'h C80:       bus_csr_readdata = csr_cycle[63:32];
     'h C81:        bus_csr_readdata = csr_time[63:32];
     'h C82:     bus_csr_readdata = csr_instret[63:32];

     'h F00:       bus_csr_readdata = csr_mcpuid;
     'h F01:       bus_csr_readdata = csr_mimpid;
     'h F10:      bus_csr_readdata = csr_mhartid;

     'h 300:      bus_csr_readdata = csr_mstatus;
     'h 301:        bus_csr_readdata = csr_mtvec;
     'h 302:      bus_csr_readdata = csr_mtdeleg;
     'h 304:          bus_csr_readdata = csr_mie;
     'h 321:     bus_csr_readdata = csr_mtimecmp;

     'h 340:     bus_csr_readdata = csr_mscratch;
     'h 341:         bus_csr_readdata = csr_mepc;
     'h 342:       bus_csr_readdata = csr_mcause;
     'h 343:     bus_csr_readdata = csr_mbadaddr;
     'h 344:          bus_csr_readdata = csr_mip;

     'h 380:        bus_csr_readdata = 0;
     'h 381:       bus_csr_readdata = 0;
     'h 382:       bus_csr_readdata = 0;
     'h 383:      bus_csr_readdata = 0;
     'h 384:       bus_csr_readdata = 0;
     'h 385:      bus_csr_readdata = 0;

     'h B01:       bus_csr_readdata = 0; // XXX It's not clear
     'h B81:      bus_csr_readdata = 0;

     'h 780:      bus_csr_readdata = csr_mtohost;
     'h 781:    bus_csr_readdata = csr_mfromhost;

     default:           bus_csr_readdata = 'h X;
     endcase

   assign bus_csr_readdata_valid = 1;

//// EXECUTE ////

   reg  [31:0] ex_load_addr;    // @annot{sanitize(ex_load_addr)}

   reg  [11:0] ex_csrd;         // @annot{sanitize(ex_csrd)}
   reg  [31:0] ex_csr_res;      // @annot{sanitize(ex_csr_res)}
   reg  [ 3:0] ex_load_byteena; // @annot{sanitize(ex_load_byteena)}
   wire [31:0] ex_loaded_data;

//// WRITEBACK ////

   reg  [11:0] wb_csrd; // @annot{sanitize(wb_csrd)}

   always @(posedge clock) begin
      ex_valid_       <= de_valid & !de_illegal_csr_access;
      ex_inst         <= de_inst;
      ex_load_addr    <= de_load_addr;
      ex_csrd         <= de_csrd;
      ex_load_byteena <= de_load_byteena;
   end


   // XXX It would be easy to support unaligned memory
   // with this setup by just calculating a different de_load_wa for
   // every slice and rotate the loaded word rather than just shifting
   // it. Similar for store.  Of course, IO access must still be
   // aligned as well as atomics.

   // REWRITE: supposed to be equal ex_load_addr[31]
   //          it's assumed that this should be eq in both runs
   // wire [31:0] ex_ld = ex_load_addr[31] ? readdata : ex_loaded_data;
   wire ex_ld_condition; 
   wire [31:0] ex_ld = ex_ld_condition ? readdata : ex_loaded_data;

   reg  [31:0] ex_ld_shifted, ex_ld_res; // @annot{sanitize(ex_ld_shifted, ex_ld_res)}

   always @(*) begin
      ex_ld_shifted = ex_ld >> (ex_load_addr[1:0] * 8);
      case (ex_inst[14:12])
         0: ex_ld_res = {{24{ex_ld_shifted[ 7]}}, ex_ld_shifted[ 7:0]};
         1: ex_ld_res = {{16{ex_ld_shifted[15]}}, ex_ld_shifted[15:0]};
         4: ex_ld_res = ex_ld_shifted[ 7:0];
         5: ex_ld_res = ex_ld_shifted[15:0];
         default: ex_ld_res = ex_ld;
      endcase
   end

   // Note, this could be done in stage DE and thus give a pipelined
   // implementation a single cycle branch penalty

   always @(posedge clock) begin
      if (ex_restart)
         $display("%05d  RESTARTING FROM %x", $time, ex_next_pc);

      ex_restart    <= 0;
      ex_next_pc    <= ex_next_pc + 4;

      // Restart if the previous instruction wrote a CSR or was fence.i
      if (de_valid && (de_inst[6 : 2] == 28 && de_csrd ||
                       de_inst[6 : 2] == 3)) begin
         ex_restart <= 1;
         ex_next_pc <= de_pc + 4;
      end

      // Take exception on illegal CSR access
      if (de_illegal_csr_access) begin
         ex_restart <= 1;
         ex_next_pc <= csr_mtvec + csr_mstatus[2:1] * 'h 40;
      end

      case ({!de_valid & !interrupt,de_inst[6 : 2]})
        24:
          if (de_branch_taken) begin
              ex_restart <= 1;
              ex_next_pc    <= de_pc + de_sb_imm;
          end 
        25: begin
           ex_restart    <= 1;
           ex_next_pc    <= (de_rs1_val + de_i_imm) & 32 'h ffff_fffe;
        end
        27: begin
           ex_restart    <= 1;
           ex_next_pc    <= de_pc + de_uj_imm;
        end
        28: begin
           case (de_inst[14:12])
           0:
             begin
                ex_restart    <= 1;
                case (de_inst[31:20])
                  0: ex_next_pc <= csr_mtvec + csr_mstatus[2:1] * 'h 40;
                  256:  ex_next_pc <= csr_mepc; //$display("        ERET -> %x", csr_mepc);
                  1: $finish; // XXX
                  default: begin
                     $display("NOT IMPLEMENTED");
                     $finish;
                  end
                endcase
             end
           endcase
           end
      endcase

      // Interrupts
      if (interrupt) begin
        ex_restart    <= 1;
        ex_next_pc    <= csr_mtvec + csr_mstatus[2:1] * 'h 40;
      end
   end

   // XXX This violates the code style above but is trivial to fix
   reg  [31:0] ex_res; // @annot{sanitize(ex_res)}
   always @(posedge clock)
      case (de_inst[6 : 2])
         4, 12:
            case (de_inst[14:12])
            0: if (de_inst[30] && de_inst[6 : 2] == 12)
                        ex_res <= de_rs1_val - de_rs2_val_imm;
                    else
                        ex_res <= de_rs1_val + de_rs2_val_imm;
            1:  ex_res <= de_rs1_val << de_rs2_val_imm[4:0];
            2:  ex_res <= $signed(de_rs1_val) < $signed(de_rs2_val_imm); // flip bit 31 of both operands
            3: ex_res <= de_rs1_val < de_rs2_val_imm;
            4:  ex_res <= de_rs1_val ^ de_rs2_val_imm;
            5:  if (de_inst[30])
                      ex_res <= $signed(de_rs1_val) >>> de_rs2_val_imm[4:0];
                   else
                      ex_res <= de_rs1_val >> de_rs2_val_imm[4:0];
            6:   ex_res <= de_rs1_val | de_rs2_val_imm;
            7:  ex_res <= de_rs1_val & de_rs2_val_imm;
          endcase

         13:     ex_res <=         {de_inst[31:12], 12'd0};
         5:   ex_res <= de_pc + {de_inst[31:12], 12'd0};

         25:    ex_res <= de_pc + 4;
         27:     ex_res <= de_pc + 4;

         28:
            case (de_inst[14:12])
            2:  begin ex_res <= de_csr_val; ex_csr_res <= de_csr_val |  de_rs1_val; end
            3:  begin ex_res <= de_csr_val; ex_csr_res <= de_csr_val &~ de_rs1_val; end
            1:  begin ex_res <= de_csr_val; ex_csr_res <=               de_rs1_val; end
            6: begin ex_res <= de_csr_val; ex_csr_res <= de_csr_val |  de_inst[19:15]; end
            7: begin ex_res <= de_csr_val; ex_csr_res <= de_csr_val &~ de_inst[19:15]; end
            5: begin ex_res <= de_csr_val; ex_csr_res <=               de_inst[19:15]; end
            endcase
      endcase

//// WRITE BACK ////

   always @(*) ex_wbv = ex_inst[6 : 2] == 0 ? ex_ld_res : ex_res;
   always @(*) ex_wben = ex_valid && ex_inst[11: 7] &&
                         ex_inst[6 : 2] != 24 && ex_inst[6 : 2] != 8;

   always @(posedge clock) begin
      if (ex_wben)
         regs[ex_inst[11: 7]] <= ex_wbv;
      de_rs1_val_r <= regs[if_inst[19:15]];
      de_rs2_val_r <= regs[if_inst[24:20]];
   end

   always @(posedge clock) begin
      wb_valid <= ex_valid;
      wb_inst <= ex_inst;
      wb_wben <= ex_wben;
      wb_wbv  <= ex_wbv;
      wb_csrd <= ex_csrd;
   end

   always @(posedge clock) begin
      if (csr_mtime == csr_mtimecmp) begin
         $display("Times up!");
         if (!csr_mie[7])
           $display("  but the interrupt currently disabled");
         csr_mip[7] <= 1;
      end

      //// outside pipeline ////

      csr_cycle   <= csr_cycle + 1;
      csr_time    <= csr_time  + 1;
      csr_mtime   <= csr_mtime + 1;
      csr_instret <= csr_instret + ex_valid;

      //// CSR updates ////

      if (de_illegal_csr_access)
        $display("%05d  exception: illegal CSR access attempted %x %x (priviledge %d, CSR %x)",
                 $time, de_pc, de_inst, csr_mstatus[2:1], de_inst[31:20]);

      if (ex_valid & !interrupt) begin
         if (ex_csrd && ex_inst[6 : 2] == 28)
           begin
           $display("          CSR%x <- %x", ex_csrd, ex_csr_res);

           case (ex_csrd)
           'h   1:    csr_fflags           <= ex_csr_res;
           'h   2:       csr_frm              <= ex_csr_res;
           'h   3: begin
              // REWRITE: remove the syntactic sugar
              // {csr_frm,csr_fflags} <= ex_csr_res;
              csr_frm    <= ex_csr_res[7:5];
              csr_fflags <= ex_csr_res[4:0];
            end

           'h 300:   csr_mstatus          <= ex_csr_res & ~(15 << 12); // No FP or XS;
           'h 304:       csr_mie              <= ex_csr_res;
           'h 321:  csr_mtimecmp         <= ex_csr_res;

           'h 340:  csr_mscratch         <= ex_csr_res;
           'h 341:      csr_mepc             <= ex_csr_res;
           'h 344:       csr_mip[3]           <= ex_csr_res[3];

           'h 781: csr_mfromhost        <= ex_csr_res;
           'h 780:
             begin
                csr_mtohost <= ex_csr_res;
                $display("        TOHOST %x", ex_csr_res);
                $finish;
             end

           'h 900:    csr_cycle           <= ex_csr_res;
           'h 901:     csr_time            <= ex_csr_res;
           'h 902:  csr_instret         <= ex_csr_res;
           'h 980:   csr_cycle[63:32]    <= ex_csr_res;
           'h 981:    csr_time[63:32]     <= ex_csr_res;
           'h 982: csr_instret[63:32]  <= ex_csr_res;
           default:
             $display("        warning: writing an unimplemented CSR");
           endcase
           end

         if (ex_inst[6 : 2] == 28 &&
             ex_inst[14:12] == 0 &&
             ex_inst[31:20] == 256)

           csr_mstatus[8:0] <= csr_mstatus[11:3];         // POP
      end

      // NB: We delay the interrupt until the instruction is valid to avoid
      // complications in computing csr_mepc.  This works as long as ex_valid
      // will eventuall be set, which is _currently_ true.

      if (de_valid &&
          (interrupt ||
           de_inst[6 : 2] == 28 &&
           de_inst[14:12] == 0 &&
           de_i_imm[11:0] != 256) ||
          de_illegal_csr_access) begin

          csr_mepc          <= de_pc;
          csr_mstatus[11:3] <= csr_mstatus[8:0];         // PUSH
          csr_mstatus[0]    <= 0;
          csr_mstatus[2:1]   <= 3;
          csr_mcause        <= interrupt                 ? interrupt_cause    :
                               de_illegal_csr_access     ? 2 :
                               de_i_imm[11:0] == 1 ? 3   :
                                                           8 + csr_mstatus[2:1];
      end

    end

//// MEMORY ACCESS ////

   wire [14-1:0]
     b_addr = bus_req_rw_go ? bus_req_address[14+1:2] :
              de_store_local ? de_store_wa : de_load_wa;
   wire [31:0] b_din = bus_req_write_go ? bus_req_data : de_rs2_val_shl;


   // XXX A store followed immediately by a load overlapping what was
   // stored will return the wrong data.  We _could_ forward the data
   // for the cases where the loaded data completely covers what was
   // loaded, but it would likely incur a cycle time penalty for this
   // extremely rare situation and it wouldn't help cases where
   // there's only a partial overlap.  Instead we should detect this
   // load-hit-store hazard and restart the load.  This still needs to
   // be done!
   wire b_wr0 = de_store_local && de_store_byteena[0] || bus_req_write_go;
   wire b_wr1 = de_store_local && de_store_byteena[1] || bus_req_write_go;
   wire b_wr2 = de_store_local && de_store_byteena[2] || bus_req_write_go;
   wire b_wr3 = de_store_local && de_store_byteena[3] || bus_req_write_go;

   wire [7:0] if_inst0, if_inst1, if_inst2, if_inst3;
   wire [7:0] ex_loaded_data0, ex_loaded_data1, ex_loaded_data2, ex_loaded_data3;

   // REWRITE: replaced the syntactic sugar
   assign if_inst        = {if_inst0, if_inst1, if_inst2, if_inst3};
   assign ex_loaded_data = {ex_loaded_data0, ex_loaded_data1, ex_loaded_data2, ex_loaded_data3};

   bram_tdp #(8, 14, {"","mem0.txt"}) mem0
     ( .a_clk(clock)
     , .a_wr(1'd 0)
     , .a_addr(ex_next_pc[14+1:2])
     , .a_din(8'h x)
     , .a_dout(if_inst0)

     , .b_clk(clock)
     , .b_wr(b_wr0)
     , .b_addr(b_addr)
     , .b_din(b_din[7:0])
     , .b_dout(ex_loaded_data0));

   bram_tdp #(8, 14, {"","mem1.txt"}) mem1
     ( .a_clk(clock)
     , .a_wr(1'd 0)
     , .a_addr(ex_next_pc[14+1:2])
     , .a_din(8'h x)
     , .a_dout(if_inst1)

     , .b_clk(clock)
     , .b_wr(b_wr1)
     , .b_addr(b_addr)
     , .b_din(b_din[15:8])
     , .b_dout(ex_loaded_data1));

   bram_tdp #(8, 14, {"","mem2.txt"}) mem2
     ( .a_clk(clock)
     , .a_wr(1'd 0)
     , .a_addr(ex_next_pc[14+1:2])
     , .a_din(8'h x)
     , .a_dout(if_inst2)

     , .b_clk(clock)
     , .b_wr(b_wr2)
     , .b_addr(b_addr)
     , .b_din(b_din[23:16])
     , .b_dout(ex_loaded_data2));

   bram_tdp #(8, 14, {"","mem3.txt"}) mem3
     ( .a_clk(clock)
     , .a_wr(1'd 0)
     , .a_addr(ex_next_pc[14+1:2])
     , .a_din(8'h x)
     , .a_dout(if_inst3)

     , .b_clk(clock)
     , .b_wr(b_wr3)
     , .b_addr(b_addr)
     , .b_din(b_din[31:24])
     , .b_dout(ex_loaded_data3));

   always @(*) begin
     writedata   = de_rs2_val_shl;
     byteena     = de_store ? de_store_byteena : de_load_byteena;
     writeenable = de_store;
     readenable  = de_valid && de_inst[6 : 2] == 0 && de_load_addr[31];
     address     = de_store ? de_store_addr[31:2] : de_load_addr[31:2];
   end

   assign bus_req_ready = !de_load && !de_store;
   reg ex_loaded_data_is_bus_res = 0; // @annot{sanitize(ex_loaded_data_is_bus_res)}
   always @(posedge clock) begin
      ex_loaded_data_is_bus_res <= bus_req_read_go;
      bus_res_valid             <= ex_loaded_data_is_bus_res;
      bus_res_data              <= ex_loaded_data;
   end

   initial $readmemh({"","initregs.txt"}, regs);































































































































































endmodule

// From http://danstrother.com/2010/09/11/inferring-rams-in-fpgas/
// A parameterized, inferable, true dual-port, dual-clock block RAM in Verilog.
module bram_tdp #(
    parameter DATA = 72,
    parameter ADDR = 10,
    parameter INIT = ""
) (
    // Port A
    input   wire                a_clk,
    input   wire                a_wr,
    input   wire    [ADDR-1:0]  a_addr,
    input   wire    [DATA-1:0]  a_din,
    output  reg     [DATA-1:0]  a_dout,

    // Port B
    input   wire                b_clk,
    input   wire                b_wr,
    input   wire    [ADDR-1:0]  b_addr,
    input   wire    [DATA-1:0]  b_din,
    output  reg     [DATA-1:0]  b_dout
);

// Shared memory
reg [DATA-1:0] mem [(2**ADDR)-1:0];

// Port A
always @(posedge a_clk) begin
    a_dout      <= mem[a_addr];
    if (a_wr) begin
        a_dout      <= a_din;
        mem[a_addr] <= a_din;
    end
end

// Port B
always @(posedge b_clk) begin
    b_dout      <= mem[b_addr];
    if (b_wr) begin
        b_dout      <= b_din;
        mem[b_addr] <= b_din;
    end
end

initial $readmemh(INIT,mem);
endmodule
