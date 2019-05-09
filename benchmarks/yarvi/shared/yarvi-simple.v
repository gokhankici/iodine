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

   // mie : machine interrupt-enable register
   reg  [ 7:0] csr_mie          = 0;               // @annot{sanitize(csr_mie)}
   // mip : machine interrupt pending
   reg  [ 7:0] csr_mip          = 0;               // @annot{sanitize(csr_mip)}
   // mstatus register keeps track of and controls the hardware thread's current operating state.
   reg  [31:0] csr_mstatus      = {2'd 3, 1'd 0};  // @annot{sanitize(csr_mstatus)}

   wire       interrupt      = (csr_mie & csr_mip) != 0 && csr_mstatus[0];

   always @(posedge clock) begin
      if (csr_mtime == csr_mtimecmp) begin
         // time is up         
         csr_mip[7] <= 1;
      end

      if (ex_valid & !interrupt) begin
         if (ex_csrd && ex_inst`opcode == `SYSTEM)
           case (ex_csrd)
             `CSR_MSTATUS: csr_mstatus <= ex_csr_res & ~(15 << 12); // No FP or XS;
             `CSR_MIE:     csr_mie     <= ex_csr_res;
             `CSR_MIP:     csr_mip[3]  <= ex_csr_res[3];
           endcase


endmodule