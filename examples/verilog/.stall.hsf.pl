/* -*- mode: prolog -*- */
/* vim: set ft=prolog: */

next(V_v_Stall, V_v_EX_ALUOp, V_v_WB_ALUOut, V_v_ID_instr, V_v_MEM_ALUOut, V_v_IF_instr, V_v_clk, VT_v_Stall, VT_v_EX_ALUOp, VT_v_WB_ALUOut, VT_v_ID_instr, VT_v_MEM_ALUOut, VT_v_IF_instr, VT_v_clk, V_done, V1_v_Stall, V1_v_EX_ALUOp, V1_v_WB_ALUOut, V1_v_ID_instr, V1_v_MEM_ALUOut, V1_v_IF_instr, V1_v_clk, VT1_v_Stall, VT1_v_EX_ALUOp, VT1_v_WB_ALUOut, VT1_v_ID_instr, VT1_v_MEM_ALUOut, VT1_v_IF_instr, VT1_v_clk, V1_done) :=
(
   VT1_v_Stall = VT_v_IF_instr, V1_v_Stall = V_v_IF_instr, 
   VT1_v_EX_ALUOp = VT_v_ID_instr, V1_v_EX_ALUOp = V_v_ID_instr, 
   (   V_v_Stall >= 1, VT1_v_ID_instr = VT_v_ID_instr, V1_v_ID_instr = V_v_ID_instr
   ;   V_v_Stall =< 0, VT1_v_ID_instr = VT_v_IF_instr, V1_v_ID_instr = V_v_IF_instr
   ), 
   VT1_v_MEM_ALUOut = VT_v_EX_ALUOp, V1_v_MEM_ALUOut = V_v_EX_ALUOp, 
   VT1_v_WB_ALUOut = VT_v_MEM_ALUOut, V1_v_WB_ALUOut = V_v_MEM_ALUOut, 
   ite(VT1_v_WB_ALUOut >= 1, V1_done = 1, V1_done = V_done), 
   V_done = 0
).

query_naming(inv(vVL_v_Stall, vVL_v_EX_ALUOp, vVL_v_WB_ALUOut, vVL_v_ID_instr, vVL_v_MEM_ALUOut, vVL_v_IF_instr, vVL_v_clk, vVLT_v_Stall, vVLT_v_EX_ALUOp, vVLT_v_WB_ALUOut, vVLT_v_ID_instr, vVLT_v_MEM_ALUOut, vVLT_v_IF_instr, vVLT_v_clk, vVL_done, vVR_v_Stall, vVR_v_EX_ALUOp, vVR_v_WB_ALUOut, vVR_v_ID_instr, vVR_v_MEM_ALUOut, vVR_v_IF_instr, vVR_v_clk, vVRT_v_Stall, vVRT_v_EX_ALUOp, vVRT_v_WB_ALUOut, vVRT_v_ID_instr, vVRT_v_MEM_ALUOut, vVRT_v_IF_instr, vVRT_v_clk, vVR_done)).

inv(VL_v_Stall, VL_v_EX_ALUOp, VL_v_WB_ALUOut, VL_v_ID_instr, VL_v_MEM_ALUOut, VL_v_IF_instr, VL_v_clk, VLT_v_Stall, VLT_v_EX_ALUOp, VLT_v_WB_ALUOut, VLT_v_ID_instr, VLT_v_MEM_ALUOut, VLT_v_IF_instr, VLT_v_clk, VL_done, VR_v_Stall, VR_v_EX_ALUOp, VR_v_WB_ALUOut, VR_v_ID_instr, VR_v_MEM_ALUOut, VR_v_IF_instr, VR_v_clk, VRT_v_Stall, VRT_v_EX_ALUOp, VRT_v_WB_ALUOut, VRT_v_ID_instr, VRT_v_MEM_ALUOut, VRT_v_IF_instr, VRT_v_clk, VR_done) :-
(
   VLT_v_IF_instr = 1, VRT_v_IF_instr = 1, 
   VLT_v_Stall = 0, VRT_v_Stall = 0, 
   VLT_v_EX_ALUOp = 0, VRT_v_EX_ALUOp = 0, 
   VLT_v_WB_ALUOut = 0, VRT_v_WB_ALUOut = 0, 
   VLT_v_ID_instr = 0, VRT_v_ID_instr = 0, 
   VLT_v_MEM_ALUOut = 0, VRT_v_MEM_ALUOut = 0, 
   VLT_v_clk = 0, VRT_v_clk = 0, 
   VL_done = 0, 
   VR_done = 0, 
   VL_v_IF_instr = VR_v_IF_instr
).

inv(VL1_v_Stall, VL1_v_EX_ALUOp, VL1_v_WB_ALUOut, VL1_v_ID_instr, VL1_v_MEM_ALUOut, VL1_v_IF_instr, VL1_v_clk, VLT1_v_Stall, VLT1_v_EX_ALUOp, VLT1_v_WB_ALUOut, VLT1_v_ID_instr, VLT1_v_MEM_ALUOut, VLT1_v_IF_instr, VLT1_v_clk, VL1_done, VR1_v_Stall, VR1_v_EX_ALUOp, VR1_v_WB_ALUOut, VR1_v_ID_instr, VR1_v_MEM_ALUOut, VR1_v_IF_instr, VR1_v_clk, VRT1_v_Stall, VRT1_v_EX_ALUOp, VRT1_v_WB_ALUOut, VRT1_v_ID_instr, VRT1_v_MEM_ALUOut, VRT1_v_IF_instr, VRT1_v_clk, VR1_done) :-
(
   ( ( VL_done = 0, 
       VL1_done = 0, 
       VR_done = 0, 
       VR1_done = 0, 
       VLT1_v_IF_instr = 1, VRT1_v_IF_instr = 1, 
       VLT1_v_Stall = 0, VRT1_v_Stall = 0, 
       VLT1_v_EX_ALUOp = 0, VRT1_v_EX_ALUOp = 0, 
       VLT1_v_WB_ALUOut = 0, VRT1_v_WB_ALUOut = 0, 
       VLT1_v_ID_instr = 0, VRT1_v_ID_instr = 0, 
       VLT1_v_MEM_ALUOut = 0, VRT1_v_MEM_ALUOut = 0, 
       VLT1_v_clk = 0, VRT1_v_clk = 0, 
       VL1_v_Stall = VL_v_Stall, VR1_v_Stall = VR_v_Stall, 
       VL1_v_EX_ALUOp = VL_v_EX_ALUOp, VR1_v_EX_ALUOp = VR_v_EX_ALUOp, 
       VL1_v_WB_ALUOut = VL_v_WB_ALUOut, VR1_v_WB_ALUOut = VR_v_WB_ALUOut, 
       VL1_v_ID_instr = VL_v_ID_instr, VR1_v_ID_instr = VR_v_ID_instr, 
       VL1_v_MEM_ALUOut = VL_v_MEM_ALUOut, VR1_v_MEM_ALUOut = VR_v_MEM_ALUOut, 
       VL1_v_IF_instr = VL_v_IF_instr, VR1_v_IF_instr = VR_v_IF_instr, 
       VL1_v_clk = VL_v_clk, VR1_v_clk = VR_v_clk ); 
     ( next(VL_v_Stall, VL_v_EX_ALUOp, VL_v_WB_ALUOut, VL_v_ID_instr, VL_v_MEM_ALUOut, VL_v_IF_instr, VL_v_clk, VLT_v_Stall, VLT_v_EX_ALUOp, VLT_v_WB_ALUOut, VLT_v_ID_instr, VLT_v_MEM_ALUOut, VLT_v_IF_instr, VLT_v_clk, VL_done, VL_v_Stall, VL_v_EX_ALUOp, VL_v_WB_ALUOut, VL_v_ID_instr, VL_v_MEM_ALUOut, VL_v_IF_instr, VL_v_clk, VLT_v_Stall, VLT_v_EX_ALUOp, VLT_v_WB_ALUOut, VLT_v_ID_instr, VLT_v_MEM_ALUOut, VLT_v_IF_instr, VLT_v_clk, VL_done), 
       next(VL_v_Stall, VL_v_EX_ALUOp, VL_v_WB_ALUOut, VL_v_ID_instr, VL_v_MEM_ALUOut, VL_v_IF_instr, VL_v_clk, VLT_v_Stall, VLT_v_EX_ALUOp, VLT_v_WB_ALUOut, VLT_v_ID_instr, VLT_v_MEM_ALUOut, VLT_v_IF_instr, VLT_v_clk, VL_done, VR_v_Stall, VR_v_EX_ALUOp, VR_v_WB_ALUOut, VR_v_ID_instr, VR_v_MEM_ALUOut, VR_v_IF_instr, VR_v_clk, VRT_v_Stall, VRT_v_EX_ALUOp, VRT_v_WB_ALUOut, VRT_v_ID_instr, VRT_v_MEM_ALUOut, VRT_v_IF_instr, VRT_v_clk, VR_done), 
       VL_v_Stall = VR_v_Stall, 
       VL1_v_IF_instr = VR1_v_IF_instr, VLT1_v_IF_instr = 0, VRT1_v_IF_instr = 0 ) ), 
   inv(VL_v_Stall, VL_v_EX_ALUOp, VL_v_WB_ALUOut, VL_v_ID_instr, VL_v_MEM_ALUOut, VL_v_IF_instr, VL_v_clk, VLT_v_Stall, VLT_v_EX_ALUOp, VLT_v_WB_ALUOut, VLT_v_ID_instr, VLT_v_MEM_ALUOut, VLT_v_IF_instr, VLT_v_clk, VL_done, VR_v_Stall, VR_v_EX_ALUOp, VR_v_WB_ALUOut, VR_v_ID_instr, VR_v_MEM_ALUOut, VR_v_IF_instr, VR_v_clk, VRT_v_Stall, VRT_v_EX_ALUOp, VRT_v_WB_ALUOut, VRT_v_ID_instr, VRT_v_MEM_ALUOut, VRT_v_IF_instr, VRT_v_clk, VR_done)
).

VR_done = 1 :-
   inv(VL_v_Stall, VL_v_EX_ALUOp, VL_v_WB_ALUOut, VL_v_ID_instr, VL_v_MEM_ALUOut, VL_v_IF_instr, VL_v_clk, VLT_v_Stall, VLT_v_EX_ALUOp, VLT_v_WB_ALUOut, VLT_v_ID_instr, VLT_v_MEM_ALUOut, VLT_v_IF_instr, VLT_v_clk, VL_done, VR_v_Stall, VR_v_EX_ALUOp, VR_v_WB_ALUOut, VR_v_ID_instr, VR_v_MEM_ALUOut, VR_v_IF_instr, VR_v_clk, VRT_v_Stall, VRT_v_EX_ALUOp, VRT_v_WB_ALUOut, VRT_v_ID_instr, VRT_v_MEM_ALUOut, VRT_v_IF_instr, VRT_v_clk, VR_done), 
   VL_done = 1.

