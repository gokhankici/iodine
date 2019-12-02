module sha256 (clk_i, rst_i, text_i_, text_o, cmd_i, cmd_w_i, cmd_o);

        input           clk_i;  // global clock input
        input           rst_i;  // global reset input , active high
        
        input   [31:0]  text_i_; // text input 32bit
        output  [31:0]  text_o; // text output 32bit
        
        input   [2:0]   cmd_i;  // command input
        input           cmd_w_i;// command input write enable
        output  [3:0]   cmd_o;  // command output(status)

        /*
                cmd
                Busy Round W R

                bit3 bit2  bit1 bit0
                Busy Round W    R
                
                Busy:
                0       idle
                1       busy
                
                Round:
                0       first round
                1       internal round
                
                W:
                0       No-op
                1       write data
                
                R:
                0       No-op
                1       read data
                        
        */

        reg     [3:0]   cmd;
        wire    [3:0]   cmd_o;
        
        reg     [6:0]   round;
        wire    [6:0]   round_plus_1;
        
        reg     [2:0]   read_counter;

        reg             busy;
        
        assign cmd_o = cmd;
        always @ (posedge clk_i)
        begin
                if (rst_i)
                        cmd <= 'b0;
                else
                if (cmd_w_i)
                      cmd[2:0] <= cmd_i[2:0];         // busy bit can't write
                else
                begin
                        cmd[3] <= busy;                 // update busy bit
                        if (~busy)
                                cmd[1:0] <= 2'b00;      // hardware auto clean R/W bits
                end
        end
        
        assign round_plus_1 = round + 1;
        
        //------------------------------------------------------------------    
        // SHA round
        //------------------------------------------------------------------
        always @(posedge clk_i)
        begin
                if (rst_i)
                begin
                        round <= 'd0;
                        busy <= 'b0;
                end
                else
                begin
                        case (round)
                        'd0:
                                begin
                                        if (cmd[1])
                                        begin
                                                busy <= 'b1;
                                                round <= round_plus_1;
                                        end
                                        else
                                        begin   // IDLE
                                                round <= 'd0;
                                        end
                                end
                        'd1:
                                begin
                                        round <= round_plus_1;
                                end
                        'd64:
                                begin
                                        round <= 'd0;
                                        busy <= 'b0;
                                end
                        default:
                                begin
                                        round <= 'd0;
                                        busy <= 'b0;
                                end
                        endcase
                end     
        end 
        
        //------------------------------------------------------------------    
        // read result 
        //------------------------------------------------------------------    
        always @ (posedge clk_i)
        begin
                if (rst_i)
                begin
                        read_counter <= 'b0;
                end
                else
                begin
                        if (cmd[0])
                        begin
                                read_counter <= 'd7;    // sha-256      256/32=8
                        end
                        else
                        begin
                        if (~busy)
                        begin
                                if (|read_counter)
                                        read_counter <= read_counter - 'd1;
                        end
                        end
                end
        end
        
endmodule
 
