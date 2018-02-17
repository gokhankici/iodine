%% next, next1, next2, inv1, inv2 ==> TBD

inv( A_L,  B_L,  C_L,  CLK_L
   , A_R,  B_R,  C_R,  CLK_R
   , A_TL, B_TL, C_TL, CLK_TL
   , A_TR, B_TR, C_TR, CLK_TR
   , Done_L, Done_R
   ) :-
        %% set taint bits of source to 1, rest to 0
        A_TL = 1, B_TL = 0, C_TL = 0, CLK_TL = 0,
        A_TR = 1, B_TR = 0, C_TR = 0, CLK_TR = 0,
        %% not done yet
        Done_L = 0, Done_R = 0,
        %% both read the same source
        A_L = A_R.

inv( A_L1,  B_L1,  C_L1,  CLK_L1
   , A_R1,  B_R1,  C_R1,  CLK_R1
   , A_TL1, B_TL1, C_TL1, CLK_TL1
   , A_TR1, B_TR1, C_TR1, CLK_TR1

   , Done_L1, Done_R1
   ) :-
        %% block1's prev inv holds
        inv1( A_L,  B_L
            , A_R,  B_R
            , A_LT, B_LT
            , A_RT, B_RT
            ),
        
        %% block1's prev inv holds
        inv2( B_L,  C_L
            , B_R,  C_R
            , B_LT, C_LT
            , B_RT, C_RT
            ),

        %% global prev inv holds
        inv( A_L,  B_L,  C_L,  CLK_L
           , A_R,  B_R,  C_R,  CLK_R
           , A_TL, B_TL, C_TL, CLK_TL
           , A_TR, B_TR, C_TR, CLK_TR
           , Done_L, Done_R
           ),

        (   %% set taint bits of source to 1, rest to 0
            A_TL1 = 1, B_TL1 = 0, C_TL1 = 0, CLK_TL1 = 0,
            A_TR1 = 1, B_TR1 = 0, C_TR1 = 0, CLK_TR1 = 0, 
            %% all valuations stay the same
            A_L1 = A_L, B_L1 = B_L, C_L1 = C_L, CLK_L1 = CLK_L,
            A_R1 = A_R, B_R1 = B_R, C_R1 = C_R, CLK_R1 = CLK_R,
            %% both executions have not finished yet
            Done_L1 = 0, Done_R1 = 0,
            Done_L = 0, Done_R = 0
           
        ;   (   %% run block1 on left
                next1( A_L,   B_L,   UF1_L
                     , A_TL,  B_TL,  UF1_TL
                     , A_L1,  B_L1,  UF1_L1
                     , A_TL1, B_TL1, UF1_TL1
                     ),
                %% run block1 on right
                next1( A_R,   B_R,   UF1_R
                     , A_TR,  B_TR,  UF1_TR
                     , A_R1,  B_R1,  UF1_R1
                     , A_TR1, B_TR1, UF1_TR1
                     ),
                %% uf equalities
                (AL = AR -> UF1L = UF1R) ->

                %% block1's new inv should hold
                inv1( A_L1,  B_L1
                    , A_R1,  B_R1
                    , A_LT1, B_LT1
                    , A_RT1, B_RT1
                    )
            ) ,
            (   %% run block2 on left
                next2( B_L,   C_L,   UF2_L
                     , B_TL,  C_TL,  UF2_TL
                     , B_L1,  C_L1,  UF2_L1
                     , B_TL1, C_TL1, UF2_TL1
                     ),
                %% run block2 on right
                next2( B_R,   C_R,   UF2_R
                     , B_TR,  C_TR,  UF2_TR
                     , B_R1,  C_R1,  UF2_R1
                     , B_TR1, C_TR1, UF2_TR1
                     ),
                %% uf equalities
                (BL = BR -> UF2L = UF2R) ->

                %% block2's new inv should hold
                inv2( B_L1,  C_L1
                    , B_R1,  C_R1
                    , B_LT1, C_LT1
                    , B_RT1, C_RT1
                    )
            )
        ).

        

 
