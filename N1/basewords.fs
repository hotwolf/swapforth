\ ###############################################################################
\ # Swapforth N1 Port - Base Words                                              #
\ ###############################################################################
\ # Description:                                                                #
\ #    This file contains the definitions of low level machine instructions of  #
\ #    the N1 processor. It has been derived from the J1a port.                 #
\ #                                                                             #
\ #    This file is part of the cross compile step:                             #
\ #       gforth cross.fs basewords.fs nuc.fs                                   #
\ #                                                                             #
\ #    Single colon definitions (:) are added to the host system's word list.   #
\ #    Double colon definitions (::) are added to the target system's word      #
\ #    list.                                                                    #
\ #                                                                             #
\ ###############################################################################
\ # Version History:                                                            #
\ #   April 17, 2019 - Dirk Heisswolf                                           #
\ #      - Initial release                                                      #
\ ###############################################################################

\ Instruction types
: jump                   h# c000           ; \ jump
: call                   h# 4000           ; \ call
: branch                 h# 2000           ; \ conditional branch
: lit                    h# 1000           ; \ literal
: alu_dc                 h# 0c00           ; \ ALU instruction with double cell result
: alu_sc                 h# 0800           ; \ ALU instruction with single cell result
: stack                  h# 0400           ; \ stack instruction
: mem_rd                 h# 0300           ; \ memory read
: mem_wr                 h# 0200           ; \ memory write
: ctrl_cc                h# 0100           ; \ concurrent control instructions
: ctrl_nc                h# 0000           ; \ non-concurrent control instructions

\ Return bit
: ret                    h# 8000           ; \ return from call

\ Jump or call addresses (absolute)
: jump_ind               h# 3fff or tcode, ; \ address is on the top of the parameter stack

\ Branch addresses (relative)
: branch_skip1           h# 0002 or tcode, ; skip over one instruction

\ ALU operators
: alu_n+t                h# 0000 or        ; \ plus
: alu_abs                h# 0020 or        ; \ absolute value
: alu_t-n                h# 0040 or        ; \ minus (reversed order)
: alu_n-t                h# 0060 or        ; \ minus
: alu_umin               h# 0080 or        ; \ unsigned minimum
: alu_max                h# 00a0 or        ; \ signed maximum
: alu_umax               h# 00c0 or        ; \ unsigned maximum
: alu_min                h# 00e0 or        ; \ signed minimum
: alu_t=n                h# 0100 or        ; \ equals comparison (unsigned)
: alu_n=t                h# 0120 or        ; \ equals comparison (signed)
: alu_t<>n               h# 0140 or        ; \ not-equals comparison (unsigned)
: alu_n<>t               h# 0160 or        ; \ not-equals comparison (signed)
: alu_t<n                h# 0180 or        ; \ unsigned lower-than comparison
: alu_n<t                h# 01a0 or        ; \ signed ower-than comparison
: alu_t>n                h# 01c0 or        ; \ unsigned greater-than comparison
: alu_n>t                h# 01e0 or        ; \ signed greater-than comparison
: alu_umul               h# 0200 or        ; \ unsigned multiplication
: alu_smul               h# 0260 or        ; \ signed multiplication
: alu_and                h# 0280 or        ; \ logic AND
: alu_xor                h# 02a0 or        ; \ logic XOR
: alu_or                 h# 02c0 or        ; \ logic OR
: alu_lsr                h# 0300 or        ; \ logic right shift
: alu_lsl                h# 0320 or        ; \ logic left shift
: alu_asr                h# 0340 or        ; \ arithmetic right shift
: alu_lit                h# 0380 or        ; \ set upper bibble of a literal value

\ ALU operands
: alu_ind                h# 0000 or tcode, ; \ both operands are on the parameter stack
: alu_simm_-1            h# 001f or tcode, ; \ -1 in oimm format
: alu_oimm_0             h# 0010 or tcode, ; \ 0 in oimm format
: alu_uimm_1             h# 0001 or tcode, ; \ 1 in uimm format

\ Literal values
: lit_false              h# 0000 or tcode, ; \ FALSE
: lit_true               h# 0fff or tcode, ; \ TRUE

\ Stack transition pattern
: ______________________ h# 0000 or tcode, ; \ no transition
: __________________R0<- h# 0001 or tcode, ; \ rdrop
: ______________P0->R0-> h# 0003 or tcode, ; \ copy P0 to R0
: ______________P0<>R0__ h# 0006 or tcode, ; \ swap P0 with R0
: ______________P0<>R0-> h# 0007 or tcode, ; \ copy R0 and swap P0 with R0
: __________P1<>P0______ h# 0018 or tcode, ; \ swap P0 with P1
: ______P2<>P1__________ h# 0060 or tcode, ; \ swap P1 with P2
: ______P2<>P1->P0->R0-> h# 006b or tcode, ; \ copy P1 and swap P1 with P2
: __P3<>P2______________ h# 0180 or tcode, ; \ swap P2 with P3
: __P3<>P2______P0<>R0-> h# 0187 or tcode, ; \ copy R0, swap R0 with P0 and swap P2 with P3
: __P3<>P2__P1<>P0______ h# 0198 or tcode, ; \ swap P0 with P1 and swap P2 with P3
: ->P3->P2->P1__________ h# 02a0 or tcode, ; \ nip
: ->P3->P2->P1->P0______ h# 02a8 or tcode, ; \ drop
: ->P3->P2->P1->P0->R0-> h# 02ab or tcode, ; \ >R
: <-P3<-P2<-P1<-P0______ h# 0350 or tcode, ; \ dup
: <-P3<-P2<-P1<-P0<-R0__ h# 0354 or tcode, ; \ R@
: <-P3<-P2<-P1<-P0<-R0<- h# 0355 or tcode, ; \ R>
: <-P3<-P2<-P1<-P0<>R0-> h# 0357 or tcode, ; \ copy R0 and P0 and swap R0 with P0
: <-P3<-P2<-P1<>P0______ h# 0358 or tcode, ; \ over
: <-P3<>P2______________ h# 0380 or tcode, ; \ copy P3 and swap P2 with P3
: <-P3<>P2__P1<>P0______ h# 0398 or tcode, ; \ copy P3, swap P0 with P1 and swap P2 with P3

\ Memory addresses
: mem_ind                h# 00ff or tcode, ; \ address is on the top of the parameter stack

\ Control instructions
: ctrl_get_psph          h# 00ff or tcode, ; \ fetch PSP (= depth)

\ Machine instructions
:: !            mem_wr            mem_ind       ; \ store x at addr ( x addr -- )
:: *            alu_sc  alu_umul  alu_ind       ; \ multiply n1|u1 by n2|u2 ( n1|u1 n2|u2 -- n3|u3 )
:: +            alu_sc  alu_n+t   alu_ind       ; \ add n1|u1 to n2|u2 ( n1|u1 n2|u2 -- n3|u3 )
:: +!           stack   ______________P0->R0->    \ add n1|u1 to the cell at addr ( n1|u1 a-adr -- )
                mem_rd            mem_ind
                alu_sc  alu_n+t   alu_ind
                stack   <-P3<-P2<-P1<-P0<-R0<-
                mem_wr            mem_ind       ;
:: -            alu_sc  alu_n-t   alu_ind       ; \ subtract n2|u2 from n1|u1 ( n1|u1 n2|u2 -- n3|u3 )
:: -2rot        stack   ______P2<>P1__________    \ rotate three cell pairs ( x1 x2 x3 x4 x5 x6 -- x5 x6 x1 x2 x3 x4 )
                stack   __P3<>P2__P1<>P0______
                stack   ->P3->P2->P1->P0->R0->
                stack   __P3<>P2__P1<>P0______
                stack   ->P3->P2->P1->P0->R0->
                stack   __P3<>P2__P1<>P0______
                stack   <-P3<-P2<-P1<-P0<-R0<-
                stack   __________P1<>P0______
                stack   <-P3<-P2<-P1<-P0<-R0<-  ;
:: -rot         stack   __________P1<>P0______    \ rotate the three topmost cells ( x1 x2 x3 -- x3 x1 x2)
                stack   ______P2<>P1__________  ;
:: 0<           alu_sc  alu_n<t   alu_oimm_0    ; \ test if n is negative ( n -- flag )
:: 0<>          alu_sc  alu_n<>t  alu_oimm_0    ; \ test if x is not zero ( x -- flag )
:: 0>           alu_sc  alu_n>t   alu_oimm_0    ; \ test if n is greater than zero ( n -- flag )
:: 0=           alu_sc  alu_n=t   alu_oimm_0    ; \ test if x is not zero ( x -- flag )
:: 1+           alu_sc  alu_n+t   alu_uimm_1    ; \ increment n1|u1 ( n1|u1 -- n2|u2 )
:: 1-           alu_sc  alu_t-n   alu_uimm_1    ; \ decrement n1|u1 ( n1|u1 -- n2|u2 )
:: 2!           stack   <-P3<-P2<-P1<-P0______    \ store x2 at addr and x1 at addr+1 ( x1 x2 addr --  )
                stack   ______P2<>P1__________
                memwr             mem_ind
                alu_sc  alu_n+t   alu_uimm_1
                memwr             mem_ind       ;
:: 2*           alu_sc  alu_lsl   alu_uimm_1    ; \ shift x1 one bit towards the MSB ( x1 -- x2 )
:: 2/           alu_sc  alu_asr   alu_uimm_1    ; \ shift x1 one bit towards the LSB ( x1 -- x2 )
:: 2@           stack   <-P3<-P2<-P1<-P0______    \ fetch x2 from addr and x1 at addr+1 ( addr -- x1 x2 )
                alu_sc  alu_n+t   alu_uimm_1
                memrd             mem_ind
                stack   __________P1<>P0______
                memrd             mem_ind       ;
:: 2drop        stack   ->P3->P2->P1->P0______    \ drop cell pair x1 x2 ( x1 x2 -- )
                stack   ->P3->P2->P1->P0______  ;
:: 2dup         stack   <-P3<-P2<-P1<>P0______    \ duplicate cell pair x1 x2 ( x1 x2 -- x1 x2 x1 x2 )
                stack   <-P3<-P2<-P1<>P0______  ;
:: 2nip         stack   ->P3->P2->P1__________    \ drop cell pair x1 x2 ( x1 x2 x3 x4 -- x3 x4 )
                stack   ->P3->P2->P1__________  ;
:: 2over        stack   <-P3<>P2______________    \ copy cell pair x1 x2 to the TOS ( x1 x2 x3 x4 -- x1 x2 x1 x2 x3 x4 x1 x2 )
                stack   ______P2<>P1__________
                stack   <-P3<>P2__P1<>P0______    \ copy cell pair x1 x2 to the TOS ( x1 x2 x3 x4 -- x1 x2 x1 x2 x3 x4 x1 x2 )
                stack   ______P2<>P1__________  ;
:: 2tuck        stack   ______P2<>P1->P0->R0->    \ copy cell pair x3 x4 below x2 cell pair x1 x2( x1 x2 x3 x4 -- x3 x4 x1 x2 x3 x4 )
                stack   __P3<>P2______P0<>R0->
                stack   __________P1<>P0______
                stack   ______P2<>P1__________
                stack   <-P3<-P2<-P1<-P0<-R0<-
                stack   <-P3<-P2<-P1<-P0<-R0<-  ;
:: 2>r          stack   ->P3->P2->P1->P0->R0->    \ shift cell pair x1 x2 to the RS ( x1 x2 -- ) (R: -- x1 x2 )
                stack   ->P3->P2->P1->P0->R0->  ;
:: 2r>          stack   <-P3<-P2<-P1<-P0<-R0<-    \ shift cell pair x1 x2 to the \gls{ps} ( -- x1 x2 ) (R: x1 x2 -- )
                stack   <-P3<-P2<-P1<-P0<-R0<-  ;
:: 2r@          stack   <-P3<-P2<-P1<-P0<-R0<-    \ shift cell pair x1 x2 to the \gls{ps} ( -- x1 x2 ) (R: x1 x2 -- )
                stack   <-P3<-P2<-P1<-P0<>R0->  ;
:: 2rdrop       stack   __________________R0<-    \ drop cell pair x1 x2  ( R: x1 x2 -- )
                stack   __________________R0<-  ;
:: 2rdup        stack   <-P3<-P2<-P1<-P0<-R0<-    \ duplicate cell pair x1 x2 ( R: x1 x2 -- x1 x2 x1 x2 )
                stack   <-P3<-P2<-P1<-P0<>R0->
                stack   ->P3->P2->P1->P0->R0->
                stack   ->P3->P2->P1->P0->R0->  ;
:: 2rot         stack   ->P3->P2->P1->P0->R0->    \ rotate three cell pairs ( x1 x2 x3 x4 x4 x5 x6 -- x3 x4 x5 x6 x1 x2 )
                stack   __P3<>P2______________
                stack   ->P3->P2->P1->P0->R0->
                stack   __P3<>P2__P1<>P0______
                stack   <-P3<-P2<-P1<-P0<>R0->
                stack   __P3<>P2__P1<>P0______
                stack   <-P3<-P2<-P1<-P0<>R0->
                stack   __P3<>P2__P1<>P0______
                stack   ______P2<>P1__________  ;
:: 2swap        stack   ______P2<>P1__________    \ swap two cell pairs ( x1 x2 x3 x4 -- x3 x4 x1 x2 )
                stack   __P3<>P2__P1<>P0______
                stack   ______P2<>P1__________  ;
:: <            alu_sc  alu_n<t   alu_ind       ; \ test if n1 is lower than n2 ( n1 n2 -- flag )
:: <>           alu_sc  alu_n<>t  alu_ind       ; \ test if x1 is different than x2 ( x1 x2 -- flag )
:: =            alu_sc  alu_n=t   alu_ind       ; \ test if x1 equals x2 ( x1 x2 -- flag )
:: >            alu_sc  alu_n>t   alu_ind       ; \ test if n1 is greater than n2 ( n1 n2 -- flag )
:: >r           stack   ->P3->P2->P1->P0->R0->  ; \ shift x on to the RS ( x -- ) (R: -- x )
:: ?dup         stack   <-P3<-P2<-P1<-P0______    \ duplicate x if it is not zero ( x -- 0|x x )
                alu_sc  alu_n=t   alu_oimm_0
                branch  branch_skip1
                stack   ->P3->P2->P1->P0______  ;
:: @            mem_rd            mem_ind       ; \ fetch x from addr ( addr -- x )
:: abs          alu_sc  alu_abs   alu_ind       ; \ absolute vale of n ( n -- u )
:: and          alu_sc  alu_and   alu_ind       ; \ bitwise logic AND of x1 and x2 ( x1 x2 -- x3 )
:: cell+        alu_sc  alu_n+t   alu_uimm_1    ; \ increment addr1 ( addr1 -- -addr2 )
:: depth        ctrl_nc           ctrl_get_psp  ; \ +n is the number of cells on the \gls{ps} without +n ( -- +n )
:: drop         stack   ->P3->P2->P1->P0______  ; \ drop x from the /gls{ps} ( x -- )
:: dup          stack   <-P3<-P2<-P1<-P0______  ; \ duplicate x ( x -- x x )
:: execute      call                  jump_ind  ; \ execute xt ( $i*$x xt -- $j*$x )
:: false        lit     lit_false               ; \ FALSE flag ( -- false )
:: invert       alu_sc  alu_xor   alu_simm_-1   ; \ bitwise inverse of x1 ( x1 -- x2 )
:: lshift       alu_sc  alu_lsl   alu_ind       ; \ shift x1 u bits towards the MSB ( x1 u -- x2 )
:: m*           alu_dc  alu_smul  alu_ind       ; \ multiply n1 by n2 ( n1 n2 -- d )
:: m+           alu_dc  alu_n+t   alu_ind       ; \ Add n1 to n2 ( n1 n2 -- d )
:: max          alu_sc  alu_max   alu_ind       ; \ n3 is the greater of n1 and n2 ( n1 n2 -- n3 )
:: min          alu_sc  alu_min   alu_ind       ; \ n3 is the lesser of n1 and n2 ( n1 n2 -- n3 )
:: negate       alu_sc  alu_n-t   alu_oimm_0    ; \ n2 is the two's complement of n1 ( n1 -- n2 )
:: nip          stack   ->P3->P2->P1__________  ; \ drop x1 ( x1 x2 -- x2 )
:: noop         stack   ______________________  ; \ do nothing ( -- )
:: or           alu_sc  alu_or    alu_ind       ; \ bitwise logic OR of x1 and x2 ( x1 x2 -- x3 )
:: over         stack   <-P3<-P2<-P1<>P0______  ; \ copy x1 to the TOS ( x1 x2 -- x1 x2 x1 )
:: r>           stack   <-P3<-P2<-P1<-P0<-R0<-  ; \ shift x to the \gls{ps} ( -- x ) (R: x -- )
:: r@           stack   <-P3<-P2<-P1<-P0<-R0__  ; \ copy x to the \gls{ps} ( -- x ) (R: x -- x )
:: rdrop        stack   __________________R0<-  ; \ drop x from the return stack  ( R: x -- )
:: rdup         stack   ______________P0<>R0->    \ duplicate x on the return stack ( R: x -- x x )
                stack   ______________P0<>R0__  ;
:: rshift       alu_sc  alu_lsr   alu_ind       ; \ shift x1 u bits towards the LSB ( x1 u -- x2 )
:: rot          stack   ______P2<>P1__________    \ rotate the three topmost cells ( x1 x2 x3 -- x2 x3 x1)
                stack   __________P1<>P0______  ;
:: s>d          alu_dc                          ; \ sign-extend n ( n -- d )
:: swap         stack   __________P1<>P0______  ; \ swap x1 and x2 ( x1 x2 -- x2 x1 )
:: true         lit     lit_true                ; \ TRUE flag ( -- true )
:: tuck         stack   <-P3<-P2<-P1<-P0______    \ copy x1 below x2 ( x1 x2 -- x2 x1 x2 )
                stack   ______P2<>P1__________  ;
:: u<           alu_sc  alu_t>n   alu_ind       ; \ test if u1 is lower than u2 ( u1 u2 -- flag)
:: u>           alu_sc  alu_t<n   alu_ind       ; \ test if u1 is greater than u2 ( u1 u2 -- flag)
:: um*          alu_dc                          ; \ multiply u1 by u2 ( u1 u2 -- d )
:: xor          alu_sc  alu_xor   alu_ind       ; \ bitwise logic XOR of x1 and x2 ( x1 x2 -- x3 )
