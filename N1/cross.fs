\ ###############################################################################
\ # Swapforth N1 Port - Cross Compiler                                          #
\ ###############################################################################
\ # Description:                                                                #
\ #    This file contains the cross compiler for the N1 port of the Swapforth   #
\ #    nucleus.                                                                 #
\ #    The implemntation has been derived from the J1a cross compiler:          #
\ #                                                                             #
\ #    >  Usage gforth cross.fs <machine.fs> <program.fs>  <                    #
\ #    >                                                   <                    #
\ #    >  Where machine.fs defines the target machine	   <                    #
\ #    >  and program.fs is the target program             <                    #
\ #                                                                             #
\ ###############################################################################
\ # Version History:                                                            #
\ #   April 25, 2019 - Dirk Heisswolf                                           #
\ #      - Initial release, derived from the J1a cross compiler                 #
\ ###############################################################################

variable lst        \ .lst output file handle

: h#
    base @ >r hex           \ set BASE to 16
    0.                      \ -- 0 0 
    bl parse                \ parse next word -- 0 0 c-addr u
    >number                 \ convert to number -- ud c-addr u
    throw                   \ error if string contains unconvertable characters -- ud c-addr
    2drop                   \ -- u (most significant cell is above the least significant cell)
    postpone literal        \ append the compilation semantics of LITERAL --
    r> base ! ; immediate   \ restore previous BASE

: tcell     2 ;
: tcells    tcell * ;
: tcell+    tcell + ;

131072 allocate throw constant tflash       \ bytes, target flash
131072 allocate throw constant _tbranches   \ branch targets, cells
tflash      31072 erase
_tbranches  131072 erase
: tbranches cells _tbranches + ;

variable tdp    0 tdp !         \ Data pointer
variable tcp    0 tcp !         \ Code pointer -> not used
: there     tdp @ ;
: islegal   ;
: tc!       islegal tflash + c! ;
: tc@       islegal tflash + c@ ;
: tw!       islegal tflash + w! ;
: tw@       islegal tflash + w@ ;
: t@        islegal tflash + uw@ ;
: twalign   tdp @ 1+ -2 and tdp ! ;
: tc,       there tc! 1 tdp +! ;
: tw,       there tw! tcell tdp +! ;
: tcode,    tdp @ tw! tcell tdp +! ;
: org       tdp ! ;

wordlist constant target-wordlist
: add-order ( wid -- ) >r get-order r> swap 1+ set-order ;
: :: get-current >r target-wordlist set-current : r> set-current ;

next-arg included       \ include the machine.fs

( Language basics for target                 JCB 19:08 05/02/12)

warnings off
:: ( postpone ( ;
:: \ postpone \ ;

:: org          org ;
:: include      include ;
:: included     included ;
:: marker       marker ;
:: [if]         postpone [if] ;
:: [else]       postpone [else] ;
:: [then]       postpone [then] ;

: literal
      dup $0fff and lit or tcode,                       \ compile plain literal instruction -- n1
      $f800 and dup if                                  \ check if the value exceeds the 12-bit range -- n2 
          dup $f800 xor if                              \ check if the value falls below the 12-bit range -- n2
	      dup 12 rshift alu_sc alu_lit or or tcode, \ compile extended literal instruction   
          then                                          \
      then                                              \
      drop                                              \ clean up stack
;

( Defining words for target                  JCB 19:04 05/02/12)

\ target data pointer as a jump address
: codeptr   tdp @ 2/ ;

: wordstr ( "name" -- c-addr u )
    >in @ >r bl word count r> >in !
;

variable link 0 link !

:: header
    twalign there
    \ cr ." link is " link @ .
    link @ tw,
    link !
    bl parse
    \ cr ." at " there . 2dup type tcp @ .
    dup tc,
    bounds do
        i c@ tc,
    loop
    twalign
;

:: header-imm
    twalign there
    link @ 1+ tw,
    link !
    bl parse
    dup tc,
    bounds do
        i c@ tc,
    loop
    twalign
;

variable wordstart

:: :
    hex
    there s>d
    <# bl hold # # # # #>
    lst @ write-file throw
    wordstr lst @ write-line throw

    there wordstart !
    create  codeptr ,
    does>
        @                   \ -- target 
        dup $c000 and if    \ check if address is within the direct address range -- target
	    literal execute \ use inirect addressing
        else                \
            call or tcode,  \ use direct addressing
        then                \
;

:: :noname
;

:: ,
    twalign
    tw,
;

:: allot
    0 ?do
        0 tc,
    loop
;

\ Shortcut implicetely done for the N1
\ : shortcut ( orig -- f ) \ insn @orig precedes ;. Shortcut it.
\     \ call becomes jump
\     dup t@ h# e000 and h# 4000 = if
\         dup t@ h# 1fff and over tw!
\         true
\     else
\         dup t@ h# e00c and h# 6000 = if
\             dup t@ h# 0080 or r-1 over tw!
\             true
\         else
\             false
\         then
\     then
\     nip
\ ;
\ 
\ :: ;
\     tdp @ wordstart @ = if
\         s" exit" evaluate
\     else
\         tdp @ 2 - shortcut \ true if shortcut applied
\         tdp @ 0 do
\             i tbranches @ tdp @ = if
\                 i tbranches @ shortcut and
\             then
\         loop
\         0= if   \ not all shortcuts worked
\             s" exit" evaluate
\         then
\     then
\ ;

:: ;
     tdp @ wordstart @ = if
         ret stack or ______________________ \ compile explicit reurn instruction	 
     else
         there 2 - tw@ ret or there 2 -tw!   \ set return bit in last compiled instruction 
     then

:: ;fallthru ;

:: jmp
    ' >body @ ubranch
;

:: constant
    create  ,
    does>   @ literal
;

:: create
    twalign
    create there ,
    does>   @ literal
;

:: inline:
    parse-name evaluate
    \ tcp @ tw! tcell tcp +! ;
    tdp @ 2 - >r
    r@ tw@ $8000 or r> tw!
    s" w," evaluate
;

\ usage "<variable> @i"
\ replaces the variable with an inline fetch using a high-call
\
:: @i
    tdp @ 2 - >r
    r@ tw@ $2000 + 2/ $4000 or r> tw!
;

( Switching between target and meta          JCB 19:08 05/02/12)

: target    only target-wordlist add-order definitions ;
: ]         target ;
:: meta     forth definitions ;
:: [        forth definitions ;

: t'        bl parse target-wordlist search-wordlist 0= throw >body @ ;

( eforth's way of handling constants         JCB 13:12 09/03/10)

: sign>number   ( c-addr1 u1 -- ud2 c-addr2 u2 )
    0. 2swap
    over c@ [char] - = if
        1 /string
        >number
        2swap dnegate 2swap
    else
        >number
    then
;

: base>number   ( caddr u base -- )
    base @ >r base !
    sign>number
    r> base !
    dup 0= if
        2drop drop literal
    else
        1 = swap c@ [char] . = and if
            drop dup literal 32 rshift literal
        else
            -1 abort" bad number"
        then
    then ;
warnings on

:: d# bl parse 10 base>number ;
:: h# bl parse 16 base>number ;
:: ['] ' >body @ 2* literal ;
:: [char] char literal ;

:: asm-0branch
    ' >body @
    0branch
;

( Conditionals                               JCB 13:12 09/03/10)

: resolve ( orig -- )
    \ forward reference from orig to this loc (tgt)
    tdp @                  \ -- orig tgt
    over                   \ -- orig tgt orig
    tbranches !            \ store tgt in branch list -- orig
    dup                    \ -- orig orig
    t@                     \ -- orig opcode
    tdp @                  \ -- orig opcode tgt
    2/ or                  \ -- orig resolved-opcode
    swap tw!               \ resolve opcode --
;

:: if
    tdp @
    0 0branch
;

:: DOUBLE
    tdp @ 2/ 1+ scall
;

:: then
    resolve
;

:: else
    tdp @
    0 ubranch 
    swap resolve
;

:: begin tdp @ ;

:: again ( dest -- )
    2/ ubranch
;
:: until
    2/ 0branch
;
:: while
    tdp @
    0 0branch
;
:: repeat
    swap 2/ ubranch
    resolve
;

4 org
: .trim ( a-addr u ) \ shorten string until it ends with '.'
    begin
        2dup + 1- c@ [char] . <>
    while
        1-
    repeat
;

( Strings                                    JCB 11:57 05/18/12)

: >str ( c-addr u -- str ) \ a new u char string from c-addr
    dup cell+ allocate throw dup >r
    2dup ! cell+    \ write size into first cell
                    ( c-addr u saddr )
    swap cmove r>
;
: str@  dup cell+ swap @ ;
: str! ( str c-addr -- c-addr' ) \ copy str to c-addr
    >r str@ r>
    2dup + >r swap
    cmove r>
;
: +str ( str2 str1 -- str3 )
    over @ over @ + cell+ allocate throw >r
    over @ over @ + r@ !
    r@ cell+ str! str! drop r>
;

: example
    s"  sailor" >str
    s" hello" >str
    +str str@ type
;

next-arg 2dup .trim >str constant prefix.
: .suffix  ( c-addr u -- c-addr u ) \ e.g. "bar" -> "foo.bar"
    >str prefix. +str str@
;
: create-output-file w/o create-file throw ;
: out-suffix ( s -- h ) \ Create an output file h with suffix s
    >str
    prefix. +str
    s" build/" >str +str str@
    create-output-file
;
:noname
    s" lst" out-suffix lst !
; execute


target included                         \ include the program.fs

[ tdp @ 0 org ] main quit [ org ]
meta

decimal
0 value file
: dumpall.16
    s" hex" out-suffix to file

    hex
    4096 0 do
        tflash i 2* + w@
        s>d <# # # # # #> file write-line throw
    loop
    file close-file
;
: dumpall.32
    s" hex" out-suffix to file

    hex
    8192 0 do
        tflash i 4 * + @
        s>d <# # # # # # # # # #> file write-line throw
    loop
    file close-file
;

dumpall.16
." tdp " tdp @ .
." tcp " tcp @ .

bye
