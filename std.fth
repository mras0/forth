: DUP DSP@ @ ;

: IF IMMEDIATE ' 0BRANCH , HERE @ 0 , ;
: THEN IMMEDIATE DUP HERE @ SWAP - SWAP ! ;
: ELSE IMMEDIATE ' BRANCH , HERE @ 0 , SWAP DUP HERE @ SWAP - SWAP ! ;

: Y 0 IF 42 THEN ; Y
: X 0 IF 42 ELSE 33 THEN ; X EMIT 10 EMIT
: Z 1 IF 42 ELSE 33 THEN ; Z EMIT 10 EMIT

: 1+ 1 + ;
: 1- 1 - ;
: CELL+ CELL-SIZE + ;
: CELLS CELL-SIZE * ;

: OVER 2 CELLS DSP@ + @ ;
: 2DUP OVER OVER ;
: DROP DSP@ CELL+ DSP! ;
: 2DROP DROP DROP ;

: DO IMMEDIATE HERE @ ' >R , ' >R , ;
: I IMMEDIATE ' RSP@ , ' CELL+ , ' @ , ;
: J IMMEDIATE ' RSP@ , ' LIT , 3 CELLS , ' + , ' @ , ;
: LOOP IMMEDIATE ' R> , ' R> , ' 1+ , ' 2DUP , ' = , ' 0BRANCH , HERE @ - , ' 2DROP , ;

: BEGIN IMMEDIATE HERE @ ;
: UNTIL IMMEDIATE ' 0BRANCH , HERE @ - , ;
: AGAIN IMMEDIATE ' BRANCH , HERE @ - , ;

: WHILE IMMEDIATE ' 0BRANCH , HERE @ 0 , ;
: REPEAT IMMEDIATE ' BRANCH , SWAP HERE @ - , DUP HERE @ SWAP - SWAP ! ;

: / /MOD SWAP DROP ;
: MOD /MOD DROP ;

: NEGATE 0 SWAP - ;

: 0= 0 = ;
: 0< 0 < ;
: 0> 0 > ;
: 0<> 0 <> ;

: FALSE 0 ;
: TRUE 0 INVERT ;
: NOT 0= ;

: LITERAL IMMEDIATE ' LIT , , ;

: RECURSE IMMEDIATE LATEST @ >CFA , ;

: [COMPILE] IMMEDIATE WORD FIND >CFA , ;

: DECIMAL 10 BASE ! ;
: HEX 16 BASE ! ;

: '\n' 10 ;
: BL 32 ;
: CR '\n' EMIT ;
: SPACE BL EMIT ;

: ':' [ CHAR : ] LITERAL ;
: ';' [ CHAR ; ] LITERAL ;
: '(' [ CHAR ( ] LITERAL ;
: ')' [ CHAR ) ] LITERAL ;
: '"' [ CHAR " ] LITERAL ;
: 'A' [ CHAR A ] LITERAL ;
: '0' [ CHAR 0 ] LITERAL ;
: '-' [ CHAR - ] LITERAL ;
: '.' [ CHAR . ] LITERAL ;

'A' EMIT '0' EMIT CR \ Print some characters

: ( IMMEDIATE 1 BEGIN KEY DUP '(' = IF DROP 1+ ELSE ')' = IF 1- THEN THEN DUP 0= UNTIL DROP ;

: SPACES BEGIN DUP 0> WHILE SPACE 1- REPEAT DROP ;

: ?DUP DUP 0<> IF DUP THEN ;
: -ROT ROT ROT ;    \ ( a b c -- c a b )
: NIP SWAP DROP ;   \ ( a b -- b )
: TUCK SWAP OVER ;  \ ( a b -- b a b )
: PICK 1+ CELLS DSP@ + @ ;

: WITHIN -ROT OVER <= IF > IF TRUE ELSE FALSE THEN ELSE 2DROP FALSE THEN ;

\ Printing of numbers
: U. BASE @ U/MOD ?DUP IF RECURSE THEN DUP 10 < IF '0' ELSE 10 - 'A' THEN + EMIT ;
: UWIDTH BASE @ / ?DUP IF RECURSE 1+ ELSE 1 THEN ;
: U.R SWAP DUP UWIDTH ROT SWAP - SPACES U. ;
: .R SWAP DUP 0< IF NEGATE 1 SWAP ROT 1- ELSE 0 SWAP ROT THEN SWAP DUP UWIDTH ROT SWAP - SPACES SWAP IF '-' EMIT THEN U. ;
: . 0 .R SPACE ;
: U. U. SPACE ;

100 U. 132456 . -42 . CR \ Test printing

\ : ALIGNED CELL-SIZE 1- + CELL-SIZE NEGATE AND ;
: ALIGN HERE @ ALIGNED HERE ! ;

: +! DUP >R @ + R> ! ;
: C, HERE @ C! 1 HERE +! ;
: C@++ DUP C@ SWAP 1+ SWAP ;

\ Strings
: S" IMMEDIATE
    STATE @ IF
        ' LITSTRING , HERE @ 0 , BEGIN KEY DUP '"' <> WHILE C, REPEAT DROP DUP HERE @ SWAP - CELL-SIZE - SWAP ! ALIGN
    ELSE
        HERE @ BEGIN KEY DUP '"' <> WHILE OVER C! 1+ REPEAT DROP HERE @ - HERE @ SWAP
    THEN ;
: TELL BEGIN ?DUP 0<> WHILE SWAP C@++ EMIT SWAP 1- REPEAT DROP ;

LATEST @ CELL+ C@++ TELL CR
S" HELLO WORLD!" TELL CR

: STRTEST S" HELLO FORTH" TELL CR ; STRTEST

: ." IMMEDIATE [COMPILE] S" STATE @ IF ' TELL , ELSE TELL THEN ;

." ANOTHER HELLO world!" CR

\ Constants/Variables/Value
: MAKE-LIT-FUNC WORD CREATE DOCOL , ' LIT , , ' EXIT ,  ;
: CONSTANT MAKE-LIT-FUNC ;
: ALLOT HERE @ SWAP HERE +! ;
: VARIABLE 1 CELLS ALLOT MAKE-LIT-FUNC ;
: VALUE MAKE-LIT-FUNC ;
: TO WORD FIND >CFA 2 CELLS + ! ;

42 CONSTANT MEANING
MEANING EMIT CR

\ Debug helpers
: DEPTH S0 DSP@ - CELL-SIZE / 1- ;
: .S DSP@ BEGIN DUP S0 < WHILE DUP @ U. SPACE CELL+ REPEAT DROP ;

42 1337 .S CR DROP DROP

: ID. CELL+ DUP C@ F_LENMASK AND BEGIN DUP 0> WHILE SWAP 1+ DUP C@ EMIT SWAP 1- REPEAT 2DROP ;
: ?HIDDEN CELL+ C@ F_HIDDEN AND ;
: ?IMMEDIATE CELL+ C@ F_IMMED AND ;
: WORDS LATEST @ BEGIN ?DUP WHILE DUP ?HIDDEN NOT IF DUP ID. SPACE THEN @ REPEAT CR ;

\ Case-statement
: CASE IMMEDIATE 0 ;
: OF IMMEDIATE ' OVER , ' = , [COMPILE] IF ' DROP , ;
: ENDOF IMMEDIATE [COMPILE] ELSE ;
: ENDCASE IMMEDIATE ' DROP , BEGIN ?DUP WHILE [COMPILE] THEN REPEAT ;

: :NONAME 0 0 CREATE HERE @ DOCOL , ] ;
: ['] IMMEDIATE ' LIT , ;

\ Run tests now that the standard functions have been defined, if possible
: TRY-RUN-TESTS S" RUN-TESTS" FIND ?DUP IF >CFA EXECUTE THEN ;
TRY-RUN-TESTS

( Print 0-padded unsigned number )
: U.0       ( u width -- )
    DUP 0= IF 2DROP EXIT THEN
    1-
    SWAP        ( width u )
    BASE @
    U/MOD       ( width rem quot )
    ROT         ( rem quot width )
    RECURSE
    DUP 9 > IF
        10 - 'A'
    ELSE
        '0'
    THEN
    +
    EMIT
;

: DUMP      ( addr len -- )
    BASE @ -ROT   ( save the current BASE at the bottom of the stack )
    HEX           ( and switch to hexadecimal mode )

    BEGIN
        ?DUP      ( while len > 0 )
    WHILE
        OVER CELL-SIZE 2 * U.0  ( print the address )
        SPACE

        ( print up to 16 words on this line )
        2DUP      ( addr len addr len )
        1- 15 AND 1+  ( addr len addr linelen )
        BEGIN
            ?DUP      ( while linelen > 0 )
        WHILE
            SWAP          ( addr len linelen addr )
            DUP C@        ( addr len linelen addr byte )
            2 U.0 SPACE    ( print the byte )
            1+ SWAP 1-    ( addr len linelen addr -- addr len addr+1 linelen-1 )
        REPEAT
        DROP      ( addr len )

        ( print the ASCII equivalents )
        2DUP 1- 15 AND 1+ ( addr len addr linelen )
        BEGIN
            ?DUP        ( while linelen > 0)
        WHILE
            SWAP        ( addr len linelen addr )
            DUP C@      ( addr len linelen addr byte )
            DUP 32 128 WITHIN IF  ( 32 <= c < 128? )
                EMIT
            ELSE
                DROP '.' EMIT
            THEN
            1+ SWAP 1-    ( addr len linelen addr -- addr len addr+1 linelen-1 )
        REPEAT
        DROP      ( addr len )
        CR

        DUP 1- 15 AND 1+ ( addr len linelen )
        TUCK      ( addr linelen len linelen )
        -         ( addr linelen len-linelen )
        >R + R>   ( addr+linelen len-linelen )
    REPEAT

    DROP          ( restore stack )
    BASE !        ( restore saved BASE )
;

: >DFA
    >CFA
    CELL+
;

: CFA>
    LATEST @        ( start at LATEST dictionary entry )
    BEGIN
        ?DUP            ( while link pointer is not null )
    WHILE
        2DUP SWAP       ( cfa curr curr cfa )
        < IF            ( current dictionary entry < cfa? )
            NIP             ( leave curr dictionary entry on the stack )
            EXIT
        THEN
        @               ( follow link pointer back )
    REPEAT
    DROP            ( restore stack )
    0               ( sorry, nothing found )
;

: SEE
    WORD FIND

    ( Now we search again, looking for the next word in the dictionary.  This gives us
      the length of the word that we will be decompiling.  (Well, mostly it does). )
    HERE @      ( address of the end of the last compiled word )
    LATEST @    ( word last curr )
    BEGIN
        2 PICK      ( word last curr word )
        OVER        ( word last curr word curr )
        <>      ( word last curr word<>curr? )
    WHILE           ( word last curr )
        NIP     ( word curr )
        DUP @       ( word curr prev (which becomes: word last curr) )
    REPEAT

	( begin the definition with : NAME [IMMEDIATE] )
	':' EMIT SPACE DUP ID. SPACE
	DUP ?IMMEDIATE IF ." IMMEDIATE " THEN

	>DFA		( get the data address, ie. points after DOCOL | end-of-word start-of-data )

	( now we start decompiling until we hit the end of the word )
	BEGIN		( end start )
		2DUP >
	WHILE
		DUP @		( end start codeword )

		CASE
		' LIT OF		    ( is it LIT ? )
			CELL+ DUP @		( get next word which is the integer constant )
			.			    ( and print it )
		ENDOF
		' LITSTRING OF		( is it LITSTRING ? )
			[ CHAR S ] LITERAL EMIT '"' EMIT SPACE ( print S"<space> )
			CELL+ DUP @		( get the length word )
			SWAP CELL+ SWAP	( end start+cellsize length )
			2DUP TELL		( print the string )
			'"' EMIT SPACE		( finish the string with a final quote )
			+ ALIGNED		( end start+cell-size aligned )
			CELL-SIZE -		( because we're about to add cell width below )
		ENDOF
		' 0BRANCH OF		( is it 0BRANCH ? )
			." 0BRANCH ( "
			CELL+ DUP @		( print the offset )
			.
			." ) "
		ENDOF
		' BRANCH OF		    ( is it BRANCH ? )
			." BRANCH ( "
			CELL+ DUP @		( print the offset )
			.
			." ) "
		ENDOF
		' ' OF			    ( is it ' (TICK) ? )
			[ CHAR ' ] LITERAL EMIT SPACE
			CELL+ DUP @		( get the next codeword )
			CFA>			( and force it to be printed as a dictionary entry )
			ID. SPACE
		ENDOF
		' EXIT OF		( is it EXIT? )
			( We expect the last word to be EXIT, and if it is then we don't print it
			  because EXIT is normally implied by ;.  EXIT can also appear in the middle
			  of words, and then it needs to be printed. )
			2DUP			( end start end start )
			CELL+			( end start end start+cellsize )
			<> IF			( end start | we're not at the end )
				." EXIT "
			THEN
		ENDOF
					( default case: )
			DUP			( in the default case we always need to DUP before using )
			CFA>			( look up the codeword to get the dictionary entry )
			ID. SPACE		( and print it )
		ENDCASE

		CELL+		( end start+cellsize )
	REPEAT

	';' EMIT CR

	2DROP		( restore stack )
    DROP        ( drop word )
;

: RDROP R> DROP ;
: EXCEPTION-MARKER RDROP 0 ; ( Only executed if no exception is thrown, drop data stack pointer from return stack )
: CATCH
    DSP@ CELL+ >R                ( Push datastack pointer (without execution token) )
    ' EXCEPTION-MARKER CELL+ >R  ( Push address of "RDROP" in EXCEPTION-MARKER as return address )
    EXECUTE                      ( Execute function )
;
: THROW
    ?DUP IF
        RSP@
        BEGIN
            DUP CELL+ R0 < ( RSP < R0 )
        WHILE
            DUP @
            ' EXCEPTION-MARKER CELL+ = IF
                CELL+ RSP!  ( Restore return stack pointer )
                DUP DUP DUP ( Reserve data stack space for below operation )
                R>          ( Get original data stack pointer)
                CELL-SIZE - ( Reserve space for result )
                SWAP OVER   ( dsp result dsp )
                !           ( Store result on stack )
                DSP! EXIT   ( Restore stack and exit )
            THEN
            CELL+
        REPEAT

        DROP
        CASE
            -1 OF ." ABORTED" CR ENDOF
                  ." UNCAUGHT THROW " DUP . CR
        ENDCASE
        QUIT
    THEN
;

: ABORT -1 THROW ;

: PRINT-STACK-TRACE
	RSP@				( start at caller of this function )
	BEGIN
		DUP CELL+ R0 <		( RSP < R0 )
	WHILE
		DUP @			( get the return stack entry )
		CASE
		' EXCEPTION-MARKER CELL+ OF	( is it the exception stack frame? )
			." CATCH ( DSP="
			CELL+ DUP @ U.		( print saved stack pointer )
			." ) "
		ENDOF
						( default case )
			DUP
			CFA>			( look up the codeword to get the dictionary entry )
			?DUP IF			( and print it )
				2DUP		( dea addr dea )
				ID.			( print word from dictionary entry )
				[ CHAR + ] LITERAL EMIT
				SWAP >DFA CELL+ - CELL-SIZE / .	( print offset )
			THEN
		ENDCASE
		CELL+			( move up the stack )
	REPEAT
	DROP
	CR
;

: Z" IMMEDIATE
    STATE @ IF
        HERE @ CELL+  ( Point at string length cell )
        [COMPILE] S"
        DUP 1 SWAP +! ( Update length )
        DUP @ CELL+ + ( Get end of string )
        1- HERE !     ( "Unalign" Here )
        0 C,          ( NUL-terminate )
        ALIGN         ( Re-align)
        ' DROP ,      ( Drop length when executing word )
    ELSE
        [COMPILE] S"
        OVER + 0 SWAP C!
    THEN
;


: STRLEN
    0
    BEGIN               ( str len )
        SWAP C@++ 0<>   ( len str+1 nul? )
    WHILE
        SWAP 1+         ( str+1 len+1 )
    REPEAT
    DROP ( Drop str )
;

SEE DEPTH

: FOO ( n -- ) PRINT-STACK-TRACE THROW ;

: TEST-EXCEPTIONS
    25 ['] FOO CATCH	\ execute 25 FOO, catching any exception
    ?DUP IF
        ." called FOO and it threw exception number: "
        . CR
        DROP		\ we have to drop the argument of FOO (25)
    THEN
;

TEST-EXCEPTIONS
-1 FOO

: FOO Z" foo bar !!! " DUP STRLEN 1+ DUMP ; FOO

( ******************************************************************* )

HEX

: RAX IMMEDIATE 0 ;
: RCX IMMEDIATE 1 ;
: RDX IMMEDIATE 2 ;
: RBX IMMEDIATE 3 ;
: RSP IMMEDIATE 4 ;
: RBP IMMEDIATE 5 ;
: RSI IMMEDIATE 6 ;
: RDI IMMEDIATE 7 ;
: R8  IMMEDIATE 8 ;
: R9  IMMEDIATE 9 ;
: R10 IMMEDIATE A ;
: R11 IMMEDIATE B ;
: R12 IMMEDIATE C ;
: R13 IMMEDIATE D ;
: R14 IMMEDIATE E ;
: R15 IMMEDIATE F ;

41 CONSTANT REX.B
48 CONSTANT REX.W

( reg -- reg rex )
: REXREG
    DUP 8 >= IF
        7 AND
        REX.B
    ELSE
        0
    THEN
;

: ?REX.B
    REXREG ?DUP IF C, THEN
;

: IMM8  C, ;
: IMM16 DUP C, 100 / C, ;
: IMM32 DUP IMM16 10000 / IMM16 ;

( **** INSTRUCTIONS **** )

: RET IMMEDIATE C3 C, ;

: INT3 IMMEDIATE CC C, ;

: LODSD IMMEDIATE AD C, ;
: LODSQ IMMEDIATE REX.W C, [COMPILE] LODSD ;

: PUSH IMMEDIATE ?REX.B 50 OR C, ;
: POP IMMEDIATE ?REX.B 58 OR C, ;

: MOVID IMMEDIATE
    ?REX.B B8 OR C, IMM32
;

: MOVIQ IMMEDIATE
    REXREG REX.W OR C, B8 OR C, ,
;

: CALL-REG IMMEDIATE
    ?REX.B
    FF C, D0 OR C,
;

: CALL-REL IMMEDIATE
    ( CALL rel32 )
    E8 C, HERE @ 5 + SWAP - IMM32
;

( Note: Since it doesn't update "LastCFA" only some words can be called )
: CALL-WORD IMMEDIATE
    WORD FIND CFA> [COMPILE] CALL-REL
;

( **** FUNCTION BUILDING **** )

( Argument: Number of local stack bytes to allocate )
: PROLOG IMMEDIATE
    ALIGNED
    20 + ( Make room for home space )
    DUP F AND 0= IF CELL+ THEN
    DUP REX.W C, 83 C, EC C, IMM8 ( SUBQ $imm8, %RSP )
    ( Leave adjustment on stack )
;

: EPILOG IMMEDIATE
    REX.W C, 83 C, C4 C, IMM8 ( ADDQ $imm8, %RSP )
;

: ;CODE IMMEDIATE
    [COMPILE] RET        ( End function with RET )
    ALIGN                ( Align )
    LATEST @ DUP
    HIDDEN               ( Unhide function - need to be done here since ; isn't called )
    DUP >DFA SWAP >CFA ! ( Point CFA at instruction area )
    [COMPILE] [          ( Back to immediate mode )
;

: DPOP IMMEDIATE
    DSP [COMPILE] RCX [COMPILE] MOVIQ
    REX.W C, 8B C, 31 C, ( MOVQ (%RCX), %RSI )
    [COMPILE] LODSQ
    REX.W C, 89 C, 31 C, ( MOVQ %RSI, (%RCX) )
;

: CALL1
    [ 0 ] PROLOG
    DPOP
    RAX PUSH
    DPOP
    [ REX.W C, 89 C, C1 C, ] ( MOV %RAX, %RCX )
    RAX POP
    RAX CALL-REG
    EPILOG
;CODE

DECIMAL

( ******************************************************************* )

S" CALL1" FIND >DFA 128 DUMP

\ : TestWin64
\     CELL-SIZE 8 <> IF EXIT THEN
\     123 Z" ExitProcess" KERNEL32 GETPROC CALL1
\ ;


Z" ../std.fth" O_RDONLY OPEN-FILE CONSTANT FD
HERE @ 1000 FD READ-FILE
HERE @ SWAP STDOUT WRITE-FILE DROP
FD CLOSE-FILE

\ TestWin64
