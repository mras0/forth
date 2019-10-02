#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <assert.h>
#include <string.h>
#include <stdbool.h>
#include <stdint.h>
#include <inttypes.h>
#include <fcntl.h>

#ifdef _MSC_VER
#include <io.h>
#define open   _open
#define close  _close
#define read   _read
#define write  _write
#define STDIN_FILENO _fileno(stdin)
#define STDOUT_FILENO _fileno(stdout)
#else
#include <unistd.h>
#endif

#define TESTS

#ifdef _MSC_VER
#define NORETURN __declspec(noreturn)
#elif defined(__GNUC__)
#define NORETURN __attribute__((__noreturn__))
#else
#define NORETURN
#endif

#define F_IMMED   0x80
#define F_HIDDEN  0x40
#define F_LENMASK 0x1F

typedef struct WordHeader {
    struct WordHeader* Prev; // Points to previous word (or NULL)
    uint8_t LengthAndFlags;  // Length possibly OR'ed with F_IMMED AND/OR F_HIDDEN
    char    Name[1];         // Note length can actually be 0 (for :NONAME)
    // ....
    // CodeWordFunc CodeWord;        <-- CFA
    // Cell* Instructions[...];      <-- Points to CFAs of instructions
} WordHeader;

typedef void (*CodeWordFunc)(void);

typedef union Cell {
    CodeWordFunc CodeWord;
    intptr_t     Number;
    uint8_t      Raw[sizeof(intptr_t)];
    union Cell*  CellPtr;
} Cell;

// Check that the Cell union has the expected size
typedef char CellHasRightSize[sizeof(Cell) == sizeof(intptr_t) ? 1 : -1];

//
// Memory layout:
//
// Return stack bottom          0
// ...
// First return stack cell      RSP_CELLS-1
// Words/Data start             RSP_CELLS     <- R0
// ...
// Data stack bottom            ???
// ...
// First data stack cell        MAX_CELLS-1 
//                              MAX_CELLS     <- S0

#define RSP_CELLS     64
#define MAX_CELLS     4096

#define R0 (&Cells[RSP_CELLS])
#define S0 (&Cells[MAX_CELLS])

#define STATE_INTERPRET 0
#define STATE_COMPILE   1

static Cell         Cells[4096];
static Cell*        RStackTop = R0;  // EBP in jonesforth
static Cell*        DStackTop = S0;  // ESP in jonesforth
static Cell*        IP;              // ESI in jonesforth
static Cell*        LastCFA;         // EAX (in certain cases) in jonesforth
static uint8_t*     Here = (uint8_t*)R0;
static WordHeader*  Latest = NULL;
static intptr_t     Base = 10;
static intptr_t     State = STATE_INTERPRET;
static intptr_t     EofFlag;
static uint8_t      WordLen;
static char         WordBuffer[F_LENMASK+1]; // +1 for NUL terminator (to ease debugging in C)
static const char*  InputBuffer;
static FILE*        InputFile;

// Stack operations
#define RPUSH(val) (assert(RStackTop > &Cells[0] && "Return stack overflow"), (*--RStackTop).CellPtr = (Cell*)(val))
#define RPOP()     (assert(RStackTop < R0 && "Return stack underflow"), (*RStackTop++).CellPtr)
#define DPUSH(val) (assert((uint8_t*)DStackTop > Here && "Data stack underflow"), (*--DStackTop).Number = (intptr_t)(val))
#define DPOP()     (assert(DStackTop < S0 && "Data stack underflow"), (*DStackTop++).Number)

// Almost corresponds to the NEXT macro in the assembly versions.
// But since we can't do direct threading, there's an infinite loop
// in InterpreterLoop instead.
#define NEXT(CFA)  (LastCFA = (Cell*)(CFA), LastCFA->CodeWord())

NORETURN static void Error(const char* format, ...)
{
    va_list vl;
    va_start(vl, format);
    vfprintf(stderr, format, vl);
    fputs("", stderr);
    va_end(vl);
    abort();
}

static WordHeader* FindWord(const char* Name, uint8_t NameLen)
{
    for (WordHeader* W = Latest; W; W = W->Prev) {
        uint8_t* N = (uint8_t*)W  + sizeof(Cell);
        if ((N[0]&(F_LENMASK|F_HIDDEN)) == NameLen && memcmp(N+1, Name, NameLen) == 0) {
            return W;
        }
    }
    return NULL;
}

static WordHeader* FindWordZ(const char* Name)
{
    assert(strlen(Name) <= F_LENMASK);
    return FindWord(Name, (uint8_t)strlen(Name));
}

static uint8_t ReadKey(void)
{
    if (InputBuffer) {
        return *InputBuffer ? *InputBuffer++ : 0;
    }
    assert(InputFile);
    if (feof(InputFile)) {
        return 0;
    }
    int ch = fgetc(InputFile);
    if (ch == EOF) {
        return 0;
    }
    return (uint8_t)ch;
}

static void ReadWord(void)
{
    uint8_t C;
SkipSpaces:
    while ((C = ReadKey()) != 0 && C <= ' ') {
    }
    if (C == '\\') {
        while ((C = ReadKey()) != 0 && C != '\n') {
        }
        goto SkipSpaces;
    }
    WordLen = 0;
    while (C > ' ' && WordLen < F_LENMASK) {
        if (C >= 'a' && C <= 'z') {
            C &= 0xdf;
        }
        WordBuffer[WordLen++] = C;
        C = ReadKey();
    }
    WordBuffer[WordLen] = '\0'; // NUL termination is only for C's sake
    //printf("Read \"%s\"\n", WordBuffer);
}

static bool ConvertNumber(const char* Buffer, uint8_t BufferLen, intptr_t* Res)
{
    if (!BufferLen) {
        return false;
    }
    bool neg = false;
    if (*Buffer == '-') {
        if (!--BufferLen) {
            return false;
        }
        ++Buffer;
        neg = true;
    }
    *Res = 0;
    do {
        uint8_t Digit = 0;
        if (*Buffer < '0') {
            return false;
        } else if (*Buffer <= '9') {
            Digit = *Buffer - '0';
        } else if (*Buffer >= 'A' && *Buffer <= 'Z') {
            Digit = 10 + *Buffer - 'A';
        } else {
            return false;
        }
        if (Digit >= Base) {
            return false;
        }
        *Res = *Res * Base + Digit;
        ++Buffer;
    } while (--BufferLen);
    if (neg) {
        *Res = -*Res;
    }
    return true;
}

static uint8_t* Align(uint8_t* ptr)
{
    return (uint8_t*)(((uintptr_t)ptr + sizeof(Cell) - 1) & (~sizeof(Cell) + 1));
}

#define IS_ALIGNED(ptr) ((uint8_t*)(ptr) == Align((uint8_t*)(ptr)))
#define PUT_HERE(type, val) (*(type*)Here = (val), Here += sizeof(type))

static Cell* WordCFA(WordHeader* WH)
{
    return (Cell*)Align((uint8_t*)WH + sizeof(Cell) + 1 + (WH->LengthAndFlags & F_LENMASK));
}

static void CreateWord(const char* Name, intptr_t NameLen)
{
    assert(NameLen >= 0 && NameLen <= F_LENMASK);
    assert(IS_ALIGNED(Here));
    WordHeader* WH = (WordHeader*)Here;
    WH->Prev = Latest;
    WH->LengthAndFlags = (uint8_t)NameLen;
    memcpy(WH->Name, Name, NameLen);
    Here = (uint8_t*)WordCFA(WH);
    Latest = WH;
}

static void DefineNativeWord(const char* Name, CodeWordFunc CodeWord)
{
    CreateWord(Name, strlen(Name));
    PUT_HERE(CodeWordFunc, CodeWord);
}

static void DoCol(void)
{
    RPUSH(IP);
    IP = LastCFA + 1;
}

///////////////////////////////////////////
/// Native words
///////////////////////////////////////////

static void Exit(void)
{
    IP = RPOP();
}

static void Branch(void)
{
    assert(IP->Number % sizeof(Cell) == 0);
    IP += IP->Number / (intptr_t)sizeof(Cell);
}

static void ZBranch(void)
{
    if (!DPOP()) {
        Branch();
    } else {
        ++IP; // Skip offset
    }
}

static void Lit(void)
{
    DPUSH((IP++)->Number);
}

static void LitString(void)
{
    const intptr_t Len = (IP++)->Number;
    DPUSH(IP);
    DPUSH(Len);
    IP += (Len+sizeof(Cell)-1)/sizeof(Cell);
}

static void Char(void)
{
    ReadWord();
    DPUSH((uint8_t)*WordBuffer);
}

static void Emit(void)
{
    putchar((int)DPOP());
}

static void Key(void)
{
    DPUSH(ReadKey());
}

static void FileOpen(void)
{
    const int mode = (int)DPOP();
    const char* filename = (const char*)DPOP();
    DPUSH(open(filename, mode));
}

static void FileClose(void)
{
    close((int)DPOP());
}

static void FileRead(void)
{
    const int fd = (int)DPOP();
    const unsigned count = (unsigned)DPOP();
    void* buf = (void*)DPOP();
    DPUSH(read(fd, buf, count));
}

static void FileWrite(void)
{
    const int fd = (int)DPOP();
    const unsigned count = (unsigned)DPOP();
    void* buf = (void*)DPOP();
    DPUSH(write(fd, buf, count));
}

static void Interpret(void)
{
    ReadWord();
    if (!WordLen) {
        EofFlag = 1;
        return;
    }
    WordHeader* WH = FindWord(WordBuffer, WordLen);
    if (WH) {
        Cell* CFA = WordCFA(WH);
        if ((WH->LengthAndFlags & F_IMMED) || State == STATE_INTERPRET) {
            NEXT(CFA);
        } else {
            PUT_HERE(Cell*, CFA);
        }
    } else {
        intptr_t Number;
        if (!ConvertNumber(WordBuffer, WordLen, &Number)) {
            Error("Invalid word \"%s\"", WordBuffer);
        }
        if (State == STATE_INTERPRET) {
            DPUSH(Number);
        } else {
            PUT_HERE(Cell*, WordCFA(FindWordZ("LIT")));
            PUT_HERE(intptr_t, Number);
        }
    }
}

static void Word(void)
{
    ReadWord();
    DPUSH(WordBuffer);
    DPUSH(WordLen);
}

static void Create(void)
{
    const intptr_t Len = DPOP();
    const char* Name = (const char*)DPOP();
    CreateWord(Name, Len);
}

static void Find(void)
{
    const intptr_t Len = DPOP();
    const char* Name = (const char*)DPOP();
    DPUSH(FindWord(Name, (uint8_t)Len));
}

static void Cfa(void)
{
    WordHeader* WH = (WordHeader*)DPOP();
    DPUSH(WordCFA(WH));
}

static void Comma(void)
{
    PUT_HERE(intptr_t, DPOP());
}

static void Tick(void)
{
    DPUSH((IP++)->Number);
}

static void Fetch(void)
{
    const intptr_t Addr = DPOP();
    DPUSH(*(intptr_t*)Addr);
}

static void Store(void)
{
    Cell* Addr = (Cell*)DPOP();
    Addr->Number = DPOP();
}

static void CFetch(void)
{
    const intptr_t Addr = DPOP();
    DPUSH(*(uint8_t*)Addr);
}

static void CStore(void)
{
    uint8_t* Addr = (uint8_t*)DPOP();
    *Addr = (uint8_t)DPOP();
}

static void DSPFetch(void)
{
    const intptr_t Temp = (intptr_t)DStackTop;
    DPUSH(Temp);
}

static void DSPStore(void)
{
    Cell* Temp = (Cell*)DPOP();
    DStackTop = Temp;
}

static void RSPFetch(void)
{
    DPUSH(RStackTop);
}

static void RSPStore(void)
{
    RStackTop = (Cell*)DPOP();
}

static void RTo(void)
{
    RPUSH(DPOP());
}

static void RFrom(void)
{
    DPUSH(RPOP());
}

static void Hidden(void)
{
    WordHeader* WH = (WordHeader*)DPOP();
    WH->LengthAndFlags ^= F_HIDDEN;
}

static void Immediate(void)
{
    Latest->LengthAndFlags ^= F_IMMED;
}

static void Swap(void)
{
    const intptr_t Temp = DStackTop->Number;
    DStackTop->Number = DStackTop[1].Number;
    DStackTop[1].Number = Temp;
}

static void Rot(void)
{
    const intptr_t a = DPOP(), b = DPOP(), c = DPOP();
    DPUSH(b); DPUSH(a); DPUSH(c);
}

static void Execute(void)
{
    NEXT(DPOP());
}

#define DEF_BINOP(Name, Op) static void Name(void) { const intptr_t r = DPOP(), l = DPOP(); DPUSH(l Op r); }
DEF_BINOP(Add, +)
DEF_BINOP(Sub, -)
DEF_BINOP(Mul, *)
DEF_BINOP(And, &)
DEF_BINOP(Or,  |)
DEF_BINOP(Xor, ^)
#undef DEF_BINOP
static void DivMod(void) {
    const intptr_t r = DPOP(), l = DPOP(); 
    DPUSH(l % r);
    DPUSH(l / r);
}
static void UDivMod(void) {
    const uintptr_t r = DPOP(), l = DPOP();
    DPUSH(l % r);
    DPUSH(l / r);
}

static void Invert(void) {
    DStackTop->Number ^= (intptr_t)-1;
}

#define DEF_RELOP(Name, Op)  static void Name(void) { const intptr_t r = DPOP(), l = DPOP(); DPUSH(l Op r ? ~(intptr_t)0 : 0); }
DEF_RELOP(Lt, <)
DEF_RELOP(Le, <=)
DEF_RELOP(Eq, ==)
DEF_RELOP(Ne, !=)
DEF_RELOP(Ge, >=)
DEF_RELOP(Gt, >)
#undef DEF_RELOP

static void Aligned(void)
{
    const intptr_t val = DPOP();
    DPUSH((val + sizeof(intptr_t) - 1) & -(intptr_t)sizeof(intptr_t));
}

#define W(N) (assert(FindWordZ(N)),(intptr_t)WordCFA(FindWordZ(N)))
#define ENDW ((intptr_t)-43) // Something that's unlikely to be used

static void InterpreterLoop(void)
{
    EofFlag = 0;
    NEXT(W("QUIT"));
    while (!EofFlag) {
        NEXT((IP++)->CellPtr);
    }
}

static void DefineWord(const char* Name, ...)
{
    va_list vl;
    va_start(vl, Name);
    CreateWord(Name, strlen(Name));
    PUT_HERE(CodeWordFunc, &DoCol);
    for (;;) {
        intptr_t N = va_arg(vl, intptr_t);
        if (N == ENDW) {
            break;
        }
        PUT_HERE(intptr_t, N);
    }
    va_end(vl);
}

static void DefineConst(const char* Name, intptr_t Value)
{
    DefineWord(Name, W("LIT"), Value, W("EXIT"), ENDW);
}

static void PlatformInit(void);

static void DefineBuiltins(void)
{
    DefineNativeWord("EXIT"       , &Exit);
    DefineNativeWord("EXECUTE"    , &Execute);
    DefineNativeWord("BRANCH"     , &Branch);
    DefineNativeWord("0BRANCH"    , &ZBranch);
    DefineNativeWord("LIT"        , &Lit);
    DefineNativeWord("LITSTRING"  , &LitString);
    DefineNativeWord("CHAR"       , &Char);
    DefineNativeWord("WORD"       , &Word);
    DefineNativeWord("CREATE"     , &Create);
    DefineNativeWord("EMIT"       , &Emit);
    DefineNativeWord("KEY"        , &Key);
    DefineNativeWord("INTERPRET"  , &Interpret);
    DefineNativeWord(">CFA"       , &Cfa);
    DefineNativeWord("FIND"       , &Find);
    DefineNativeWord(","          , &Comma);
    DefineNativeWord("'"          , &Tick);
    DefineNativeWord("@"          , &Fetch);
    DefineNativeWord("!"          , &Store);
    DefineNativeWord("C@"         , &CFetch);
    DefineNativeWord("C!"         , &CStore);
    DefineNativeWord("DSP@"       , &DSPFetch);
    DefineNativeWord("DSP!"       , &DSPStore);
    DefineNativeWord("RSP@"       , &RSPFetch);
    DefineNativeWord("RSP!"       , &RSPStore);
    DefineNativeWord(">R"         , &RTo);
    DefineNativeWord("R>"         , &RFrom);
    DefineNativeWord("HIDDEN"     , &Hidden);
    DefineNativeWord("IMMEDIATE"  , &Immediate); Immediate();
    DefineNativeWord("SWAP"       , &Swap);
    DefineNativeWord("ROT"        , &Rot);
    DefineNativeWord("+"          , &Add);
    DefineNativeWord("-"          , &Sub);
    DefineNativeWord("*"          , &Mul);
    DefineNativeWord("AND"        , &And);
    DefineNativeWord("OR"         , &Or);
    DefineNativeWord("XOR"        , &Xor);
    DefineNativeWord("/MOD"       , &DivMod);
    DefineNativeWord("U/MOD"      , &UDivMod);
    DefineNativeWord("INVERT"     , &Invert);
    DefineNativeWord("<"          , &Lt);
    DefineNativeWord("<="         , &Le);
    DefineNativeWord("="          , &Eq);
    DefineNativeWord("<>"         , &Ne);
    DefineNativeWord(">="         , &Ge);
    DefineNativeWord(">"          , &Gt);
    DefineNativeWord("ALIGNED"    , &Aligned);
    DefineNativeWord("OPEN-FILE"  , &FileOpen);
    DefineNativeWord("CLOSE-FILE" , &FileClose);
    DefineNativeWord("READ-FILE"  , &FileRead);
    DefineNativeWord("WRITE-FILE" , &FileWrite);

    DefineConst("LATEST"    , (intptr_t)&Latest);
    DefineConst("HERE"      , (intptr_t)&Here);
    DefineConst("BASE"      , (intptr_t)&Base);
    DefineConst("STATE"     , (intptr_t)&State);
    DefineConst("DSP"       , (intptr_t)&DStackTop);
    DefineConst("S0"        , (intptr_t)S0);
    DefineConst("R0"        , (intptr_t)R0);
    DefineConst("F_LENMASK" , F_LENMASK);
    DefineConst("F_IMMED"   , F_IMMED);
    DefineConst("F_HIDDEN"  , F_HIDDEN);
    DefineConst("DOCOL"     , (intptr_t)&DoCol);
    DefineConst("CELL-SIZE" , (intptr_t)sizeof(Cell));
    DefineConst("O_RDONLY"  , O_RDONLY);
    DefineConst("O_WRONLY"  , O_WRONLY);
    DefineConst("O_RDWR"    , O_RDWR);
    DefineConst("O_CREAT"   , O_CREAT);
    DefineConst("STDIN"     , STDIN_FILENO);
    DefineConst("STDOUT"    , STDOUT_FILENO);

    DefineWord("[",
        W("LIT"), (intptr_t)STATE_INTERPRET,
        W("LIT"), (intptr_t)&State, 
        W("!"), W("EXIT"), ENDW); Immediate();

    DefineWord("]",
        W("LIT"), (intptr_t)STATE_COMPILE,
        W("LIT"), (intptr_t)&State, 
        W("!"), W("EXIT"), ENDW);

    DefineWord(":",
        W("WORD"), W("CREATE"),
        W("LIT"), (intptr_t)&DoCol, W(","),
        W("LATEST"), W("@"), W("HIDDEN"),
        W("]"), W("EXIT"), ENDW);

    DefineWord(";", W("LIT"), W("EXIT"), W(","), 
        W("LATEST"), W("@"), W("HIDDEN"), 
        W("["), W("EXIT"), ENDW); Immediate();

    DefineWord("QUIT", 
        W("R0"), W("RSP!"),
        W("INTERPRET"), W("BRANCH"), -(intptr_t)(2 * sizeof(Cell)),
        ENDW);

    PlatformInit();
}

static void RunFile(const char* Filename)
{
    if (!strcmp(Filename, "-")) {
        InputFile = stdin;
    } else {
        InputFile = fopen(Filename, "r");
        if (!InputFile) {
            Error("Could not open \"%s\"", Filename);
        }
    }
    InterpreterLoop();
    if (InputFile != stdin) {
        fclose(InputFile);
    }
    InputFile = NULL;
}

#ifdef TESTS
static void Test(const char* Text, const intptr_t* ExpectedStack, intptr_t ExpectedStackSize)
{
    uint8_t* SavedHere = Here;
    WordHeader* SavedLatest  = Latest;
    Cell *OldDSP = DStackTop, *OldRSP = RStackTop;
    intptr_t OldBase = Base;
    Base = 10;

    InputBuffer = Text;
    InterpreterLoop();
    const intptr_t ActualStackSize = OldDSP - DStackTop;
    bool Eq = ActualStackSize == ExpectedStackSize;

    if (Eq) {
        for (intptr_t i = 0; i < ActualStackSize; ++i) {
            Eq &= ExpectedStack[i] == OldDSP[-i-1].Number;
        }
    }

    if (!Eq) {
        printf("Test \"%s\" failed:\n", Text);
        for (intptr_t i = 0, m = ActualStackSize > ExpectedStackSize ? ActualStackSize : ExpectedStackSize; i < m; ++i) {
            if (i < ActualStackSize && i < ExpectedStackSize && ExpectedStack[i] == OldDSP[-i-1].Number) {
                continue;
            }
            printf("%d: ", (int)i);
            if (i < ActualStackSize) printf("%0*" PRIXPTR, (int)(2*sizeof(intptr_t)), OldDSP[-i-1].Number);
            else                     printf("????????");
            printf(" != ");
            if (i < ExpectedStackSize) printf("%0*" PRIXPTR, (int)(2*sizeof(intptr_t)), ExpectedStack[i]);
            else                       printf("????????");
            printf("\n");
        }
        Error("Test failed");
    }

    Here = SavedHere;
    Latest = SavedLatest;
    DStackTop = OldDSP;
    RStackTop = OldRSP;
    Base = OldBase;
    InputBuffer = NULL;
    EofFlag = 0;
}

#define TEST(Input, ...) do {                                  \
    const intptr_t expected[] = { __VA_ARGS__ };               \
    Test(Input, expected, sizeof(expected)/sizeof(*expected)); \
} while (0)

void RunTests(void)
{
    puts("Running Tests...");
    TEST("42", 42);
    TEST("1 2 3", 1, 2, 3);
    TEST("1 2 3 SWAP", 1, 3, 2);
    TEST("1 2 3 DROP", 1, 2);
    TEST("1 2 DUP", 1, 2, 2);
    TEST("1 2 OVER", 1, 2, 1);
    TEST("1 2 3 ROT", 2, 3, 1);
    TEST("1 2 3 -ROT", 3, 1, 2);
    TEST(": X 1 2 ; X X", 1, 2, 1, 2);
    TEST(": X -42 ; : Y X X 1 ; Y Y", -42, -42, 1, -42, -42, 1);
    TEST("2 -3 +", -1);
    TEST("20 2 -", 18);
    TEST("3 4 *", 12);
    TEST("10 3 AND", 2);
    TEST("10 3 XOR", 9);
    TEST("10 3 OR", 11);
    TEST("123 10 /MOD", 3, 12);
    TEST("-42 10 /MOD", -2, -4);
    TEST("42 10 MOD", 2);
    TEST("42 10 /", 4);
    TEST("-3 INVERT", 2);
    TEST("42 NEGATE", -42);
    TEST("FALSE", 0);
    TEST("TRUE", ~(intptr_t)0);
    TEST("2 NOT", 0);
    TEST("0 NOT", ~(intptr_t)0);
    TEST("1 2 <", -1);
    TEST("2 1 <", 0);
    TEST("1 1 <", 0);
    TEST("2 1 <=", 0);
    TEST("1 1 <=", -1);
    TEST("2 2 =", -1);
    TEST("2 22 =", 0);
    TEST("3 3 =", -1);
    TEST("3 -3 =", 0);
    TEST("3 3 <>", 0);
    TEST("3 -3 <>", -1);
    TEST("3 4 >=", 0);
    TEST("4 3 >=", -1);
    TEST("3 4 >", 0);
    TEST("3 3 >", 0);
    TEST("4 3 >", -1);
    TEST("1 0=", 0);
    TEST("0 0=", ~(intptr_t)0);
    TEST("1 0<", 0);
    TEST("-1 0<", ~(intptr_t)0);
    TEST("1 0>", ~(intptr_t)0);
    TEST("-1 0>", 0);
    TEST("1 0<>", ~(intptr_t)0);
    TEST("0 0<>", 0);
    TEST(": X IF 2 THEN ; 0 X 1 333 X 3", 1, 2, 3);
    TEST(": X IF 42 ELSE 33 THEN ; 0 X 333 X ", 33, 42);
    TEST(": X IF 1 ELSE 2 ELSE 3 ELSE 4 ELSE 5 THEN ; 0 X 42 -1 X", 2, 4, 42, 1, 3, 5);
    TEST("4 1+", 5);
    TEST("4 1-", 3);
    TEST("1 2 2DUP", 1, 2, 1, 2);
    TEST("1 2 3 4 2DROP", 1, 2);
    TEST("STATE @", STATE_INTERPRET);
    TEST("20 CELLS", 20*sizeof(Cell));
    TEST("100 CELL+", 100+sizeof(Cell));
    TEST(": FOO 52 48 DO I LOOP ; FOO", '0', '1', '2', '3');
    TEST(": X 2 0 DO 1 -1 DO I J LOOP LOOP ; X", -1, 0, 0, 0, -1, 1, 0, 1);
    TEST(": X BEGIN 42 -1 UNTIL ; X", 42);
    TEST(": X 0 BEGIN 1+ DUP DUP 3 = UNTIL DROP ; X", 1, 2, 3);
    TEST(": X 0 BEGIN DUP 1 = IF EXIT ELSE 1+ THEN AGAIN ; X", 1);
    TEST(": GI3 BEGIN DUP 5 < WHILE DUP 1+ REPEAT ; 0 GI3", 0, 1, 2, 3, 4, 5);
    TEST(": GI3 BEGIN DUP 5 < WHILE DUP 1+ REPEAT ; 4 GI3", 4, 5);
    TEST(": GI3 BEGIN DUP 5 < WHILE DUP 1+ REPEAT ; 6 GI3", 6);
    TEST("CHAR * CHAR AB", 42, 65);
    TEST(": ':' [ CHAR : ] LITERAL ; ':'", ':');
    TEST(": FAC DUP 1 > IF DUP 1- RECURSE * THEN ; 1 FAC 4 FAC", 1, 24);
    TEST("1 ( 2 \n 42 ) 3", 1, 3);
    TEST("1 0 ?DUP", 1, 0);
    TEST("1 2 ?DUP", 1, 2, 2);
    TEST("1 2 NIP", 2);
    TEST("1 2 TUCK", 2, 1, 2);
    TEST("5 4 3 2 1 3 PICK", 5, 4, 3, 2, 1, 4);
    TEST("BASE @", 10);
    TEST("HEX BASE @", 16);
    TEST("1234 UWIDTH", 4);
    TEST("HEX FFFF ALIGNED", (intptr_t)(0xFFFF + sizeof(Cell)) & -(intptr_t)sizeof(Cell));
    TEST("1 2 3 DEPTH", 1, 2, 3, 3);
    TEST(": FOO S\" XY\" ; FOO SWAP DUP C@ SWAP 1+ C@ ", 2, 88, 89);
    TEST("S\" XY\" SWAP DUP C@ SWAP 1+ C@ ", 2, 88, 89);
    TEST("S\" ABC\" DROP DUP C@++ DROP C@++ DROP C@++ -ROT SWAP -", 'C', 3);
    TEST("10 CONSTANT TEN TEN TEN", 10, 10);
    TEST("VARIABLE X 42 X ! X @ 5 X +! X @", 42, 47);
    TEST("111 VALUE X X 222 TO X X", 111, 222);
    TEST(": A ; LATEST @ ?IMMEDIATE : X IMMEDIATE ; LATEST @ ?IMMEDIATE 0<>", 0, -1);
    TEST(": X ; LATEST @ DUP ?HIDDEN SWAP DUP HIDDEN ?HIDDEN 0<>", 0, -1);
    TEST("1 32 128 WITHIN 32 32 128 WITHIN 127 32 128 WITHIN 128 32 128 WITHIN", 0, -1, -1, 0);
    TEST(": X CASE 1 OF 60 ENDOF 2 OF 61 ENDOF 62 TUCK DROP ENDCASE ; 0 X 1 X 2 X 3 X", 62, 60, 61, 62);
    TEST(": X 42 ; : Y ' X ; Y EXECUTE", 42);
    TEST(":NONAME 2 + ; 40 SWAP EXECUTE", 42);
    TEST(": X 60 ; : A ['] X EXECUTE ; A", 60);
    TEST("31 ALIGNED", 32);
}
#endif

#ifdef WIN32
#include <Windows.h>

static void GetProc(void)
{
    const HMODULE Module = (HMODULE)DPOP();
    const char* Name = (const char*)DPOP();
    DPUSH(GetProcAddress(Module, Name));
}

static void PlatformInit(void)
{
    DWORD flOldProtect;
    if (!VirtualProtect(Cells, sizeof(Cells), PAGE_EXECUTE_READWRITE, &flOldProtect)) {
        Error("VirtualProtect failed: %d", GetLastError());
    }

    DefineNativeWord("GETPROC", &GetProc);
    DefineConst("KERNEL32", (intptr_t)GetModuleHandleA("KERNEL32.DLL"));
}
#else
static void Dummy(void) { DPUSH(NULL); }
static void PlatformInit(void)
{
    // TODO FIXME
    DefineNativeWord("GETPROC", &Dummy);
    DefineConst("KERNEL32", 0);
}
#endif

int main(int argc, char* argv[])
{
#ifndef NDEBUG
    memset(Cells, 0xCC, sizeof(Cells));
#endif

    DefineBuiltins();
#ifdef TESTS
    DefineNativeWord("RUN-TESTS", &RunTests);
#endif

    if (argc > 1) {
        for (int i = 1; i < argc; ++i) {
            RunFile(argv[i]);
        }
    } else {
        RunFile("-");
    }
}
