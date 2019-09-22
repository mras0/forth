#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdbool.h>

#include <fcntl.h>

#ifdef _MSC_VER
#include <io.h>
#define open  _open
#define close _close
#define read  _read
#define write _write
#define STDIN_FILENO _fileno(stdin)
#define STDOUT_FILENO _fileno(stdout)
#else
#include <unistd.h>
#endif

#define F_HIDDEN  0x80
#define F_IMMED   0x40
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

extern WordHeader* Latest;
extern uint8_t WordBuffer[];
extern intptr_t Base;
extern intptr_t State;
extern intptr_t StdoutFile;
extern intptr_t InputFile;
extern intptr_t OutputFile;
extern int ForthMain(int argc, char* argv[]);

intptr_t C_Open(const char* Filename, int mode)
{
    if (mode != O_RDONLY) {
        printf("TODO: open(%s, %d)\n", Filename, mode);
        exit(1);
    }
    return open(Filename, mode);
}

void C_Close(intptr_t fd)
{
    close((int)fd);
}

intptr_t C_Read(intptr_t fd, void* buf, unsigned len)
{
    return read((int)fd, buf, len);
}

intptr_t C_Write(intptr_t fd, const void* buf, unsigned len)
{
    return write((int)fd, buf, len);
}

static const char* InputText;

uint8_t C_ReadKey(void)
{
    if (InputText) {
        return *InputText ? *InputText++ : 0;
    }
    uint8_t ch;
    if (C_Read(InputFile, &ch, 1) != 1) {
        return 0;
    }
    return ch;
}

void C_ReadWord(void)
{
    uint8_t C;
SkipSpaces:
    while ((C = C_ReadKey()) != 0 && C <= ' ') {
    }
    if (C == '\\') {
        while ((C = C_ReadKey()) != 0 && C != '\n') {
        }
        goto SkipSpaces;
    }
    int WordLen = 0;
    while (C > ' ' && WordLen < F_LENMASK) {
        if (C >= 'a' && C <= 'z') {
            C &= 0xdf;
        }
        WordBuffer[WordLen++] = C;
        C = C_ReadKey();
    }
    WordBuffer[WordLen] = '\0'; // NUL termination is only for C's sake
    WordBuffer[F_LENMASK+1] = (uint8_t)WordLen;
    printf("Read word: \"%s\"\n", WordBuffer);
}

WordHeader* C_FindWord(uint8_t WordLen, const char* Name)
{
    //printf("Finding word \"%*.*s\"\n", WordLen, WordLen, Name);
    for (WordHeader* WH = Latest; WH; WH = WH->Prev) {
        if ((WH->LengthAndFlags & (F_LENMASK|F_HIDDEN)) == WordLen && !memcmp(Name, WH->Name, WordLen)) {
            //printf("Found word \"%*.*s\" -> %p\n", WordLen, WordLen, Name, (void*)WH);
            return WH;
        }
    }
    return NULL;
}

intptr_t C_ConvertNumber(uint8_t WordLen, const char* Name)
{
    if (!WordLen) {
        goto Error;
    }
    uint8_t n = WordLen;
    const char* s = Name;
    bool neg = false;
    if (*s == '-') {
        neg = true;
        s++;
        n--;
        if (!n) {
            goto Error;
        }
    }

    intptr_t Res = 0;
    while (n--) {
        uint8_t ch = *s++;
        uint8_t dig;
        if (ch >= '0' && ch <= '9') {
            dig = ch - '0';
        } else if (ch >= 'A' && ch <= 'Z') {
            dig = ch - 'A' + 10;
        } else {
            goto Error;
        }
        if (dig >= Base) {
            goto Error;
        }
        Res = Res*Base + dig;
    }
    //printf("Converted \"%*.*s\" to %d\n", WordLen, WordLen, Name, (int)(neg ? -Res : Res));
    return neg ? -Res : Res;
Error:
    printf("Invalid number \"%*.*s\"\n", WordLen, WordLen, Name);
    exit(1);
}

void C_Emit(uint8_t ch)
{
    printf("Emitting %c (%02X)\n", ch, ch);
    C_Write(STDOUT_FILENO, &ch, 1);
}

void C_PrintWords(void)
{
    for (WordHeader* WH = Latest; WH; WH = WH->Prev) {
        printf("%p: %*.*s %02X\n", (void*)WH, F_LENMASK, WH->LengthAndFlags & F_LENMASK, WH->Name, WH->LengthAndFlags & ~F_LENMASK);
    }
}


#if 0
extern int QDiv32(int a, int b);
extern int RDiv32(int a, int b);
void testdiv(int a, int b)
{
    printf("%+4d / %+4d = %+4d (mod %+4d) ----- CHECK: ", a, b, QDiv32(a, b), RDiv32(a, b));
    printf("%+4d / %+4d = %+4d (mod %+4d)\n", a, b, a / b, a % b);
}
void testdivs(void)
{
    testdiv(-67, 13);
    testdiv(-67, -13);
    testdiv(67, 13);
    testdiv(67, -13);
}
#endif

void C_Debug(intptr_t val)
{
    printf("Debug: %d (%08X)\n", (int)val, (unsigned)val);
}

int main(int argc, char* argv[])
{
#if 1
    OutputFile = StdoutFile = STDOUT_FILENO;
    if (argc > 1) {
        InputFile = C_Open(argv[1], O_RDONLY);
        if (InputFile == -1) {
            printf("Error opening %s\n", argv[1]);
            return 1;
        }
    } else {
        InputFile = STDIN_FILENO;
    }
#endif
    C_PrintWords();
    int retval = ForthMain(argc, argv);
    printf("Interpreter exited with code %d (%08X) State=%d\n", retval, retval, (int)State);
    return 0;
}
