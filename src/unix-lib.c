/* unix-lib.c - make the Rebel shared library

    Copyright (C) 2020 Lutz Mueller
    Copyright (C) 2025 Ufko (ufko.org)

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

*/

#include "rebel.h"
#include "protos.h"

extern int bigEndian;
extern char preLoad[];
extern CELL *sysEvalString(char *str, SYMBOL *context, CELL *proc, int mode);
extern void setupAllSignals(void);
extern int evalSilent;
extern int opsys;
extern SYMBOL *mainArgsSymbol;
extern char linkOffset[];
extern FILE *IOchannel;
extern int rebelLibConsoleFlag;

int libInitialized = 0;

#ifdef MAC_OSX
    #define LIBNAME "rebel.dylib"
#else
    #define LIBNAME "rebel.so"
#endif

void initializeMain(void)
{
    char name[MAX_LINE];
    char *initFile;

    opsys += 64;

    #ifdef SUPPORT_UTF8
    opsys += 128;
    #endif

    opsys += 256;

    #ifdef FFI
    opsys += 1024;
    initFFI();
    #endif


    bigEndian = (*((char *)&bigEndian) == 0);


    initLocale();
    initStacks();
    initialize();
    mainArgsSymbol->contents = (UINT)makeCell(CELL_EXPRESSION, (UINT)stuffString(LIBNAME));
    /* setupAllSignals(); taken out for LIBRARY in 10.5.7 */
    sysEvalString(preLoad, mainContext, nilCell, EVAL_STRING);


    IOchannel = stdin;
    initDefaultInAddr();

    initFile = getenv("REBELLIB_INIT");
    if(initFile)
    {
        strncpy(name, initFile, MAX_LINE);
        name[MAX_LINE - 1] = 0;
        loadFile(name, 0, 0, mainContext);
    }

    libInitialized = 1;
    reset();
}

extern STREAM errorStream;
STREAM libStrStream = {NULL, NULL, 0, 0, 0};

/* ---- imported and called from a client using rebel.so ---- */

char *rebelEvalStr(char *cmd)
{
    if(!libInitialized)
    {
        initializeMain();
    }

    if(setjmp(errorJump))
    {
        /* setupAllSignals(); taken out for LIBRARY in 10.5.7 */

        reset();
        initStacks();

        if(errorReg)
        {
            executeSymbol(errorEvent, NULL, NULL);
            return(libStrStream.buffer);
        }
        else
        {
            return(errorStream.buffer);
        }
    }

    openStrStream(&libStrStream, MAX_STRING, 1);
    executeCommandLine(cmd, (UINT)&libStrStream, NULL);

    if(evalSilent)
    {
        evalSilent = 0;
    }

    return(libStrStream.buffer);
}

/* don't let stdout be included in return string */
int rebelLibConsole(int flag)
{
    rebelLibConsoleFlag = flag;
    return(flag);
}

/* callbacks from rebel library into caller

currently only tested with Rebel as caller:

(import "rebel.dylib" "rebelEvalStr")
(import "rebel.dylib" "rebelCallback")
(define (callme p1 p2 p3) (println "p1 => " p1 " p2 => " p2 " p3 => " p3) "hello world")
(rebelCallback "callme" (callback 0 'callme) "cdecl")
(get-string (rebelEvalStr {(get-string (callme 123 456 789))})) ; for string return
;(get-string (rebelEvalStr {(callme 123 456 789)})) ; for number return

*/

long rebelCallback(char *funcName, long funcAddr, char *callType)
{
    CELL *pCell;
    SYMBOL *symbol;

    if(!libInitialized)
    {
        initializeMain();
    }

    if(callType != NULL && strcmp(callType, "stdcall") ==  0)
    {
        pCell = getCell(CELL_IMPORT_DLL);
    }
    else
    {
        pCell = getCell(CELL_IMPORT_CDECL);
    }

    symbol = translateCreateSymbol(funcName, pCell->type, currentContext, TRUE);

    if(isProtected(symbol->flags))
    {
        errorProcExt2(ERR_SYMBOL_PROTECTED, stuffSymbol(symbol));
        return(-1);
    }

    deleteList((CELL *)symbol->contents);
    symbol->contents = (UINT)pCell;
    pCell->contents = (UINT)funcAddr;

    pCell->aux = (UINT)symbol->name;

    return(funcAddr);
}
/* eof */
