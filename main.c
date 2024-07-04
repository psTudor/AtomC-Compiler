#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "utils.c"
#include "lexer.c"
#include "parser_gc.c"
#include "vm.c"
#include "ad.c"
#include "at.c"
#include "gc.c"


int main() {
    char *inBuf=loadFile("./tests/testgc.c");
    Token *tokens = tokenize(inBuf);
    free(inBuf);
    //showTokens(tokens);
    pushDomain();
    vmInit();
    parse(tokens);
    Symbol *symMain=findSymbolInDomain(symTable,"main");
    if(!symMain)err("missing main function");
    Instr *entryCode=NULL;
    addInstr(&entryCode,OP_CALL)->arg.instr=symMain->fn.instr;
    addInstr(&entryCode,OP_HALT);
    run(entryCode); 
    //showDomain(symTable, "global");
    //Instr *testCode = genTestProgram2();
    //run(testCode);
    dropDomain();
    return 0;
}