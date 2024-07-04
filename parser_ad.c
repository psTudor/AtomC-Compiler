#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdbool.h>

#include "ad.h"
#include "ad.c"
#include "parser.h"


Token *iTk;		// the iterator in the tokens list
Token *consumedTk;		// the last consumed token
Symbol *owner = NULL;



bool structDef();
bool varDef();
bool typeBase(Type *t);
bool arrayDecl(Type *t);
bool fnDef();
bool fnParam();
bool stmCompound(bool newDomain);
bool expr();
bool exprAssign();
bool exprOr();
bool exprAnd();
bool exprEq();
bool exprRel();
bool exprAdd();
bool exprMul();
bool exprCast();
bool exprUnary();
bool exprPostfix();
bool exprPrimary();

const char *tkCodeName(int code) {
		printf("%d\t",iTk->line);
		switch (code) {
		case TYPE_INT:return "TYPE_INT";break;
		case TYPE_CHAR:return "TYPE_CHAR";break;
		case TYPE_DOUBLE:return "TYPE_DOUBLE";break;
		case ID:return "ID";break;
		case LPAR:	return "LPAR";break;
		case RPAR:return "RPAR";break;
		case LACC:return "LACC";break;
		case RACC:return "RACC";break;
		case LBRACKET:return "LBRACKET";break;
		case RBRACKET:return "RBRACKET";break;
		case SEMICOLON:return "SEMICOLON";break;
		case INT:return "INT";break;
		case DOUBLE:return "DOUBLE";break;
		case STRING:return "STRING";break;
		case CHAR:return "CHAR";break;
		case WHILE:return "WHILE";break;
		case LESS:return "LESS";break;
		case DIV:return "DIV";break;
		case ADD:return "ADD";break;
		case AND:return "AND";break;
		case MUL:return "MUL";break;
		case IF:return "IF" ;break;
		case ASSIGN:return "ASSIGN" ;break;
		case EQUAL:return "EQUAL";break;
		case RETURN:return "RETURN";break;
		case END:return "END";break;
		case ELSE:return "ELSE";break;
		case STRUCT:return "STRUCT";break;
		case VOID:return "VOID";break;
		case SUB:return "SUB";break;
		case OR:return "OR";break;
		case NOT:return "NOT";break;
		case NOTEQ:return "NOTEQ";break;
		case LESSEQ:return "LESSEQ";break;
		case GREATER:return "GREATER";break;
		case GREATEREQ:return "GREATEREQ";break;
		case COMMA:return "COMMA";break;
		case DOT:return "DOT";break;
		}
    return "UNKNOWN";
}


void tkerr(const char *fmt,...){
	fprintf(stderr,"error in line %d: ",iTk->line);
	va_list va;
	va_start(va,fmt);
	vfprintf(stderr,fmt,va);
	va_end(va);
	fprintf(stderr,"\n");
	exit(EXIT_FAILURE);
	}

bool consume(int code){
    //printf("consume(%s)",tkCodeName(code));
	if(iTk->code==code){
		consumedTk=iTk;
		iTk=iTk->next;
		return true;
	}
    //printf(" => found %s\n",tkCodeName(iTk->code));  
	return false;
}

// unit: ( structDef | fnDef | varDef )* END
bool unit(){
    Token *start = iTk;
	for(;;){
		if(structDef()){}
		else if(fnDef()){}
		else if(varDef()){}
		else break;
	}
	if(consume(END)){
		return true;
	}
    iTk = start;
	return false;
}


bool structDef() {
    Token *start = iTk;
    if (consume(STRUCT)) {
        if (consume(ID)) {
            Token *tkName = consumedTk;
            if (consume(LACC)) {
                Symbol *s = findSymbolInDomain(symTable,tkName->text);
                if (s) 
                    tkerr("symbol redefinition: %s",tkName->text);
                s=addSymbolToDomain(symTable,newSymbol(tkName->text,SK_STRUCT));
                s->type.tb=TB_STRUCT;
                s->type.s=s;
                s->type.n=-1;
                pushDomain();
                owner=s;
                for (;;) {
                    if (varDef()) {}
                    else break;
                }
                if (consume(RACC)) {
                    if (consume(SEMICOLON)) {
                        owner = NULL;
                        dropDomain();
                        return true;
                    } else tkerr("lipsa ; dupa declarare struct");
                } else tkerr("lipsa } la struct");
            } 
        } else tkerr("Lipsa nume struct");
    }
    iTk = start;
    return false;
}

bool varDef() {
    printf("varDef\n");
    Token *start = iTk;
    Token *tkName;
    Type t;
    if (typeBase(&t)) {
        if (consume(ID)) {
            tkName = consumedTk;
            if (arrayDecl(&t)) {
                if(t.n==0)tkerr("a vector variable must have a specified dimension");
            }
            if (consume(SEMICOLON)) {
                Symbol *var=findSymbolInDomain(symTable,tkName->text);
                if(var)
                    tkerr("symbol redefinition: %s",tkName->text);
                var=newSymbol(tkName->text,SK_VAR);
                var->type=t;
                var->owner=owner;
                addSymbolToDomain(symTable,var);
                if(owner){
                    switch(owner->kind){
                        case SK_FN:
                            var->varIdx=symbolsLen(owner->fn.locals);
                            addSymbolToList(&owner->fn.locals,dupSymbol(var));
                            break;
                            
                        case SK_STRUCT:
                            var->varIdx=typeSize(&owner->type);
                            addSymbolToList(&owner->structMembers,dupSymbol(var));
                            break;
                    }
                }else {
                    var->varMem = safeAlloc(typeSize(&t));
                }
                return true;
            } else tkerr("Lipsa ; dupa %s", tkName->text);
        } else tkerr("Lipsa nume variabila");
    }

    iTk = start;
    return false;
}

// typeBase: TYPE_INT | TYPE_DOUBLE | TYPE_CHAR | STRUCT ID
bool typeBase(Type *t){
    printf("typeBase\n");

    Token *start = iTk;
    t->n = -1;
	if(consume(TYPE_INT)){
        t->tb = TB_INT;
		return true;
	}
	if(consume(TYPE_DOUBLE)){
        t->tb = TB_DOUBLE;
		return true;
	}
	if(consume(TYPE_CHAR)){
        t->tb = TB_CHAR;
		return true;
	}
	if(consume(STRUCT)){
		if(consume(ID)){
            Token *tkName = consumedTk;
            t->tb=TB_STRUCT;
            t->s=findSymbol(tkName->text);
            if(!t->s)tkerr("structura nedefinita: %s",tkName->text);
            return true;
			} else tkerr("Lipsa nume dupa INT | DOUBLE | CHAR | STRUCT ");
		}
    iTk = start;    
	return false;
}

bool arrayDecl(Type *t) {
    printf("\narrayDecl\n");
    Token *start = iTk;
    if (consume(LBRACKET)) {
        if (consume(INT)) {
            Token *tkSize = consumedTk;
            t->n=tkSize->i;
        } else {
            t->n = 0;
        }
        if (consume(RBRACKET)) {
            return true;
        } else tkerr("lipsa ] la array");
    }
    iTk = start;
    return false;   
}

bool fnDefPrim(Type* t) {
    Token* tkName;
    printf("\nfnDefPrim\n");
    if(consume(ID)) {
        tkName = consumedTk;
        if(consume(LPAR)) {
            Symbol* fn = findSymbolInDomain(symTable, tkName->text);
            if(fn)
                tkerr("Symbol redefinition: %s", tkName->text);
            fn = newSymbol(tkName->text, SK_FN);
            fn->type = *t;
            addSymbolToDomain(symTable, fn);
            owner = fn;
            pushDomain();
            if(fnParam()) {
                for(;;) {
                    if(consume(COMMA)) {
                        if(fnParam()) {}
                        else {
                            tkerr("Missing function argument after ,");
                        }
                    } else break;
                }
            }
            if(consume(RPAR)) {
                if(stmCompound(false)) {
                    dropDomain();
                    owner = NULL;
                    return true;
                }
            } else {
                tkerr("Missing ) after arguments list");
            }
        }
    }
    return false;
}

bool fnDef() {
    printf("\nfnDef\n");
    Type t;
    Token* start = iTk;
    if (typeBase(&t)) {
        if (fnDefPrim(&t)) {
            return true;
        }
    } else if (consume(VOID)) {
        t.tb = TB_VOID;
        if (fnDefPrim(&t)) {
            return true;
        }
    }
    iTk = start;
    return false;
}

bool fnParam() {
    printf("fnParam\n");
    Token *start = iTk;
    Token *tkName;
    Type t;
    if (typeBase(&t)) {
        if (consume(ID)) {
            tkName = consumedTk;
            if (arrayDecl(&t)) {
                t.n = 0;
            }
            Symbol *param=findSymbolInDomain(symTable,tkName->text);
            if(param)tkerr("symbol redefinition: %s",tkName->text);
            param=newSymbol(tkName->text,SK_PARAM);
            param->type=t;
            param->owner=owner;
            param->paramIdx=symbolsLen(owner->fn.params);
            addSymbolToDomain(symTable,param);
            addSymbolToList(&owner->fn.params,dupSymbol(param));
            return true;
        }else tkerr("Lipsa nume parametru");
    }
    iTk = start;
    return false;
}


bool stm(){
    printf("stm\n");
	Token *start = iTk;
	if(stmCompound(true)) 
        return true;
    iTk = start;
	if(consume(IF)) {
		if(consume(LPAR)) {
			if(expr()) {
				if(consume(RPAR)) {
					if(stm()) {
						if(consume(ELSE)) {
							if(stm()) {} else tkerr("Lipsa statement dupa else");
                            return true;
						}
						return true;
					} else tkerr("Lipa statement in if");
				} else tkerr("lipsa ) la if");
			} else tkerr("Lipsa conditie la if");
		} else tkerr("Lipsa ( la if");
	}
    iTk = start;
	if(consume(WHILE)) {
		if(consume(LPAR)) {
			if(expr()) {
				if(consume(RPAR)) {
					if(stm()) {
						return true;
					} else tkerr("Lipsa statement in while");
				} else tkerr("Lipsa ) la while");
			} else tkerr("Lipsa conditie la while");
		} else tkerr("Lipsa ( la while");
	}
    iTk = start;
	if(consume(RETURN)) {
		if(expr()) {}
		if(consume(SEMICOLON)) { 
			return true;
		} else tkerr("Lipsa ; la return");
	}
    iTk = start;
	if(expr()) {}
	if(consume(SEMICOLON)) { 
		return true;
    }
	iTk = start;
	return false;
}

bool stmCompound(bool newDomain) {
    printf("\nstmCompound\n");
    Token *start = iTk;
    if (consume(LACC)) {
        if (newDomain)
            pushDomain();
        while (varDef() || stm()) {}
        if (consume(RACC)) {
            if (newDomain) 
                dropDomain();
            return true;
        } else tkerr("Lipsa } dupa statement");
    }
    iTk = start;
    return false;
}

bool expr() {
    printf("expr\n");
    Token *start = iTk;
    if (exprAssign()) return true;
    iTk = start;
    return false;
}

bool exprAssign() {
    Token *start = iTk;
    if (exprUnary()) {
        if (consume(ASSIGN)) {
            if (exprAssign()) {
                return true;
            } else tkerr("Lipsa expresie dupa '='");
        }
    }
    iTk = start;
    if (exprOr()) {
        return true;
    }
    iTk = start;
    return false;
}

/* exprOr: exprOr OR exprAnd | exprAnd
exprOr: exprAnd exprOrPrim
exprOrPrim: OR exprAnd exprOrPrim | epsilon
*/

bool exprOrPrim() {
    if (consume(OR)) {
        if (exprAnd()) {
            if (exprOrPrim()) {
                return true;
            }
        } else tkerr("eroare de sintaxa dupa '||'");
    }
    return true;
}

bool exprOr() { 
    Token *start = iTk;
    if (exprAnd()) {
        if (exprOrPrim()) {
            return true;
        } else tkerr("eroare de sintaxa");
    }
    iTk = start;
    return false;
}


/* exprAnd: exprAnd AND exprEq | exprEq
    exprAnd: exprEq exprAndPrim
    exprAndPrim: AND exprEq exprAndPrim | epsilon
*/

bool exprAndPrim() {
    if (consume(AND)) {
        if (exprEq()) {
            if (exprAndPrim()) {
                return true;
            }
        } else tkerr("eroare de sintaxa dupa '&&'");
    }
    return true;
}

bool exprAnd() {
    Token *start = iTk;
    if (exprEq()) {
        if (exprAndPrim()) {
            return true;
        } else tkerr("eroare de sintaxa");
    }
    iTk = start;
    return false;
}


/* exprEq: exprEq ( EQUAL | NOTEQ ) exprRel | exprRel
   exprEq: exprRel exprEqPrim
   exprEqPrim: (EQUAL | NOTEQ) exprRel exprEqPrim | epsilon
*/

bool exprEqPrim() {
    if (consume(EQUAL) || consume(NOTEQ)) {
        if (exprRel()) {
            if (exprEqPrim()) {
                return true;
            }
        } else tkerr("eroare de sintaxa dupa '==' sau '!=");
    }
    return true;
}

bool exprEq() {
    Token *start = iTk;
    if (exprRel()) {
        if (exprEqPrim()) {
            return true;
        } else tkerr("eroare de sintaxa");
    }
    iTk = start;
    return false;
}


/* exprRel: exprRel ( LESS | LESSEQ | GREATER | GREATEREQ ) exprAdd | exprAdd
    exprRel: exprAdd exprRelPrim
    exprRelPrim: ( LESS | LESSEQ | GREATER | GREATEREQ ) exprAdd exprRelPrim | epsilon
*/
bool exprRelPrim() {
    if (consume(LESS)) {
        if (exprAdd()) {
            if (exprRelPrim()) {
                return true;
            }
        }else tkerr("lipsa expresie dupa '<'");
    }

    if (consume(LESSEQ)) {
        if (exprAdd()) {
            if (exprRelPrim()) {
                return true;
            } 
        } else tkerr("lipsa expresie dupa '<='");
    }

    if (consume(GREATER)) {
        if (exprAdd()) {
            if (exprRelPrim()) {
                return true;
            }
        } else tkerr("lipsa expresie dupa '>'");
    }

    if (consume(GREATEREQ)) {
        if (exprAdd()) {
            if (exprRelPrim()) {
                return true;
            } 
        } else tkerr("lipsa expresie dupa '>='");
    }

    return true;
}

bool exprRel() {
    Token *start = iTk;
    if (exprAdd()) {
        if (exprRelPrim()) {
            return true;
        } else tkerr("eroare de sintaxa");
    }
    iTk = start;
    return false;
}

/*exprAdd: exprAdd ( ADD | SUB ) exprMul | exprMul
  exprAdd: exprMul exprAddPrim
  exprAddPrim: ( ADD | SUB) exprMul exprAddPrim | epsilon*/

bool exprAddPrim() {
    if (consume(ADD) || consume(SUB)) {
        if (exprMul()) {
            if (exprAddPrim()) {
                return true;
            } 
        } tkerr("eroare de sintaxa dupa '+' sau '-'");
    }
    return true;
}


bool exprAdd() {
    Token *start = iTk;
    if (exprMul()) {
        if (exprAddPrim()) {
            return true;
        } else tkerr("eroare de sintaxa");
    }
    iTk = start;
    return false;
}

/*exprMul: exprMul ( MUL | DIV ) exprCast | exprCast
  exprMul: exprCast exprMulPrim
  exprMulPrim: ( MUL | DIV ) exprCast exprMulPrim | epsilon
*/
bool exprMulPrim() {
    if (consume(MUL) || consume(DIV)) {
        if (exprCast()) {
            if (exprMulPrim()) {
                return true;
            }
        } else tkerr("eroare de sintaxa dupa '*' sau '/'");
    }
    return true;
}

bool exprMul() {
    Token *start = iTk;
    if (exprCast()) {
        if (exprMulPrim()) {
            return true;
        } else tkerr("eroare de sintaxa");
    }
    iTk = start;
    return false;
}


bool exprCast() {
    Token *start = iTk;
    Type t;
    if (consume(LPAR)) {
        if (typeBase(&t)) {
            if (arrayDecl(&t)) {}
            if (consume(RPAR)) {
                if (exprCast()) {
                    return true;
                }
            } else tkerr("Lipsa ) dupa ( la cast");
        }
    }
    iTk = start;
    if (exprUnary()) {
        return true;
    }
    iTk = start;
    return false;
}

bool exprUnary() {
    Token *start = iTk;
    if (consume(SUB) || consume(NOT)) {
        if (exprUnary()) {
            return true;
         }
    }
    iTk = start;
    if (exprPostfix()) {
        return true;
    }
    iTk = start;
    return false;
}

/* exprPostfix: exprPostfix LBRACKET expr RBRACKET
| exprPostfix DOT ID
| exprPrimary

exprPostfix: exprPrimary exprPostfixPrim

exprPostfixPrim: LBRACKET expr RBRACKET exprPostfixPrim
| DOT ID exprPostfixPrim
| epsilon

*/
bool exprPostfixPrim() {
    if (consume(LBRACKET)) {
        if (expr()) {
            if (consume(RBRACKET)) {
                if (exprPostfixPrim()) {
                    return true;
                }
            } else tkerr("Lipsa ]");
        }
    }
    if (consume(DOT)) {
        if (consume(ID)) {
            if (exprPostfixPrim()) {
                return true;
            }
        } else tkerr("Lipsa nume");
    }   
    return true;
}


bool exprPostfix() {
    Token *start = iTk;
    if (exprPrimary()) {
        if (exprPostfixPrim()) {
            return true;
        }
    }
    iTk = start;
    return false;
}

bool exprPrimary() {
    Token *start = iTk;
    if (consume(ID)) {
        if (consume(LPAR)) {
            if (expr()) {
                for (;;) {
                    if (consume(COMMA)) {
                        if (expr()) {}
                    } else break;
                }
            }
            if (consume(RPAR)) {
                return true;
            } else tkerr("Lipsa ) dupa (");
        }
        return true;
    }
    if (consume(INT) || consume(DOUBLE) || consume(CHAR) || consume(STRING)) {
        return true;
    }
    if (consume(LPAR)) {
        if (expr()) {
            if (consume(RPAR)) {
                return true;
            } else tkerr("Lipsa ) dupa (");
        }
    }
    iTk = start;
    return false;
}

void parse(Token *tokens){
	iTk=tokens;
	if(!unit())tkerr("syntax error");
}