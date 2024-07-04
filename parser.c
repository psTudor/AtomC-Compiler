#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdbool.h>

#include "parser.h"

Token *iTk;		// the iterator in the tokens list
Token *consumedTk;		// the last consumed token

bool unit();

bool structDef();

bool varDef();

bool typeBase();

bool arrayDecl();

bool fnDef();

bool fnParam();

bool stm();

bool stmCompound();

bool expr();

bool exprAssign();

bool exprOr();

bool exprOrPrim();

bool exprAnd();

bool exprAndPrim();

bool exprEq();

bool exprEqPrim();

bool exprRel();

bool exprRelPrim();

bool exprAdd();

bool exprAddPrim();

bool exprMul();

bool exprMulPrim();

bool exprCast();

bool exprUnary();

bool exprPostfix();

bool exprPostfixPrim();

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
    printf("consume(%s)",tkCodeName(code));
	if(iTk->code==code){
		consumedTk=iTk;
		iTk=iTk->next;
		return true;
	}
    printf(" => found %s\n",tkCodeName(iTk->code));  
	return false;
}

// typeBase: TYPE_INT | TYPE_DOUBLE | TYPE_CHAR | STRUCT ID
bool typeBase(){
    Token *start = iTk;
	if(consume(TYPE_INT)){
		return true;
		}
	if(consume(TYPE_DOUBLE)){
		return true;
		}
	if(consume(TYPE_CHAR)){
		return true;
		}
	if(consume(STRUCT)){
		if(consume(ID)){
			return true;
			} else tkerr("Lipsa nume dupa INT | DOUBLE | CHAR | STRUCT ");
		}
    iTk = start;    
	return false;
}


bool arrayDecl() {
    Token *start = iTk;
    if (consume(LBRACKET)) {
        if (consume(INT)) {}
        if (consume(RBRACKET)) {
            return true;
        } else tkerr("lipsa ] la array");
    }
    iTk = start;
    return false;   
}


bool varDef() {
    Token *start = iTk;
    if (typeBase()) {
        if (consume(ID)) {
            Token *numeTk = consumedTk;
            if (arrayDecl()) {}
            if (consume(SEMICOLON)) {
                return true;
            } else tkerr("Lipsa ; dupa %s", numeTk->text);
        } else tkerr("Lipsa nume variabila");
    }
    iTk = start;
    return false;
}

bool structDef() {
    Token *start = iTk;
    if (consume(STRUCT)) {
        if (consume(ID)) {
            if (consume(LACC)) {
                for (;;) {
                    if (varDef()) {}
                    else break;
                }
                if (consume(RACC)) {
                    if (consume(SEMICOLON)) {
                        return true;
                    } else tkerr("lipsa ; dupa declarare struct");
                } else tkerr("lipsa } la struct");
            } 
        } else tkerr("Lipsa nume struct");
    }
    iTk = start;
    return false;
}

bool fnParam() {
    Token *start = iTk;
    if (typeBase()) {
        if (consume(ID)) {
            if (arrayDecl()) {}
            return true;
        }else tkerr("Lipsa nume parametru");
    }
    iTk = start;
    return false;
}


bool stm(){
	Token *start = iTk;
	if(stmCompound()) return true;
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
	if(consume(RETURN)) {
		if(expr()) {}
		if(consume(SEMICOLON)) { 
			return true;
		} else tkerr("Lipsa ; la return");
	}
	if(expr()) {}
	if(consume(SEMICOLON)) { 
		return true;
    }
	iTk = start;
	return false;
}

bool stmCompound() {
    Token *start = iTk;
    if (consume(LACC)) {
        for(;;) {
            if (varDef()) {}
            if (stm()) {} 
            else break;
        }
        if (consume(RACC)) {
            return true;
        } 
    }
    iTk = start;
    return false;
}

bool fnDef() {
    Token *start = iTk;
    if (typeBase() || consume(VOID)) {
        if (consume(ID)) {
            if (consume(LPAR)) {
                if (fnParam()) {
                    for (;;) {
                        if (consume(COMMA)) {
                            if (fnParam()) {}
                            else err("Lipsa/invalid parametru dupa ,");
                        } else break;
                    }
                }
                if (consume(RPAR)) {
                    if(stmCompound()) {
                        return true;
                    }
                } else tkerr("Lipsa ) la functie");
            }
        } else tkerr("Lipsa nume functie");
    }
    iTk = start;
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


void parse(Token *tokens){
	iTk=tokens;
	if(!unit())tkerr("syntax error");
}

bool expr() {
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

/* exprAnd: exprAnd AND exprEq | exprEq
    exprAnd: exprEq exprAndPrim
    exprAndPrim: AND exprEq exprAndPrim | epsilon
*/
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

/* exprEq: exprEq ( EQUAL | NOTEQ ) exprRel | exprRel
   exprEq: exprRel exprEqPrim
   exprEqPrim: (EQUAL | NOTEQ) exprRel exprEqPrim | epsilon
*/
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

/* exprRel: exprRel ( LESS | LESSEQ | GREATER | GREATEREQ ) exprAdd | exprAdd
    exprRel: exprAdd exprRelPrim
    exprRelPrim: ( LESS | LESSEQ | GREATER | GREATEREQ ) exprAdd exprRelPrim | epsilon
*/
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

/*exprAdd: exprAdd ( ADD | SUB ) exprMul | exprMul
  exprAdd: exprMul exprAddPrim
  exprAddPrim: ( ADD | SUB) exprMul exprAddPrim | epsilon*/
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

/*exprMul: exprMul ( MUL | DIV ) exprCast | exprCast
  exprMul: exprCast exprMulPrim
  exprMulPrim: ( MUL | DIV ) exprCast exprMulPrim | epsilon
*/
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

bool exprCast() {
    Token *start = iTk;
    if (consume(LPAR)) {
        if (typeBase()) {
            if (arrayDecl()) {}
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