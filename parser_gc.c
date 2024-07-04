#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdbool.h>

#include "parser.h"
#include "utils.h"

#include "vm.h"
#include "ad.h"
#include "at.h"
#include "gc.h"

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
bool expr(Ret *r);
bool exprAssign(Ret *r);
bool exprOr(Ret *r);
bool exprAnd(Ret *r);
bool exprEq(Ret *r);
bool exprRel(Ret *r);
bool exprAdd(Ret *r);
bool exprMul(Ret *r);
bool exprCast(Ret *r);
bool exprUnary(Ret *r);
bool exprPostfix(Ret *r);
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
    //iTk = start;
	return false;
}


bool structDef() {
    Token *start = iTk;
    Instr *startInstr = owner?lastInstr(owner->fn.instr):NULL;
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
    if(owner)delInstrAfter(startInstr);
    iTk = start;
    return false;
}

bool varDef() {
    //printf("varDef\n");
    Token *start = iTk;
    Instr *startInstr = owner?lastInstr(owner->fn.instr):NULL;
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

    if(owner)delInstrAfter(startInstr);
    iTk = start;
    return false;
}

// typeBase: TYPE_INT | TYPE_DOUBLE | TYPE_CHAR | STRUCT ID
bool typeBase(Type *t){
    //printf("typeBase\n");
    Instr *startInstr = owner?lastInstr(owner->fn.instr):NULL;
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

    if(owner)delInstrAfter(startInstr);
    iTk = start;    
	return false;
}

bool arrayDecl(Type *t) {
    //printf("\narrayDecl\n");
    Instr *startInstr = owner?lastInstr(owner->fn.instr):NULL;
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

    if(owner)delInstrAfter(startInstr);
    iTk = start;
    return false;   
}

bool fnDefPrim(Type* t) {
    Token* tkName;
    Token *start = iTk;
    Instr *startInstr = owner?lastInstr(owner->fn.instr):NULL;
    //printf("\nfnDefPrim\n");
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
                addInstr(&fn->fn.instr,OP_ENTER);
                if(stmCompound(false)) {
                    fn->fn.instr->arg.i=symbolsLen(fn->fn.locals);
                    if(fn->type.tb==TB_VOID)
                        addInstrWithInt(&fn->fn.instr,OP_RET_VOID,symbolsLen(fn->fn.params));
                    dropDomain();
                    owner = NULL;
                    return true;
                }
            } else {
                tkerr("Missing ) after arguments list");
            }
        }
    }
    return true;
}

bool fnDef() {
    //printf("\nfnDef\n");
    Type t;
    Token* start = iTk;
    Instr *startInstr = owner?lastInstr(owner->fn.instr):NULL;
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
    if(owner)delInstrAfter(startInstr);
    iTk = start;
    return false;
}

bool fnParam() {
    //printf("fnParam\n");
    Instr *startInstr = owner?lastInstr(owner->fn.instr):NULL;
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

    if(owner)delInstrAfter(startInstr);
    iTk = start;
    return false;
}


bool stm(){
    //printf("stm\n");
	Token *start = iTk;
    Instr *startInstr = owner?lastInstr(owner->fn.instr):NULL;
    Ret rCond, rExpr;
	if(stmCompound(true)) {
        return true;
    }
    
    if(owner)delInstrAfter(startInstr);
    iTk = start;
    if(consume(IF)) {
	    if(consume(LPAR)) {
			if(expr(&rCond)) {
                if(!canBeScalar(&rCond))tkerr("the if condition must be a scalar value");
				if(consume(RPAR)) {
                    addRVal(&owner->fn.instr,rCond.lval,&rCond.type);
                    Type intType={TB_INT,NULL,-1};
                    insertConvIfNeeded(lastInstr(owner->fn.instr),&rCond.type,&intType);
                    Instr *ifJF=addInstr(&owner->fn.instr,OP_JF);
					if(stm()) {
						if(consume(ELSE)) {
                            Instr *ifJMP=addInstr(&owner->fn.instr,OP_JMP);
                            ifJF->arg.instr=addInstr(&owner->fn.instr,OP_NOP);
							if(stm()) {
                                ifJMP->arg.instr=addInstr(&owner->fn.instr,OP_NOP);
                            } else tkerr("Lipsa statement dupa else");
						}
						ifJF->arg.instr=addInstr(&owner->fn.instr,OP_NOP);
                        return true;
					} else tkerr("Lipa statement in if");
				} else tkerr("lipsa ) la if");
			} else tkerr("Lipsa conditie la if");
		} else tkerr("Lipsa ( la if");
	}

    if(owner)delInstrAfter(startInstr);
    iTk = start;
	if(consume(WHILE)) {
        Instr *beforeWhileCond = lastInstr(owner->fn.instr);
		if(consume(LPAR)) {
			if(expr(&rCond)) {
                if(!canBeScalar(&rCond))tkerr("the while condition must be a scalar value");
				if(consume(RPAR)) {
                    addRVal(&owner->fn.instr,rCond.lval,&rCond.type);
                    Type intType={TB_INT,NULL,-1};
                    insertConvIfNeeded(lastInstr(owner->fn.instr),&rCond.type,&intType);
                    Instr *whileJF=addInstr(&owner->fn.instr,OP_JF);
					if(stm()) {
                        addInstr(&owner->fn.instr,OP_JMP)->arg.instr=beforeWhileCond->next;
                        whileJF->arg.instr=addInstr(&owner->fn.instr,OP_NOP);
						return true;
					} else tkerr("Lipsa statement in while");
				} else tkerr("Lipsa ) la while");
			} else tkerr("Lipsa conditie la while");
		} else tkerr("Lipsa ( la while");
	}

	if(consume(RETURN)) {
		if(expr(&rExpr)) {
            addRVal(&owner->fn.instr,rExpr.lval,&rExpr.type);
            insertConvIfNeeded(lastInstr(owner->fn.instr),&rExpr.type,&owner->type);
            addInstrWithInt(&owner->fn.instr,OP_RET,symbolsLen(owner->fn.params));
            if(owner->type.tb==TB_VOID)tkerr("a void function cannot return a value");
			if(!canBeScalar(&rExpr))tkerr("the return value must be a scalar value");
			if(!convTo(&rExpr.type,&owner->type))tkerr("cannot convert the return expression type to the function return type");
        } else {
            addInstr(&owner->fn.instr,OP_RET_VOID);
            if(owner->type.tb!=TB_VOID)tkerr("a non-void function must return a value");
        }
		if(consume(SEMICOLON)) { 
			return true;
		} else tkerr("Lipsa ; la return");
	}

    if(owner)delInstrAfter(startInstr);
    iTk = start;
	if(expr(&rExpr)) {}
	if(consume(SEMICOLON)) { 
		return true;
    }

    if(owner)delInstrAfter(startInstr);
    iTk = start;
	return false;
}

bool stmCompound(bool newDomain) {
    //printf("\nstmCompound\n");
    Instr *startInstr = owner?lastInstr(owner->fn.instr):NULL;
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

    if(owner)delInstrAfter(startInstr);
    iTk = start;
    return false;
}

bool expr(Ret *r) {
    //printf("expr\n");
    Instr *startInstr = owner?lastInstr(owner->fn.instr):NULL;
    Token *start = iTk;
    if (exprAssign(r)) return true;

    if(owner)delInstrAfter(startInstr);
    iTk = start;
    return false;
}

bool exprAssign(Ret *r) {
    Token *start = iTk;
    Instr *startInstr = owner?lastInstr(owner->fn.instr):NULL;
    Ret rDst;
    if (exprUnary(&rDst)) {
        if (consume(ASSIGN)) {
            if (exprAssign(r)) {
                addRVal(&owner->fn.instr,r->lval,&r->type);
                insertConvIfNeeded(lastInstr(owner->fn.instr),&r->type,&rDst.type);
                switch(rDst.type.tb){
                    case TB_INT:addInstr(&owner->fn.instr,OP_STORE_I);break;
                    case TB_DOUBLE:addInstr(&owner->fn.instr,OP_STORE_F);break;
                }
                if(!rDst.lval)tkerr("the assign destination must be a left-value");
				if(rDst.ct)tkerr("the assign destination cannot be constant");
				if(!canBeScalar(&rDst))tkerr("the assign destination must be scalar");
				if(!canBeScalar(r))tkerr("the assign source must be scalar");
				if(!convTo(&r->type,&rDst.type))tkerr("the assign source cannot be converted to destination");
				r->lval=false;
				r->ct=true;
                return true;
            } else tkerr("Lipsa expresie dupa '='");
        }
    }
    if(owner)delInstrAfter(startInstr);
    iTk = start;
    if (exprOr(r)) {
        return true;
    }
    if(owner)delInstrAfter(startInstr);
    iTk = start;
    return false;
}

/* exprOr: exprOr OR exprAnd | exprAnd
exprOr: exprAnd exprOrPrim
exprOrPrim: OR exprAnd exprOrPrim | epsilon
*/

bool exprOrPrim(Ret *r) {
    Token *start = iTk;
    Instr *startInstr = owner?lastInstr(owner->fn.instr):NULL;
    if (consume(OR)) {
        Ret right;
        if (exprAnd(&right)) {
            Type tDst;
			if(!arithTypeTo(&r->type,&right.type,&tDst))tkerr("invalid operand type for ||");
			*r=(Ret){{TB_INT,NULL,-1},false,true};
			exprOrPrim(r);
			return true;
        } else tkerr("eroare de sintaxa dupa '||'");
    }
    if(owner)delInstrAfter(startInstr);
    iTk = start;
    return true;
}

bool exprOr(Ret *r) { 
    Token *start = iTk;
    Instr *startInstr = owner?lastInstr(owner->fn.instr):NULL;
    if (exprAnd(r)) {
        if (exprOrPrim(r)) {
            return true;
        } else tkerr("eroare de sintaxa");
    }
    if(owner)delInstrAfter(startInstr);
    iTk = start;
    return false;
}


/* exprAnd: exprAnd AND exprEq | exprEq
    exprAnd: exprEq exprAndPrim
    exprAndPrim: AND exprEq exprAndPrim | epsilon
*/

bool exprAndPrim(Ret *r) {
    Token *start = iTk;
    Instr *startInstr = owner?lastInstr(owner->fn.instr):NULL;
    if (consume(AND)) {
        Ret right;
        if (exprEq(&right)) {
            Type tDst;
			if(!arithTypeTo(&r->type,&right.type,&tDst))tkerr("invalid operand type for &&");
			*r=(Ret){{TB_INT,NULL,-1},false,true};
			exprAndPrim(r);
            return true;
        } else tkerr("eroare de sintaxa dupa '&&'");
    }
    if(owner)delInstrAfter(startInstr);
    iTk = start;
    return true;
}

bool exprAnd(Ret *r) {
    Token *start = iTk;
    Instr *startInstr = owner?lastInstr(owner->fn.instr):NULL;
    if (exprEq(r)) {
        if (exprAndPrim(r)) {
            return true;
        } else tkerr("eroare de sintaxa");
    }
    if(owner)delInstrAfter(startInstr);
    iTk = start;
    return false;
}


/* exprEq: exprEq ( EQUAL | NOTEQ ) exprRel | exprRel
   exprEq: exprRel exprEqPrim
   exprEqPrim: (EQUAL | NOTEQ) exprRel exprEqPrim | epsilon
*/

bool exprEqPrim(Ret *r) {
    Token *start = iTk;
    Instr *startInstr = owner?lastInstr(owner->fn.instr):NULL;
    if (consume(EQUAL) || consume(NOTEQ)) {
        Ret right;
        if (exprRel(&right)) {
            Type tDst;
			if(!arithTypeTo(&r->type,&right.type,&tDst))tkerr("invalid operand type for ==");
			*r=(Ret){{TB_INT,NULL,-1},false,true};
			exprEqPrim(r);
			return true;
        } else tkerr("eroare de sintaxa dupa '==' sau '!=");
    }
    if(owner)delInstrAfter(startInstr);
    iTk = start;
    return true;
}

bool exprEq(Ret *r) {
    Token *start = iTk;
    Instr *startInstr = owner?lastInstr(owner->fn.instr):NULL;
    if (exprRel(r)) {
        if (exprEqPrim(r)) {
            return true;
        } else tkerr("eroare de sintaxa");
    }

    if(owner)delInstrAfter(startInstr);
    iTk = start;
    return false;
}


/* exprRel: exprRel ( LESS | LESSEQ | GREATER | GREATEREQ ) exprAdd | exprAdd
    exprRel: exprAdd exprRelPrim
    exprRelPrim: ( LESS | LESSEQ | GREATER | GREATEREQ ) exprAdd exprRelPrim | epsilon
*/
bool exprRelPrim(Ret *r) {
    Token *start = iTk;
    Instr *startInstr = owner?lastInstr(owner->fn.instr):NULL;
    Token *op;
    if (consume(LESS)) {
        Ret right;
        op = consumedTk;
        Instr *lastLeft=lastInstr(owner->fn.instr);
        addRVal(&owner->fn.instr,r->lval,&r->type);
        if (exprAdd(&right)) {
            Type tDst;
			if(!arithTypeTo(&r->type,&right.type,&tDst))tkerr("invalid operand type for <");
            addRVal(&owner->fn.instr,right.lval,&right.type);
            insertConvIfNeeded(lastLeft,&r->type,&tDst);
            insertConvIfNeeded(lastInstr(owner->fn.instr),&right.type,&tDst);
            switch(op->code){
                case LESS:
                    switch(tDst.tb){
                        case TB_INT:addInstr(&owner->fn.instr,OP_LESS_I);break;
                        case TB_DOUBLE:addInstr(&owner->fn.instr,OP_LESS_F);break;
                    }
                    break;
            }
			*r=(Ret){{TB_INT,NULL,-1},false,true};
			exprRelPrim(r);
			return true;
        }else tkerr("lipsa expresie dupa '<'");
    }

    if (consume(LESSEQ)) {
        Ret right;
        op = consumedTk;
        Instr *lastLeft=lastInstr(owner->fn.instr);
        addRVal(&owner->fn.instr,r->lval,&r->type);
        if (exprAdd(&right)) {
            Type tDst;
			if(!arithTypeTo(&r->type,&right.type,&tDst))tkerr("invalid operand type for <=");
            addRVal(&owner->fn.instr,right.lval,&right.type);
            insertConvIfNeeded(lastLeft,&r->type,&tDst);
            insertConvIfNeeded(lastInstr(owner->fn.instr),&right.type,&tDst);
            switch(op->code){
                case LESS:
                    switch(tDst.tb){
                        case TB_INT:addInstr(&owner->fn.instr,OP_LESS_I);break;
                        case TB_DOUBLE:addInstr(&owner->fn.instr,OP_LESS_F);break;
                    }
                    break;
            }
			*r=(Ret){{TB_INT,NULL,-1},false,true};
			exprRelPrim(r);
			return true;
        } else tkerr("lipsa expresie dupa '<='");
    }

    if (consume(GREATER)) {
        Ret right;
        op = consumedTk;
        Instr *lastLeft=lastInstr(owner->fn.instr);
        addRVal(&owner->fn.instr,r->lval,&r->type);
        if (exprAdd(&right)) {
            Type tDst;
			if(!arithTypeTo(&r->type,&right.type,&tDst))tkerr("invalid operand type for >");
            addRVal(&owner->fn.instr,right.lval,&right.type);
            insertConvIfNeeded(lastLeft,&r->type,&tDst);
            insertConvIfNeeded(lastInstr(owner->fn.instr),&right.type,&tDst);
            switch(op->code){
                case LESS:
                    switch(tDst.tb){
                        case TB_INT:addInstr(&owner->fn.instr,OP_LESS_I);break;
                        case TB_DOUBLE:addInstr(&owner->fn.instr,OP_LESS_F);break;
                    }
                    break;
            }
			*r=(Ret){{TB_INT,NULL,-1},false,true};
			exprRelPrim(r);
			return true;
        } else tkerr("lipsa expresie dupa '>'");
    }

    if (consume(GREATEREQ)) {
        Ret right;
        op = consumedTk;
        Instr *lastLeft=lastInstr(owner->fn.instr);
        addRVal(&owner->fn.instr,r->lval,&r->type);
        if (exprAdd(&right)) {
            Type tDst;
			if(!arithTypeTo(&r->type,&right.type,&tDst))tkerr("invalid operand type for >=");
            addRVal(&owner->fn.instr,right.lval,&right.type);
            insertConvIfNeeded(lastLeft,&r->type,&tDst);
            insertConvIfNeeded(lastInstr(owner->fn.instr),&right.type,&tDst);
            switch(op->code){
                case LESS:
                    switch(tDst.tb){
                        case TB_INT:addInstr(&owner->fn.instr,OP_LESS_I);break;
                        case TB_DOUBLE:addInstr(&owner->fn.instr,OP_LESS_F);break;
                    }
                    break;
            }
			*r=(Ret){{TB_INT,NULL,-1},false,true};
			exprRelPrim(r);
			return true;
        } else tkerr("lipsa expresie dupa '>='");
    }
    if(owner)delInstrAfter(startInstr);
    iTk = start;
    return true;
}

bool exprRel(Ret *r) {
    Token *start = iTk;
    Instr *startInstr = owner?lastInstr(owner->fn.instr):NULL;
    if (exprAdd(r)) {
        if (exprRelPrim(r)) {
            return true;
        } else tkerr("eroare de sintaxa");
    }
    if(owner)delInstrAfter(startInstr);
    iTk = start;
    return false;
}

/*exprAdd: exprAdd ( ADD | SUB ) exprMul | exprMul
  exprAdd: exprMul exprAddPrim
  exprAddPrim: ( ADD | SUB) exprMul exprAddPrim | epsilon*/

bool exprAddPrim(Ret *r) {
    Token *start = iTk;
    Instr *startInstr = owner?lastInstr(owner->fn.instr):NULL;
    Token *op;
    if (consume(ADD)) {
        Ret right;
        op = consumedTk;
        Instr *lastLeft=lastInstr(owner->fn.instr);
        addRVal(&owner->fn.instr,r->lval,&r->type);
        if (exprMul(&right)) {
            Type tDst;
			if(!arithTypeTo(&r->type,&right.type,&tDst))tkerr("invalid operand type for +");
            addRVal(&owner->fn.instr,right.lval,&right.type);
            insertConvIfNeeded(lastLeft,&r->type,&tDst);
            insertConvIfNeeded(lastInstr(owner->fn.instr),&right.type,&tDst);
            switch(op->code){
                case ADD:
                    switch(tDst.tb){
                        case TB_INT:addInstr(&owner->fn.instr,OP_ADD_I);break;
                        case TB_DOUBLE:addInstr(&owner->fn.instr,OP_ADD_F);break;
                    }
                    break;
                case SUB:
                    switch(tDst.tb){
                        case TB_INT:addInstr(&owner->fn.instr,OP_SUB_I);break;
                        case TB_DOUBLE:addInstr(&owner->fn.instr,OP_SUB_F);break;
                    }
                    break;
            }
			*r=(Ret){tDst,false,true};
			exprAddPrim(r);
			return true;
        } tkerr("eroare de sintaxa dupa '+'");
    }
    if (consume(SUB)) {
        Ret right;
        op = consumedTk;
        Instr *lastLeft=lastInstr(owner->fn.instr);
        addRVal(&owner->fn.instr,r->lval,&r->type);
        if (exprMul(&right)) {
            Type tDst;
			if(!arithTypeTo(&r->type,&right.type,&tDst))tkerr("invalid operand type for -");
            addRVal(&owner->fn.instr,right.lval,&right.type);
            insertConvIfNeeded(lastLeft,&r->type,&tDst);
            insertConvIfNeeded(lastInstr(owner->fn.instr),&right.type,&tDst);
            switch(op->code){
                case ADD:
                    switch(tDst.tb){
                        case TB_INT:addInstr(&owner->fn.instr,OP_ADD_I);break;
                        case TB_DOUBLE:addInstr(&owner->fn.instr,OP_ADD_F);break;
                    }
                    break;
                case SUB:
                    switch(tDst.tb){
                        case TB_INT:addInstr(&owner->fn.instr,OP_SUB_I);break;
                        case TB_DOUBLE:addInstr(&owner->fn.instr,OP_SUB_F);break;
                    }
                    break;
            }
			*r=(Ret){tDst,false,true};
			exprAddPrim(r);
			return true;
        } tkerr("eroare de sintaxa dupa '-'");
    }
    if(owner)delInstrAfter(startInstr);
    iTk = start;
    return true;
}


bool exprAdd(Ret *r) {
    Token *start = iTk;
    Instr *startInstr = owner?lastInstr(owner->fn.instr):NULL;
    if (exprMul(r)) {
        if (exprAddPrim(r)) {
            return true;
        } else tkerr("eroare de sintaxa");
    }
    if(owner)delInstrAfter(startInstr);
    iTk = start;
    return false;
}

/*exprMul: exprMul ( MUL | DIV ) exprCast | exprCast
  exprMul: exprCast exprMulPrim
  exprMulPrim: ( MUL | DIV ) exprCast exprMulPrim | epsilon
*/
bool exprMulPrim(Ret *r) {
    Token *start = iTk;
    Instr *startInstr = owner?lastInstr(owner->fn.instr):NULL;
    Token *op;
    if (consume(MUL)) {
        Ret right;
        op = consumedTk;
        Instr *lastLeft = lastInstr(owner->fn.instr);
        addRVal(&owner->fn.instr, r->lval, &r->type);
        if (exprCast(&right)) {
            Type tDst;
			if(!arithTypeTo(&r->type,&right.type,&tDst))tkerr("invalid operand type for *");
            addRVal(&owner->fn.instr,right.lval,&right.type);
            insertConvIfNeeded(lastLeft,&r->type,&tDst);
            insertConvIfNeeded(lastInstr(owner->fn.instr),&right.type,&tDst);
            switch(op->code) {
                case MUL:
                    switch (tDst.tb) {
                        case TB_INT:
                            addInstr(&owner->fn.instr, OP_MUL_I);
                            break;
                        case TB_DOUBLE:
                            addInstr(&owner->fn.instr, OP_MUL_F);
                            break;
                    }
                    break;
                case DIV:
                    switch (tDst.tb) {
                        case TB_INT:
                            addInstr(&owner->fn.instr, OP_DIV_I);
                            break;
                        case TB_DOUBLE:
                            addInstr(&owner->fn.instr, OP_DIV_F);
                            break;
                    }
                    break;
            }
			*r=(Ret){tDst,false,true};
			exprMulPrim(r);
			return true;
        } else tkerr("eroare de sintaxa dupa '*'");
    }
    if (consume(DIV)) {
        Ret right;
        op = consumedTk;
        Instr *lastLeft = lastInstr(owner->fn.instr);
        addRVal(&owner->fn.instr, r->lval, &r->type);
        if(exprCast(&right)) {
            Type tDst;
			if(!arithTypeTo(&r->type,&right.type,&tDst))tkerr("invalid operand type for /");
            addRVal(&owner->fn.instr,right.lval,&right.type);
            insertConvIfNeeded(lastLeft,&r->type,&tDst);
            insertConvIfNeeded(lastInstr(owner->fn.instr),&right.type,&tDst);
            switch(op->code) {
                case MUL:
                    switch (tDst.tb) {
                        case TB_INT:
                            addInstr(&owner->fn.instr, OP_MUL_I);
                            break;
                        case TB_DOUBLE:
                            addInstr(&owner->fn.instr, OP_MUL_F);
                            break;
                    }
                    break;
                case DIV:
                    switch (tDst.tb) {
                        case TB_INT:
                            addInstr(&owner->fn.instr, OP_DIV_I);
                            break;
                        case TB_DOUBLE:
                            addInstr(&owner->fn.instr, OP_DIV_F);
                            break;
                    }
                    break;
            }
			*r=(Ret){tDst,false,true};
			exprMulPrim(r);
			return true;
        } else tkerr("eroare de instaxa dupa '/'");
    }
    if(owner)delInstrAfter(startInstr);
    iTk = start;
    return true;
}

bool exprMul(Ret *r) {
    Token *start = iTk;
    Instr *startInstr = owner?lastInstr(owner->fn.instr):NULL;
    if (exprCast(r)) {
        if (exprMulPrim(r)) {
            return true;
        } else tkerr("eroare de sintaxa");
    }
    if(owner)delInstrAfter(startInstr);
    iTk = start;
    return false;
}


bool exprCast(Ret *r) {
    Token *start = iTk;
    Instr *startInstr = owner?lastInstr(owner->fn.instr):NULL;
    Type t;
    Ret op;
    if (consume(LPAR)) {
        if (typeBase(&t)) {
            if (arrayDecl(&t)) {}
            if (consume(RPAR)) {
                if (exprCast(&op)) {
                    if(t.tb==TB_STRUCT)tkerr("cannot convert to a struct type");
					if(op.type.tb==TB_STRUCT)tkerr("cannot convert a struct");
					if(op.type.n>=0&&t.n<0)tkerr("an array can be converted only to another array");
					if(op.type.n<0&&t.n>=0)tkerr("a scalar can be converted only to another scalar");
					*r=(Ret){t,false,true};
                    return true;
                }
            } else tkerr("Lipsa ) dupa ( la cast");
        }
    }
    if(owner)delInstrAfter(startInstr);
    iTk = start;
    if (exprUnary(r)) {
        return true;
    }
    if(owner)delInstrAfter(startInstr);
    iTk = start;
    return false;
}

bool exprUnary(Ret *r) {
    Token *start = iTk;
    Instr *startInstr = owner?lastInstr(owner->fn.instr):NULL;
    if (consume(SUB)) {
        if (exprUnary(r)) {
            if(!canBeScalar(r))tkerr("unary - must have a scalar operand");
			r->lval=false;
			r->ct=true;
			return true;
        } else tkerr("lipsa expresie dupa -");
    }
    if (consume(NOT)) {
        if (exprUnary(r)) {
            if(!canBeScalar(r))tkerr("unary ! must have a scalar operand");
			r->lval=false;
			r->ct=true;
			return true;
        } else tkerr("lipsa expresie dupa !");
    }

    if(owner)delInstrAfter(startInstr);
    iTk = start;
    if (exprPostfix(r)) {
        return true;
    }
    if(owner)delInstrAfter(startInstr);
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
bool exprPostfixPrim(Ret *r) {
    Token *start = iTk;
    Instr *startInstr = owner?lastInstr(owner->fn.instr):NULL;
    if (consume(LBRACKET)) {
        Ret idx;
        if (expr(&idx)) {
            if (consume(RBRACKET)) {
            	if(r->type.n<0)tkerr("only an array can be indexed");
				Type tInt={TB_INT,NULL,-1};
				if(!convTo(&idx.type,&tInt))tkerr("the index is not convertible to int");
				r->type.n=-1;
				r->lval=true;
				r->ct=false;
				exprPostfixPrim(r);
				return true;
            } else tkerr("Lipsa ]");
        }
    }
    if (consume(DOT)) {
        if (consume(ID)) {
			Token *tkName=consumedTk;
			if(r->type.tb!=TB_STRUCT)tkerr("a field can only be selected from a struct");
			Symbol *s=findSymbolInList(r->type.s->structMembers,tkName->text);
			if(!s)tkerr("the structure %s does not have a field %s",r->type.s->name,tkName->text);
			*r=(Ret){s->type,true,s->type.n>=0};
			exprPostfixPrim(r);
			return true;
        } else tkerr("Lipsa nume");
    }
    if(owner)delInstrAfter(startInstr);
    iTk = start;   
    return true;
}


bool exprPostfix(Ret *r) {
    Instr *startInstr = owner?lastInstr(owner->fn.instr):NULL;
    Token *start = iTk;
    if (exprPrimary(r)) {
        if (exprPostfixPrim(r)) {
            return true;
        }
    }

    if(owner)delInstrAfter(startInstr);
    iTk = start;
    return false;
}

bool exprPrimary(Ret *r) {
    Token *start = iTk;
    Instr *startInstr = owner?lastInstr(owner->fn.instr):NULL;
    if (consume(ID)) {
        Token *tkName=consumedTk;
		Symbol *s=findSymbol(tkName->text);
		if(!s)tkerr("undefined id: %s",tkName->text);
        if (consume(LPAR)) {
            if(s->kind!=SK_FN)tkerr("only a function can be called");
			Ret rArg;
			Symbol *param=s->fn.params;
            if (expr(&rArg)) {
                if(!param)tkerr("too many arguments in function call");
				if(!convTo(&rArg.type,&param->type))tkerr("in call, cannot convert the argument type to the parameter type");
                addRVal(&owner->fn.instr,rArg.lval,&rArg.type); 
                insertConvIfNeeded(lastInstr(owner->fn.instr),&rArg.type,&param->type);
				param=param->next;
                for (;;) {
                    if (consume(COMMA)) {
                        if (expr(&rArg)) {
                            if(!param)tkerr("too many arguments in function call");
							if(!convTo(&rArg.type,&param->type))tkerr("in call, cannot convert the argument type to the parameter type");
							param=param->next;
                        }
                    } else break;
                }
            }
            if (consume(RPAR)) {
                if(s->fn.extFnPtr){
                    addInstr(&owner->fn.instr,OP_CALL_EXT)->arg.extFnPtr=s->fn.extFnPtr;
                }else{
                    addInstr(&owner->fn.instr,OP_CALL)->arg.instr=s->fn.instr;
                }
                if(param)tkerr("too few arguments in function call");
				*r=(Ret){s->type,false,true};
				return true;
            } else tkerr("Lipsa ) dupa (");
        } else {
            if(s->kind==SK_FN)tkerr("a function can only be called");
			*r=(Ret){s->type,true,s->type.n>=0};
            if(s->kind==SK_VAR){
                if(s->owner==NULL){ // global variables
                    addInstr(&owner->fn.instr,OP_ADDR)->arg.p=s->varMem;
                }else{ // local variables
                    switch(s->type.tb){
                        case TB_INT:addInstrWithInt(&owner->fn.instr,OP_FPADDR_I,s->varIdx+1);break;
                        case TB_DOUBLE:addInstrWithInt(&owner->fn.instr,OP_FPADDR_F,s->varIdx+1);break;
                    }
                }
            }
        }
        if(s->kind==SK_PARAM){
            switch(s->type.tb){
                case TB_INT:
                    addInstrWithInt(&owner->fn.instr,OP_FPADDR_I,s->paramIdx-symbolsLen(s->owner->fn.params)-
                                                                 1); break;
                case TB_DOUBLE:
                    addInstrWithInt(&owner->fn.instr,OP_FPADDR_F,s->paramIdx-symbolsLen(s->owner->fn.params)-
                                                                 1); break;
            }
        }
        return true;
    }

    //if(owner)delInstrAfter(startInstr);
    //iTk = start;
    if (consume(INT)) {
        Token *ct = consumedTk;
        addInstrWithInt(&owner->fn.instr,OP_PUSH_I,ct->i);
		*r=(Ret){{TB_INT,NULL,-1},false,true};
		return true;
    }
    if (consume(DOUBLE)) {
        Token *ct = consumedTk;
        addInstrWithDouble(&owner->fn.instr,OP_PUSH_F,ct->d);
        *r=(Ret){{TB_DOUBLE,NULL,-1},false,true};
		return true;
    }
    if (consume(CHAR)) {
        Token *ct = consumedTk;
        *r=(Ret){{TB_CHAR,NULL,-1},false,true};
		return true;
    }
    if (consume(STRING)) {
        Token *ct = consumedTk;
        *r=(Ret){{TB_CHAR,NULL,0},false,true};
		return true;
    }
    if (consume(LPAR)) {
        if (expr(r)) {
            if (consume(RPAR)) {
                return true;
            } else tkerr("Lipsa ) dupa (");
        }
    }
    if(owner)delInstrAfter(startInstr);
    iTk = start;
    return false;
}

void parse(Token *tokens){
	iTk=tokens;
	if(!unit())tkerr("syntax error");
}