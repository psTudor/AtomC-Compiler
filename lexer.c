#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <stdbool.h>

#include "lexer.h"
#include "utils.h"

Token *tokens;	// single linked list of tokens
Token *lastTk;		// the last token in list

int line=1;		// the current line in the input file

// adds a token to the end of the tokens list and returns it
// sets its code and line
Token *addTk(int code)
{
	Token *tk = safeAlloc(sizeof(Token));
	tk->code = code;
	tk->line = line;
	tk->next = NULL;
	if (lastTk)
	{
		lastTk->next = tk;
	}
	else
	{
		tokens = tk;
	}
	lastTk = tk;
	return tk;
}

char *extract(const char *begin, const char *end)
{
	size_t length = end - begin;
	char *result = safeAlloc(length + 1);
	strncpy(result, begin, length);
	result[length] = '\0';
	return result;
}

Token *tokenize(const char *pch){
	const char *start;
	Token *tk;
	for(;;){
		switch(*pch){
        case ' ':
		case '\t': pch++; break;
		case '\r': // handles different kinds of newlines (Windows: \r\n, Linux: \n, MacOS, OS X: \r or \n)
			if (pch[1] == '\n') pch++;
			// fallthrough to \n
		case '\n':
			line++;
			pch++;
			break;
		case '\0':
			addTk(END);
			return tokens;
		case ',':addTk(COMMA);pch++;break;
		case '(':addTk(LPAR);pch++;break;
		case ')':addTk(RPAR);pch++;break;
		case '{':addTk(LACC);pch++;break;
		case '}':addTk(RACC);pch++;break;
		case '[':addTk(LBRACKET);pch++;break;
		case ']':addTk(RBRACKET);pch++;break;
		case ';':addTk(SEMICOLON);pch++;break;
		case '+':addTk(ADD);pch++;break;
		case '*':addTk(MUL);pch++;break;
		case '-':addTk(SUB);pch++;break;
		case '.':addTk(DOT);pch++;break;
		case '!':
			if (pch[1] == '=') {
				addTk(NOTEQ);
				pch += 2;
			} else {
				addTk(NOT);
				pch += 1;
			}
            break;
		case '|':
			if (pch[1] == '|') {
				addTk(OR);
				pch += 2;
			} else {
                err("A fost gasit doar un |");
            }
			break;
		case '<':
			if (pch[1] == '=') {
				addTk(LESSEQ);
				pch += 2;
			} else {
				addTk(LESS);
				pch++;
			}
            break;
		case '>':
			if (pch[1] == '=') {
				addTk(GREATEREQ);
				pch += 2;
			} else {
				addTk(GREATER);
				pch++;
			}
            break;
        case '=':
			if (pch[1] == '=') {
				addTk(EQUAL);
				pch += 2;
			} else {
				addTk(ASSIGN);
				pch++;
			}
			break;
		case '&':
			if (pch[1] == '&') {
				addTk(AND);
				pch += 2;
			} else {
                err("A fost gasit doar un &");
            }
			break;
		case '/':
			if (pch[1] == '/') {
				while (*pch != '\0' && *pch != '\n') {
					pch++;
				}
			}
			else {
				addTk(DIV);
				pch++;
			}
			break;

		default:
            if(isalpha(*pch)||*pch=='_'){
				for(start=pch++;isalnum(*pch)||*pch=='_';pch++){}
				char *text=extract(start,pch);
				if (strcmp(text, "char") == 0) addTk(TYPE_CHAR);
				else if(strcmp(text, "double") == 0) addTk(TYPE_DOUBLE);
				else if(strcmp(text, "else") == 0) addTk(ELSE);
				else if(strcmp(text, "if") == 0) addTk(IF);
				else if(strcmp(text, "int") == 0) addTk(TYPE_INT);
				else if(strcmp(text, "return") == 0) addTk(RETURN);
				else if(strcmp(text, "struct") == 0) addTk(STRUCT);
				else if(strcmp(text, "void") == 0) addTk(VOID);
				else if(strcmp(text, "while") == 0) addTk(WHILE);
				else {
					tk = addTk(ID);
					tk->text = text;
				}

                } else if (isdigit(*pch)) {
                    bool dotFlag = false;
                    bool eFlag = false;
                    for (start = pch++; isalnum(*pch) || *pch == '.' || *pch == 'e' || *pch == 'E' || *pch == '-' || *pch == '+'; pch++) {
                        if (*pch == '.') {
							pch++;
							if (isdigit(*pch)) {
								dotFlag = true;
							} else {
								err("Eroare double");
							}
                        }
                        if (*pch == 'e' || *pch == 'E') {
                            eFlag = true;
                        }
                    }
                    char *text = extract(start, pch);

                    if (dotFlag || eFlag) {
                        tk = addTk(DOUBLE);
                        char *end;
                        tk->d = strtod(text, &end);
                    } else {
                        tk = addTk(INT);
                        tk->i = atoi(text);
                    }

                } else if (isalpha(*pch) || *pch == '"' || *pch == '\'') {
                    bool charFlag  = false;
                    pch++;
                    for (start = pch++; isalnum(*pch) || *pch == '"' || *pch == '\''; pch++) {
                        if (*pch == '\'') {
							charFlag = true;
                        } 
                    }

                    if (charFlag) {
						char *character = extract(start, pch - 1);
                        tk = addTk(CHAR);
                        tk->c = *character;

					} else if (*(pch - 1) != '"' || (*(pch - 2) == '\\' && *(pch - 3) != '\\')) {
						err("Eroare inchidere string");
					} else {
						char *text = extract(start, pch - 1);
                        tk = addTk(STRING);
                        tk->text = text;
						}
                    } else {
                    err("invalid char: %c (%d)", *pch, *pch);
                }
		}
	}
}

void showTokens(const Token *tokens)
{
	int line_counter = 1;
	for (const Token *tk = tokens; tk; tk = tk->next) {
		printf("%d\t", tk->line);
		switch (tk->code) {
		case TYPE_INT:printf("TYPE_INT\n");break;
		case TYPE_CHAR:printf("TYPE_CHAR\n");break;
		case TYPE_DOUBLE:printf("TYPE_DOUBLE\n");break;
		case ID:printf("ID:%s\n", tk->text);break;
		case LPAR:	printf("LPAR\n");break;
		case RPAR:printf("RPAR\n");break;
		case LACC:printf("LACC\n");break;
		case RACC:printf("RACC\n");break;
		case LBRACKET:printf("LBRACKET\n");break;
		case RBRACKET:printf("RBRACKET\n");break;
		case SEMICOLON:printf("SEMICOLON\n");break;
		case INT:printf("INT:%d\n", tk->i);break;
		case DOUBLE:printf("DOUBLE:%f\n", tk->d);break;
		case STRING:printf("STRING:%s\n", tk->text);break;
		case CHAR:printf("CHAR:%c\n", tk->c);break;
		case WHILE:printf("WHILE\n");break;
		case LESS:printf("LESS\n");break;
		case DIV:printf("DIV\n");break;
		case ADD:printf("ADD\n");break;
		case AND:printf("AND\n");break;
		case MUL:printf("MUL\n");break;
		case IF:printf("IF\n");break;
		case ASSIGN:printf("ASSIGN\n");break;
		case EQUAL:printf("EQUAL\n");break;
		case RETURN:printf("RETURN\n");break;
		case END:printf("END\n");break;
		case ELSE:printf("ELSE\n");break;
		case STRUCT:printf("STRUCT\n");break;
		case VOID:printf("VOID\n");break;
		case SUB:printf("SUB\n");break;
		case OR:printf("OR\n");break;
		case NOT:printf("NOT\n");break;
		case NOTEQ:printf("NOTEQ\n");break;
		case LESSEQ:printf("LESSEQ\n");break;
		case GREATER:printf("GREATER\n");break;
		case GREATEREQ:printf("GREATEREQ\n");break;
		case DOT:printf("DOT\n");break;
		default:
			break;
		}
		line_counter += 1;
	}
}
