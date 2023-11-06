%option nounput
%option noinput
%option noyywrap

%{

#include <stdio.h>
#include <stdbool.h>
#include <assert.h>

#include "x-grammar.h"

/**
 * @brief Флаг наличия ошибки
 */
static bool GotError = false;

static int line = 1;

static void printLexeme(const char * tokName)
{
    printf("%s, \"%s\"\n",
           tokName,
           yytext);
}

static void incrLine(int num) { line += num; }
static int Line(void) { return line; }
#define YY_USER_ACTION { \
    yylloc.first_line   = Line(); \
}

%}

/* Здесь описываем состояния, в которые попадает лексический анализатор */

INTEGER [0-9]+
SPACE [ \t]*
IDENTIFIER [a-zA-Z_][a-zA-Z_0-9]*
NOT_AVIABLE [^a-zA-Z_0-9+\-*/><!;,=(){}\[\]&|'" \n\t]

%%
 /* Ключевые слова */
if                      { return TOK_IF; }
else                    { return TOK_ELSE; }
while                   { return TOK_WHILE; }
return                  { return TOK_RET; }
print                   { return TOK_PRINT; }

 /* Обработка единичных символов */
[+\-*/><!;,=(){}\[\]&]   { return yytext[0]; }

 /* Операции из нескольких символов */
"&&"                    { return TOK_LOGIC_AND; }
"||"                    { return TOK_LOGIC_OR; }
"=="                    { return TOK_IS_EQ; }
"!="                    { return TOK_IS_NOT_EQ; }
">="                    { return TOK_IS_GEQ; }
"<="                    { return TOK_IS_LEQ; }

{INTEGER}               { return TOK_INT; }
{IDENTIFIER}            { return TOK_IDENT; }
{SPACE}                 { /* Пробелы игнорируем*/ }

<*>\n                   { incrLine(1); }

 /* Все недопустимые символы являются ошибкой */
{NOT_AVIABLE}+          { printLexeme("ERROR"); GotError = true;}

 /* Здесь ловим то что не удалось поймать предыдущими правилами */
.                       { printLexeme("UNKNOWN"); }

<<EOF>>                 { printf("OK\n"); 
                          return GotError;}
%%
