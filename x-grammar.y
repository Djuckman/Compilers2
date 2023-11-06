%locations

%{
  #include "x-lexer.h"
  /* Funcs needed for bison */
  #define NOT_INSIDE_IF -1
  #define NAME_LENGTH 20
  #define STACK_SIZE 10
  #define HEAP_SPACE 25
  #define NO_MEM_LEFT_ON_HEAP 256
  #define TMP_HEAP "STORING"
  #define MARK "MARK"
  typedef struct expr_stack
  {
    int cur_idx;
    char* element[STACK_SIZE][NAME_LENGTH];
  } ex_stack;
  ex_stack stack; // is needed for expressions
  int count_marks = 0;
  int exit_marks_stack[STACK_SIZE] = { -1 };
  int next_marks_stack[STACK_SIZE] = { -1 };
  int while_stack[STACK_SIZE] = { -1 };
  int while_stack_out[STACK_SIZE] = { -1 };
  unsigned char heap_space[HEAP_SPACE] = { 0 }; // 0 - free; 1 - occupied
  char variables[NAME_LENGTH][NAME_LENGTH];
  void yyerror (char const * s);
  /* Our funcs */
  void init();
  void clean_stack();
  void insert(char* name);
  void check_var(char* name);
  void push(char* elem);
  void execute_binary(char *operation);
  void execute_unar(char* operation);
  int find_free_space();
  void occup_space(int idx);
  void free_space(int idx);
  void if_generation_start(); // here we take result expr from the stack and gen if-logic in asm
  void if_generation_end();
  void if_init();
  void if_deinit();
  void push_mark(int val, int* st);
  int pop_mark(int* st);
  void elif_pre_generation();
  char* get_resulting_variable();
  void start_while();
  void end_while();
  void while_gener_cond();
  void print_expr_asm(int res_idx, char* operand1, char* operation, char* operand2);
%}

%token TOK_IF TOK_ELSE TOK_WHILE TOK_RET TOK_PRINT
%token TOK_IDENT TOK_INT
%token TOK_LOGIC_AND TOK_LOGIC_OR TOK_IS_EQ TOK_IS_NOT_EQ
%token TOK_IS_GEQ TOK_IS_LEQ

%left '-' '+'
%left '*' '/'
%left TOK_LOGIC_AND TOK_LOGIC_OR TOK_IS_GEQ TOK_IS_LEQ '<' '>'
%left TOK_IS_EQ TOK_IS_NOT_EQ 
%precedence '!'     
/*
 * Begginning of the grammar
 */
%start program

%%
/* Аксиома грамматики */
program: { init(); }
  statements { }
;

statements:
  statements statement  { }
| %empty                { }
;

/* Allowed statements*/
statement:
  single_statement ';'  { }
| { if_init(); } if_statement          {  elif_pre_generation(); if_deinit(); } // elif_pre_generation if ends with elif
| while_statement       { }
;

/* Expressions that end with ';' ; ** ADD PRINT & RET <----------*/
single_statement:
  assign_expr       { }
| TOK_RET           { }
| print_expr        { }
;

/* Conditions */
if_statement:
  TOK_IF '(' expr ')' { if_generation_start(); clean_stack(); } '{' statements '}' { if_generation_end(); } elif_statement { }
;
/* Последовательность конструкций elif | TOK_ELIF -> ELSE + IF*/
elif_statement:
  TOK_ELSE TOK_IF { elif_pre_generation(); } '(' expr ')' { if_generation_start(); clean_stack(); } '{' statements '}' { if_generation_end(); } elif_statement    { }
| TOK_ELSE { elif_pre_generation(); } '{' statements '}'                                      { }
| %empty                                                           { }
;

/* Цикловая конструкция */
while_statement:
  TOK_WHILE { start_while(); } '(' loop_expr  ')' {while_gener_cond(); clean_stack(); } '{' statements '}' { end_while(); }
;

/* Expression for while loop */
loop_expr:
  expr      
;

/* Выражение с присваиванием */
assign_expr:
  ident_token { push(yytext); insert(yytext); } '=' expr  { execute_binary("="); clean_stack(); }
;

print_expr:
  TOK_PRINT ident_token { printf("push %s\ncall print\n", yytext); }
;

/* Допустимые выражения. Здесь допускается только rvalue (т.е. непосредственно вычисления) */
expr:
  const_token                                 {  push(yytext); }
| ident_token                                 {  check_var(yytext); push(yytext); }
| '(' expr ')'                                {  }
| expr '+' expr                               {  execute_binary("+"); }
| expr '-' expr                               {  execute_binary("-"); }
| expr '*' expr                               {  execute_binary("*"); }
| expr '/' expr                               {  execute_binary("/"); }
| expr '<' expr                               {  execute_binary("<"); }
| expr '>' expr                               {  execute_binary(">"); }
| expr TOK_LOGIC_AND expr                     {  execute_binary("&&"); }
| expr TOK_LOGIC_OR expr                      {  execute_binary("||"); }
| expr TOK_IS_EQ expr                         {  execute_binary("=="); }
| expr TOK_IS_NOT_EQ expr                     {  execute_binary("!="); }
| expr TOK_IS_GEQ expr                        {  execute_binary(">="); }
| expr TOK_IS_LEQ expr                        {  execute_binary("<="); }
| '!' expr                                    {  execute_unar("!"); }
;

/* Здесь обрабатываем лексему идентификатора */
ident_token:
  TOK_IDENT     { }
;

/* Здесь обрабатываем константные значения */
const_token:
  TOK_INT       { }
;
%%

void yyerror(char const * msg)
{
  printf("Line %d: ERROR\n", yylloc.first_line);
  exit(0);
}

void clean_stack() // cleans temporary objects, results of the exprs
{
  if (stack.cur_idx == 0)
    return;
  for (int i = stack.cur_idx - 1; i >= 0; i--)
  {
    if (strstr(stack.element[i], TMP_HEAP) != NULL)
    {
      int idx_to_free = atoi(stack.element[i]);
      //printf("Actually clearing %d (%s)\n", idx_to_free, stack.element[i]);
      free_space(idx_to_free);
    }
    memset(stack.element[i], 0x00, NAME_LENGTH * sizeof(char));
  }  
  stack.cur_idx = 0;
}

void init()
{
  for (int i = 0; i < STACK_SIZE; i++)
  {
    memset(stack.element[i], 0x00, NAME_LENGTH * sizeof(char));
  }
  stack.cur_idx = 0;
  // init stacks
  for (int i = 0; i < STACK_SIZE; i++)
  {
    exit_marks_stack[i] = -1;
    next_marks_stack[i] = -1;
    while_stack[i] = -1;
    while_stack_out[i] = -1;
    for (int j = 0; j < STACK_SIZE; j++)
    {
      variables[i][j] = '\0';
    }
  }
}

void push(char* elem)
{
  strcpy(stack.element[stack.cur_idx], elem);
  stack.cur_idx++;
}

void execute_unar(char* operation)
{
  if (stack.cur_idx - 1 < 0)
    yyerror("Problems with stack! (inside ogical part)");
  int idx_tmp_heap = 0;
  idx_tmp_heap = find_free_space();
  occup_space(idx_tmp_heap);
  if (operation[0] == '!')
  {
    printf("inv %d%s\n", idx_tmp_heap, TMP_HEAP);
  }
  if (strstr(stack.element[stack.cur_idx - 1], TMP_HEAP) != NULL)
  {
    int idx_to_free = atoi(stack.element[stack.cur_idx - 1]);
    free_space(idx_to_free);
  }
  memset(stack.element[stack.cur_idx - 1], 0x00, NAME_LENGTH * sizeof(char));
  sprintf(stack.element[stack.cur_idx - 1], "%d%s", idx_tmp_heap, TMP_HEAP);
}

void execute_binary(char* operation)
{
  if (stack.cur_idx - 2 < 0)
    yyerror("Problems with stack! (inside ogical part)");
  if (operation[0] != '=')
  {
    int idx_tmp_heap = 0;
    idx_tmp_heap = find_free_space();
    occup_space(idx_tmp_heap);
    print_expr_asm(idx_tmp_heap, stack.element[stack.cur_idx - 2], operation, stack.element[stack.cur_idx - 1]);
    //printf("%d%s = %s %s %s \n", idx_tmp_heap, TMP_HEAP, stack.element[stack.cur_idx - 2], operation, stack.element[stack.cur_idx - 1]);
    // free space on heap
    for (int i = 2; i >= 1; i--)
    {
      if (strstr(stack.element[stack.cur_idx - i], TMP_HEAP) != NULL)
      {
        int idx_to_free = atoi(stack.element[stack.cur_idx - i]);
        //printf("Free idx %d (%s)\n", idx_to_free, stack.element[stack.cur_idx - i]);
        free_space(idx_to_free);
      }
      memset(stack.element[stack.cur_idx - i], 0x00, NAME_LENGTH * sizeof(char));
    }
    sprintf(stack.element[stack.cur_idx - 2], "%d%s", idx_tmp_heap, TMP_HEAP);
    stack.cur_idx--;
  }
  else
  {
    //printf("%s %s %s \n", stack.element[stack.cur_idx - 2], operation, stack.element[stack.cur_idx - 1]);
    if (operation[0] == '=')
    {
      printf("push %s\n", stack.element[stack.cur_idx - 1]);
      printf("pop %s\n", stack.element[stack.cur_idx - 2]);
    }
    // need to clean stack after a command; is done after expr
  }
}

int find_free_space()
{
  for (int i = 0; i < HEAP_SPACE; i++)
    if (heap_space[i] == 0)
      return i;
  return NO_MEM_LEFT_ON_HEAP;
}

void occup_space(int idx)
{
  if (idx >= HEAP_SPACE) 
  {
    yyerror("Heap error!");
    return;
  }
  heap_space[idx] = 1;
}

void free_space(int idx)
{
  if (idx >= HEAP_SPACE) 
  {
    yyerror("Heap error!");
    return;
  }
  heap_space[idx] = 0;
}

void if_generation_start() // MARKcount_marks - mark to the next block in if-constr
{
  printf("test %s\njz MARK%d\n", get_resulting_variable(), count_marks);
  push_mark(count_marks, next_marks_stack);
  count_marks++;
}

void if_generation_end() // jmp to end int the end of the block
{
  int val = pop_mark(exit_marks_stack);
  push_mark(val, exit_marks_stack);
  printf("jmp %s%d\n", MARK, val);
}

void if_init()
{
  push_mark(count_marks, exit_marks_stack);
  count_marks++;
}

void if_deinit()
{
  printf("%s%d:\n", MARK, pop_mark(exit_marks_stack));
}

void push_mark(int val, int* st)
{
  if (st[0] == -1)
  {
    st[0] = val;
    return;
  }
  for (int i = STACK_SIZE - 1; i >= 0; i--)
  {
    if (st[i] != -1)
    {
      st[i + 1] = val;
      return;
    }
  }
}

int pop_mark(int* st)
{
  for (int i = STACK_SIZE - 1; i >= 0; i--)
  {
    if (st[i] != -1)
    {
      int tmp = st[i];
      st[i] = -1;
      return  tmp;
    }
  }
}

void elif_pre_generation() // put mark
{
  int mark_idx = pop_mark(next_marks_stack);
  printf("%s%d:\n", MARK, mark_idx);
}

char* get_resulting_variable()
{
  return stack.element[0];
}

void start_while()
{
  printf("%s%d:\n", MARK, count_marks);
  push_mark(count_marks, while_stack);
  count_marks++;
  push_mark(count_marks, while_stack_out);
  count_marks++;
}

void end_while()
{
  int return_mark = pop_mark(while_stack);
  printf("jmp %s%d\n", MARK, return_mark);
  return_mark = pop_mark(while_stack_out);
  printf("%s%d:\n", MARK, return_mark);
}

void while_gener_cond()
{
  int return_mark = pop_mark(while_stack_out);
  push_mark(return_mark, while_stack_out);
  printf("test %s\njz MARK%d\n", get_resulting_variable(), return_mark);
}

void print_expr_asm(int res_idx, char* operand1, char* operation, char* operand2)
{
  printf("push %s\n", operand1);
  printf("push %s\n", operand2);
  switch (operation[0])
  {
    case '+':
      printf("add %d%s\n", res_idx, TMP_HEAP);
      break;
    case '-':
      printf("sub %d%s\n", res_idx, TMP_HEAP);
      break;
    case '*':
      printf("imul %d%s\n", res_idx, TMP_HEAP);
      break;
    case '/':
      printf("div %d%s\n", res_idx, TMP_HEAP);
      break;
    case '&':
      if (operation[1] == '&')
        printf("and %d%s\n", res_idx, TMP_HEAP);
      break;
    case '|':
      printf("or %d%s\n", res_idx, TMP_HEAP);
      break;
    case '=':
      if (operation[1] == '=')
      {
        printf("cmpeq %d%s\n", res_idx, TMP_HEAP);
      }
      break;
    case '!':
      if (operation[1] == '=')
      {
        printf("cmpneq %d%s\n", res_idx, TMP_HEAP);
      }
      else
        printf("inv %d%s\n", res_idx, TMP_HEAP);
      break;
    case '>':
      if (operation[1] == '=')
      {
        printf("cmp %d%s\n", res_idx, TMP_HEAP);
        printf("push %d%s\n", res_idx, TMP_HEAP);
        printf("add %d%s, 1", res_idx, TMP_HEAP);
        printf("pop\n");
        printf("inv %d%s\n", res_idx, TMP_HEAP);
      }
      else{
      	printf("cmpgreater %d%s\n", res_idx, TMP_HEAP);
      }
      break;
    case '<':
      if (operation[1] == '=')
      {
        printf("cmp %d%s\n", res_idx, TMP_HEAP);
        printf("push %d%s\n", res_idx, TMP_HEAP);
        printf("sub %d%s, 1", res_idx, TMP_HEAP);
        printf("pop\n");
        printf("inv %d%s\n", res_idx, TMP_HEAP);
      }
      else{
        printf("cmpless %d%s\n", res_idx, TMP_HEAP);
      }
      break;
  }
  // printf("pop\n");
  // printf("pop\n");
}

void insert(char* name)
{
  int logic = 0;
  for (int i = 0; i < STACK_SIZE; i++)
  {
    if (variables[i][0] == '\0')
    {
      strcpy(variables[i], name);
      return;
    }
  }
}

void check_var(char* name)
{
  for (int i = 0; i < STACK_SIZE; i++)
  {
    if (!strcmp(variables[i], name))
    {
      return;
    }
  }
  yyerror("Unassigned variable");
}
