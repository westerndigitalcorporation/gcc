/**
  
author :  Ibrahim Qashqoush, Western Digital Corporation (ibrahim.qashqoush@wdc.com) 

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>. 
*/

/* This file managing insn value dependency stack.
   On building insn value dependency data (ree_BuildDB.C)  we push to the stack nodes
  Value range analyses(ree_range_analyses.c) use this stack to evaluate the range and update the nodes  */



#ifndef GCC_REE_STACK_MANAGE
#define GCC_REE_STACK_MANAGE


#include <stack>

/* Insns_to_value node types */
enum insns_to_value_types{
 D_BTM_INSN = 0,
 D_BTM_EXPR = 1
};

/* Used to chose a stack to work with it */
enum STACK_LIST{
	ST_MANAGE_STACK = 0,
	ST_MANAGE_TEMP_STACK = 1
};

struct insns_to_value
{
  /*We have two types:
      1) D_BTM_INSN : if the node represent insn (rtx_insn)
      2) D_BTM_EXPR : if the node represent operand(sub insn) (rtx)*/
  int type;

  /* Point to last insn/expression(REG) that depend on current expression */
  insns_to_value * father;

  /* Point to previous node on the stack */
  insns_to_value * previous;

  /* Current instruction if the node is operand current_insn will be the containing instruction
   (i.e the instruction that containing the operand) */
  rtx_insn *current_insn;

  /* Current expression */
  rtx current_expr;

  /* Holds upper bound values of the expressions that the current expression Depending on them
   (holds sons upper bound values) */
  std::list<long long int> operands_upper_bound;
 
  /* Holds current expression code (i.e REG,PLUS,CONST)*/
  rtx_code code;

  /* Current expression Upper boud */
  long long int upper_bound;

  /*Operand number used to calculate the id start counting from 1, 
    if current node is insn operanNum value will be 0.*/
  int opernadNum;

  /* Unique id to verify if we visted the expression before. */
  int id;

  bool valid_value;

  bool is_supported;

  bool is_visited;

  /* when extreme_value_path = true we evaluate the expression upper bound  as the maximum possible upper bound */
  bool extreme_value_path;

};


static std::stack< insns_to_value*> stack;
static std::stack< insns_to_value*> temp_stack;


/* Returns the chosen stack by reference*/
std::stack<insns_to_value*>& stack_manage_GetStack(int stack_choice);

/* returns true if we support the expression code */
static bool stack_manage_is_CODE_Supported(rtx_code  code);

/* insn_to_value_node constructor */
insns_to_value* stack_manage_insns_to_value_Node_constructor(rtx_insn *curr_insn,rtx curr_expr,int opernadNum ,insns_to_value *father, int type);

 

/* Returns unique ID for operand */
static int stack_manage_InsnToVal_calcOpernadId(insns_to_value * node);

/* Returns unique ID for expression (insn/operand) */
static int stack_manage_InsnToVal_calcId(insns_to_value * node);

 
/*
this function push the node to the chosen stack
input:
  1) node
  2)stack_choise (stack that we want to work with it ) :
      2.1) stack : (enum val=0) ST_MANAGE_STACK
      2.2) temp_stack : (enum val=1) ST_MANAGE_TEMP_STACK
*/
void stack_manage_push_insn_to_stack(int stack_choice, insns_to_value * node);

/*
this function pop node from the chosen stack
input:
  1)stack_choise should be one of :
      1.1) (enum val=0) ST_MANAGE_STACK : stack 
      1.2) (enum val=1) ST_MANAGE_TEMP_STACK : temp_stack 
*/
void stack_manage_Stack_Pop(int stack_choice);

/* return true if the chosen stack is empty */
bool stack_manage_Stack_Is_Empty(int stack_choice);

/* return chosen stack head*/
insns_to_value* stack_manage_Stack_Top(int stack_choice);


/*
this function pop and free node from the chosen stack
input:
  1)stack_choise should be one of :
      1.1) (enum val=0) ST_MANAGE_STACK : stack 
      1.2) (enum val=1) ST_MANAGE_TEMP_STACK : temp_stack 
*/
void stack_manage_Stack_Pop_and_free(int stack_choice);

/**/
void stack_manage_stack_free();


#endif //  GCC_REE_STACK_MANAGE