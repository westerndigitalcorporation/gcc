/**
  
author :  Ibrahim Qashqoush, Western Digital Corporation 
                                                   (ibrahim.qashqoush@wdc.com) 

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

 

/* This file manage two global generic stacks , origin stack and temp stack  */

#ifndef GCC_REE_STACK_MANAGE
#define GCC_REE_STACK_MANAGE
#include <stack>

typedef long long int machine_x_length;

 struct insns_to_value
{
  /* We have two types:
      1) D_BTM_INSN : if the node represent insn (rtx_insn)
      2) D_BTM_EXPR : if the node represent operand(sub insn) (rtx)  */
  int type;

  /* Point to last insn/expression(REG) that depend on current expression  */
  insns_to_value * father;

  /* Point to previous node on the stack  */
  insns_to_value * previous;

  /* Current instruction if the node is operand current_insn 
     will be the containing instruction 
     (i.e the instruction that containing the operand)  */
  rtx_insn *current_insn;

  /* Current expression  */
  rtx current_expr;

  /* Holds upper bound values of the expressions that the 
     current expression Depending on them (holds sons upper bound values)  */
  std::list<machine_x_length> operands_upper_bound;
 
  /* Holds current expression code (i.e REG,PLUS,CONST)  */
  rtx_code code;

  /* Current expression Upper boud  */
  machine_x_length upper_bound;

  /* Operand number used to calculate the id start counting from 1, 
     if current node is insn operanNum value will be 0.  */
  int opernad_num;

  /* Unique id to verify if we visted the expression before. */
  int id;

  bool valid_value;

  bool is_supported;

  bool is_visited;

  /* when extreme_value_path = true we evaluate the expression upper bound  
     as the maximum possible upper bound  */
  bool extreme_value_path;

};

  /* Used to chose a stack to work with it  */
  typedef enum stack_list{
  	STACK_MANAGE_ORIGIN = 0,
  	STACK_MANAGE_TEMP = 1
  }stack_manage_enum;


  /* Returns the chosen stack by reference  */
 static std::stack<insns_to_value*>& 
  stack_manage_get(stack_manage_enum stack_choice);

 
  /* This function push the node to the chosen stack
     Input:
       1) node
       2)stack_choise : the stack that we want to work with it
         stack_choise should be one of :
         2.1) (enum val=0) STACK_MANAGE_ORIGIN : origin stack 
         2.2) (enum val=1) STACK_MANAGE_TEMP : temp stack  */
  void 
  stack_manage_push(stack_manage_enum stack_choice, insns_to_value * node);


  /* This function pop node from the chosen stack
     Input:
     1)stack_choise : the stack that we want to work with it
        stack_choise should be one of :
          1.1) (enum val=0) STACK_MANAGE_ORIGIN : origin stack 
          1.2) (enum val=1) STACK_MANAGE_TEMP : temp stack  */
  void 
  stack_manage_pop(stack_manage_enum stack_choice);


  /* Return true if the chosen stack is empty
     1)stack_choise : the stack that we want to work with it
       stack_choise should be one of :
         1.1) (enum val=0) STACK_MANAGE_ORIGIN : origin stack 
         1.2) (enum val=1) STACK_MANAGE_TEMP : temp stack  */
  bool 
  stack_manage_is_empty(stack_manage_enum stack_choice);


  /* Return chosen stack head
     Input:
     1)stack_choise : the stack that we want to work with it
       stack_choise should be one of :
         1.1) (enum val=0) STACK_MANAGE_ORIGIN : origin stack 
         1.2) (enum val=1) STACK_MANAGE_TEMP : temp stack  */
 insns_to_value*
 stack_manage_top(stack_manage_enum stack_choice);


  /* 
   Pop and return the stack top (stack head)
   Input:
   1)stack_choise : the stack that we want to work with it
       stack_choise should be one of :
         1.1) (enum val=0) STACK_MANAGE_ORIGIN : origin stack 
         1.2) (enum val=1) STACK_MANAGE_TEMP : temp stack  */
  insns_to_value* 
  stack_manage_pop_and_return_top(stack_manage_enum stack_choice);
  
  
  /* Return the stack size  */
  int 
  stack_manage_size(stack_manage_enum stack_choice);

#endif //  GCC_REE_STACK_MANAGE