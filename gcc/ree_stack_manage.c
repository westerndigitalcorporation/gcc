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
  Value range analyses(ree_range_analyses.c) use this stack and update the nodes  */
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "df.h"
#include "memmodel.h"
#include "tm_p.h"
#include "optabs.h"
#include "regs.h"
#include "emit-rtl.h"
#include "recog.h"
#include "cfgrtl.h"
#include "expr.h"
#include "tree-pass.h"

#include <list>
#include <map>
#include <algorithm>
#include <pthread.h>
 
#include "ree_stack_manage.h"


/* Input  : Node 
   Output : Return unique ID for operand */
static int
stack_manage_InsnToVal_calcOpernadId(insns_to_value * node){
  int exprId,insnId,exprShift=10,tempInId;
  insns_to_value * fatherNode = node->father;
  
  insnId = fatherNode->id;
  tempInId = insnId;
  /* shifting the bits */
  while(tempInId > 9){
    exprShift=exprShift*10;
    tempInId=tempInId/10;
  }
  /* shift node opernand number and add father id to the result */
  exprId=(exprShift * node->opernadNum) + insnId; // exprId= opnum|insnId(father id) =  num|unique == unique

  return exprId;
}

/* Calculate and return expression ID
   Input  :  Node
   Output :  Unique ID for the expression 
*/
static int 
stack_manage_InsnToVal_calcId(insns_to_value * node)
{
  int idin;
  if(node->type == D_BTM_INSN){
    idin= INSN_UID(node->current_insn);
    return idin;
  }
  return  stack_manage_InsnToVal_calcOpernadId(node);
}


/* Input  :  Expression Code (i.e 'REG' , 'PLUS' et cetera )
   Output :  True if we support the expression code
             False else
*/
static bool 
stack_manage_is_CODE_Supported(rtx_code  code)
{
  if((code == MEM)||(code == CALL_INSN) || (code == CALL) )
    return false;
  return true;
}

/* Node  constructor 
  Input:
   curr_insn : the node insn
   curr_expr : the node expression
   opernadNum : operand number in the insn (if the node represents insn opernadNum will be zero)
   father : the node father 
   type : 
        if the node represents insn 'type' should be : D_BTM_INSN
		if the node represents insn 'type' should be : D_BTM_EXPR
  Output : pointer to the new node
		
   
*/
insns_to_value*                        
stack_manage_insns_to_value_Node_constructor(rtx_insn *curr_insn,rtx curr_expr,int opernadNum ,insns_to_value *father, int type)
{
  insns_to_value *node = new  insns_to_value; 
  rtx expr;
  node->type = type;
  node->father = father; 
  node->current_insn = curr_insn;

  node->current_expr = curr_expr;
  if(node->type == D_BTM_INSN)
    node->current_expr =single_set(node->current_insn);
  
  node->code = GET_CODE(node->current_expr);
  node->opernadNum = opernadNum;
  node->valid_value = false;
  node->is_supported = stack_manage_is_CODE_Supported(node->code);
  node->is_visited = false;
  node->extreme_value_path = false;
  node->id = stack_manage_InsnToVal_calcId(node);

return node;

}

/* Return chosen stack by reference */
std::stack<insns_to_value*>& stack_manage_GetStack(int stack_choice)
{
  if(stack_choice == ST_MANAGE_TEMP_STACK){
   static std::stack<insns_to_value*> temp_stack;
   return temp_stack;
  }

  /* stack_choice == ST_MANAGE_STACK */
  static std::stack<insns_to_value*> stack;
  return stack;
}


/*
This function push the node to the chosen stack
Input:
  1) node
  2) stack_choise (stack that we want to work with it ) :
      2.1) stack : (enum val=0) ST_MANAGE_STACK
      2.2) temp_stack : (enum val=1) ST_MANAGE_TEMP_STACK
*/
void 
stack_manage_push_insn_to_stack(int stack_choice, insns_to_value * node)
{
  std::stack<insns_to_value*>& st = stack_manage_GetStack(stack_choice);
  st.push(node);
}



/* Returns true if the chosen stack is empty */
bool 
stack_manage_Stack_Is_Empty(int stack_choice)
{
  std::stack<insns_to_value*>& st = stack_manage_GetStack(stack_choice);
  return st.empty();
}

/* Returns the chosen stack head*/
insns_to_value* 
stack_manage_Stack_Top(int stack_choice)
{
  std::stack<insns_to_value*>& st = stack_manage_GetStack(stack_choice);
  return st.top();
}

/*
This function pop the node from the chosen stack  
input:
  1)stack_choise should be one of :
      1.1) (enum val=0) ST_MANAGE_STACK : stack 
      1.2) (enum val=1) ST_MANAGE_TEMP_STACK : temp_stack 
*/
void 
stack_manage_Stack_Pop(int stack_choice)
{
    std::stack<insns_to_value*>& st = stack_manage_GetStack(stack_choice);
    st.pop();

}

/*
This function pop the node from the chosen stack and free it
Input:
  1)stack_choise should be one of :
      1.1) (enum val=0) ST_MANAGE_STACK : stack 
      1.2) (enum val=1) ST_MANAGE_TEMP_STACK : temp_stack 
*/
void stack_manage_Stack_Pop_and_free(int stack_choice)
{
    std::stack<insns_to_value*>& st = stack_manage_GetStack(stack_choice);
    insns_to_value* node = st.top();
    node->operands_upper_bound.clear();
    st.pop();
    free(node);


}