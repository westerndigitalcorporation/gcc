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



/*
Description:
*/
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
#include <math.h>
#include <algorithm>
#include <pthread.h>
#include "ree_stack_manage.h"
#include "ree_eval.h"
#include "machmode.h"




/* Returns false if we dont support the value*/
static bool eval_is_value_not_supported(long long int value)
{
  return (value < 0);
}


/* Return the category of the evaluation problem */
static int eval_evaluation_problem_category(insns_to_value* node)
{
  if((GET_RTX_CLASS (node->code) == RTX_CONST_OBJ))
    return CONST_EVALUATION;

  if(node->code == REG)
    return REG_EVALUATION;

  return EXPRESSION_EVALUATION;

}

/* Evaluate Const value upper bound */
static long long int eval_evaluate_const_upper_bound(insns_to_value *node)
{
  return XINT(node->current_expr, 0);
}

/* This Function return the max value that can fit in the Register mode */
static long long int eval_max_RegExpr_Value(rtx expr)
{
  long long int value;
  machine_mode mode;
  mode = GET_MODE(expr);

  switch (mode)
    {
      /* Bit */
      case BImode:
        value= 1;
        break;
      /* Quarter integer */
      case QImode:
        value= eval_bits_to_max_BitsWord_value(8-1);
        break;
      /* Half integer */
      case HImode:
        value= eval_bits_to_max_BitsWord_value(16-1);
        break;
      /* Single Integer */ 
      case SImode:
        value= eval_bits_to_max_BitsWord_value(32-1);
        break;

      /* Double Integer */
      case DImode:
        value= eval_bits_to_max_BitsWord_value(64-1);
        break;

      /* If we didn't define the mode on this switch case return the maximum value */
      default:
        value = eval_Machine_word_max_value();
    }

 return value;
}

/* Evaluate Register extremum upper bound value
   1)if there is no candidate dependency (use def) on this function
	 the Upper bound will be the max value that can fit on the REG mode
   2)else the upper bound will be the maximum value bettwen
	  2.1) Maximum dependency Upper bound 
	  and
	  2.2)The max value that can fit on the REG mode (2.1 result) */
static long long int eval_evaluate_REG_extremum_upper_bound(insns_to_value *node)
{
   long long int max_operand_value;
  std::list<long long int>::iterator OpValListIter;

  long long int currentRegExremum = eval_max_RegExpr_Value(node->current_expr);
  if(node->operands_upper_bound.begin() == node->operands_upper_bound.end() )
    return currentRegExremum;
  
  /* Get maximum dependency expression upper bound*/
  max_operand_value = * (node->operands_upper_bound.begin());
  for(OpValListIter = node->operands_upper_bound.begin(); OpValListIter != node->operands_upper_bound.end(); ++OpValListIter )
  {
    if( max_operand_value < (*OpValListIter))
      max_operand_value =  (*OpValListIter);
  }

  /* If current register upper bound greater than maximum dependency upper bound 
     return current register exremum else return maximum dependency upper bound */
  if(max_operand_value < currentRegExremum )
    return currentRegExremum;
  return max_operand_value;


}


/* Evaluate Register value upper bound 
   1)if there is no candidate dependency (use def) on this function
	 the Upper bound will be the max value that can fit on the REG mode
   2)else the Upper bound will be equal to the maximum dependency Upper bound 
   */
static long long int eval_evaluate_REG_upper_bound(insns_to_value *node)
{
  long long int max_operand_value;
  std::list<long long int>::iterator OpValListIter;

  long long int currentRegExremum = eval_max_RegExpr_Value(node->current_expr);
  if(node->operands_upper_bound.begin() == node->operands_upper_bound.end() )
    return currentRegExremum;

  /* Get maximum dependency expression upper bound */
  max_operand_value = * (node->operands_upper_bound.begin());
  for(OpValListIter = node->operands_upper_bound.begin(); OpValListIter != node->operands_upper_bound.end(); ++OpValListIter )
  {
    if( max_operand_value < (*OpValListIter))
      max_operand_value =  (*OpValListIter);
  }

  /* Return maximum dependency upper bound */
  return max_operand_value;

}


/* Evaluate general/math expression value upper bound */
static long long int eval_evaluate_expr_upper_bound(insns_to_value *node)
{
  std::list<long long int>::iterator OpValListIter;
  long long int value = 0;
  int i;
  long long int x,y; 

  if(node->operands_upper_bound.size()>1){
    x = * (node->operands_upper_bound.begin());
    y = * (++node->operands_upper_bound.begin());
  }
  OpValListIter = node->operands_upper_bound.begin();
  switch(node->code)
  {
    /* If the node code one of :NEG, ZERO_EXTEND,SET, SUBREG, SIGN_EXTEND, TRUNCATE 
	   then the Absolute value will still the same */
    case ZERO_EXTEND:
    case SET:
    case SUBREG:
    case SIGN_EXTEND:
    case TRUNCATE:
      value = *OpValListIter;
      break;
    
    case PLUS:
      value=0;
      for(OpValListIter = node->operands_upper_bound.begin(); OpValListIter != node->operands_upper_bound.end(); ++OpValListIter )
      {
        value=value + *OpValListIter;
      }
      break;

    case MINUS:
      value =x-y;
      break;

    case MULT:
      value=1;
      for(OpValListIter = node->operands_upper_bound.begin(); OpValListIter != node->operands_upper_bound.end(); ++OpValListIter )
      {
        value = value * (*OpValListIter);
      }
      break;

    case DIV:
      value =x/y;
      break;

    case MOD:
      value =x%y;
      break;

    case AND :
      value =x&y;
      break;

    case IOR:
      value =x|y;
      break;

    case XOR:
      value =x ^ y;
      break;

    case NOT:
      value = ~(*OpValListIter);
      break;

    case NEG:
      value =  0 - (*OpValListIter);
      break;

    case ASHIFT:
      value = x << y;
      break;

    case LSHIFTRT:
      value = x >> y;
      break;

    case ASHIFTRT:
      value = x >> y;
      if ((x < 0 ) && (y > 0))
        value = ( x >> y | ~(~0U >> y));//x>>y | 0xffff>>y
      break;

    /* If we didn't support the expression return -1 to be evaluated again as exremum upper bound */
    default:
      value = -1;
    
  }

  return value;

}


/* The returned value is the node expression upper bound */
static long long int eval_evaluate_node_upper_bound(insns_to_value* node)
{
  long long int  nodeUpperBound;
  int evaluation_problem_category;

  evaluation_problem_category = eval_evaluation_problem_category(node);

  switch (evaluation_problem_category)
  {
    case CONST_EVALUATION:
      nodeUpperBound = eval_evaluate_const_upper_bound(node);
      break;
    case REG_EVALUATION:
      nodeUpperBound = eval_evaluate_REG_upper_bound(node);
      break;
    case EXPRESSION_EVALUATION:
      nodeUpperBound = eval_evaluate_expr_upper_bound(node);
      break;
  }

  return nodeUpperBound;

}


/**/
static int eval_Get_Value_Width_In_Bits(long long int value)
{
  int bitsCounter=0;
  if(value<0)
	  value=0 - value;
  while(value != 0)
  {
    value = value >> 1;
    bitsCounter++;
  }
  return bitsCounter;
}


/* This function evaluate the expression extremum Upper bound 
   By evaluation the maximum number that can fit on the maximum result on bits that we could have
   for example: if we have the expression x = y + z 
   where y = any number with width 4'bits
         z = any number with width 7'bits
   x value can't be gretter than the minimum bettwen
     1) the maximum number with width of 8'bits (7+1)(the result width can't be more than the widt
	 and
	 2) the maximum number that can fit the REG width */
static long long int eval_evaluate_expr_extremum_upper_bound(insns_to_value* node)
{
  std::list<long long int>::iterator OpValListIter;
  long long int value=0;
  long long int machinWordMaxVal = eval_Machine_word_max_value();
  int i, x_InBits, y_InBits, value_InBits;
  long long int x,y; 
  /*If the expression have more than one operand save first operand upper value in 'x'
    and the second operand in 'y'*/
  if(node->operands_upper_bound.size()>1)
  {
    x = * (node->operands_upper_bound.begin());
    y = * (++node->operands_upper_bound.begin());
    x_InBits = eval_Get_Value_Width_In_Bits(x);
    y_InBits = eval_Get_Value_Width_In_Bits(y);

  }

  OpValListIter = node->operands_upper_bound.begin();
  
  
  switch(node->code)
  { 
   /* if the node code one of :
      NEG, ZERO_EXTEND,SET, SUBREG, SIGN_EXTEND, TRUNCATE then 
	  the upper bound will be the maximum number that can fit on the expression operand upper bound*/
    case NEG:
    case ZERO_EXTEND:
    case SET:
    case SUBREG:
    case SIGN_EXTEND:
    case TRUNCATE:
      value = *OpValListIter;
      value_InBits = eval_Get_Value_Width_In_Bits(value);
      value = eval_bits_to_max_BitsWord_value(value_InBits);
      break;
    
    /* If the  expression is Mod the maximum value can't be greater than the Mod second operand
	   i.e if we have expression z = x MOD y
	   z value can't be greater than y*/
    case MOD:
      value = eval_bits_to_max_BitsWord_value(y_InBits);
      break;
    /* By add/sub two numbere the result can't be represented on more than 
	   max number width(bit's) +1'bit   */
    case MINUS:
    case PLUS:
      value_InBits = x_InBits;
      if(y_InBits > x_InBits)
        value_InBits =y_InBits;
      /* value_InBits = value_InBits*2 */
      value_InBits=value_InBits+1;
      if(value_InBits < machinWordMaxVal )
        value = eval_bits_to_max_BitsWord_value(value_InBits);
      else
        value = eval_Machine_word_max_value(); 
      break;
    
    /* By execute the following codes result can't be wider than max operand width */
    case AND:
    case DIV:
    case XOR:
    case IOR:
      value_InBits = x_InBits;
      if(y_InBits > x_InBits)
        value_InBits =y_InBits;
      value = eval_bits_to_max_BitsWord_value(value_InBits);
      break;

    /* The mull result can't be wider thand the sum of operands width */
    case MULT:
      value_InBits = x_InBits + y_InBits;
      if(value_InBits < machinWordMaxVal )
        value = eval_bits_to_max_BitsWord_value(value_InBits);
      else
        value = eval_Machine_word_max_value(); 
      break;
    /* By shift the number y bits we will increase the width y bits */
    case ASHIFT:
      value_InBits = x_InBits+y;
      if(value_InBits < machinWordMaxVal)
        value = eval_bits_to_max_BitsWord_value(value_InBits);
      else
        value = eval_Machine_word_max_value(); 
      break;

    case LSHIFTRT:
      value_InBits = x_InBits-y;
      if(value_InBits <= 0)
        value = 0;
      else
        value = eval_bits_to_max_BitsWord_value(value_InBits); 
      break;
    


    default: //case NOT..
      value = eval_Machine_word_max_value( );
      break;


  }

}

/* this function evaluate the node upper bound.
   there is three different situations
   1) The Node is constant: on such situation the upper bound will be equals to the constant value
   2) The Node is REG: on such situation:
      2.1)if there is no candidate dependency (use def) on this function
	      the Upper bound will be the max value that can fit on the REG mode
	  2.2)else the upper bound will be the maximum value bettwen
	    2.2.1) Maximum dependency Upper bound 
		and
		2.2.2)The max value that can fit on the REG mode (2.1 result) 
   3)Expression:  evaluate the expression Upper value */
static long long int eval_evaluate_node_extremum_upper_bound(insns_to_value* node)
{
  long long int  nodeExtremumUpperBound;
  int evaluation_problem_category;

  evaluation_problem_category = eval_evaluation_problem_category(node);

  switch (evaluation_problem_category)
  {
    case CONST_EVALUATION:
      nodeExtremumUpperBound = eval_evaluate_const_upper_bound(node);
      break;
    case REG_EVALUATION:
      nodeExtremumUpperBound = eval_evaluate_REG_extremum_upper_bound(node);
      break;
    case EXPRESSION_EVALUATION:
      nodeExtremumUpperBound = eval_evaluate_expr_extremum_upper_bound(node);
      break;q
  }

  return nodeExtremumUpperBound;

}

/* Evaluate node as exremum upper bound if the node part of extremum subgraph
   else it will evaluate the node upper bound(tight upper bound i.e as small as posible) */
static long long int eval_evaluate_node(insns_to_value* node)
{
  if(node->extreme_value_path)
    return eval_evaluate_node_extremum_upper_bound(node);
  return eval_evaluate_node_upper_bound(node);

}

/* Return mask for first x'bits*/
static long long int eval_get_x_bit_Mask(int xBits)
{
  long long int mask = 1;
  int i;
  for(i = 1; i < xBits; i++)
  {
    mask = mask << 1;
    mask = mask + 1;
  }

  return mask;
}

/* Updae Node upper bound and push node upper bound to father operands_upper_bound List */
static void eval_update_upper_bound(insns_to_value* node, long long int value)<<<<<<<<<<<<move to buildDB???
{

  long long int machinWordMaxVal = eval_Machine_word_max_value();
  if(value > machinWordMaxVal)
    value = machinWordMaxVal;  

  node->upper_bound = value;
  if(node->father != NULL)
    node->father->operands_upper_bound.push_back(value);


}


/* This function takes an un-supported expression(node) and marks the path from the node to the root.
   each marked node later will be evaluated as the maximum possible upper bound */
static void eval_mark_extreme_value_path(insns_to_value* node)
{
  insns_to_value* cnode;
  cnode = node;
  while(cnode != NULL)
  {
    cnode->extreme_value_path=true;
    cnode=cnode->father;
  }

}



/*  The function target to evaluate the first insn upper bound in the stack  (first pushed /last popped),
    that's is the zext that we want to check if it is removable by evaluating insn dependency,
    the insn dependency graph is sorted in the stack that's is each entry on the stack have all his dependency before it.
    so when element popped from the stack here dependency already evaluated 
    output: first pushed expression(root insn) with updated upper bound    */
static insns_to_value* eval_evaluate_insn_value_dependency_stack()
{
  insns_to_value  *currentNode;

  while(!stack_manage_Stack_Is_Empty(ST_MANAGE_STACK))
  {
    currentNode = stack_manage_Stack_Top(ST_MANAGE_STACK);
    long long int expr_upper_bound;
    
    /* The default upper bound (before an evaluation) is the machine's maximum word value.
       if we can enhance the upper bound in evaluation we will update the upper bound value */
    currentNode->upper_bound = eval_Machine_word_max_value();

    /* If we don't support the expression code. mark the path from the current node to the root (subgraph)
       to be evaluated as extremum upper bound */
    if(! currentNode->is_supported)
      eval_mark_extreme_value_path(currentNode);

    /* Evaluate expression upper bound*/
    expr_upper_bound = eval_evaluate_node(currentNode);

    /* If expr_upper_bound value is not supported mark the node as not-supported ,
       mark the path  (subgraph) to be evaluated as extremum upper bound, and reevaluate 
       the expression upper bound as extremum upper bound */
    if(eval_is_value_not_supported(expr_upper_bound) )
    {
      currentNode->is_supported = false;
      eval_mark_extreme_value_path(currentNode); 
      expr_upper_bound = eval_evaluate_node(currentNode);
    }
    
    /* Updae Node upper bound and push node upper bound to father operands_upper_bound List */
    eval_update_upper_bound(currentNode, expr_upper_bound);

    /*If current expression is not the first pushed element (root insn) pop and free the node
      else only pop the node (we return the first element on the stack)*/
    if(stack.size() > 1)
      stack_manage_Stack_Pop_and_free(ST_MANAGE_STACK);
    else
      stack_manage_Stack_Pop(ST_MANAGE_STACK);


  }

  return currentNode;

}

/* This function evaluate the root insn Upper bound 
   (the root insn is the insn that we give as argument to buildDB_build_insn_value_dependency_Data(rtx_insn*))  
   and determines if we need the zext Based on the insn upper bound */
bool eval_zext_is_reachable_and_removable()
{
  rtx dest;
  insns_to_value  *node;
  long long int modeMaxValue;

  /* This Function evaluate the root insn Upper Bound by evaluating the insn dependency upper bound (recursively)*/
  node = eval_evaluate_insn_value_dependency_stack();

  /* Check if removable and return the result */
  if(node->code == SET)
  {
    if(GET_CODE(SET_SRC(node->current_expr ))== ZERO_EXTEND){

      dest=XEXP(SET_SRC(node->current_expr ),0);
      modeMaxValue = eval_max_RegExpr_Value(dest);
      if((modeMaxValue >= node->upper_bound )&&(node->upper_bound >= 0))
        return true;
    
    }

  }

  return false;

}