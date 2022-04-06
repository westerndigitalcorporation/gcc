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
//#include "ree_stack_manage.h"
#include "ree_stack_manage.c"

#include "ree_BuildDB.h"

#include "ree_eval.h"
#include "machmode.h"
using namespace stm;
#define EVAL_MAGIC  0x03f08c5392f756cd
int eval_table[64] = {
     0,  1, 12,  2, 13, 22, 17,  3,
    14, 33, 23, 36, 18, 58, 28,  4,
    62, 15, 34, 26, 24, 48, 50, 37,
    19, 55, 59, 52, 29, 44, 39,  5,
    63, 11, 21, 16, 32, 35, 57, 27,
    61, 25, 47, 49, 54, 51, 43, 38,
    10, 20, 31, 56, 60, 46, 53, 42,
     9, 30, 45, 41,  8, 40,  7,  6,
};

//#define EVAL_GET_SET_BIT_INDEX(x) eval_table[((( __int128)(x & -x) * EVAL_MAGIC)) >> 58]
 /* this function return first 0 from lsb  */
 int EVAL_GET_SET_BIT_INDEX(long long int x) {

  unsigned long lsb = (~x) & -(~x);
  int result =  eval_table[(lsb * EVAL_MAGIC) >> 58] ;
  if(result == 0)
     return 64;
}

/* Returns false if we dont support the value*/
static bool eval_is_value_not_supported(machine_x_length value)
{   
  return (value < 0);
}


/* Return the category of the evaluation problem */
static int eval_category(insns_to_value* node)
{
  if((GET_RTX_CLASS (node->code) == RTX_CONST_OBJ))
    return CONST_EVALUATION;

  if(node->code == REG)
    return REG_EVALUATION;

  return EXPRESSION_EVALUATION;

}


/* This function returns the mode length  */
static int 
eval_get_mode_length(rtx expr)
{
  machine_mode mode;
    int mode_in_bytes;

  mode = GET_MODE(expr);
  if(mode == BImode)
    return 1;
  /* Dimode == 7 ... Qimode ==4 
     then 1 << (mode-1) give us the mode bits length on case of 
     Qimode, Himode , Simode  , Dimode  */
  mode_in_bytes = mode - 1;
  if((mode_in_bytes <= 6)&&(mode_in_bytes > 2))
    return (1 << mode_in_bytes );
  else
    return -1;

}


/* This Function return the max value that can fit in the Register mode */
static machine_x_length eval_max_RegExpr_Value(rtx expr)
{
  machine_x_length value;
  int mode_length;
  mode_length = eval_get_mode_length(expr);
  if(mode_length == -1 )
    value = MACHINE_WORD_MAX_VALUE;
  else
   value = EVAL_BITS_TO_MAX_BITS_WORD_VALUE(mode_length-1);

 return value;
}

/* Evaluate Register extremum upper bound value
   1)if there is no candidate dependency (use def) on this function
	 the Upper bound will be the max value that can fit on the REG mode
   2)else the upper bound will be the maximum value bettwen
	  2.1) Maximum dependency Upper bound 
	  and
	  2.2)The max value that can fit on the REG mode (2.1 result) */
static machine_x_length 
eval_REG_extremum_upper_bound(insns_to_value *node)
{
   machine_x_length max_operand_value;
  std::list<machine_x_length>::iterator OpValListIter;

  machine_x_length currentRegExremum = eval_max_RegExpr_Value(node->current_expr);
  /* if the register operands list is empty that's mean that on the current 
     function no predecessor expression use the  as register destenation 
     register  */
  if(node->operands_upper_bound.begin() == node->operands_upper_bound.end() )
  {
    /* We can't find out the register value so the register upper bound 
       will be the maximum value that the register could have  */
    return currentRegExremum;
  }
  
  /* Get maximum dependency expression upper bound  */
  max_operand_value = * (node->operands_upper_bound.begin());
  for(OpValListIter = node->operands_upper_bound.begin();
      OpValListIter != node->operands_upper_bound.end(); ++OpValListIter )
  {
    if( max_operand_value < (*OpValListIter))
      max_operand_value =  (*OpValListIter);
  }

  /* If current register upper bound greater than maximum dependency upper bound 
     return current register exremum else return maximum dependency upper 
     bound  */
  if(max_operand_value < currentRegExremum )
    return currentRegExremum;
  return max_operand_value;


}


/* Evaluate Register value upper bound 
   1)if there is no candidate dependency (use def) on this function
	 the Upper bound will be the max value that can fit on the REG mode
   2)else the Upper bound will be equal to the maximum dependency Upper bound  */
static machine_x_length eval_REG_upper_bound(insns_to_value *node)
{
  machine_x_length max_operand_value;
  std::list<machine_x_length>::iterator OpValListIter;

  machine_x_length currentRegExremum = eval_max_RegExpr_Value(node->current_expr);
  if(node->operands_upper_bound.begin() == node->operands_upper_bound.end() )
    return currentRegExremum;

  /* Get maximum dependency expression upper bound  */
  max_operand_value = * (node->operands_upper_bound.begin());
  for(OpValListIter = node->operands_upper_bound.begin();
      OpValListIter != node->operands_upper_bound.end(); ++OpValListIter )
  {
    if( max_operand_value < (*OpValListIter))
      max_operand_value =  (*OpValListIter);
  }

  /* Return maximum dependency upper bound  */
  return max_operand_value;

}


/* Evaluate general/math expression value upper bound */
static machine_x_length eval_expr_upper_bound(insns_to_value *node)
{
  std::list<machine_x_length>::iterator OpValListIter;
  machine_x_length value = 0;
  int i;
  machine_x_length x,y; 
  machine_x_length mode_length_mask;
  int mode_length;
  machine_mode mode;

   if(node->operands_upper_bound.size()>1){
    x = * (node->operands_upper_bound.begin());
    y = * (++node->operands_upper_bound.begin());
  }
  OpValListIter = node->operands_upper_bound.begin();
  switch(node->code)
  {
    /* If the node code one of :NEG,ZERO_EXTEND,SET,SUBREG,SIGN_EXTEND,TRUNCATE 
	   then the Absolute value will still the same  */
    case ZERO_EXTEND:
    case SET:
    case SUBREG:
    case SIGN_EXTEND:
    case TRUNCATE:
      value = *OpValListIter;

      break;
    
    case PLUS:
      value=0;
      for(OpValListIter = node->operands_upper_bound.begin();
          OpValListIter != node->operands_upper_bound.end(); ++OpValListIter )
      {
        value=value + *OpValListIter;
      }
      break;

    case MINUS:
      value =x-y;
      break;

    case MULT:
      value = 1;
      for(OpValListIter = node->operands_upper_bound.begin(); 
          OpValListIter != node->operands_upper_bound.end(); ++OpValListIter )
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

    /* If we didn't support the expression 
       return -1 to be evaluated again as exremum upper bound  */
    default:
      value = -1;
    
  }
  /* Check if mode MSB is 1 , if MSB is 1 we can't determine if it
     signed / unsigned in such case we return -1 , convert the node 
     to extremum node , update the extremum path and evaluate the node
     again as extremum node  */
  mode_length = eval_get_mode_length(XEXP (node->current_expr, 0));
  /* if we cant know the mode length return -1 to convert the node 
     to extremum node  */
  if(mode_length == -1)
    mode_length_mask = 0;
  else
    mode_length_mask = GET_MASK(mode_length-1);

  if((~mode_length_mask) & value != 0 )
    value = -1;
  else
    value = value & mode_length_mask;

  return value;

}


/* The returned value is the node expression upper bound */
static machine_x_length eval_node_cumulative_upper_bound(insns_to_value* node)
{
  machine_x_length  nodeUpperBound;
  int category;

  category = eval_category(node);

  switch (category)
  {
    case CONST_EVALUATION:
      nodeUpperBound = INTVAL(node->current_expr);
      break;
    case REG_EVALUATION:
      nodeUpperBound = eval_REG_upper_bound(node);
      break;
    case EXPRESSION_EVALUATION:
      nodeUpperBound =eval_expr_upper_bound(node);
      break;
  }

  return nodeUpperBound;

}


/* input : value  
   output : If the value length greater than (machine_word_length -1) 
            the function will return machine_word_length -1
            else the function will return the value length in bits 
            (the location of last 1 from LSB i.e if the machine word
            length 32 and the value = 0x05 the function will return 3)  */
static int eval_Get_Value_Width_In_Bits(machine_x_length value)
{
  int bitsCounter=0;
  /*  machine positive word length (sign) is machine_word_length -1  */
  int machine_positive_word_length =((UNITS_PER_WORD * 8) - 1 );
  machine_x_length machine_pword_mask = GET_MASK(machine_positive_word_length);
                                                 
  /* if the sign bit is on or we have overfolow 
     return max machine length -1  */
  if(((~machine_pword_mask) & value) != 0)
    return ((UNITS_PER_WORD * 8) - 1 );

  //  return EVAL_GET_SET_BIT_INDEX(value);
  while(value != 0)
  {
    value = value >> 1;
    bitsCounter++;
  }
  return bitsCounter;
}


/* This function evaluate the expression extremum Upper bound 
   By evaluation the maximum number that can fit on the maximum result on bits 
   that we could have
   for example: if we have the expression x = y + z 
   where y = any number with width 4'bits
         z = any number with width 7'bits
   x value can't be gretter than the minimum bettwen
     1) the maximum number with width of 8'bits (7+1)(the result width can't be  
     more than the width
	   and
	   2) the maximum number that can fit the REG width */
static machine_x_length 
eval_expr_extremum_upper_bound(insns_to_value* node)
{
  std::list<machine_x_length>::iterator OpValListIter;
  machine_x_length value=0;
  int machine_word_length = (UNITS_PER_WORD * 8);

  int i, x_max_bits_length, y_max_bits_length, value_max_bits_length;
  machine_x_length x,y; 
  /*If the expression have more than one operand  
    save first operand upper value in 'x' and the second operand in 'y' */
  if(node->operands_upper_bound.size()>1)
  {
    x = * (node->operands_upper_bound.begin());
    y = * (++node->operands_upper_bound.begin());
    x_max_bits_length = eval_Get_Value_Width_In_Bits(x);
    y_max_bits_length = eval_Get_Value_Width_In_Bits(y);

  }

  OpValListIter = node->operands_upper_bound.begin();
  
  
  switch(node->code)
  { 
   /* if the node code one of :
      NEG, ZERO_EXTEND,SET, SUBREG, SIGN_EXTEND, TRUNCATE then 
      the destenation length will be the same as the src  lengeth  */
    case NEG:
    case ZERO_EXTEND:
    case SET:
    case SUBREG:
    case SIGN_EXTEND:
    case TRUNCATE:
      value = *OpValListIter;
      value_max_bits_length = eval_Get_Value_Width_In_Bits(value);
      break;
    
    /* If the  expression is Mod the maximum value can't be greater than  
	     the Divisor */
    case MOD:
      value_max_bits_length = y_max_bits_length; 
      break;
    /* By add/sub two numbere the result can't be represented on more than 
	     maximum src length  +1'bit  */
    case MINUS:
    case PLUS:
      value_max_bits_length = x_max_bits_length;
      if(y_max_bits_length > x_max_bits_length)
        value_max_bits_length = y_max_bits_length;
      /* value_max_bits_length less or equal to value_max_bits_length*2 */
      value_max_bits_length=value_max_bits_length + 1;
      break;
    
    /* By execute the following codes result 
       can't be wider than the maximum operand length  */
    case AND:
    case DIV:
    case XOR:
    case IOR:
      value_max_bits_length = x_max_bits_length;
      if(y_max_bits_length > x_max_bits_length)
        value_max_bits_length = y_max_bits_length;
      break;

    /* The mull result can't be wider thand the sum of operands length 
       (than  Multiplicand length + Multiplier length)  */
    case MULT:
      value_max_bits_length = x_max_bits_length + y_max_bits_length;      
      break;

    /* By shift the number y bits we will increase the length by y bits */
    case ASHIFT:
      value_max_bits_length = x_max_bits_length + y;
      break;

    case LSHIFTRT:
      value_max_bits_length = x_max_bits_length - y;
      if(value_max_bits_length <= 0)
        value_max_bits_length = 1;
      break;

    default: 
      value_max_bits_length = machine_word_length;
      break;
  }
   if(value_max_bits_length > (machine_word_length - 1) )
        value_max_bits_length = machine_word_length - 1;
  value = EVAL_BITS_TO_MAX_BITS_WORD_VALUE(value_max_bits_length); 

  return value;
}

/* this function evaluate the node upper bound.
   there is three different situations
   1) The Node is constant: on such situation the upper bound will be equals to 
      the constant value.
   2) The Node is REG: on such situation:
      2.1)if there is no candidate dependency (use def) on this function
	      the Upper bound will be the max value that can fit on the REG mode
	  2.2)else the upper bound will be the maximum value bettwen
	    2.2.1) Maximum dependency Upper bound 
		and
		2.2.2)The max value that can fit on the REG mode (2.1 result) 
   3)Expression:  evaluate the expression Upper value */
static machine_x_length eval_node_extremum_upper_bound(insns_to_value* node)
{
  machine_x_length  nodeExtremumUpperBound;
  int category,value_max_bits_length;
  int machine_word_length = (UNITS_PER_WORD * 8);

  category = eval_category(node);

  switch (category)
  {
    case CONST_EVALUATION:
      nodeExtremumUpperBound = INTVAL(node->current_expr);
      value_max_bits_length = eval_Get_Value_Width_In_Bits(
                                                        nodeExtremumUpperBound);
      if((value_max_bits_length > (machine_word_length - 1)))
        value_max_bits_length = (machine_word_length - 1);
      nodeExtremumUpperBound = EVAL_BITS_TO_MAX_BITS_WORD_VALUE(
                                                         value_max_bits_length);
       break;
    case REG_EVALUATION:
      nodeExtremumUpperBound = eval_REG_extremum_upper_bound(node);
      break;
    case EXPRESSION_EVALUATION:
      nodeExtremumUpperBound = eval_expr_extremum_upper_bound(node);
      break;
  }

  return nodeExtremumUpperBound;

}

/* Evaluate node as exremum upper bound if the node part of extremum subgraph
   else it will evaluate the node upper bound
      (tight upper bound i.e as small as posible) */
static machine_x_length eval_node_upper_bound(insns_to_value* node)
{
  if(node->extreme_value_path)
    return eval_node_extremum_upper_bound(node);
  return eval_node_cumulative_upper_bound(node);

}

 



/* This function takes an un-supported expression(node) and 
   marks the path from the node to the root.
   each marked node later will be evaluated 
   as the maximum possible upper bound.  
   input : un supported node
   output : this function updates all the nodes on the path from the 
            current node(including) to the root(including) and set them
            as part the of extremum path to be evaluated as extremum
            evaluation.  */
static void eval_set_extreme_path_value(insns_to_value* node)
{
  /* we dont want to change the node pointer location so we use 
     temp_node pointer  */
  insns_to_value* temp_node;
  temp_node = node;
  while(temp_node != NULL)
  {
    temp_node->extreme_value_path = true;
    temp_node = temp_node->father;
  }

}



/*  The function target to evaluate the first insn upper bound in the stack  
    (first pushed /last popped), that's is the zext that we want to check if 
    if it is removable by evaluating insn dependency.
    the insn dependency graph is sorted in the stack 
    that's is each entry on the stack have all his dependency before it.
    so when element popped from the stack here dependency already evaluated 
    output: first pushed expression(root insn) with updated upper bound    */
static insns_to_value* eval_insn_value()
{
  insns_to_value  *currentNode, *tempn;
  machine_x_length machinWordMaxVal = MACHINE_WORD_MAX_VALUE;

  while(!stm::stack_manage_is_empty<insns_to_value>(STACK_MANAGE_ORIGIN))
  {
    currentNode = stm::stack_manage_top<insns_to_value>(STACK_MANAGE_ORIGIN);
    machine_x_length expr_upper_bound;
    
    /* The default upper bound (before an evaluation) is the machine's 
       maximum word value.
       if we can enhance the upper bound in evaluation we will update
       the upper bound value. */
    currentNode->upper_bound = machinWordMaxVal;

    /* If we don't support the expression code. mark the path from the current-
       -node to the root (subgraph) to be evaluated as extremum upper bound */
        
    if(! currentNode->is_supported)
    {
      /* mark new extremum path from currentNode to the root to be evaluated 
         as extremum evaluation  */
      eval_set_extreme_path_value(currentNode);
    }

    /* Evaluate expression upper bound */
    expr_upper_bound = eval_node_upper_bound(currentNode);

    /* If expr_upper_bound value is not supported mark the node as not-supported,
       mark the path  (subgraph) to be evaluated as extremum upper bound,
       and reevaluate the expression upper bound as extremum upper bound  */
    if(eval_is_value_not_supported(expr_upper_bound) )
    {
      currentNode->is_supported = false;
      /* mark new extremum path from currentNode to the root to be evaluated 
         as extremum evaluation  */
      eval_set_extreme_path_value(currentNode); 
      expr_upper_bound = eval_node_extremum_upper_bound(currentNode);
    }
    
    /* Updae Node upper bound and push
       node upper bound to father operands_upper_bound List  */

    /* if expr_upper_bound length greater than the machine word width 
       then update expr_upper_bound to be the machine word upper bound value  */
    if(expr_upper_bound > machinWordMaxVal)
      expr_upper_bound = machinWordMaxVal;  
    /* update current Node Upper bound */
    currentNode->upper_bound = expr_upper_bound;
    /* if the current Node had father, 
       push the Upper bound to currentNode father operands_upper_bound list  */
    if(currentNode->father != NULL)
      currentNode->father->operands_upper_bound.push_back(expr_upper_bound);

    /* Pop from the stack  */

    /* If current expression is not the first pushed element (root insn)
       pop and free the node else only pop the node  */
    tempn = stm::stack_manage_pop_and_return<insns_to_value>(
                                                        STACK_MANAGE_ORIGIN);
    /* If current expression is not the first pushed element  */
    if(stm::stack_manage_get<insns_to_value>(STACK_MANAGE_ORIGIN).size() > 1)
    {
      /* Free the node  */
     db_free_node(tempn);
    }

  }/* While  */

  return currentNode;

}

/* This function evaluates the root insn Upper bound 
   (the root insn is the insn that we give as argument to 
                         buildDB_build_insn_value_dependency_Data(rtx_insn*))  
   and determines if we need the zext Based on the insn upper bound 
   output:
     true : if the zext is removable
     false : if we can't determine if the zext is removable */
bool eval_zext_is_reachable_and_removable(rtx_insn* insn)
{
  rtx dest;
  insns_to_value  *node;
  machine_x_length modeMaxValue;
  /* build value dependency graph  */
  db_build_value_dependency_graph(insn);
  /* This function return the 
     first pushed node to the stack (root insn) with updated upper bound */
  node = eval_insn_value();

  /* Check if the insn type is (SET (Dest REG) (zero_extend(...)) ) */
  if(node->code == SET)
  {
    /* Check if the SET insn Src is ZERO_EXTEND */
    if(GET_CODE(SET_SRC(node->current_expr ))== ZERO_EXTEND)
    {
      /* Get the ZERO_EXTEND destenation  operand  */
      dest=XEXP(SET_SRC(node->current_expr ),0);
      /* Calculate the maximum value that can   
         fit on the register mode (the Zero Extend operand) */
      modeMaxValue = eval_max_RegExpr_Value(dest);
      /* If the zero extend doesn't cut the value
         (the src upper bound less than the dest mode upper bound) */
      if((modeMaxValue >= node->upper_bound ) && (node->upper_bound >= 0))
        return true;
    
    }

  }

  return false;

}
