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
#ifndef GCC_REE_EVAL
#define GCC_REE_EVAL
 
/* Input bits : number of bits (width) 
   Output : the maximum value that we can represent on number with bits width  */ 
 
#define EVAL_BITS_TO_MAX_BITS_WORD_VALUE(bits) ((machine_x_length)((1 << bits) - 1)) 
#define GET_MASK(num_of_bits)(EVAL_BITS_TO_MAX_BITS_WORD_VALUE(num_of_bits))


typedef enum evaluation_type
{
   REG_EVALUATION = 0,
   CONST_EVALUATION = 1,
   EXPRESSION_EVALUATION = 2
   
}eval_type;



/**/
static machine_x_length eval_max_RegExpr_Value(rtx expr);

/* input : value  
   output : If the value length greater than (machine_word_length -1) 
            the function will return machine_word_length -1
            else the function will return the value length in bits 
            (the location of last 1 from LSB i.e if the machine word
            length 32 and the value = 0x05 the function will return 3)  */
static int eval_Get_Value_Width_In_Bits(machine_x_length value);

/* return the category of the evaluation problem */
static int eval_category(insns_to_value* node);

/* returns if the expression value is not supported on the current version */
static bool eval_is_value_not_supported(machine_x_length value);

/* evaluate Register value upper bound */
static machine_x_length eval_REG_upper_bound(insns_to_value *node);

/* evaluate math expression value upper bound*/
static machine_x_length eval_expr_upper_bound(insns_to_value *node);

/* The returned value is the node expression upper bound
   (tight upper bound i.e as small as posible)  */
static machine_x_length eval_node_cumulative_upper_bound(insns_to_value* node);

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
      2) the maximum number that can fit the REG width  */
static machine_x_length eval_expr_extremum_upper_bound(insns_to_value* node);

/* Evaluate Register extremum upper bound value
   1)if there is no candidate dependency (use def) on this function
    the Upper bound will be the max value that can fit on the REG mode
   2)else the upper bound will be the maximum value bettwen
     2.1) Maximum dependency Upper bound 
     and
     2.2)The max value that can fit on the REG mode (2.1 result) */
static machine_x_length  eval_REG_extremum_upper_bound(insns_to_value* node);

/* The returned value is the node expression extremum upper bound */
static machine_x_length eval_node_extremum_upper_bound(insns_to_value* node);

/* evaluate node as exremum upper bound if the node part of extremum subgraph
   else it will evaluate the node upper bound
   (tight upper bound i.e as small as posible) */
static machine_x_length eval_node_upper_bound(insns_to_value* node);


/* This function takes an un-supported expression(node) and marks the path 
   from the node to the root. each marked node later will be evaluated as the 
   maximum possible upper bound  */
static void eval_set_extreme_path_value(insns_to_value* node);

/* The function target to evaluate the first insn in the stack 
  (first pushed /last popped) by evaluating insn dependency the dependency graph
  is sorted in the stack that's is each entry on the stack have all his 
  dependency before it so when element popped from the stack here dependency 
  already evaluated 
   output: first pushed expression(insn) with updated upper bound  */
static  insns_to_value* eval_insn_value();

/* This function evaluate the insn Upper bound (the insn that we give as 
   argument to buildDB_build_insn_value_dependency_Data(rtx_insn*))  
   and determines if we need the zext Based on the insn upper bound  */
bool eval_zext_is_reachable_and_removable(rtx_insn* insn);

/* This function returns the mode length  */
static int eval_get_mode_length(rtx expr);

 

#endif //GCC_REE_EVAL