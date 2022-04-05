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
#ifndef GCC_REE_EVAL
#define GCC_REE_EVAL
/* return the max value that the target machine can handle.
   for example, if the machine target is 32'bit return the max value that we could have in 32'bit REG */
#define eval_Machine_word_max_value() ((long long int)pow(2, (UNITS_PER_WORD * 8) - 1 ) - 1); 
/* Input bits : number of bits (width) 
   Output : the maximum value that we can represent on number with bits width */
#define eval_bits_to_max_BitsWord_value(bits) ((long long int)pow(2, bits ) - 1) 


enum evaluation_problem
{
   REG_EVALUATION = 0,
   CONST_EVALUATION = 1,
   EXPRESSION_EVALUATION = 2
   
};

/* This function return false if we didn't support the expression code */
//static bool eval_is_code_supported(insns_to_value* node);

/* return mask for first x'bits*/
static long long int eval_get_x_bit_Mask(int xBits);

/**/
static long long int eval_max_RegExpr_Value(rtx expr);

/**/
static int eval_Get_Value_Width_In_Bits(long long int value);

/* return the category of the evaluation problem */
static int eval_evaluation_problem_category(insns_to_value* node);

/* returns if the expression value is not supported on the current version */
static bool eval_is_value_not_supported(long long int value);

/* evaluate Const value upper bound */
static long long int eval_evaluate_const_upper_bound(insns_to_value *node);

/* evaluate Register value upper bound */
static long long int eval_evaluate_REG_upper_bound(insns_to_value *node);

/* evaluate math expression value upper bound*/
static long long int eval_evaluate_expr_upper_bound(insns_to_value *node);

/* The returned value is the node expression upper bound(tight upper bound i.e as small as posible) */
static long long int eval_evaluate_node_upper_bound(insns_to_value* node);

/**/
static long long int eval_evaluate_expr_extremum_upper_bound(insns_to_value* node);

/**/
static long long int  eval_evaluate_REG_extremum_upper_bound(insns_to_value* node);

/* The returned value is the node expression extremum upper bound */
static long long int eval_evaluate_node_extremum_upper_bound(insns_to_value* node);

/* evaluate node as exremum upper bound if the node part of extremum subgraph
   else it will evaluate the node upper bound(tight upper bound i.e as small as posible) */
static long long int eval_evaluate_node(insns_to_value* node);


/* Updae Node upper bound and push node upper bound to father operands_upper_bound List */
static void eval_update_upper_bound(insns_to_value* node, long long int value);

/* This function takes an un-supported expression(node) and marks the path from the node to the root.
   each marked node later will be evaluated as the maximum possible upper bound*/
static void eval_mark_extreme_value_path(insns_to_value* node);

/* The function target to evaluate the first insn in the stack (first pushed /last popped)
   by evaluating insn dependency the dependency graph is sorted in the stack that's is each entry on the stack
   have all his dependency before it so when element popped from the stack here dependency already evaluated 
   output: first pushed expression(insn) with updated upper bound  */
static  insns_to_value* eval_evaluate_insn_value_dependency_stack();

/* This function evaluate the insn Upper bound (the insn that we give as argument to buildDB_build_insn_value_dependency_Data(rtx_insn*))  
   and determines if we need the zext Based on the insn upper bound */
bool eval_zext_is_reachable_and_removable();




 

#endif //GCC_REE_EVAL