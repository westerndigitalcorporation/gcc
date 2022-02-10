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
This file builds a dependency graph for given instruction (function level) and prepares the dependency
graph in the stack for range value analyzer.

it containing also helpful and useful data.

constructors:
1) buildDB_build_RTX_extension_Data() : builds RTX Extension Data:
	Data:
	1.1) Map : (insn UID)-->(insn)
	1.2) Map : (Basic block index)-->(Basic Block)
	APIs:
	1.1) buildDB_get_bb(int id) :: input : Basic block index(id) , output : the basic block (where basicblock->index == id)
	1.2) buildDB_get_insn(int uid) input : insn uid, output: the insns  (where INSN_UID(insn) == id)

2) buildDB_build_ifelseData() : builds if_then_else Data
	Data:
	2.1) Map : bbToIfElseNode     : (IF_THEN_ELSE Basic Block index) --> (IF_THEN_ELSE Node) 
	2.2) Map : ExitBBtoIfElseNode : (Exit Basic block Index) --> (IF_THEN_ELSE node)

	IMPORTANT NOTE : Before calling buildDB_build_ifelseData() constructor 
					 buildDB_build_RTX_extension_Data() constructor SHOULD BE CALLED

3)  buildDB_build_insn_value_dependency_Data() : 

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
#include <algorithm>
#include <stack>
#include <pthread.h>

/* insns_to_value types */ 

#ifndef GCC_REE_BUILD_DB
#define GCC_REE_BUILD_DB

#define RTX_INSN_NULL (rtx_insn*)0

/*insns_to_value node type*/
enum insns_to_value_types{
 D_BTM_INSN = 0,
 D_BTM_EXPR = 1
};

/* IF_THEN_ELSE Nesting categorization */
enum ifelse_nesting_type{
  HAS_NO_NESTING = 0, 
  HAS_NESTING_IN_IF = 1,
  HAS_NESTING_IN_ELSE = 10,
  HAS_NESTING_IN_IF_ELSE = 11
};

/** structs **/

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

}buildDB_insns_to_value;



struct if_then_else_node
{
  /* IF_THEN_ELSE instruction that the node is represinting */
  rtx_insn* insn;

  /* First if(condition=true code) Basic Block index */
  int if_destBB_index;

  /* First else(condition=false code) Basic Block index 
  Note: in case we have only 'if' without 'else' the value will be the Exit Basic block index*/
  int else_destBB_index;

  /* EXIT Basic block: next excuted BasicBlock after executing if/else code*/
  int ifelse_exit_bb_index;

  /* Unique id */
  int id;

  /* if there nested IF_THEN_ELSE*/
  bool hasNested;

  /* we done updating all the node fields */
  bool isValid;

}buildDB_if_then_else_node;


/*** Globals ***/

/* We didn't support yet the case that candidate dependency can come from two different 
 IF_THEN_ELSE that write to the same REG.
 (i.e the last IF_THEN_ELSE has only one candidate dependency and no candidate between the two IF_THEN_ELSE insns). 
  also, we didn't support nested IF_THEN_ELSE as result for now we support only one IF_THEN_ELSE per function  */
bool functionHaveMoreThanOneIfThenElse;

/* Map : (Exit Basic block Index) --> (IF_THEN_ELSE node)
 IF_THEN_ELSE EXIT BasicBlock is the first basic block that will be executed after the 'if/else' code */
std::map<int, if_then_else_node*> ExitBBtoIfElseNode;

/* Map : (IF_THEN_ELSE Basic Block index) --> (IF_THEN_ELSE Node)  */
std::map<int, if_then_else_node* > bbToIfElseNode;

/* Map : (insn UID) --> (insn)  */
std::map<int,rtx_insn*> uidToInsnMap;

/* Basic block index to BasicBlock map */
std::map<int,basic_block> bbIndexToBBmap;

/* list hold IF_THE_ELSE insns on current function*/
std::list <rtx_insn* > ifThenElseInsnsList;



/***** reference functions *****/

/* return IF_THEN_ELSE insn List (ifThenElseInsnsList) by reference */
std::list <rtx_insn* >& buildDB_GetTableIfelseList();

/* return ExitBBtoIfElseNode map by reference */
std::map<int,if_then_else_node* >& buildDB_GetTableExitBBtoIfElseNode();

/* return bbIndexToBBmap map by reference */
std::map<int,basic_block>& buildDB_GetTableBB();

/* return uidToInsnMap map by reference */
std::map<int,rtx_insn*>& buildDB_GetTableInsn();

/* return bbToIfElseNode map by reference */
std::map<int,if_then_else_node* >& buildDB_GetTableIfElseNode();


/** APIs **/

 
/* insn_to_value_node constructor */
static insns_to_value* buildDB_insns_to_value_Node_constructor(rtx_insn *curr_insn,rtx curr_expr,int opernadNum ,insns_to_value *father, int type);

/* if_then_else_node constructor */
static if_then_else_node * buildDB_if_then_else_node_constructor(rtx_insn * ins);

/* Returns unique ID for operand */
static int buildDB_InsnToVal_calcOpernadId(insns_to_value * node);

/* Returns unique ID for expression (insn/operand) */
static int buildDB_InsnToVal_calcId(insns_to_value * node);


/******  	 RTX extension Data 	*****/

/* This function builds:
	Map : (insn UID)-->(insn)
	Map : (Basic block index)-->(Basic Block)
	for the current function
*/
void buildDB_build_RTX_extension_Data();


/* input: insn uid
   output : the insn (where INSN_UID(insn) == id)*/
rtx_insn* buildDB_get_insn(int id);


/* input : basic block index 
   output : the  basic block (where basic_block->index == id ) */
basic_block buildDB_get_bb(int id);



/*****		 IF_THEN_ELSE Data 		*****/
/* This function searches for jump insn on the given Basic block
   and returns the jump insn if the Basic block not function entry or function exit and 
   the Jump insn found, else if there is no jump insn in the Basic block returns NULL 
   
   input : Basic block Index
*/
static rtx_insn* buildDB_find_jump_insn(int bbIndex);

/* If the jump_insns kind is : (jump_insn (set (pc)(label_ref ))
This function returns the basic block index of the jump_insn destination 
else it will return -1 */
static int buildDB_getJumpDestBBindex(rtx_insn* jmpinsn);

/* this function builds a list that contains all IF_THEN_ELSE insns in the current function */
static void buildDB_build_ifThenElse_list();

/* This function build new IF_THEN_ELSE node for each IF_THEN_ELSE insns and initializing 
   the map with the new nodes.
   NOTE: after executing the function only node->id and node->insn value will be valid */
static void buildDB_bbToIfElseNode_initialize_map();

/* this function finding out the if /else  destination and updating the node (node->if_destBB_index, node->else_destBB_index)
   Note: we holding the if_then_else nodes on bbToIfElseNode Map */
static void buildDB_update_ifelse_node_destBB(if_then_else_node* node);

/* This function updates the if/else destination for all the nodes in bbToIfElseNode Map */
static void buildDB_update_bbToIfElseNode_ifelse_destBB();


/* This function categorize the nesting kind of IF_THEN_ELSE insn  
  and returns :
  HAS_NO_NESTING (enum val=0) : if there is no nested IF_THEN_ELSE.
  HAS_NESTING_IN_IF (enum val=1) : if there nested IF_THEN_ELSE only on the 'IF' code.
  HAS_NESTING_IN_ELSE (enum val=10) : if there nested IF_THEN_ELSE only on the 'else' code.
  HAS_NESTING_IN_IF_ELSE (enum val=11) : if there nested IF_THEN_ELSE in both 'if'/'else' code.
 */
static int buildDB_ifelse_nesting_categorization();


/* This function updates the IF_THEN_ELSE Exit basic block for all the nodes in bbToIfElseNode Map
   IF_THEN_ELSE Exit BasicBlock is the first basic block that will be executed after the 'if/else' code 
*/
static void buildDB_update_bbToIfElseNode_ifelse_ExitBB();

/* This Function build the ExitBBtoIfElseNode map
  ExitBBtoIfElseNode map :  (Exit Basic block Index) --> (IF_THEN_ELSE node) */
static void buildDB_build_ExitBBtoIfElseNode();


/* this function builds if else Data Base 
   Output Data (globals):
		1) ifThenElseInsnsList :  list hold IF_THE_ELSE insns in current function
		2) bbToIfElseNode : Map  (IF_THEN_ELSE Basic Block index) --> (IF_THEN_ELSE Node)
		3) ExitBBtoIfElseNode : Map  (Exit Basic block Index) --> (IF_THEN_ELSE node)
   */
void buildDB_build_ifelseData();





/***** Insn Value Dependency Data *****/

/* */
static void buildDB_build_reaching_defs_Data();

/* */
void buildDB_build_insn_value_dependency_Data();


/**** FREE DATA ***/

/*This function clear data valid per function*/
void buildDB_clear_function_Data();

/*This function clear data valid per function*/
void buildDB_clear_insn_Data();



#endif //GCC_REE_BUILD_DB