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
 
#ifndef GCC_REE_BUILD_DB
#define GCC_REE_BUILD_DB

#include "rtl.h"
#include <list>
#include <map>
#include <algorithm>
 




#define BUILDDB_ALLOW_SUPPORT_ON_ONE_IF_ONLY

#define RTX_INSN_NULL (rtx_insn*)0

 


/* IF_THEN_ELSE Nesting categorization */
enum ifelse_nesting_type{
  HAS_NO_NESTING = 0, 
  HAS_NESTING_IN_IF = 1,
  HAS_NESTING_IN_ELSE = 10,
  HAS_NESTING_IN_IF_ELSE = 11
};

enum node_dependency_problem{
  INSN_DEPENDENCY_PROBLEM = 0,
  OPERAND_DEPENDENCY_PROBLEM = 1,
  NO_DEPENDENCY_PROBLEM = 2
};


enum  insn_dependency_problem_mask{
  LAST_DEPENDENCY_PROBLEM_MASK = 0b1,
  IF_THEN_ELSE_DEPENDENCY_PROBLEM_MASK = 0b10,
  LAST_DEPENDENCY_BEFORE_IF_ELSE_PROBLEM_MASK = 0b100,
  LAST_DEPENDENCY_BETTWEN_IF_ELSE_PROBLEM_MASK = 0b1000,
  LOOP_DEPENDENCY_PROBLEM_MASK = 0b10000
 };

/** struct **/



struct if_then_else_node
{
  /* IF_THEN_ELSE instruction that the node is represinting */
  rtx_insn* insn;

  /*  If (condition == true code) first Basic Block index */
  int if_destBB_index;

  /*  Else(condition == false code) first Basic Block index 
  Note: in case we have only 'if' without 'else' the else_destBB_index value will be the Exit Basic block index*/
  int else_destBB_index;

  /* EXIT Basic block: next excuted BasicBlock after executing if/else code*/
  int ifelse_exit_bb_index;

  /* Unique id */
  int id;

  /* if there nested IF_THEN_ELSE*/
  bool hasNested;

  /* True if we are done updating all the node fields */
  bool isValid;

};


/***** Globals *****/

/***  RTX Database ***/

/* Map : (insn UID) --to-> (insn)  */
extern std::map<int,rtx_insn*> uidToInsnMap;

/* Map : (Basic block index) --to-> (BasicBlock )  */
extern std::map<int,basic_block> bbIndexToBBmap;


/***  IF_THEN_ELSE Database ***/

/* We don't support yet the case that candidate dependency can come from two different 
 IF_THEN_ELSE that write to the same REG.
 (i.e the last IF_THEN_ELSE has only one candidate dependency and no candidate between the two IF_THEN_ELSE insns). 
  also, we didn't support nested IF_THEN_ELSE as result for now we support only one IF_THEN_ELSE per function  */
extern bool functionHaveMoreThanOneIfThenElse;

/* Map : (IF_THEN_ELSE Exit Basic block Index) --> (IF_THEN_ELSE node)
 IF_THEN_ELSE EXIT BasicBlock is the first basic block that will be executed after the 'if/else' code */
extern std::map<int, if_then_else_node*> ExitBBtoIfElseNode;

/* Map : (IF_THEN_ELSE Basic Block index) --> (IF_THEN_ELSE Node)  */
extern std::map<int, if_then_else_node* > bbToIfElseNode;

/* List that hold IF_THE_ELSE insns for the current function*/
extern std::list <rtx_insn* > ifThenElseInsnsList;
 

/*** Value dependency Database ***/

/* Map : (node->id)-->(last candidate dependency List)
   mapping node id to list of the last dependency candidate  of the node expression (NOT ALL DEPENDENCY only the last dependency) */
extern std::map<int, std::list<if_then_else_node*> > exprId_to_LastDep;//<< ToDelete

/* Map : (node->id)-->(Map : (BB ID)-> (def insns in BB)) 
   Mapping expression ID to map(key=expression BB index , value = expression def insn in BB(key BB))*/
extern std::map<int/*node->id*/, std::map<int/*BB ID*/,std::list<rtx_insn*>/*Defs*/ > > exprId_to_mapUseDefToBBid;//<<To DELETE

/* Map : (BB ID)-> (def insns in BB) */
extern std::map<int, std::list<rtx_insn*>> BBindex_to_PredsDefs_map;

/* Map : (insn UID)-->( candidate dependency insns)*/
extern std::map<int, std::list<rtx_insn*>> InsnUID_to_DepInsns;

/** Value dependency intermediate data (temp) **/

/* */
extern auto_vec<rtx_insn *> insn_defs;

/* List of predecessors Basic blocks for the current instruction(insn) */
extern std::list<int> g_predsBBindex; 


/***** RTX Data Reference functions *****/

/* return bbIndexToBBmap map by reference */
std::map<int,basic_block>& buildDB_GetTableBB();

/* return uidToInsnMap map by reference */
std::map<int,rtx_insn*>& buildDB_GetTableInsn();


/***** IF_THEN_ELSE Data Reference functions *****/

/* return IF_THEN_ELSE insn List (ifThenElseInsnsList) by reference */
std::list <rtx_insn* >& buildDB_GetTableIfelseList();

/* return ExitBBtoIfElseNode map by reference */
std::map<int,if_then_else_node* >& buildDB_GetTableExitBBtoIfElseNode();

/* return bbToIfElseNode map by reference */
std::map<int,if_then_else_node* >& buildDB_GetTableIfElseNode();


/***** Value dependency Data Reference functions *****/

/* return exprId_to_mapUseDefToBBid map by reference */
std::map<int, std::map<int,std::list<rtx_insn*> > >& buildDB_GetTable_exprId_to_mapUseDefToBBid();//<<to delete

/* return BBindex_to_PredsDefs_map map by reference */
std::map<int, std::list<rtx_insn*>>& buildDB_GetTable_BBindex_to_PredsDefs_map();

/* return InsnUID_to_DepInsns map by reference */
std::map<int, std::list<rtx_insn*>>& buildDB_GetTable_InsnUID_to_DepInsns();

/* Return predsBBindex List by reference */
std::list<int>& buildDB_GetTable_g_predsBBindex();


/** APIs **/

 
/* if_then_else_node constructor */
static if_then_else_node * buildDB_if_then_else_node_constructor(rtx_insn * ins);

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
   and returns the jump insn if the Basic block does not function entry or function exit and 
   the Jump insn exist in the BB, else if there is no jump insn in the Basic block returns NULL 
   
   input : Basic block Index
*/
static rtx_insn* buildDB_find_jump_insn(int bbIndex);

/* If the kind of the jump_insns is : (jump_insn (set (pc)(label_ref ))
This function will return the basic block index of the jump_insn destination 
else it will return -1 */
static int buildDB_getJumpDestBBindex(rtx_insn* jmpinsn);

/* This function builds a list that contains all IF_THEN_ELSE insns of the current function */
static void buildDB_build_ifThenElse_list();

/* This function builds a new IF_THEN_ELSE node for each IF_THEN_ELSE insns and initializes 
   the map with the new nodes.
   NOTE: After executing the function only node->id and node->insn value will be valid*/
static void buildDB_bbToIfElseNode_initialize_map();

/* This function finding out the if /else  destination and updating the node 
   (updating : node->if_destBB_index, node->else_destBB_index)
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


/* This function builds if else Data Base 
   Output Data (globals):
		1) ifThenElseInsnsList :  list hold IF_THE_ELSE insns in current function
		2) bbToIfElseNode : Map  (IF_THEN_ELSE Basic Block index) --> (IF_THEN_ELSE Node)
		3) ExitBBtoIfElseNode : Map  (Exit Basic block Index) --> (IF_THEN_ELSE node)
   */
void buildDB_build_ifelseData();





/***** Insn Value Dependency Data *****/

/*
This function inserts insn on a map from the type : key = int value =  std::list<rtx_insn*>
the insn will be inserted on the element list (the element with key=id . list) 
if there is no such element(with key=id) it will create a new element with key =id and will insert the insn in the new element list
*/
static void buildDB_insert_insn_to_map_List(std::map<int, std::list<rtx_insn*>>& theMap, int id,rtx_insn* insn);

/* This function builds reachable definitions Data and returns true if there are reachable definitions instructions(insn).
  reachable definitions will be held on insn_defs vector */
static bool buildDB_build_expr_reaching_defs_Data(struct insns_to_value* node );

/*This function build global Map BBindex_to_PredsDefs_map : (Basic block index)--> (List of BasicBlock UseDefs )*/
static void buildDB_build_BBindex_to_PredsDefs_map(rtx_insn* currentInsn);

/* This function returns true if the dep_insn is predecessors to currentInsn 
   i.e if dep_insn in predecessors Basic block or come before currentInsn in the same basic block */
static bool buildDB_depInsn_predTo_currentInsn(rtx_insn* dep_insn, rtx_insn* currentInsn);

/*in this function, we build an Insn value dependency graph,
  we sort the graph on the stack(ST_MANAGE_STACK) for the eval Phase,
  where nodes on the stack head should be evaluated before nodes on the bottom of the stack.*/
void buildDB_build_insn_value_dependency_Data(rtx_insn* insn);

/* This function categorize the expression(node) dependency problem
   we have three different categories:
   1) there is no dependency, i.e if the expression is constant we have the expression value,
    and it does not depend on any expression or insn.
   2) insn dependency which means other instruction influencing or could influence the expression value.
    i.e if the current expression is src REG and other instruction could edit his value (note: we didn't support MEM/CALL).
   3) operand dependency i.e we have sub-expression as src operand (for example we write the result PLUS to expression destination)  
   */
static int buildDB_categorize_Node_dependency_problem(insns_to_value* node );

/* Push Src operands to the stacks */
static void buildDB_push_operands_to_stack(insns_to_value* node);

/* */
static bool buildDB_keyExistInMap(std::map<int, std::list<rtx_insn*>> theMap, int id);

/* return sorted list of curent and  predecessors basic blocks
  input : Basic block (current)
  output: sorted list containing current Basic block index and her predecessors basic blocks index */
static std::list<int> buildDB_get_bb_preds(basic_block bb);

/*This function builds global current insn predecessors basic blocks(indexs) sorted List g_predsBBindex */
static void buildDB_build_g_predsBBindex_List(basic_block bb);

/**/
static void buildDB_findAndPush_Last_Dependency_insn(insns_to_value* node);

/* This function find out if the current insn has a dependency on IF_THEN_ELSE code
   that's is if the IF_THEN_ELSE Exit Basic block and current insn Basic block is the same 
   Basic block, and some insn in IF/ELSE Basic block write to current insn Src REG */
static void buildDB_findAndPush_IfThenElse_Dependency_insns(insns_to_value* node);

/**/
static void buildDB_findAndPush_Last_Dependency_insn_before_IfThenElse(insns_to_value* node);

/* return true if bb1 is predecessor Basic  block for bb2 */
static bool buildDB_is_bb1_pred_to_bb2(int bb1_Index, int bb2_Index);

/* return expression first Src operand(index)*/
static int buildDB_get_firstSrcOperand_index(insns_to_value* node);

/* return true if current operand(expression) is a src operand */
static bool buildDB_isSrcOperand(insns_to_value * node, int opIndex);

/* */
static rtx_insn* buildDB_getBBLastInsn(std::list<rtx_insn*> defInsnsList);

/* */
static void buildDB_Push_candidate_insns_dependency_to_stack(insns_to_value* node);

/* This function returns a binary number where each bit on the number represents the problem(Take a look at enum  insn_dependency_problem_mask).
   for example, if we get as result 6(0b110) that's mean we have two problems loop problem and if_then_else problem */
static int buildDB_categorize_insn_dependency_problem();

static void buildDB_insn_dependency_data(insns_to_value* node);


/**** FREE DATA ***/

/*This function clear data valid per function*/
void buildDB_clear_function_Data();

/*This function clear data valid per function*/
void buildDB_clear_insn_Data();

 

#endif //GCC_REE_BUILD_DB