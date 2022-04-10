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
This file builds a dependency graph for given instruction (function level) 
and prepares the dependency graph in the stack for range value analyzer.

it containing also helpful and useful data.

constructors:
1) db_build_rtx_extension_data() : builds RTX Extension Data:
	Data:
	1.1) Map : (insn UID)-->(insn)
	1.2) Map : (Basic block index)-->(Basic Block)
	APIs:
	1.1) db_get_bb(int id) :: 
        input : Basic block index(id) ,
        output : the basic block (where basicblock->index == id)

	1.2) db_get_insn(int uid) 
        input : insn uid, 
        output: the insns  (where INSN_UID(insn) == id)

2) db_build_ifelse_data() : builds if_then_else Data
	Data:
	2.1) Map : bb_to_if_else_node_map     : 
        (IF_THEN_ELSE Basic Block index) --> (IF_THEN_ELSE Node) 

	2.2) Map : if_else_bb_to_node_map : 
        (Exit Basic block Index) --> (IF_THEN_ELSE node)

	IMPORTANT NOTE : 
         Before calling db_build_ifelse_data() constructor 
			db_build_rtx_extension_data() constructor SHOULD BE CALLED

3)  db_build_value_dependency_graph () : 

*/
 
#ifndef GCC_REE_BUILD_DB
#define GCC_REE_BUILD_DB

#include "rtl.h"
#include <list>
#include <map>
#include <algorithm>
#include "ree_stack_manage.h"



 
#define BUILDDB_ALLOW_SUPPORT_ON_ONE_IF_ONLY

#define RTX_INSN_NULL (rtx_insn*)0

 /* return the max value that the target machine can handle.
   for example, if the machine target is 32'bit return the 
   max value that we could have in 32'bit REG  */
#define REE_MACHINE_WORD_MAX_VALUE()\
((machine_x_length)((1 << ((UNITS_PER_WORD * 8) - 1 )) - 1))

#define MACHINE_WORD_MAX_VALUE REE_MACHINE_WORD_MAX_VALUE()


typedef enum node_dependency_type{
  INSN_DEPENDENCY_PROBLEM = 0,
  OPERAND_DEPENDENCY_PROBLEM = 1,
  NO_DEPENDENCY_PROBLEM = 2
}db_dependency_type;


typedef enum  insn_dependency_type_mask{
  LAST_DEPENDENCY_PROBLEM_MASK = 0x1,
  IF_THEN_ELSE_DEPENDENCY_PROBLEM_MASK = 0x2,
  LAST_DEPENDENCY_BEFORE_IF_ELSE_PROBLEM_MASK = 0x4,
  LAST_DEPENDENCY_BETTWEN_IF_ELSE_PROBLEM_MASK = 0x8,
  LOOP_DEPENDENCY_PROBLEM_MASK = 0x10
 }db_insn_dependency_type_mask;


/* Insns_to_value node types */
typedef enum node_type{
 D_BTM_INSN = 0,
 D_BTM_EXPR = 1
}db_node_type;

/* * struct *  */



struct if_then_else_node
{
  /* IF_THEN_ELSE instruction that the node is represinting  */
  rtx_insn* insn;

  /* If (condition == true code) first Basic Block index  */
  int if_dest_bb_index;

  /* Else(condition == false code) first Basic Block index 
     Note: in case we have only 'if' without 'else' the else_dest_bb_index 
     value will be the Exit Basic block index  */
  int else_dest_bb_index;

  /* EXIT Basic block: next excuted BasicBlock after executing if/else code  */
  int ifelse_exit_bb_index;

  /* Unique id  */
  int id;

  /* if there nested IF_THEN_ELSE  */
  bool has_nested;

  /* True if we are done updating all the node fields  */
  bool is_valid;

};


/***** Globals *****/

/*** constructor Apis ***/

/* */
insns_to_value* 
db_create_node(rtx_insn *curr_insn,rtx curr_expr,
                           int opernad_num ,insns_to_value *father, int type);

/* */
static int 
db_calc_node_id(insns_to_value * node);

/* */
static bool 
db_is_code_supported(rtx_code  code);


/***  RTX Database ***/

/* Map : (insn UID) --to-> (insn)  */
extern std::map<int,rtx_insn*> uid_to_insn_map;

/* Map : (Basic block index) --to-> (BasicBlock )  */
extern std::map<int,basic_block> bb_index_to_bb_map;


/* **  IF_THEN_ELSE Database **  */

struct ifelse_data
{
/* Map : (IF_THEN_ELSE Exit Basic block Index) --> (IF_THEN_ELSE node)
 IF_THEN_ELSE EXIT BasicBlock is the first basic block 
 that will be executed after the 'if/else' code  */
std::map<int, if_then_else_node*> if_else_bb_to_node_map;

/* Map : (IF_THEN_ELSE Basic Block index) --> (IF_THEN_ELSE Node)  */
std::map<int, if_then_else_node* > bb_to_if_else_node_map;

/* List that hold IF_THE_ELSE insns for the current function  */
 std::list <rtx_insn* > if_then_else_insns_list;
 
};

/* ** Value dependency Database **  */

  /* */
 extern auto_vec<rtx_insn *> insn_defs; 

struct dependency_data
{
 /* Map : (BB ID)-> (def insns in BB)  */
 std::map<int, std::list<rtx_insn*>> bb_index_to_preds_defs_map;

 /* Map : (insn UID)-->( candidate dependency insns)*/
 std::map<int, std::list<rtx_insn*>> insn_uid_to_dep_insns;

/** Value dependency intermediate data (temp) **/

 /* List of predecessors Basic blocks for the current instruction(insn)  */
 std::list<int> g_preds_bb_index; 

 //std::list<int> processed_insns_id_history;; <<

};
/* main  */
void 
db_main(rtx_insn* insn);

/* **** RTX Data Reference functions ****  */

/* return bb_index_to_bb_map map by reference  */
std::map<int,basic_block>& db_get_bb_index_to_bb_map();

/* return uid_to_insn_map map by reference  */
std::map<int,rtx_insn*>& db_get_uid_to_insn_map();


/* **** IF_THEN_ELSE Data Reference functions ****  */

/* return IF_THEN_ELSE insn List (if_then_else_insns_list) by reference  */
//std::list <rtx_insn* >& db_get_ifelse_insns_list();

/* return if_else_bb_to_node map by reference  */
//std::map<int,if_then_else_node* >& db_get_ifelse_bb_to_node_map();

/* return bb_to_if_else_node_map by reference  */
//std::map<int,if_then_else_node* >& db_get_bb_to_if_else_node_map();


/* **** Value dependency Data Reference functions ****  */



/* * APIs *  */

 
/* if_then_else_node constructor  */
static if_then_else_node * 
db_create_ifelse_node(rtx_insn * ins);

/* *****  	 RTX extension Data 	****  */

/* This function builds:
	Map : (insn UID)-->(insn)
	Map : (Basic block index)-->(Basic Block)
	for the current function  */
void 
db_build_rtx_extension_data();


/* input: insn uid
   output : the insn (where INSN_UID(insn) == id)  */
rtx_insn* 
db_get_insn(int id);


/* input : basic block index 
   output : the  basic block (where basic_block->index == id )  */
basic_block 
db_get_bb(int id);



/* ****		 IF_THEN_ELSE Data 		****  */


/* This function searches for jump insn on the given Basic block
   and returns the jump insn if the Basic block contains jump insn and 
   the Basic block does not function entry or function exit , 
   else returns NULL .
   
   input : Basic block Index
   output : jump insn / NULL  */
static rtx_insn* 
db_find_jump_insn(int bbIndex);

/* If the kind of the jump_insns is : (jump_insn (set (pc)(label_ref ))
   This function will return the basic block index of the 
   jump_insn destination else it will return -1  */
static int 
db_get_jump_dest_bb(rtx_insn* jmpinsn);

/* This function builds a list that contains 
   all IF_THEN_ELSE insns of the current function  */
static void 
db_build_ifelse_list(ifelse_data *ifelse_data_node);

/* This function builds a new IF_THEN_ELSE node for each IF_THEN_ELSE insns 
   and initializes the map with the new nodes.
   NOTE: After executing the function only node->id and
         node->insn value will be valid  */
static void 
db_build_bb_to_ifelse_map(ifelse_data *ifelse_data_node);

/* This function finding out the if /else  destination and updating the node 
   (updating : node->if_dest_bb_index, node->else_dest_bb_index)
   Note: we holding the if_then_else nodes on bb_to_if_else_node Map  */
static void 
db_update_ifelse_node_dest_bb(if_then_else_node* node);

/* This function updates the if/else destination for all the nodes 
   in bb_to_if_else_node Map  */
static void 
db_update_map_dest_bb(ifelse_data *ifelse_data_node);


/*    */
static void 
db_update_ifelse_has_nested();


/* This function updates the IF_THEN_ELSE Exit basic block for all 
   the nodes in bb_to_if_else_node Map
   IF_THEN_ELSE Exit BasicBlock is the first basic block that will
   be executed after the 'if/else' code  */
static void 
db_update_map_exit_bb(ifelse_data *ifelse_data_node );

/* This Function build the if_else_bb_to_node map
  if_else_bb_to_node map :  
  (Exit Basic block Index) --> (IF_THEN_ELSE node) */
static void 
db_if_else_bb_to_node(ifelse_data *ifelse_data_node);


/* This function builds if else Data Base 
   Output Data (globals):
		1) if_then_else_insns_list :  
            list hold IF_THE_ELSE insns in current function

		2) bb_to_if_else_node_map : 
            Map  (IF_THEN_ELSE Basic Block index) --> (IF_THEN_ELSE Node)

		3) if_else_bb_to_node_map : 
            Map  (Exit Basic block Index) --> (IF_THEN_ELSE node)
   */
void 
db_build_ifelse_data();





/* **** Insn Value Dependency Data ****  */

/* This function inserts insn on a map from the type : 
   key = int value =  std::list<rtx_insn*> 
   the insn will be inserted on the element list 
   (the element with key=id . list) if there is no such element(with key=id) 
   it will create a new element with key =id and will insert the insn 
   in the new element list  */
static void 
db_insert_insn_to_map_List(std::map<int, std::list<rtx_insn*>>& theMap,
                                int id,rtx_insn* insn);

/* This function builds reachable definitions Data and returns true if 
   there are reachable definitions instructions(insn).
   reachable definitions will be held on insn_defs vector  */
static bool 
db_build_expr_reaching_defs_Data(struct insns_to_value* node );

/* This function build global Map bb_index_to_preds_defs_map :
   (Basic block index)--> (List of BasicBlock UseDefs )  */
static void 
db_bb_index_to_preds_defs_map(rtx_insn* currentInsn, 
                                          dependency_data * dependency_db);

/* This function returns true if the dep_insn is predecessors to currentInsn 
   i.e if dep_insn in predecessors Basic block or come before currentInsn in 
   the same basic block  */
static bool 
db_depInsn_predTo_currentInsn(rtx_insn* dep_insn, rtx_insn* currentInsn);

/* in this function, we build an Insn value dependency graph,
   we sort the graph on the stack(STACK_MANAGE_ORIGIN) for the eval Phase,
   where nodes on the stack head should be evaluated before nodes on the 
   bottom of the stack.  */
void 
db_build_value_dependency_graph (rtx_insn* insn);

/* This function categorize the expression(node) dependency problem
   we have three different categories:
   1) there is no dependency, i.e if the expression is constant we have the 
      expression value, and it does not depend on any expression or insn.
   2) insn dependency which means other instruction influencing or could 
      influence the expression value. i.e if the current expression is src 
      REG and other instruction could edit his value 
      (note: we didn't support MEM/CALL).
   3) operand dependency i.e we have sub-expression as src operand 
      (for example we write the result PLUS to expression destination)  */
static int 
db_get_dependency_type(insns_to_value* node );

/* Push Src operands to the stacks  */
static void 
db_push_operands(insns_to_value* node);

/* */
static bool 
db_keyExistInMap(std::map<int, std::list<rtx_insn*>> theMap, int id);


/* This function return true if there use def on the given Basic block  */
static bool 
db_is_bb_has_UseDefs( int bbId, dependency_data * dependency_db);


/* return sorted list of curent and  predecessors basic blocks
   input : Basic block (current)
   output: sorted list containing current Basic block index and her 
           predecessors basic blocks index  */
static std::list<int> 
db_get_bb_preds(basic_block bb);

/* This function copy to preds_list sorted list containing current Basic block
   index and her predecessors basic blocks index except to entry basic block.
       input :
            bb :  Basic block (current)

    input/output: 
            preds_list : this function add to the list  sorted list containing
            current Basic block index and her predecessors basic blocks index
            except to entry basic block  */  
static void 
db_build_basic_block_predecessors_List(basic_block bb,
                                                 std::list<int>& preds_list);

/**/
static void 
db_findAndPush_Last_Dependency_insn(insns_to_value* node, 
                                             dependency_data * dependency_db);

/* This function find out if the current insn has a dependency on
   IF_THEN_ELSE code that's is if the IF_THEN_ELSE Exit Basic block
   and current insn Basic block is the same Basic block, and some insn
   in IF/ELSE Basic block write to current insn Src REG  */
static void 
db_findAndPush_IfThenElse_Dependency_insns(insns_to_value* node, 
              ifelse_data *ifelse_data_node, dependency_data * dependency_db);

/**/
static void 
db_findAndPush_Last_Dependency_insn_before_IfThenElse(insns_to_value* node, 
             ifelse_data *ifelse_data_node, dependency_data * dependency_db);

/* return true if bb1 is predecessor Basic  block for bb2  */
static bool 
db_is_bb1_pred_to_bb2(int bb1_Index, int bb2_Index);

/* return expression first Src operand(index)  */
static int 
db_get_firstSrcOperand_index(insns_to_value* node);

/* return true if current operand(expression) is a src operand  */
static bool 
db_isSrcOperand(insns_to_value * node, int opIndex);

/* */
static rtx_insn* 
db_getBBLastInsn(std::list<rtx_insn*> defInsnsList);

/* This function return the closest predecessor Basic block with use def */
static int 
db_getClosestBBwithUseDef(std::list<int> predsBBindex,
                                             dependency_data * dependency_db);

/* This function push the node candidadte dependencies to the stack  */
static void 
db_Push_candidate_insns_dependency_to_stack(insns_to_value* node, 
                                             dependency_data * dependency_db);
 

/* This function returns a binary number where each bit on the mask 
   represents the problem(Take a look at enum  insn_dependency_problem_mask).
   for example, if we get as result 6(0b110) that's mean 
   we have two problems loop problem and if_then_else problem  */
static int 
db_categorize_insn_dependency_problem(insns_to_value* node,
              ifelse_data *ifelse_data_node, dependency_data * dependency_db);

static void 
db_insn_dependency_data(insns_to_value* node, ifelse_data *ifelse_data_node,
                        dependency_data * dependency_db);


/* *** FREE DATA **  */

/* This function clear data valid per function  */
void 
db_clear_function_Data(ifelse_data *ifelse_data_node, 
                                          dependency_data * dependency_db);

/* This function clear data valid per function  */
void 
db_clear_insn_Data(dependency_data * dependency_db);

/* Free node  */
void
db_free_node(insns_to_value* node);

#endif //GCC_REE_BUILD_DB