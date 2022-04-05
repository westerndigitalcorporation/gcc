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
SECTION 1  :
1) buildDB_build_RTX_extension_Data() : builds RTX Extension Data:
  Data:
  1.1) Map : (insn UID)-->(insn)
  1.2) Map : (Basic block index)-->(Basic Block)
  APIs:
  1.1) buildDB_get_bb(int id) :: input : Basic block index(id) , output : the basic block (where basicblock->index == id)
  1.2) buildDB_get_insn(int uid) input : insn uid, output: the insns  (where INSN_UID(insn) == id)

SECTION 2 :
2) buildDB_build_ifelseData() : builds if_then_else Data
  Data:
  2.1) Map : bbToIfElseNode     : (IF_THEN_ELSE Basic Block index) --> (IF_THEN_ELSE Node) 
  2.2) Map : ExitBBtoIfElseNode : (Exit Basic block Index) --> (IF_THEN_ELSE node)

  IMPORTANT NOTE : Before calling buildDB_build_ifelseData() constructor 
           buildDB_build_RTX_extension_Data() constructor SHOULD BE CALLED

SECTION 3 :
3)  buildDB_build_insn_value_dependency_Data() : 


destructor: 
  API:
  1)buildDB_clear_function_Data()
  2)buildDB_clear_insn_Data()

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
#include "ree_BuildDB.h"
#include "ree_stack_manage.h"
//#include "ree_eval.h"
#include <math.h>

 

 

extern  struct df_link * get_defs (rtx_insn *insn, rtx reg, vec<rtx_insn *> *dest);

/*##### SECTION 0 : Globals and struct constructors #####*/

/***** Globals *****/

/***  RTX Database ***/

/* Map : (insn UID) --> (insn)  */
std::map<int,rtx_insn*> uidToInsnMap;

/* Basic block index to BasicBlock map */
std::map<int,basic_block> bbIndexToBBmap;


/***  IF_THEN_ELSE Database ***/

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

/* list hold IF_THE_ELSE insns on current function*/
std::list <rtx_insn* > ifThenElseInsnsList;
 

/*** Value dependency Database ***/

/* Map : (node->id)-->(last candidate dependency List)
   mapping node id to list of the last dependency candidate  of the node expression (NOT ALL DEPENDENCY only the last dependency) */
std::map<int, std::list<if_then_else_node*> > exprId_to_LastDep; //<<TO DELETE

/* Map : (node->id)-->(Map : (BB ID)-> (def insns in BB)) 
   Mapping expression ID to map(key=expression BB index , value = expression def insn in BB(key BB))*/
std::map<int/*node->id*/, std::map<int/*BB ID*/,std::list<rtx_insn*>/*Defs*/ > > exprId_to_mapUseDefToBBid; //<<to DELETE

/** Value dependency intermediate data (temp) **/
auto_vec<rtx_insn *> insn_defs;


/* Reference functions */

/* Return IF_THEN_ELSE insn List (ifThenElseInsnsList) by reference */
std::list <rtx_insn* >&
buildDB_GetTableIfelseList()
{
  static std::list <rtx_insn* > ifThenElseInsnsList;
  return ifThenElseInsnsList;
}

/* Return ExitBBtoIfElseNode map by reference */
std::map<int,if_then_else_node*>& 
buildDB_GetTableExitBBtoIfElseNode()
{
  static std::map<int,if_then_else_node*> ExitBBtoIfElseNode;
  return ExitBBtoIfElseNode;
}

/* Return bbIndexToBBmap map by reference */
std::map<int,basic_block>& 
buildDB_GetTableBB()
{
  static std::map<int,basic_block> bbIndexToBBmap;
  return bbIndexToBBmap;
}

/* Return uidToInsnMap map by reference */
std::map<int,rtx_insn*>& 
buildDB_GetTableInsn()
{
  static std::map<int,rtx_insn*> uidToInsnMap;
  return uidToInsnMap;
}

/* Return bbToIfElseNode map by reference */
std::map<int,if_then_else_node*>& 
buildDB_GetTableIfElseNode()
{
  static std::map<int,if_then_else_node*> bbToIfElseNode;
  return bbToIfElseNode;
}

/* Return exprId_to_mapUseDefToBBid map by reference */
std::map<int, std::map<int,std::list<rtx_insn*> > >& 
buildDB_GetTable_exprId_to_mapUseDefToBBid()
{
  static std::map<int, std::map<int,std::list<rtx_insn*> >  > exprId_to_mapUseDefToBBid;
  return exprId_to_mapUseDefToBBid; //<<TO DELETE
}

/* Return BBindex_to_PredsDefs_map map by reference*/
std::map<int, std::list<rtx_insn*>>& 
buildDB_GetTable_BBindex_to_PredsDefs_map()
{
  static std::map<int, std::list<rtx_insn*>> BBindex_to_PredsDefs_map;
  return BBindex_to_PredsDefs_map;
}

/* Return InsnUID_to_DepInsns map by reference*/
std::map<int, std::list<rtx_insn*>>& 
buildDB_GetTable_InsnUID_to_DepInsns()
{
  static std::map<int, std::list<rtx_insn*>> InsnUID_to_DepInsns;
  return InsnUID_to_DepInsns;
}

/* Return g_predsBBinde List by reference */
std::list<int>& 
buildDB_GetTable_g_predsBBindex()
{
  static std::list<int> predsBBindex;
  return predsBBindex;
}



/*** structs constructors ***/


 

/* if_then_else_node constructor */
static if_then_else_node *
buildDB_if_then_else_node_constructor(rtx_insn * ins)
{
 if_then_else_node *node = new  if_then_else_node;
 node->insn=ins;
 node->isValid = false;
 node->hasNested = false;
 node->id=INSN_UID(ins);
 node-> ifelse_exit_bb_index = -1;
 node-> if_destBB_index = -1;
 node-> else_destBB_index = -1;
 return node;
}


/*##### SECTION 1 : RTX extension Data #####*/


/* This function builds:
  Map : (insn UID)-->(insn)
  Map : (Basic block index)-->(Basic Block)
  for the current function
*/
void 
buildDB_build_RTX_extension_Data(){
std::map<int,basic_block>& bbIndexToBBmap = buildDB_GetTableBB();
std::map<int,rtx_insn*>& uidToInsnMap = buildDB_GetTableInsn();
basic_block bb;
rtx_insn *insn,*insncopy;
int id;
  /* For each basic block (bb) except to Entry/Exit BasicBlocks */
  FOR_EACH_BB_FN (bb, cfun)
  {
    basic_block newbb=bb;
    /*Insert new element to map : (Basic block index)-->(Basic Block) */
    bbIndexToBBmap.insert (std::pair<int, basic_block >(newbb->index,newbb));
    /* For each insn in bb */
    FOR_BB_INSNS (bb, insn)
    {
      insncopy=insn;
      id=INSN_UID(insncopy);
      /*Insert new element to map : (insn UID)-->(insn) */
      uidToInsnMap.insert (std::pair<int, rtx_insn* >(id,insncopy));
    }
  }
}


/* Input: insn uid
   Output : the insn (where INSN_UID(insn) == id) */
rtx_insn*
buildDB_get_insn(int id){
  std::map<int,rtx_insn*>& uidToInsnMap = buildDB_GetTableInsn();
  std::map<int, rtx_insn* >::iterator theMapIt;
  
  /* Find and return the insn */
  theMapIt=uidToInsnMap.find(id);
   return theMapIt->second;
}

/* Input : basic block index 
   Output : the  basic block (where basic_block->index == id ) */
basic_block
buildDB_get_bb(int id){
    std::map<int,basic_block>& bbIndexToBBmap= buildDB_GetTableBB();
    std::map<int, basic_block >::iterator theMapIt;

    theMapIt=bbIndexToBBmap.find(id);
    /* If the basic block does not exist in the map return NULL*/
    if(theMapIt==bbIndexToBBmap.end())
      return NULL;

    /* Return the basic block*/
    return theMapIt->second;
}




/*##### SECTION 2 :  IF_THEN_ELSE Data  #####*/


/* 
   This function searches for jump insn on the given Basic block
   and returns the jump insn if the Basic block does not function entry or function exit and 
   the Jump insn exist in the BB, else if there is no jump insn in the Basic block returns NULL 
   
   input : Basic block Index
*/
static rtx_insn*
buildDB_find_jump_insn(int bbIndex){
  rtx_insn* bbinsn;
  basic_block bb;

  /* If current basci block is entry block or exit block return NULL */
  if((bbIndex == ENTRY_BLOCK) || (bbIndex == EXIT_BLOCK))
    return NULL;

  /* Get the Basic block */
  bb = buildDB_get_bb(bbIndex);

  gcc_assert(bb != NULL);

  /* for each insn in the Basic block if the insn is jump insn return it */
  FOR_BB_INSNS (bb, bbinsn)
  {
    if(GET_CODE(bbinsn) == JUMP_INSN)
      return  bbinsn;    
  }

  /* if the  basic block does not contain jump insn */
  return NULL;
}


/* If the kind of the jump_insns is : (jump_insn (set (pc)(label_ref ))
   This function will return the basic block index of the jump_insn destination 
   else it will return -1 */
static int
buildDB_getJumpDestBBindex(rtx_insn* jmpinsn){
 
  rtx_insn* nextInsn;

  /* Check jump_insn kind  */
  rtx label_ref = XEXP(single_set(jmpinsn),1); 
  if(GET_CODE(label_ref) != LABEL_REF)
    return -1;

  /* Get code label as rtx_insn */
  rtx code_label = XEXP(label_ref,0);
  nextInsn = buildDB_get_insn(INSN_UID(code_label));

  /* Find first insn after code label */
  while((GET_CODE(nextInsn)!=INSN) && (GET_CODE(nextInsn)!=CALL_INSN) && (GET_CODE(nextInsn)!=JUMP_INSN))
  {
    nextInsn=NEXT_INSN(nextInsn);
    if(nextInsn == NULL)
       break; 
  }
   
   /* If there is insns on the basic block 
      return the index of the jump destenation Basic block
      else return -1 */
    if(nextInsn != NULL)
      return BLOCK_FOR_INSN(nextInsn)->index;
    return -1;
}

 
/* This function builds a list that contains all IF_THEN_ELSE insns of the current function */
static void
buildDB_build_ifThenElse_list()
{
  basic_block bb;
  rtx_insn *insn;
  std::list<rtx_insn* >& ifThenElseInsnsList = buildDB_GetTableIfelseList();

  /* For each basic block in function except to ENTRY/EXIT Basic block*/
  FOR_EACH_BB_FN (bb, cfun)
  {
    /* For each insn in Basic block*/
    FOR_BB_INSNS (bb, insn)
    {
      /* If the current insn is IF_THEN_ELSE push it to the list */
      if(GET_CODE(insn) == JUMP_INSN)
        /* Skip jump insns like : jump_insn 174 221 175 12 (eh_return)*/
        if(NULL_RTX != single_set(insn) ) 
          /* If the jump insns type is: (jump_insn(set(pc) (IF_THEN_ELSE)...)) push insn to the List */
          if(GET_CODE(XEXP(single_set(insn),1)) == IF_THEN_ELSE )
            ifThenElseInsnsList.push_back(insn);
    }
  }
}


/* This function builds a new IF_THEN_ELSE node for each IF_THEN_ELSE insns and initializes 
   the map with the new nodes.
   NOTE: After executing the function only node->id and node->insn value will be valid */
static void
buildDB_bbToIfElseNode_initialize_map()
{
  int bbIndex;

  /* IF_THEN_ELSE insns list */
  std::list <rtx_insn* >& ifThenElseInsnsList = buildDB_GetTableIfelseList();
  std::list <rtx_insn* >::iterator IfElseListIter;

  /* Map : (IF_THEN_ELSE Basic Block index) --> (IF_THEN_ELSE Node) */
  std::map<int, if_then_else_node* >& bbToIfElseNode = buildDB_GetTableIfElseNode();
  std::map<int, if_then_else_node*>::iterator theMapIt;
   

  /* For each if_then_else insn */
  for(IfElseListIter=ifThenElseInsnsList.begin(); IfElseListIter != ifThenElseInsnsList.end(); ++IfElseListIter)
  {
   
    /* Get IF_THEN_ELSE insn basic block index */
    bbIndex=BLOCK_FOR_INSN(*IfElseListIter)->index;
   
    /* Creat new if_then_else node (initial node) */
    if_then_else_node* ifelsenode = buildDB_if_then_else_node_constructor( *IfElseListIter);
      
    /* If there is more than IF_THEN_ELSE insns on the same basic block then there is a Bug */
    theMapIt = bbToIfElseNode.find(bbIndex);
    gcc_assert(theMapIt == bbToIfElseNode.end());

    /* Insert IF_THEN_ELSE insn to the map
       key= (basic block index) value= (if_then_else node)*/
    bbToIfElseNode.insert (std::pair<int, if_then_else_node*>(bbIndex,ifelsenode)); 

  }

}


/* This function finding out the if /else  destination and updating the node (node->if_destBB_index, node->else_destBB_index)
   Note: we holding the if_then_else nodes on bbToIfElseNode Map */
static void
buildDB_update_ifelse_node_destBB(if_then_else_node* node)
{
  rtx_insn *ifelse_insn, *nextInsn;
  
  /*** Updating 'else' destination basic block ***/

  /* Get if then else label_ref*/
  rtx label_ref=XEXP(XEXP(single_set(node->insn),1),1); 
  /* Get code label */
  rtx code_label=XEXP(label_ref,0); 

  /* Get code label as rtx_insn */
  nextInsn = buildDB_get_insn(INSN_UID(code_label));

  /* Find first insn in 'else' destination block */
  while((GET_CODE(nextInsn) != INSN) && (GET_CODE(nextInsn) != CALL_INSN) && (GET_CODE(nextInsn) != JUMP_INSN))
  {
    nextInsn=NEXT_INSN(nextInsn);
    if(nextInsn == NULL)
       break; 
  }
  /* If 'else' insns does not deleted*/
  if(nextInsn != NULL)
    /* Update else destination basic block (index) */
    node->else_destBB_index = (BLOCK_FOR_INSN(nextInsn)->index);


  /*** Updating 'if' destination basic block ***/

  /* Get IF_THEN_ELSE insn*/
  ifelse_insn=node->insn;

  /* Find first insn in if_basic block*/
  nextInsn=NEXT_INSN(ifelse_insn);
  if(nextInsn != NULL)
  {
    while( (GET_CODE(nextInsn)!=INSN) && (GET_CODE(nextInsn)!=CALL_INSN) && (GET_CODE(nextInsn)!=JUMP_INSN) )
    {
      nextInsn=NEXT_INSN(nextInsn);
        if(nextInsn == NULL)
          break; 
    }
  }
  /* If 'if' insns does not deleted */
  if(nextInsn != NULL)
  /* Update if destination basic block (index) */
    node->if_destBB_index = (BLOCK_FOR_INSN(nextInsn)->index);

}


/* This function updates the if/else destination for all the nodes in bbToIfElseNode Map */
static void
buildDB_update_bbToIfElseNode_ifelse_destBB()
{
  std::map<int, if_then_else_node* >& bbToIfElseNode = buildDB_GetTableIfElseNode();
  std::map<int, if_then_else_node*>::iterator bbToIfElseNode_iter;
  
  /* For each IF_THEN_ELSE node in the map*/
  for(bbToIfElseNode_iter = bbToIfElseNode.begin(); bbToIfElseNode_iter != bbToIfElseNode.end(); ++bbToIfElseNode_iter)
  {
    /* Update the Node If/Else Basic blocks */
    buildDB_update_ifelse_node_destBB(bbToIfElseNode_iter->second);
  }
   
}

/* This function categorize the nesting kind of IF_THEN_ELSE insn  
  and returns :
  HAS_NO_NESTING (enum val=0) : if there is no nested IF_THEN_ELSE.
  HAS_NESTING_IN_IF (enum val=1) : if there nested IF_THEN_ELSE only on the 'IF' code.
  HAS_NESTING_IN_ELSE (enum val=10) : if there nested IF_THEN_ELSE only on the 'else' code.
  HAS_NESTING_IN_IF_ELSE (enum val=11) : if there nested IF_THEN_ELSE in both 'if'/'else' code.
 */
static int
buildDB_ifelse_nesting_categorization(if_then_else_node* node)
{
  basic_block if_dest_bb, else_dest_bb;
  rtx_insn* insn;
  int result = 0;
   
  gcc_assert(node != NULL);

  if(node->else_destBB_index != -1)
  {
    else_dest_bb = buildDB_get_bb(node->else_destBB_index);
    /* if there IF_THEN_ELSE insns in the 'else' destination put 10 on result */
    FOR_BB_INSNS (else_dest_bb, insn)
    {
      if(GET_CODE(insn) == JUMP_INSN)
        if(NULL_RTX != single_set(insn)) 
          if(GET_CODE(XEXP(single_set(insn),1)) == IF_THEN_ELSE ){
            result=10;
            node->hasNested = true;
            break;
          }
    }
  }

  if(node->if_destBB_index != -1)
  {
    /* if there IF_THEN_ELSE insns in the 'if' destination add 1 to the result */
    if_dest_bb = buildDB_get_bb(node->if_destBB_index);
    FOR_BB_INSNS (if_dest_bb, insn)
    {
      if(GET_CODE(insn) == JUMP_INSN)
        if(NULL_RTX != single_set(insn)) 
          if(GET_CODE(XEXP(single_set(insn),1)) == IF_THEN_ELSE ){
            result=result+1;
            node->hasNested = true;
            break;
          }
    }
  }

  return result;

}

/* This function updates the IF_THEN_ELSE Exit basic block for all the nodes in bbToIfElseNode Map
   IF_THEN_ELSE Exit BasicBlock is the first basic block that will be executed after the 'if/else' code 
 */
static void
buildDB_update_bbToIfElseNode_ifelse_ExitBB()
{
  int nesting_kind;
  std::map<int, if_then_else_node* >& bbToIfElseNode = buildDB_GetTableIfElseNode();
  std::map<int, if_then_else_node*>::iterator bbToIfElseNode_iter;
  if_then_else_node *currentNode;
  rtx_insn* jump_insn;
  int jump_dest_BbIdx;
  
  /* For each element on bbToIfElseNode Map*/
  for(bbToIfElseNode_iter = bbToIfElseNode.begin(); bbToIfElseNode_iter != bbToIfElseNode.end(); ++bbToIfElseNode_iter)
  {
    /* Get the element If_then_else node */
    currentNode = bbToIfElseNode_iter->second;
    
    /* Categorize the nesting kind of the IF_THEN_ELSE insn */
    nesting_kind=buildDB_ifelse_nesting_categorization(bbToIfElseNode_iter->second);

    /* If the current IF_THEN_ELSE insn has no nesting IF_THEN_ELSE */
    if(nesting_kind == HAS_NO_NESTING)
    {
      /*If there is a jump insn in the 'if' code update the IF_THEN_ELSE Exit Basic block 
        to be the jump destination */
      if (currentNode->if_destBB_index != -1)
      {
        jump_insn = buildDB_find_jump_insn(currentNode->if_destBB_index);
        if(jump_insn != NULL)
        {
          jump_dest_BbIdx = buildDB_getJumpDestBBindex(jump_insn);
          currentNode->ifelse_exit_bb_index=jump_dest_BbIdx;
          currentNode->isValid=true;
        }
      }


      /* If the IF_THEN_ELSE statement have only 'if' (without 'else') */
      if(currentNode->ifelse_exit_bb_index == -1)
        currentNode->ifelse_exit_bb_index=currentNode->else_destBB_index;
        
      /* If the IF_THEN_ELSE Exit Basic block value is valid mark the node as valid */
      if(currentNode->ifelse_exit_bb_index != -1)
        currentNode->isValid=true;


    }//if HAS_NO_NESTING


  }//for

}

/* This Function builds the ExitBBtoIfElseNode map
  ExitBBtoIfElseNode map :  (Exit Basic block Index) --> (IF_THEN_ELSE node) */
static void 
buildDB_build_ExitBBtoIfElseNode()
{
  std::map<int, if_then_else_node* >& bbToIfElseNode = buildDB_GetTableIfElseNode();
  std::map<int, if_then_else_node* >& ExitBBtoIfElseNode = buildDB_GetTableExitBBtoIfElseNode();
  std::map<int, if_then_else_node*>::iterator bbToIfElseNode_iter;
  if_then_else_node *currentNode;

  /* For each valid node on bbToIfElseNode build pair key = IF_THEN_ELSE Exit basic block
     and value = IF_THEN ELSE node and push the pair to ExitBBtoIfElseNode map */
  for(bbToIfElseNode_iter = bbToIfElseNode.begin(); bbToIfElseNode_iter != bbToIfElseNode.end(); ++bbToIfElseNode_iter)
  {
    currentNode = bbToIfElseNode_iter->second;
    if(currentNode->isValid)
      ExitBBtoIfElseNode.insert (std::pair<int, if_then_else_node*>(currentNode->ifelse_exit_bb_index,currentNode)); 
  }


}


/* This function builds if_then_else DataBase */
void 
buildDB_build_ifelseData()
{

  /* Build a list that contains all IF_THEN_ELSE insns in the current function */
  buildDB_build_ifThenElse_list();

  /* Initialize the bbToIfElseNode map with initial IF_THEN_ELSE nodes */
  buildDB_bbToIfElseNode_initialize_map();

  /* For each IF_THEN_ELSE node in bbToIfElseNode find if/else destination Basic block and update the node  */
  buildDB_update_bbToIfElseNode_ifelse_destBB();

  /* For each IF_THEN_ELSE node in bbToIfElseNode find IF_THEN_ELSE Exit Basic Block and update the node */
  buildDB_update_bbToIfElseNode_ifelse_ExitBB();

  /* Build ExitBBtoIfElseNode map ((Exit Basic block Index) --> (IF_THEN_ELSE node)) */
  buildDB_build_ExitBBtoIfElseNode();


}


/*##### SECTION 3 : Insn Value Dependency Data #####*/


/* This function builds reachable definitions Data and returns true if there are reachable definitions instructions(insn).
  reachable definitions will be held on insn_defs vector
*/
static bool 
buildDB_build_expr_reaching_defs_Data(insns_to_value* node )
{
  /* Clear insn_defs vector previous data */
  insn_defs.truncate (0);
  
  /* Push insn reachable defs to insn_defs vector and return true if the vector is not empty */
  if (get_defs (node->current_insn,node->current_expr /*src_reg*/, &insn_defs) != NULL)
  {
   return true;
  }
  
  /* If there is no reachable defs return false*/
  return false;
}

/* This function categorize the expression(node) dependency problem
   We have three different categories:
   1) There is no dependency, i.e if the expression is constant we have the expression value,
      and it does not depend on any expression or insn.
   2) Insn dependency which means other instruction influencing or could influence the expression value.
      i.e if the current expression is src REG and other instruction could edit his value (note: we didn't support MEM/CALL).
   3) Operand dependency i.e we have sub-expression as src operand (for example we write the result PLUS to expression destination 
      (insn 100 99 101 15 (set (reg:SI 25 s9 [157])    <--- expression destination
        (plus:SI (reg:SI 15 a5 [156])                  <--- sub-expression(src operand)
          (reg/v:SI 25 s9 [orig:86 nextFreeIndex ] [86]))) 3 {addsi3}

   */
static int 
buildDB_categorize_Node_dependency_problem(insns_to_value* node )
{
  if(node->code == REG)
    return INSN_DEPENDENCY_PROBLEM;
  if((GET_RTX_CLASS (node->code) == RTX_CONST_OBJ))
    return NO_DEPENDENCY_PROBLEM;
  return OPERAND_DEPENDENCY_PROBLEM;
}

 

/* return expression first Src operand (index)*/
static int 
buildDB_get_firstSrcOperand_index(insns_to_value* node)
{
    if((node->code == SUBREG)||(GET_RTX_CLASS (node->code) == RTX_COMM_ARITH)||(GET_RTX_CLASS (node->code) == RTX_UNARY)||
      (GET_RTX_CLASS (node->code) == RTX_BIN_ARITH)||(GET_RTX_CLASS (node->code) == RTX_AUTOINC))
      return 0;
    return 1;
}

/*return true if current operand is src operand*/
static bool 
buildDB_isSrcOperand(insns_to_value * node, int opIndex){
  if((opIndex < buildDB_get_firstSrcOperand_index(node)))
    return false;
  return true;

}

/* Push expression Src operands to the stacks */
static void 
buildDB_push_operands_to_stack(insns_to_value* node)
{
  int i,numOfOperands;
  insns_to_value* operandNode;
  rtx operand,expr;

  /* Node expression */
  expr=node->current_expr;

  /* Number of expression operands */  
  numOfOperands = GET_RTX_LENGTH(GET_CODE(expr));
  
  /* For each operand on the expression */
  for(i=0; i<numOfOperands; i++)
  {
    operand=XEXP(expr, i);

    /* If the operand is src operand*/
    if(buildDB_isSrcOperand(node, i))
    {
      /* Creat node for the operand */
      operandNode = stack_manage_insns_to_value_Node_constructor(node->current_insn, operand, i+1,node, D_BTM_EXPR);

      /* Push the node to the stack and to the temporary stack*/
      stack_manage_push_insn_to_stack(ST_MANAGE_STACK, operandNode);
      stack_manage_push_insn_to_stack(ST_MANAGE_TEMP_STACK, operandNode);

    }
  }
}


/*This function builds global predecessors basic blocks(indexs) sorted List (g_predsBBindex) for current insn     */
static void buildDB_build_g_predsBBindex_List(basic_block bb)
{
  std::list<int>& g_predsBBindex =  buildDB_GetTable_g_predsBBindex();
  std::list<int>::iterator visited_bb_it;

  if(bb != NULL)
  {
    /* Push current basic block index to the list */
    g_predsBBindex.push_back(bb->index);

    while(bb->prev_bb!= NULL)
    {

      /* If the basic block is visited return the list */
      visited_bb_it = std::find(g_predsBBindex.begin(), g_predsBBindex.end(), bb->prev_bb->index);
      if (visited_bb_it != g_predsBBindex.end())
        break;
      else
      {
        /* if the basic block not visted push it to list */
        g_predsBBindex.push_back(bb->prev_bb->index);
        /* current bb = prev bb*/
        bb=bb->prev_bb;
      }
    }

  }

}


/* return sorted list of curent and  predecessors basic blocks
  input : Basic block (current)
  output: sorted list containing current Basic block index and her predecessors basic blocks index */
static std::list<int> 
buildDB_get_bb_preds(basic_block bb ){
  std::list<int>bb_preds_list;
  std::list<int>::iterator visited_bb_it;

  if(bb==NULL)
    return bb_preds_list;

  /* push current basic block index to the list */
  bb_preds_list.push_back(bb->index);

  while(bb->prev_bb!= NULL){

    /* if the basic block is visited return the list */
    visited_bb_it= std::find(bb_preds_list.begin(), bb_preds_list.end(), bb->prev_bb->index);
    if (visited_bb_it != bb_preds_list.end())
      return bb_preds_list;


    /* if the basic block not visted push it to list */
    bb_preds_list.push_back(bb->prev_bb->index);
    bb=bb->prev_bb;
  }

  return bb_preds_list;
}

/*This function return true if there use def on the given Basic block*/
static bool buildDB_is_bb_has_UseDefs( int bbId)
{
  std::map<int,std::list<rtx_insn*>>& BBindex_to_PredsDefs_map = buildDB_GetTable_BBindex_to_PredsDefs_map();
  std::map<int,std::list<rtx_insn*>>::iterator bbDefIter;

  if(buildDB_keyExistInMap(BBindex_to_PredsDefs_map, bbId))
    {
      bbDefIter=BBindex_to_PredsDefs_map.find(bbId);
      if(!(bbDefIter->second).empty())
        return true;

    }

  return false;

}


/* This function return the closest predecessor Basic block with use def */
static int 
buildDB_getClosestBBwithUseDef(std::list<int> predsBBindex)
{
  int bbId;
  /*for each predecessor BB (sorted closest first) */
  while(!predsBBindex.empty()){

    bbId = predsBBindex.front();

    /* if the predecessor Basic block contains Use defs return the basic block index */
    if(buildDB_is_bb_has_UseDefs( bbId))
      return bbId;

    predsBBindex.pop_front();
  }

  /* if there is no dependency with predecessor insns */
  return -1;   
}


/*return true if bb1 is predecessor Basic  block for bb2 */
static bool buildDB_is_bb1_pred_to_bb2(int bb1_Index, int bb2_Index)
{
  basic_block bb2;
  std::list<int> predBB; 
  std::list<int>::iterator predBB_iter; 

  /* get BB predecessor Basic block */
  bb2 = buildDB_get_bb(bb2_Index);
  predBB = buildDB_get_bb_preds(bb2);
  predBB.pop_front();

  /* find bb1 on predecessors list */
  predBB_iter = std::find(predBB.begin(), predBB.end(), bb1_Index);
  
  /* return is bb1 predecessor for bb2  */
  return (predBB_iter != predBB.end());
}

/* This function returns a binary number where each bit on the number represents the problem(Take a look at enum  insn_dependency_problem_mask).
   for example, if we get as result 6(0b110) that's mean we have two problems loop problem and if_then_else problem */
static int buildDB_categorize_insn_dependency_problem(insns_to_value* node)
{
  int problemBitwiseFlags = 0, lastBBindex;
  basic_block insnBasicblock;
  if_then_else_node* ifelesNode;
  bool ifBBhasUseDef, elseBBhasUseDef;

  std::list<int> predBB; 
  std::map<int,if_then_else_node* >& ExitBBtoIfElseNode = buildDB_GetTableExitBBtoIfElseNode();
  std::map<int,if_then_else_node* >::iterator ExitBBtoIfElseIterator;

  /* If the insn in some IF_THE_ELSE Exit Basic block, rise IF_THEN_ELSE_problem bit */
  insnBasicblock = BLOCK_FOR_INSN(node->current_insn);
  ExitBBtoIfElseIterator = ExitBBtoIfElseNode.find(insnBasicblock->index);
  if(ExitBBtoIfElseIterator != ExitBBtoIfElseNode.end())
    problemBitwiseFlags = (problemBitwiseFlags | IF_THEN_ELSE_DEPENDENCY_PROBLEM_MASK);

  /* If the insn in some IF_THE_ELSE Exit Basic block */
  if((problemBitwiseFlags & IF_THEN_ELSE_DEPENDENCY_PROBLEM_MASK) > 0)
  {
    /* Get closest predecessor basic block with use-def */
    predBB= buildDB_get_bb_preds(insnBasicblock);
    lastBBindex=buildDB_getClosestBBwithUseDef(predBB);
 
    /* If there is dependency after the if/else then the value will be taken from the last dependency
       and we don't have IF_THEN_ELSE dependency problem  */
    if(lastBBindex == insnBasicblock->index)
    {
      problemBitwiseFlags = (problemBitwiseFlags ^ IF_THEN_ELSE_DEPENDENCY_PROBLEM_MASK);
      problemBitwiseFlags = (problemBitwiseFlags | LAST_DEPENDENCY_PROBLEM_MASK);

    }
    else
    {
      ifelesNode=ExitBBtoIfElseIterator->second;
      /* Check if 'if BB'/'else BB' has Use Defs(dependency)*/
      ifBBhasUseDef = buildDB_is_bb_has_UseDefs(ifelesNode->if_destBB_index);
      elseBBhasUseDef = buildDB_is_bb_has_UseDefs(ifelesNode->else_destBB_index);

      /* If no dependency on IF_THEN_ELSE code then last Use def before the insn is a candidate dependency */
      if((ifBBhasUseDef == false) && (elseBBhasUseDef==false))
      {
        problemBitwiseFlags = (problemBitwiseFlags | LAST_DEPENDENCY_PROBLEM_MASK);
        problemBitwiseFlags = (problemBitwiseFlags ^ IF_THEN_ELSE_DEPENDENCY_PROBLEM_MASK);
      }
      else
      {
        /* If we have dependency only in 'if' or 'else' code (not in both)
           then last Use Def before the IF_THEN_ELSE insn (or bettwen if/else Basic blocks) is a candidate dependency 
        */
        if(ifBBhasUseDef != elseBBhasUseDef)
          problemBitwiseFlags = (problemBitwiseFlags | LAST_DEPENDENCY_BEFORE_IF_ELSE_PROBLEM_MASK);
      }

    }
    
  }
  else /* If there is no IF_THEN_ELSE dependency*/
    problemBitwiseFlags = (problemBitwiseFlags | LAST_DEPENDENCY_PROBLEM_MASK);


return problemBitwiseFlags;

}

/* This function returns true if the dep_insn is predecessors to currentInsn 
   i.e if dep_insn in predecessors Basic block or come before currentInsn in the same basic block */
static bool buildDB_depInsn_predTo_currentInsn(rtx_insn* dep_insn, rtx_insn* currentInsn){
  basic_block dep_bb, current_bb;
  int dep_LUID, current_LUID;
  
  std::list<int>& g_predsBBindex = buildDB_GetTable_g_predsBBindex();
  std::list<int>::iterator predsListIter;

  dep_bb = BLOCK_FOR_INSN(dep_insn);
  current_bb = BLOCK_FOR_INSN(currentInsn);


  /*If dep_insn and currentinsn in the same Basic block.*/
  if(dep_bb->index == current_bb->index)
  {
    /* If dep_insn come before currentInsn return true else return false*/
    dep_LUID = DF_INSN_LUID(dep_insn);
    current_LUID = DF_INSN_LUID(currentInsn);
    return(dep_LUID < current_LUID);
  }
  
  /* If dep_insn in predecessors basic block return true else return false */
  predsListIter = std::find(g_predsBBindex.begin(), g_predsBBindex.end(), dep_bb->index);
  return (predsListIter != g_predsBBindex.end());

}

/**/
static bool 
buildDB_keyExistInMap(std::map<int, std::list<rtx_insn*>> theMap, int id){
  std::map<int,std::list<rtx_insn*>>::iterator theMapIt;
 
 if(theMap.empty())
  return false;

 theMapIt = theMap.find(id);
  return (theMapIt != theMap.end());
}

/*This function build global Map BBindex_to_PredsDefs_map : (Basic block index)--> (List of BasicBlock UseDefs )*/
static void buildDB_build_BBindex_to_PredsDefs_map(rtx_insn* currentInsn)
{
  rtx_insn *dep_insn;
  basic_block depBB;
  int depBBindex;
 
  /* Get global BBindex_to_PredsDefs_map map */
  std::map<int,std::list<rtx_insn*>>& BBindex_to_PredsDefs_map = buildDB_GetTable_BBindex_to_PredsDefs_map();
  std::map<int,std::list<rtx_insn*>>::iterator BBtoDefIter;

  /* For each reachable def (insn dependency) */
  while(!insn_defs.is_empty ())
  {
    
    dep_insn = insn_defs.pop();
    
    /* If dep_insn(reachable def) is predecessors to the currentInsn (come before currentInsn ) 
      add dep_insn to the Map */
    if(buildDB_depInsn_predTo_currentInsn(dep_insn, currentInsn))
    {
      /* Get dep_insn Basic Block index */
      depBB = BLOCK_FOR_INSN(dep_insn);
      depBBindex = depBB->index; 
      
      /* Add dependency insn to dep_insn use-def list */
      buildDB_insert_insn_to_map_List(BBindex_to_PredsDefs_map, depBBindex ,dep_insn);

    }
  }
}

/**/
static rtx_insn* 
buildDB_getBBLastInsn(std::list<rtx_insn*> defInsnsList)
{
  int maxId,curId;
  rtx_insn *cinsn,*result;
  std::list<rtx_insn *>::iterator defListIter;

  /* if list not empty mark first elemet as the soloution  */
  if(defInsnsList.size() > 0)
  {
    cinsn= *(defInsnsList.begin());
    maxId=DF_INSN_LUID(cinsn);
    result=cinsn;
  }

}


/*
This function inserts insn on a map from the type : key = int value =  std::list<rtx_insn*>
the insn will be inserted on the element list (the element with key=id . list) 
if there is no such element(with key=id) it will create a new element with key =id and will insert the insn in the new element list
*/
static void buildDB_insert_insn_to_map_List(std::map<int, std::list<rtx_insn*>>& theMap, int id,rtx_insn* insn)
{
  std::map<int, std::list<rtx_insn*>>::iterator theMapIter;
   /* If element with key = id  exist add insn to element List*/
   if(buildDB_keyExistInMap(theMap, id))
      {
        theMapIter = theMap.find(id);
        theMapIter->second.push_back(insn);
      }
      else
      {
        /* If no element with key= id on the map, create and add a new element with key= id 
           and add the insn to the new element list */
        std::list<rtx_insn*> newlist;
        theMap.insert (std::pair<int, std::list<rtx_insn*>>(id, newlist));
        theMapIter=theMap.find(id);
        theMapIter->second.push_back(insn);
      }
}

/* */
static void buildDB_findAndPush_Last_Dependency_insn(insns_to_value* node)
{
  int lastBBindex;
  rtx_insn* lastInsn;
  std::list<int> predBB; 
  std::list<int>& g_predsBBindex = buildDB_GetTable_g_predsBBindex();
  std::map<int,std::list<rtx_insn*>>& BBindex_to_PredsDefs_map = buildDB_GetTable_BBindex_to_PredsDefs_map();
  std::map<int,std::list<rtx_insn*>>::iterator BBtoDefIter;
  std::map<int, std::list<rtx_insn*>>& InsnUID_to_DepInsns= buildDB_GetTable_InsnUID_to_DepInsns();



  /* get closest predecessor basic block */
  lastBBindex = buildDB_getClosestBBwithUseDef(g_predsBBindex);
  
  if(lastBBindex != -1)
  {
    /* find closest Basic block UseDef List */
    BBtoDefIter = BBindex_to_PredsDefs_map.find(lastBBindex);
    /* get last use-def insn in the basic block */
    lastInsn = buildDB_getBBLastInsn(BBtoDefIter->second);

    /*add lastInsn to current_insn dependency list  */
    buildDB_insert_insn_to_map_List(InsnUID_to_DepInsns, INSN_UID(node->current_insn) ,lastInsn);



  }

}

/* This function find out if the current insn has a dependency on IF_THEN_ELSE code
   that's is if the IF_THEN_ELSE Exit Basic block and current insn Basic block is the same 
   Basic block, and some insn in IF/ELSE Basic block write to current insn Src REG */
static void buildDB_findAndPush_IfThenElse_Dependency_insns(insns_to_value* node)
{
  std::map<int,if_then_else_node* >& ExitBBtoIfElseNode = buildDB_GetTableExitBBtoIfElseNode();
  std::map<int,if_then_else_node* >::iterator ExitBBtoIfElseIterator;
  std::map<int,std::list<rtx_insn*>>& BBindex_to_PredsDefs_map = buildDB_GetTable_BBindex_to_PredsDefs_map();
  std::map<int, std::list<rtx_insn*>>& InsnUID_to_DepInsns= buildDB_GetTable_InsnUID_to_DepInsns();

  std::map<int,std::list<rtx_insn*>>::iterator BBtoDefIter;
  basic_block insnBasicblock;
  if_then_else_node* ifelesNode;
  bool ifBBhasUseDef, elseBBhasUseDef;
  rtx_insn* dep_insn; 


  /* Find the IF_THEN_ELSE node that his Exit Basic Block is the same Basic block as Current insn */
  insnBasicblock = BLOCK_FOR_INSN(node->current_insn);
  ExitBBtoIfElseIterator = ExitBBtoIfElseNode.find(insnBasicblock->index);

  /*WE Execute this function only if we know the current insn basic block also 
    is IF_THEN_ELSE Exit Basic block for some IF_THEN_ELSE insn */
  gcc_assert(ExitBBtoIfElseIterator != ExitBBtoIfElseNode.end());

  ifelesNode = ExitBBtoIfElseIterator->second;
  /* check if 'if BB'/'else BB' has Use Defs(dependency)*/
  ifBBhasUseDef = buildDB_is_bb_has_UseDefs(ifelesNode->if_destBB_index);
  elseBBhasUseDef = buildDB_is_bb_has_UseDefs(ifelesNode->else_destBB_index);

  /* If the 'IF Basic block' has UseDef (that's is insns that write to the current insn SRC REG)
     find the last UseDef and push it to the dependency list */
  if(ifBBhasUseDef)
  {
    BBtoDefIter = BBindex_to_PredsDefs_map.find(ifelesNode->if_destBB_index);
    dep_insn = buildDB_getBBLastInsn(BBtoDefIter->second);
    buildDB_insert_insn_to_map_List(InsnUID_to_DepInsns, INSN_UID(node->current_insn) ,dep_insn);

  }

  /* If the 'ELSE Basic block' has UseDef (that's is insns that write to the current insn SRC REG)
     find the last UseDef and push it to the dependency list */
  if(elseBBhasUseDef)
  {
    BBtoDefIter = BBindex_to_PredsDefs_map.find(ifelesNode->else_destBB_index);
    dep_insn = buildDB_getBBLastInsn(BBtoDefIter->second);
    buildDB_insert_insn_to_map_List(InsnUID_to_DepInsns, INSN_UID(node->current_insn) ,dep_insn);

  }




}

/* */
static void buildDB_findAndPush_Last_Dependency_insn_before_IfThenElse(insns_to_value* node)
{
  std::map<int,if_then_else_node* >& ExitBBtoIfElseNode = buildDB_GetTableExitBBtoIfElseNode();
  std::map<int,if_then_else_node* >::iterator ExitBBtoIfElseIterator;
  std::map<int,std::list<rtx_insn*>>& BBindex_to_PredsDefs_map = buildDB_GetTable_BBindex_to_PredsDefs_map();
  std::map<int, std::list<rtx_insn*>>& InsnUID_to_DepInsns= buildDB_GetTable_InsnUID_to_DepInsns();

  std::map<int,std::list<rtx_insn*>>::iterator BBtoDefIter;
  std::list<int> IfElsePredsBBs;
  basic_block insnBasicblock, ifElseBB;

  if_then_else_node* ifelesNode;
  bool ifBBhasUseDef, elseBBhasUseDef;
  rtx_insn* dep_insn; 
  int lastBBindex;

  
  /* find IF_THEN_ELSE insn*/
  insnBasicblock = BLOCK_FOR_INSN(node->current_insn);
  ExitBBtoIfElseIterator = ExitBBtoIfElseNode.find(insnBasicblock->index);
  gcc_assert(ExitBBtoIfElseIterator != ExitBBtoIfElseNode.end());
  ifelesNode = ExitBBtoIfElseIterator->second;

  /*make sorted list with IF_THEN_ELSE predecessors basic blocks*/
  ifElseBB=BLOCK_FOR_INSN(ifelesNode->insn);
  IfElsePredsBBs= buildDB_get_bb_preds(ifElseBB);
  IfElsePredsBBs.pop_front();

  /* get closest predecessor basic block with use def */
  lastBBindex = buildDB_getClosestBBwithUseDef(IfElsePredsBBs);
  
  /*if we found predecessor basic block with use def */
  if(lastBBindex != -1)
  {
     /* find closest Basic block UseDef List */
    BBtoDefIter = BBindex_to_PredsDefs_map.find(lastBBindex);
    /* get last use-def insn in the basic block */
    dep_insn = buildDB_getBBLastInsn(BBtoDefIter->second);

    /*add dep_insn to current_insn dependency list  */
    buildDB_insert_insn_to_map_List(InsnUID_to_DepInsns, INSN_UID(node->current_insn) ,dep_insn);
  }

}

/* This function finding out expression value dependency insns and add new entry on
   exprID_To_DependencyInsns map where key = (expression Id) value = (candidate insns dependency List)*/
static void buildDB_insn_dependency_data(insns_to_value* node)
{
 
  bool ThereIsReachingDefs;
  int dependency_problems;
  std::list<rtx_insn* >& ifThenElseInsnsList = buildDB_GetTableIfelseList();

  /* Build reaching defs vector(insn_defs) and updates ThereIsReachingDefs value
     (if there reaching defs ThereIsReachingDefs value will be true )  */
  ThereIsReachingDefs = buildDB_build_expr_reaching_defs_Data(node);

  /* If there reaching defs */
  if(ThereIsReachingDefs)# we stop here
  {
    /* Build predecessor  basic blocks  global list for the current insn */
    buildDB_build_g_predsBBindex_List(BLOCK_FOR_INSN(node->current_insn));
    
    /* Build global Map BBindex_to_PredsDefs_map : (Basic block index)--> (List of BasicBlock UseDefs) */
    buildDB_build_BBindex_to_PredsDefs_map(node->current_insn);


    /* Analyse and categorize dependency problem */
    dependency_problems = buildDB_categorize_insn_dependency_problem(node);

	/* 
	If the define BUILDDB_ALLOW_SUPPORT_ON_ONE_IF_ONLY exist means that we don't want to support optimization on more than one IF_THEN_ELSE insn
	In such case, if we had more the one IF_THEN_ELSE insn on if then else dependency problem we deal with the current REG as un-supported insn
	that's is  :
	 1)  mark the node as not-supported
     2)  don't push the if_then_esle dependency
	 3)  put the upper bound to machine word size extremum value (as Shortcut we put on the REG operands Upper bound list
	    element with  machine word size extremum value and on eval the REG value will be machine word size extremum value )
    */
    #ifdef BUILDDB_ALLOW_SUPPORT_ON_ONE_IF_ONLY
      if((dependency_problems & LAST_DEPENDENCY_PROBLEM_MASK) > 0)
        if(ifThenElseInsnsList.size() > 1)
        {
		  /* Push  machine word size extremum value on the operands List */
          node->operands_upper_bound.push_back((long long int)(pow(2, (UNITS_PER_WORD * 8) - 1 ) - 1));
          /* Mark node as un-supported */
          node->is_supported = false;
		  /* Exit from function */
          return;
        }

    #endif


    /* If the dependency problem category is the Last dependency problem find and push the last dependency insn 
       to insn dependency List */
    if((dependency_problems & LAST_DEPENDENCY_PROBLEM_MASK) > 0){
      buildDB_findAndPush_Last_Dependency_insn(node);
     }

    /* If the dependency problem category is IF THEN ELSE dependency problem find and push the IF/ELSE dependency insn 
       to insn dependency List */
    if((dependency_problems & IF_THEN_ELSE_DEPENDENCY_PROBLEM_MASK) > 0)
      buildDB_findAndPush_IfThenElse_Dependency_insns(node);<<<HERE
    
    /* If the dependency problem category is the last dependency insn before IF THEN ELSE problem
       find and push the last dependency  problem befor IF/ELSE dependency insn to insn dependency List */
    if((dependency_problems & LAST_DEPENDENCY_BEFORE_IF_ELSE_PROBLEM_MASK) > 0)
      buildDB_findAndPush_Last_Dependency_insn_before_IfThenElse(node);


   /* Clear insn dependency data */
   buildDB_clear_insn_Data();
  }

  



}

/* */
static void buildDB_Push_candidate_insns_dependency_to_stack(insns_to_value* node)
{
  std::map<int, std::list<rtx_insn*>>& InsnUID_to_DepInsns= buildDB_GetTable_InsnUID_to_DepInsns();
  std::map<int, std::list<rtx_insn*>>::iterator InsnToDepIter;
  std::list<rtx_insn*> dep_list;
  std::list<rtx_insn*>::iterator depListIter;
  rtx_insn* dep_insn;
  insns_to_value *dep_insn_Node;


  if(buildDB_keyExistInMap(InsnUID_to_DepInsns, INSN_UID(node->current_insn)))
  {
    /* Get current insn dependency List*/
    InsnToDepIter =   InsnUID_to_DepInsns.find(INSN_UID(node->current_insn));

    dep_list=InsnToDepIter->second;
  
    for(depListIter=dep_list.begin(); depListIter != dep_list.end(); ++depListIter)
    {
      /* creat node for dep_insn */
      dep_insn = *depListIter;
      dep_insn_Node = stack_manage_insns_to_value_Node_constructor(dep_insn, single_set(dep_insn), 0, node, D_BTM_INSN);
    
      /* push node to stacks*/
      stack_manage_push_insn_to_stack(ST_MANAGE_STACK, dep_insn_Node);
      stack_manage_push_insn_to_stack(ST_MANAGE_TEMP_STACK, dep_insn_Node);
    }
  }
}


/* In this function, will build an Insn value dependency graph,
   we sort the graph on the stack(ST_MANAGE_STACK) for the eval Phase,
   where nodes on the stack head should be evaluated before nodes on the bottom of the stack. */
void 
buildDB_build_insn_value_dependency_Data(rtx_insn* insn) #we stoped here
{
  insns_to_value *insnNode, *currentNode;
  int problem;

  /* Creat node for insn */
  insnNode=stack_manage_insns_to_value_Node_constructor(insn, NULL, 0, NULL, D_BTM_INSN);

  /* Push the node to the stack and the temporary stack */
  stack_manage_push_insn_to_stack(ST_MANAGE_STACK, insnNode);
  stack_manage_push_insn_to_stack(ST_MANAGE_TEMP_STACK, insnNode);
  
  /* While TEMP stack not empty */
  while(!stack_manage_Stack_Is_Empty(ST_MANAGE_TEMP_STACK))
  {
    /* Get stack Top */
    currentNode = stack_manage_Stack_Top(ST_MANAGE_TEMP_STACK);
    /* If the stack top is visited pop it from the stack */
    if(currentNode->is_visited)
    {
       stack_manage_Stack_Pop(ST_MANAGE_TEMP_STACK);
    }
    else /* If the stack top in not visited*/
    {
       currentNode->is_visited=true;

      /* If we support the CurrentNode expression code */
      if(currentNode->is_supported)
      {

        /* Categorize the Node dependency
           We have three cases:
            Case 1: we did not have a dependency problem (the expression is constant). in that case do nothing (only mark node as visited)
            Case 2: the value of the node expression depends on his operands. in that case push his operands to the stack (and mark the node as visited)
            Case 3: the node expression is REG. in that case find candidate insn dependency (insns that the REG value depend on them) and push them
                    to the stack (and mark the node as visited) */

        problem = buildDB_categorize_Node_dependency_problem(currentNode);

        /* Case 2 : Operands dependency
           Build node for each dependency and push the node to the stack */
        if(problem == OPERAND_DEPENDENCY_PROBLEM)
          buildDB_push_operands_to_stack(currentNode);

        /* Case 3 : Insn dependency
           Build candidate insn dependency Data  
           and push candidate dependency to the stack (creat node for each dependency)
          */
        if(problem == INSN_DEPENDENCY_PROBLEM)
        {
          buildDB_insn_dependency_data(currentNode);
          buildDB_Push_candidate_insns_dependency_to_stack(currentNode);
        }

      }

      

    }

  }


}








 

/*** FREE DATA ***/
void buildDB_clear_function_Data(){
  std::list<rtx_insn* >& ifThenElseInsnsList = buildDB_GetTableIfelseList();
  std::map<int, if_then_else_node* >& bbToIfElseNode = buildDB_GetTableIfElseNode();
  std::map<int,basic_block>& bbIndexToBBmap = buildDB_GetTableBB();
  std::map<int,rtx_insn*>& uidToInsnMap = buildDB_GetTableInsn();
  std::map<int, if_then_else_node* >& ExitBBtoIfElseNode = buildDB_GetTableExitBBtoIfElseNode();
  std::map<int, std::list<rtx_insn*>>& InsnUID_to_DepInsns= buildDB_GetTable_InsnUID_to_DepInsns();



  std::map<int, if_then_else_node* >::iterator nodeIter;

  /* free ifThenElseInsnsList */
  ifThenElseInsnsList.clear();

  /* free bbToIfElseNode map */
  for(nodeIter = bbToIfElseNode.begin(); nodeIter!=bbToIfElseNode.end();++nodeIter)
    free(nodeIter->second);
  bbToIfElseNode.clear();

  /* free ExitBBtoIfElseNode map 
     Note: we free the nodes on 'free bbToIfElseNode map' */
  ExitBBtoIfElseNode.clear();

  /*free bbIndexToBBmap*/
  bbIndexToBBmap.clear();

  /*free uidToInsnMap */
  uidToInsnMap.clear();

  /*free InsnUID_to_DepInsns*/
  InsnUID_to_DepInsns.clear();


}

/* */
void buildDB_clear_insn_Data(){
  std::list<int>& g_predsBBindex =  buildDB_GetTable_g_predsBBindex();
  std::map<int,std::list<rtx_insn*>>& BBindex_to_PredsDefs_map = buildDB_GetTable_BBindex_to_PredsDefs_map();
  std::map<int,std::list<rtx_insn*>>::iterator BBtoPredsIter;

  /* free g_predsBBindex */
  g_predsBBindex.clear(); 

  /* free BBindex_to_PredsDefs_map */
  for(BBtoPredsIter = BBindex_to_PredsDefs_map.begin(); BBtoPredsIter != BBindex_to_PredsDefs_map.end(); ++BBtoPredsIter)
    BBtoPredsIter->second.clear();
  BBindex_to_PredsDefs_map.clear();



 
}