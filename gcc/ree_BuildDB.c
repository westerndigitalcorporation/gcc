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
 
#include "ree_BuildDB.h"
 

#define Machine_word_size() (-1); 




/* reference functions */

/* return IF_THEN_ELSE insn List (ifThenElseInsnsList) by reference */
std::list <rtx_insn* >&
buildDB_GetTableIfelseList()
{
  static std::list <rtx_insn* > ifThenElseInsnsList;
  return ifThenElseInsnsList;
}

/* return ExitBBtoIfElseNode map by reference */
std::map<int,if_then_else_node*>& 
buildDB_GetTableExitBBtoIfElseNode()
{
  static std::map<int,if_then_else_node*> ExitBBtoIfElseNode;
  return ExitBBtoIfElseNode;
}

/* return bbIndexToBBmap map by reference */
std::map<int,basic_block>& 
buildDB_GetTableBB()
{
  static std::map<int,basic_block> bbIndexToBBmap;
  return bbIndexToBBmap;
}

/* return uidToInsnMap map by reference */
std::map<int,rtx_insn*>& 
buildDB_GetTableInsn()
{
  static std::map<int,rtx_insn*> uidToInsnMap;
  return uidToInsnMap;
}

/* return bbToIfElseNode map by reference */
std::map<int,if_then_else_node*>& 
buildDB_GetTableIfElseNode()
{
  static std::map<int,if_then_else_node*> bbToIfElseNode;
  return bbToIfElseNode;
}


/*** structs constructors ***/


/* insn_to_value_node constructor */
static insns_to_value*
buildDB_insns_to_value_Node_constructor(rtx_insn *curr_insn,rtx curr_expr,int opernadNum ,insns_to_value *father, int type)
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
  node->is_supported = true;
  node->upper_bound = Machine_word_size();
  node->id = buildDB_InsnToVal_calcId(node);

return node;

}

/* if_then_else_node constructor */
static if_then_else_node *
buildDB_if_then_else_node_constructor(rtx_insn * ins)
{
 if_then_else_node *node = new  if_then_else_node;
 node->insn=ins;
 node->isValid=false;
 node->id=INSN_UID(ins);
 node-> ifelse_exit_bb_index = -1;
 node-> if_destBB_index = -1;
 node-> else_destBB_index = -1;
 return node;
}


/*     RTX extension Data   */


/* this function build:
  Map : (insn UID)-->(insn)
  Map : (Basic block index)-->(Basic Block)
  for the current function
*/
void 
buildDB_build_RTX_extension_Data(){
std::map<int,basic_block>& bbIndexToBBmap = buildDB_GetTableBB();
std::map<int,rtx_insn*>& uidToInsnMap = buildDB_GetTableInsn();
basic_block bb;
rtx_insn *insn,*insncopy,*debug;
int id;
  /* for each basic block (bb) except Entry/Exit BasicBlocks */
  FOR_EACH_BB_FN (bb, cfun)
  {
    basic_block newbb=bb;
    /*insert to map (Basic block index)-->(Basic Block) */
    bbIndexToBBmap.insert (std::pair<int, basic_block >(newbb->index,newbb));
    /* for each insn in bb */
    FOR_BB_INSNS (bb, insn)
    {
       insncopy=insn;
       id=INSN_UID(insncopy);
      /*insert to map (insn UID)-->(insn) */
      uidToInsnMap.insert (std::pair<int, rtx_insn* >(id,insncopy));
      debug=uidToInsnMap.end()->second;
    }
  }
}


/* input: insn uid
   output : the insn (where INSN_UID(insn) == id)*/
rtx_insn*
buildDB_get_insn(int id){
  std::map<int,rtx_insn*>& uidToInsnMap = buildDB_GetTableInsn();
  std::map<int, rtx_insn* >::iterator theMapIt;

  theMapIt=uidToInsnMap.find(id);
  return theMapIt->second;

}

/* input : basic block index 
   output : the  basic block (where basic_block->index == id ) */
basic_block
buildDB_get_bb(int id){
    std::map<int,basic_block>& bbIndexToBBmap= buildDB_GetTableBB();
    std::map<int, basic_block >::iterator theMapIt;

    theMapIt=bbIndexToBBmap.find(id);
    if(theMapIt==bbIndexToBBmap.end())
      return NULL;
    return theMapIt->second;
}





/*****     IF_THEN_ELSE Data    *****/

/* This function searches for jump insn on the given Basic block
   and returns the jump insn if the Basic block not function entry or function exit and 
   the Jump insn found, else if there is no jump insn in the Basic block returns NULL 
   
   input : Basic block Index
*/
static rtx_insn*
buildDB_find_jump_insn(int bbIndex){
  rtx_insn* bbinsn;
  basic_block bb;

  if((bbIndex == ENTRY_BLOCK) || (bbIndex == EXIT_BLOCK))
    return NULL;

  /*get Basic block*/
  bb = buildDB_get_bb(bbIndex);

  gcc_assert(bb != NULL);

  FOR_BB_INSNS (bb, bbinsn)
  {
    if(GET_CODE(bbinsn) == JUMP_INSN)
      return  bbinsn;    
  }
  return NULL;
}


/* If the jump_insns kind is : (jump_insn (set (pc)(label_ref ))
This function returns the basic block index of the jump_insn destination 
else it will return -1 */
static int
buildDB_getJumpDestBBindex(rtx_insn* jmpinsn){
 
  rtx_insn* nextInsn;

  /* check jump_insn kind  */
  rtx label_ref = XEXP(single_set(jmpinsn),1); 
  if(GET_CODE(label_ref) != LABEL_REF)
    return -1;

  /* get code label as rtx_insn */
  rtx code_label = XEXP(label_ref,0);
  nextInsn = buildDB_get_insn(INSN_UID(code_label));

  /*find first insn after code label */
  while((GET_CODE(nextInsn)!=INSN) && (GET_CODE(nextInsn)!=CALL_INSN) && (GET_CODE(nextInsn)!=JUMP_INSN))
  {
    nextInsn=NEXT_INSN(nextInsn);
    if(nextInsn == NULL)
       break; 
  }
   
   /* return */
    if(nextInsn != NULL)
      return BLOCK_FOR_INSN(nextInsn)->index;
    return -1;
}



/* this function builds a list that contains all IF_THEN_ELSE insns in the current function */
static void
buildDB_build_ifThenElse_list()
{
  basic_block bb;
  rtx_insn *insn;
  std::list<rtx_insn* >& ifThenElseInsnsList = buildDB_GetTableIfelseList();

  /* for each basic block in function except to ENTRY/EXIT Basic block*/
  FOR_EACH_BB_FN (bb, cfun)
  {
    /* for each insn in Basic block*/
    FOR_BB_INSNS (bb, insn)
    {
      /* if the current insn is IF_THEN_ELSE push it to the list */
      if(GET_CODE(insn) == JUMP_INSN)
        /* skip jump insns like : jump_insn 174 221 175 12 (eh_return)*/
        if(NULL_RTX != single_set(insn) ) 
          /* if the jump insns from type (jump_insn(set(pc) (IF_THEN_ELSE)...)) push insn to List */
          if(GET_CODE(XEXP(single_set(insn),1)) == IF_THEN_ELSE )
            ifThenElseInsnsList.push_back(insn);
    }
  }

}


/* This function build new IF_THEN_ELSE node for each IF_THEN_ELSE insns and initializing 
   the map with the new nodes.
   NOTE: after executing the function only node->id and node->insn value will be valid */
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
   

  /* for each if_then_else insn */
  for(IfElseListIter=ifThenElseInsnsList.begin(); IfElseListIter != ifThenElseInsnsList.end(); ++IfElseListIter){
   
    /* get IF_THEN_ELSE insn basic block index */
    bbIndex=BLOCK_FOR_INSN(*IfElseListIter)->index;
   
    /* creat new if_then_else node (initial node */
    if_then_else_node* ifelsenode = buildDB_if_then_else_node_constructor( *IfElseListIter);
      
    /* if there more than IF_THEN_ELSE insns on the same basic block then there is bug */
    theMapIt = bbToIfElseNode.find(bbIndex);
    gcc_assert(theMapIt == bbToIfElseNode.end());

    /* insert IF_THEN_ELSE insn to the map
       key= (basic block index) value= (if_then_else node)*/
    bbToIfElseNode.insert (std::pair<int, if_then_else_node*>(bbIndex,ifelsenode)); 

  }

}


/* this function finding out the if /else  destination and updating the node (node->if_destBB_index, node->else_destBB_index)
   Note: we holding the if_then_else nodes on bbToIfElseNode Map */
static void
buildDB_update_ifelse_node_destBB(if_then_else_node* node)
{
  rtx_insn *ifelse_insn, *nextInsn;
  
  /*** updating else destination basic block ***/

  /*get if then else label_ref*/
  rtx label_ref=XEXP(XEXP(single_set(node->insn),1),1); 
  /* get code label */
  rtx code_label=XEXP(label_ref,0); 

  /* get code label as rtx_insn */
  nextInsn = buildDB_get_insn(INSN_UID(code_label));

  /* find first insn in else destination block */
  while((GET_CODE(nextInsn) != INSN) && (GET_CODE(nextInsn) != CALL_INSN) && (GET_CODE(nextInsn) != JUMP_INSN)){
    nextInsn=NEXT_INSN(nextInsn);
    if(nextInsn == NULL)
       break; 
  }
  /*if 'else' insns not deleted*/
  if(nextInsn != NULL)
    /* update else destination basic block (index) */
    node->else_destBB_index = (BLOCK_FOR_INSN(nextInsn)->index);


  /*** updating if destination basic block ***/

  /* get IF_THEN_ELSE insn*/
  ifelse_insn=node->insn;

  /*find first insn in if_basic block*/
  nextInsn=NEXT_INSN(ifelse_insn);
  if(nextInsn != NULL){
    while( (GET_CODE(nextInsn)!=INSN) && (GET_CODE(nextInsn)!=CALL_INSN) && (GET_CODE(nextInsn)!=JUMP_INSN) ){
      nextInsn=NEXT_INSN(nextInsn);
        if(nextInsn == NULL)
          break; 
    }
  }
  /* if 'if' insns not deleted */
  if(nextInsn != NULL)
  /* update if destination basic block (index) */
    node->if_destBB_index = (BLOCK_FOR_INSN(nextInsn)->index);

}


/* This function updates the if/else destination for all the nodes in bbToIfElseNode Map */
static void
buildDB_update_bbToIfElseNode_ifelse_destBB()
{
  std::map<int, if_then_else_node* >& bbToIfElseNode = buildDB_GetTableIfElseNode();
  std::map<int, if_then_else_node*>::iterator bbToIfElseNode_iter;
  
  for(bbToIfElseNode_iter = bbToIfElseNode.begin(); bbToIfElseNode_iter != bbToIfElseNode.end(); ++bbToIfElseNode_iter)
  {
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
  int result;
   
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

  for(bbToIfElseNode_iter = bbToIfElseNode.begin(); bbToIfElseNode_iter != bbToIfElseNode.end(); ++bbToIfElseNode_iter)
  {

    currentNode = bbToIfElseNode_iter->second;

    nesting_kind=buildDB_ifelse_nesting_categorization(bbToIfElseNode_iter->second);

    if(nesting_kind == HAS_NO_NESTING)
    {
      /*If there is jump insn in the 'if' code update IF_THEN_ELSE Exit Basic block 
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


      /*If we did't find the IF_THEN_ELSE Exit Basic block yet and in the 'else' code jump insn
        update the IF_THEN_ELSE Exit Basic block to be the jump destination   */
     /* if ((currentNode->else_destBB_index != -1 )&&(currentNode->ifelse_exit_bb_index == -1))
      {
        jump_insn = buildDB_find_jump_insn(currentNode->else_destBB_index);
        if(jump_insn != NULL)
        {
          jump_dest_BbIdx = buildDB_getJumpDestBBindex(jump_insn);
          currentNode->ifelse_exit_bb_index = jump_dest_BbIdx;
          currentNode->isValid = true;
        }

      }*///<<<<<<<<<<<<<<<DELETED?

      /* if the IF_THEN_ELSE statement have only 'if' (without 'else') */
      if(currentNode->ifelse_exit_bb_index == -1)
        currentNode->ifelse_exit_bb_index=currentNode->else_destBB_index;
        
      /* if the IF_THEN_ELSE Exit Basic block value is valid mark the node as valid */
      if(currentNode->ifelse_exit_bb_index != -1)
        currentNode->isValid=true;


    }//if HAS_NO_NESTING


  }//for

}

/* This Function build the ExitBBtoIfElseNode map
  ExitBBtoIfElseNode map :  (Exit Basic block Index) --> (IF_THEN_ELSE node) */
static void 
buildDB_build_ExitBBtoIfElseNode()
{
  std::map<int, if_then_else_node* >& bbToIfElseNode = buildDB_GetTableIfElseNode();
  std::map<int, if_then_else_node* >& ExitBBtoIfElseNode = buildDB_GetTableExitBBtoIfElseNode();
  std::map<int, if_then_else_node*>::iterator bbToIfElseNode_iter;
  if_then_else_node *currentNode;

  /*for each valid node on bbToIfElseNode build pair key = IF_THEN_ELSE Exit basic block
    and value = IF_THEN ELSE node and push the pair to ExitBBtoIfElseNode map*/
  for(bbToIfElseNode_iter = bbToIfElseNode.begin(); bbToIfElseNode_iter != bbToIfElseNode.end(); ++bbToIfElseNode_iter)
  {
    currentNode = bbToIfElseNode_iter->second;
    if(currentNode->isValid)
      ExitBBtoIfElseNode.insert (std::pair<int, if_then_else_node*>(currentNode->ifelse_exit_bb_index,currentNode)); 
  }


}


/* this function builds if else Data Base */
void 
buildDB_build_ifelseData()
{

  /* build a list that contains all IF_THEN_ELSE insns in the current function */
  buildDB_build_ifThenElse_list();

  /* initialize the bbToIfElseNode map with initial IF_THEN_ELSE nodes */
  buildDB_bbToIfElseNode_initialize_map();

  /* for each IF_THEN_ELSE node in bbToIfElseNode find if/else destination Basic block and update the node  */
  buildDB_update_bbToIfElseNode_ifelse_destBB();

  /* for each IF_THEN_ELSE node in bbToIfElseNode find IF_THEN_ELSE Exit Basic Block and update the node */
  buildDB_update_bbToIfElseNode_ifelse_ExitBB();

  /* build ExitBBtoIfElseNode map ((Exit Basic block Index) --> (IF_THEN_ELSE node)) */
  buildDB_build_ExitBBtoIfElseNode();


}




/***** Insn Value Dependency Data *****/

/* */
static void 
buildDB_build_reaching_defs_Data()
{

}

void 
buildDB_build_insn_value_dependency_Data()
{
  /*  */
  buildDB_build_reaching_defs_Data();

}








/* return unique ID for operand */
static int
buildDB_InsnToVal_calcOpernadId(insns_to_value * node){
  int exprId,insnId,exprShift=10,tempInId;
  insns_to_value * fatherNode = node->father;
  insnId=fatherNode->id;
  tempInId=insnId;

  while(tempInId > 9){
    exprShift=exprShift*10;
    tempInId=tempInId/10;
  }

  exprId=(exprShift * node->opernadNum) + insnId; // exprId= num|unique == unique

  return exprId;
}

/* calculate expression ID*/
static int 
buildDB_InsnToVal_calcId(insns_to_value * node)
{
  int idin;
  if(node->type == D_BTM_INSN){
    idin= INSN_UID(node->current_insn);
    return idin;
  }
  return  buildDB_InsnToVal_calcOpernadId(node);
}

/*** FREE DATA ***/
void buildDB_clear_function_Data(){
  std::list<rtx_insn* >& ifThenElseInsnsList = buildDB_GetTableIfelseList();
  std::map<int, if_then_else_node* >& bbToIfElseNode = buildDB_GetTableIfElseNode();
  std::map<int,basic_block>& bbIndexToBBmap = buildDB_GetTableBB();
  std::map<int,rtx_insn*>& uidToInsnMap = buildDB_GetTableInsn();
  std::map<int, if_then_else_node* >& ExitBBtoIfElseNode = buildDB_GetTableExitBBtoIfElseNode();



  std::map<int, if_then_else_node* >::iterator nodeIter;

  /* free ifThenElseInsnsList */
  ifThenElseInsnsList.clear();

  /* free bbToIfElseNode map */
  for(nodeIter = bbToIfElseNode.begin(); nodeIter!=bbToIfElseNode.end();++nodeIter)
    free(nodeIter->second);
  bbToIfElseNode.clear();

  /* free ExitBBtoIfElseNode map */
  for(nodeIter = ExitBBtoIfElseNode.begin(); nodeIter!=ExitBBtoIfElseNode.end();++nodeIter)
    free(nodeIter->second);
  ExitBBtoIfElseNode.clear();

  /*free bbIndexToBBmap*/
  bbIndexToBBmap.clear();

  /*free uidToInsnMap */
  uidToInsnMap.clear();

}