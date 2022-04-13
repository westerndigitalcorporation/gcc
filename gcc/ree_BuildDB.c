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
This file builds a dependency graph for given instruction (function level) and
prepares the dependency graph in the stack for range value analyzer.

it containing also helpful and useful data.

constructors:
SECTION 1  :
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

SECTION 2 :
2) db_build_ifelse_data() : builds if_then_else Data
  Data:
  2.1) Map : bb_to_if_else_node_map     : 
                      (IF_THEN_ELSE Basic Block index) --> (IF_THEN_ELSE Node) 

  2.2) Map : if_else_bb_to_node_map : (Exit Basic block Index) --> (IF_THEN_ELSE node)

  IMPORTANT NOTE : Before calling db_build_ifelse_data() constructor 
           db_build_rtx_extension_data() constructor SHOULD BE CALLED

SECTION 3 :
3)  db_value_dependency_graph () : 


destructor: 
  API:
  1)db_clear_function_Data()
  2)db_clear_insn_Data()

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
//#include "ree_stack_manage.h"
 
//#include "ree_eval.h"
#include <math.h>

 

 

extern  struct df_link * get_defs (rtx_insn *insn, rtx reg,
                                                       vec<rtx_insn *> *dest);

/*##### SECTION 0 : Globals and struct constructors #####*/

/* **** Globals ****  */

/* **  RTX Database **  */

/* Map : (insn UID) --> (insn)  */
std::map<int,rtx_insn*> uid_to_insn_map;

/* Basic block index to BasicBlock map  */
std::map<int,basic_block> bb_index_to_bb_map;


/* ** Value dependency Database **  */


/* * Value dependency intermediate data (temp) *  */
auto_vec<rtx_insn *> insn_defs;


/* Reference functions */


/* Return bb_index_to_bb_map map by reference  */
std::map<int,basic_block>& 
db_get_bb_index_to_bb_map()
{
  static std::map<int,basic_block> bb_index_to_bb_map;
  return bb_index_to_bb_map;
}

/* Return uid_to_insn_map map by reference  */
std::map<int,rtx_insn*>& 
db_get_uid_to_insn_map()
{
  static std::map<int,rtx_insn*> uid_to_insn_map;
  return uid_to_insn_map;
}



/* ** structs constructors **  */


/* if_then_else_node constructor */
static if_then_else_node *
db_create_ifelse_node(rtx_insn * ins)
{
 if_then_else_node *node = new  if_then_else_node;
 node->insn=ins;
 node->is_valid = false;
 node->has_nested = false;
 node->id=INSN_UID(ins);
 node-> ifelse_exit_bb_index = -1;
 node-> if_dest_bb_index = -1;
 node-> else_dest_bb_index = -1;
 return node;
}


/* Input  : Node 
   Output : Return unique ID for operand  */
static int
stack_manage_InsnToVal_calcOpernadId (insns_to_value  *node){
  int exprId,insnId,exprShift=10,tempInId;
  insns_to_value * fatherNode = node->father;
  
  insnId = fatherNode->id;
  tempInId = insnId;
  /* shifting the bits */
  while(tempInId > 9){
    exprShift=exprShift*10;
    tempInId=tempInId/10;
  }
  /* shift node opernand number and add father id to the result  */
  /* exprId= opnum|insnId(father id) =  num|unique == unique  */
  exprId=(exprShift * node->opernad_num) + insnId; 

  return exprId;
}

/* Calculate and return expression ID
   Input  :  Node
   Output :  Unique ID for the expression  */
static int 
db_calc_node_id(insns_to_value * node)
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
             False else  */
static bool 
db_is_code_supported(rtx_code  code)
{
  if((code == MEM)||(code == CALL_INSN) || (code == CALL) )
    return false;
  return true;
}

/* Node  constructor 
   Input:
   curr_insn : the node insn
   curr_expr : the node expression
   opernad_num : operand number in the insn 
   (if the node represents insn opernad_num will be zero)
   
   father : the node father 
   type : 
        if the node represents insn 'type' should be : D_BTM_INSN
    if the node represents insn 'type' should be : D_BTM_EXPR
  Output : pointer to the new node  */
insns_to_value*                        
db_create_node(rtx_insn *curr_insn,rtx curr_expr,
                            int opernad_num ,insns_to_value *father, int type)
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
  node->opernad_num = opernad_num;
  node->valid_value = false;
  node->is_supported = db_is_code_supported(node->code);
  node->is_visited = false;
  node->extreme_value_path = false;
  node->id = db_calc_node_id(node);

return node;

}

/* ##### SECTION 1 : RTX extension Data #####  */


/* This function builds:
   Map : (insn UID)-->(insn)
   Map : (Basic block index)-->(Basic Block)
   for the current function  */
void 
db_build_rtx_extension_data()
{
  /* get global maps by reference  */
  std::map<int,basic_block>& bb_index_to_bb_map = db_get_bb_index_to_bb_map();
  std::map<int,rtx_insn*>& uid_to_insn_map = db_get_uid_to_insn_map();
  basic_block bb;
  rtx_insn *insn,*insncopy;
  int id;
  /* For each basic block (bb) except to Entry/Exit BasicBlocks  */
  FOR_EACH_BB_FN (bb, cfun)
  {
    basic_block newbb=bb;
    /* Insert new element to map : (Basic block index)-->(Basic Block)  */
    bb_index_to_bb_map.insert (std::pair<int, basic_block >(newbb->index,newbb));
    /* For each insn in bb  */
    FOR_BB_INSNS (bb, insn)
    {
      insncopy=insn;
      id=INSN_UID(insncopy);
      /* Insert new element to map : (insn UID)-->(insn)  */
      uid_to_insn_map.insert (std::pair<int, rtx_insn* >(id,insncopy));
    }
  }
}


/* Input: insn uid
   Output : the insn (where INSN_UID(insn) == id)  */
rtx_insn*
db_get_insn(int id){
  std::map<int,rtx_insn*>& uid_to_insn_map = db_get_uid_to_insn_map();
  std::map<int, rtx_insn* >::iterator mapIter;
  
  /* Find and return the insn */
  mapIter=uid_to_insn_map.find(id);
   return mapIter->second;
}

/* Input : basic block index 
   Output : the  basic block (where basic_block->index == id ) */
basic_block
db_get_bb(int id)
{
  std::map<int,basic_block>& bb_index_to_bb_map= db_get_bb_index_to_bb_map();
  std::map<int, basic_block >::iterator mapIter;

  mapIter=bb_index_to_bb_map.find(id);
  /* If the basic block does not exist in the map return NULL  */
  if(mapIter==bb_index_to_bb_map.end())
    return NULL;

  /* Return the basic block  */
  return mapIter->second;
}




/* ##### SECTION 2 :  IF_THEN_ELSE Data  #####  */


/* This function searches for jump insn on the given Basic block
   and returns the jump insn if the Basic block does not function entry 
   or function exit and the Jump insn exist in the BB, 
   else if there is no jump insn in the Basic block returns NULL 
   
   input : Basic block Index  */
static rtx_insn*
db_find_jump_insn(int bbIndex){
  rtx_insn* bbinsn;
  basic_block bb;

  /* If current basci block is entry block or exit block return NULL  */
  if((bbIndex == ENTRY_BLOCK) || (bbIndex == EXIT_BLOCK))
    return NULL;

  /* Get the Basic block */
  bb = db_get_bb(bbIndex);
  gcc_assert(bb != NULL);

  /* for each insn in the Basic block if the insn is jump insn return it  */
  FOR_BB_INSNS (bb, bbinsn)
  {
    if(GET_CODE(bbinsn) == JUMP_INSN)
      return  bbinsn;    
  }

  /* if the  basic block does not contain jump insn  */
  return NULL;
}


/* If the kind of the jump_insns is : (jump_insn (set (pc)(label_ref ))
   This function will return the basic block index of the jump_insn destination 
   else it will return -1  */
static int
db_get_jump_dest_bb(rtx_insn* jmpinsn){
 
  rtx_insn* nextInsn;

  /* Check jump_insn kind  */
  rtx label_ref = XEXP(single_set(jmpinsn),1); 
  if(GET_CODE(label_ref) != LABEL_REF)
    return -1;

  /* Get code label as rtx_insn */
  rtx code_label = XEXP(label_ref,0);
  nextInsn = db_get_insn(INSN_UID(code_label));

  /* Find first insn after code label */
  while(nextInsn != NULL)
  {
    if((GET_CODE(nextInsn) == INSN) || (GET_CODE(nextInsn) == CALL_INSN) ||
       (GET_CODE(nextInsn) == JUMP_INSN))
      break;
    else
      nextInsn=NEXT_INSN(nextInsn);
  }

  
   /* If there is insns on the basic block 
      return the index of the jump destenation Basic block
      else return -1 */
    if(nextInsn != NULL)
      return BLOCK_FOR_INSN(nextInsn)->index;
    return -1;
}

 
/* This function builds a list that contains all IF_THEN_ELSE insns of the 
   current function  */
static void
db_build_ifelse_list(ifelse_data *ifelse_data_node)
{
  basic_block bb;
  rtx_insn *insn;

  /* For each basic block in function except to ENTRY/EXIT Basic block  */
  FOR_EACH_BB_FN (bb, cfun)
  {
    /* For each insn in Basic block  */
    FOR_BB_INSNS (bb, insn)
    {
      /* If the current insn is IF_THEN_ELSE push it to the list  */
      if(GET_CODE(insn) == JUMP_INSN)
        /* Skip jump insns like : jump_insn 174 221 175 12 (eh_return)  */
        if(NULL_RTX != single_set(insn) ) 
          /* If the jump insns type is: (jump_insn(set(pc) (IF_THEN_ELSE)...))
             push insn to the List  */
          if(GET_CODE(XEXP(single_set(insn),1)) == IF_THEN_ELSE )
            ifelse_data_node->if_then_else_insns_list.push_back(insn);
    }/* FOR INSN  */
  }/* FOR BB */
}


/* This function builds a new IF_THEN_ELSE node for each IF_THEN_ELSE insns
   and initializes the map with the new nodes.
   NOTE: After executing the function only node->id and node->insn value will
   be valid

   input:
     1- IF_THEN_ELSE insns list
     2- Map : (IF_THEN_ELSE Basic Block index) --> (IF_THEN_ELSE Node)
            */
static void
db_build_bb_to_ifelse_map(ifelse_data *ifelse_data_node)
{
  int bbIndex;

  /* IF_THEN_ELSE insns list itertator */
  std::list <rtx_insn* >::iterator IfElseListIter;
  std::map<int, if_then_else_node*>::iterator mapIter;

  /* For each if_then_else insn */
  for(IfElseListIter=ifelse_data_node->if_then_else_insns_list.begin(); 
      IfElseListIter != ifelse_data_node->if_then_else_insns_list.end(); ++IfElseListIter)
  {
   
    /* Get IF_THEN_ELSE insn basic block index */
    bbIndex=BLOCK_FOR_INSN(*IfElseListIter)->index;
   
    /* Creat new if_then_else node (initial node) */
    if_then_else_node* 
    ifelsenode = db_create_ifelse_node( *IfElseListIter);
      
    /* If there is more than IF_THEN_ELSE insns on the same basic block 
       then there is a Bug  */
    mapIter = ifelse_data_node->bb_to_if_else_node_map.find(bbIndex);
    gcc_assert(mapIter == ifelse_data_node->bb_to_if_else_node_map.end());

    /* Insert IF_THEN_ELSE insn to the map
       key= (basic block index) value= (if_then_else node)*/
    ifelse_data_node->bb_to_if_else_node_map.insert (
                      std::pair<int, if_then_else_node*>(bbIndex,ifelsenode)); 

  }

}


/* This function finding out the if /else  destination and updating the 
   node (node->if_dest_bb_index, node->else_dest_bb_index)
   Note: we holding the if_then_else nodes on bb_to_if_else_node_map Map  */
static void
db_update_ifelse_node_dest_bb(if_then_else_node* node)
{
  rtx_insn *ifelse_insn, *nextInsn;
  
  /* ** Updating 'else' destination basic block **  */

  /* Get if then else label_ref  */
  rtx label_ref=XEXP(XEXP(single_set(node->insn),1),1); 
  /* Get code label  */
  rtx code_label=XEXP(label_ref,0); 

  /* Get code label as rtx_insn  */
  nextInsn = db_get_insn(INSN_UID(code_label));

  /* Find first insn in 'else' destination block  */
  while(nextInsn != NULL)
  {
    if((GET_CODE(nextInsn) == INSN) || (GET_CODE(nextInsn) == CALL_INSN) ||
       (GET_CODE(nextInsn) == JUMP_INSN))
      break;
    else
      nextInsn=NEXT_INSN(nextInsn);
  }

  /* If 'else' insns does not deleted  */
  if(nextInsn != NULL)
    /* Update else destination basic block (index)  */
    node->else_dest_bb_index = (BLOCK_FOR_INSN(nextInsn)->index);


  /* ** Updating 'if' destination basic block **  */

  /* Get IF_THEN_ELSE insn  */
  ifelse_insn=node->insn;

  /* Find first insn in if_basic block  */
  nextInsn=NEXT_INSN(ifelse_insn);
  if(nextInsn != NULL)
  {
    while( (GET_CODE(nextInsn)!=INSN) && (GET_CODE(nextInsn)!=CALL_INSN) && 
           (GET_CODE(nextInsn)!=JUMP_INSN) )
    {
      nextInsn=NEXT_INSN(nextInsn);
        if(nextInsn == NULL)
          break; 
    }
  }
  /* If 'if' insns does not deleted  */
  if(nextInsn != NULL)
  /* Update if destination basic block (index)  */
    node->if_dest_bb_index = (BLOCK_FOR_INSN(nextInsn)->index);

}


/* This function updates the if/else destination for all the nodes in 
   bb_to_if_else_node_map Map  */
static void
db_update_map_dest_bb(ifelse_data *ifelse_data_node)
{

  std::map<int, if_then_else_node*>::iterator bbToIfElseNode_iter;
  
  /* For each IF_THEN_ELSE node in the map  */
  for(bbToIfElseNode_iter = ifelse_data_node->bb_to_if_else_node_map.begin();
   bbToIfElseNode_iter != ifelse_data_node->bb_to_if_else_node_map.end();
    ++bbToIfElseNode_iter)
  {
    /* Update the Node If/Else Basic blocks  */
    db_update_ifelse_node_dest_bb(bbToIfElseNode_iter->second);
  }
   
}

/* This Ture if the If th */
static void
db_update_ifelse_has_nested(if_then_else_node* node)
{
  basic_block if_dest_bb, else_dest_bb;
  rtx_insn* insn;
   
  gcc_assert(node != NULL);

  if(node->else_dest_bb_index != -1)
  {
    else_dest_bb = db_get_bb(node->else_dest_bb_index);
  /* if there IF_THEN_ELSE insns in the 'else' destination put 10 on result */
    FOR_BB_INSNS (else_dest_bb, insn)
    {
      if(GET_CODE(insn) == JUMP_INSN)
        if(NULL_RTX != single_set(insn)) 
          if(GET_CODE(XEXP(single_set(insn),1)) == IF_THEN_ELSE )
          {
            node->has_nested = true;
            break;
          }
    }/* FOR */
  }

  if(node->if_dest_bb_index != -1)
  {
    /* if there IF_THEN_ELSE insns in the 'if' destination add 1 to 
       the result  */
    if_dest_bb = db_get_bb(node->if_dest_bb_index);
    FOR_BB_INSNS (if_dest_bb, insn)
    {
      if(GET_CODE(insn) == JUMP_INSN)
        if(NULL_RTX != single_set(insn)) 
          if(GET_CODE(XEXP(single_set(insn),1)) == IF_THEN_ELSE )
          {
            node->has_nested = true;
            break;
          }
    }/* FOR */
  }

}

/* This function updates the IF_THEN_ELSE Exit basic block for all the nodes 
   in bb_to_if_else_node_map Map.
   IF_THEN_ELSE Exit BasicBlock is the first basic block that will
   be executed after the 'if/else' code  */
static void
db_update_map_exit_bb(ifelse_data *ifelse_data_node )
{
 
  std::map<int, if_then_else_node*>::iterator bbToIfElseNode_iter;
  if_then_else_node *currentNode;
  rtx_insn* jump_insn;
  int jump_dest_BbIdx;
  
  /* For each element on bb_to_if_else_node Map  */
  for(bbToIfElseNode_iter = ifelse_data_node->bb_to_if_else_node_map.begin();
   bbToIfElseNode_iter != ifelse_data_node->bb_to_if_else_node_map.end();
    ++bbToIfElseNode_iter)
  {
    /* Get the element If_then_else node  */
    currentNode = bbToIfElseNode_iter->second;
    
    /*  Checks if the IF_THEN_ELSE insn has nesting IF_THEN_ELSE insn   
        and update the has_nested node boolean flag.  */
     db_update_ifelse_has_nested(currentNode);

    /* If the current IF_THEN_ELSE insn has no nesting IF_THEN_ELSE  */
    if(! currentNode->has_nested)
    {
      /* If there is a jump insn in the 'if' code update the IF_THEN_ELSE 
         Exit Basic block to be the jump destination  */
      if (currentNode->if_dest_bb_index != -1)
      {
        jump_insn = db_find_jump_insn(currentNode->if_dest_bb_index);
        if(jump_insn != NULL)
        {
          jump_dest_BbIdx = db_get_jump_dest_bb(jump_insn);
          currentNode->ifelse_exit_bb_index = jump_dest_BbIdx;
          currentNode->is_valid = true;
        }
      }


      /* If the IF_THEN_ELSE statement have only 'if' (without 'else')  */
      if(currentNode->ifelse_exit_bb_index == -1)
        currentNode->ifelse_exit_bb_index=currentNode->else_dest_bb_index;
        
      /* if there is no optimization that delete the IF_THEN_ELSE
         mark it as valid  */
      if(currentNode->ifelse_exit_bb_index != -1)
        currentNode->is_valid=true;


    }/* If the IF_THEN_ELSE has no nested IF_THEN_ELSE  */


  }/* For each element  */

}

/* This Function builds the if_else_bb_to_node map
  if_else_bb_to_node map :  (Exit Basic block Index) --> (IF_THEN_ELSE node)  */
static void 
db_if_else_bb_to_node(ifelse_data *ifelse_data_node)
{

  std::map<int, if_then_else_node*>::iterator bbToIfElseNode_iter;
  if_then_else_node *currentNode;

  /* For each valid node on bb_to_if_else_node_map build pair 
     key = IF_THEN_ELSE Exit basic block and value = IF_THEN ELSE node 
     and push the pair to if_else_bb_to_node map  */
  for(bbToIfElseNode_iter = ifelse_data_node->bb_to_if_else_node_map.begin();
   bbToIfElseNode_iter != ifelse_data_node->bb_to_if_else_node_map.end();
    ++bbToIfElseNode_iter)
  {
    currentNode = bbToIfElseNode_iter->second;
    if(currentNode->is_valid){
      ifelse_data_node->if_else_bb_to_node_map.insert (
        std::pair<int, if_then_else_node*>
                             (currentNode->ifelse_exit_bb_index,currentNode)); 

      ifelse_data_node->if_else_bb_to_node_map.insert (
        std::pair<int, if_then_else_node*>
                                  (currentNode->if_dest_bb_index,currentNode)); 

      ifelse_data_node->if_else_bb_to_node_map.insert (
        std::pair<int, if_then_else_node*>
                                (currentNode->else_dest_bb_index,currentNode)); 

    }/* if is_valid */
  }/* for  */

}


/* This function builds if_then_else DataBase  */
void 
db_build_ifelse_data(ifelse_data* ifelse_data_node)
{

  /* Build a list that contains all IF_THEN_ELSE insns in 
     the current function  */
  db_build_ifelse_list(ifelse_data_node);

  /* Initialize the bb_to_if_else_node map with
     initial IF_THEN_ELSE nodes  */
  db_build_bb_to_ifelse_map(ifelse_data_node);

  /* For each IF_THEN_ELSE node in bb_to_if_else_node_map 
     find if/else destination Basic block and update the node  */
  db_update_map_dest_bb(ifelse_data_node);

  /* For each IF_THEN_ELSE node in bb_to_if_else_node_map 
     find IF_THEN_ELSE Exit Basic Block and update the node  */
  db_update_map_exit_bb(ifelse_data_node);

  /* Build if_else_bb_to_node_map map 
     ((ifelse Basic block Index) --> (IF_THEN_ELSE node))  
     note : ifelse basic block can be : if bb / else bb / Exit bb  */
  db_if_else_bb_to_node(ifelse_data_node);

}


/* ##### SECTION 3 : Insn Value Dependency Data #####  */


/* This function builds reachable definitions Data and returns true if 
   there are reachable definitions instructions(insn).
   reachable definitions will be held on insn_defs vector  */
static bool 
db_build_expr_reaching_defs_Data(insns_to_value* node )
{
  /* Clear insn_defs vector previous data */
  insn_defs.truncate (0);
  
  /* Push insn reachable defs to insn_defs vector and return true if the
     vector is not empty  */
  if (get_defs (node->current_insn,node->current_expr /*src_reg*/,
                &insn_defs) != NULL)
  {
   return true;
  }
  
  /* If there is no reachable defs return false  */
  return false;
}

/* This function categorize the expression(node) dependency problem
   We have three different categories:
   1) There is no dependency, i.e if the expression is constant we have the 
      expression value, and it does not depend on any expression or insn.

   2) Insn dependency which means other instruction influencing or could
      influence the expression value.
      i.e if the current expression is src REG and other instruction could 
      edit his value (note: we didn't support MEM/CALL).

   3) Operand dependency i.e we have sub-expression as src operand 
      (for example we write the result PLUS to expression destination 
      (insn 100 99 101 15 (set (reg:SI 25 s9 [157])<--- expression destination
        (plus:SI (reg:SI 15 a5 [156])         <--- sub-expression(src operand)
          (reg/v:SI 25 s9 [orig:86 nextFreeIndex ] [86]))) 3 {addsi3}  */
static int 
db_get_dependency_type(insns_to_value* node )
{
  if(node->code == REG)
    return INSN_DEPENDENCY_PROBLEM;
  if((GET_RTX_CLASS (node->code) == RTX_CONST_OBJ))
    return NO_DEPENDENCY_PROBLEM;
  return OPERAND_DEPENDENCY_PROBLEM;
}

 

/* return expression first Src operand (index)  */
static int 
db_get_firstSrcOperand_index(insns_to_value* node)
{
  if((node->code == SUBREG)||(GET_RTX_CLASS (node->code) == RTX_COMM_ARITH)||
    (GET_RTX_CLASS (node->code) == RTX_UNARY)||
    (GET_RTX_CLASS (node->code) == RTX_BIN_ARITH)||
    (GET_RTX_CLASS (node->code) == RTX_AUTOINC))
    return 0;
  return 1;
}

/* return true if current operand is src operand  */
static bool 
db_isSrcOperand(insns_to_value * node, int opIndex)
{
  if((opIndex < db_get_firstSrcOperand_index(node)))
    return false;
  return true;

}

/* Push expression Src operands to the stacks  */
static void 
db_push_operands(insns_to_value* node)
{
  int i,numOfOperands;
  insns_to_value* operandNode;
  rtx operand,expr;

  /* Node expression  */
  expr=node->current_expr;

  /* Number of expression operands  */  
  numOfOperands = GET_RTX_LENGTH(GET_CODE(expr));
  
  /* For each operand on the expression  */
  for(i=0; i<numOfOperands; i++)
  {
    operand=XEXP(expr, i);

    /* If the operand is src operand  */
    if(db_isSrcOperand(node, i))
    {
      /* Creat node for the operand  */
      operandNode = db_create_node(node->current_insn,
                                               operand, i+1,node, D_BTM_EXPR);

      /* Push the node to the stack and to the temporary stack*/
      stack_manage_push(STACK_MANAGE_ORIGIN, operandNode);
      stack_manage_push(STACK_MANAGE_TEMP, operandNode);

    } /* if is src operand  */
  } /* for  */
}


  
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
                                                   std::list<int>& preds_list)
{
  gcc_assert(bb != NULL);
  std::list<int>::iterator visited_bb_it;
  visited_bb_it = std::find(preds_list.begin(), preds_list.end(), bb->index);

  /*  For each predecessor Basick block except to the entry Basic block  */
  while((bb->prev_bb != NULL)&&(visited_bb_it == preds_list.end()))
  {
      
    /*  If the basic block not visted push it to list  */
    preds_list.push_back(bb->index);
    /*  current bb = prev bb  */
    bb=bb->prev_bb;
    /*  If the basic block is visited stop and return  */
    visited_bb_it = std::find(preds_list.begin(), preds_list.end(),bb->index);      

  }/*  while  */

}


/* This function return true if there use def on the given Basic block  */
static bool 
db_is_bb_has_UseDefs( int bbId, dependency_data * dependency_db)
{

  std::map<int,std::list<rtx_insn*>>::iterator bbDefIter;

  if(db_keyExistInMap(dependency_db->bb_index_to_preds_defs_map, bbId))
    {
      bbDefIter=dependency_db->bb_index_to_preds_defs_map.find(bbId);
      if(!(bbDefIter->second).empty())
        return true;

    }

  return false;

}


/* This function return the closest predecessor Basic block with use def */
static int 
db_getClosestBBwithUseDef(std::list<int> predsBBindex,
                                             dependency_data * dependency_db)
{
  int bbId;
  /* for each predecessor BB (sorted closest first)  */
  while(!predsBBindex.empty()){

    bbId = predsBBindex.front();

    /* if the predecessor Basic block contains Use defs 
       return the basic block index  */
    if(db_is_bb_has_UseDefs( bbId, dependency_db))
      return bbId;

    predsBBindex.pop_front();
  }

  /* if there is no dependency with predecessor insns  */
  return -1;   
}


/* return true if bb1 is predecessor Basic  block for bb2  */
static bool 
db_is_bb1_pred_to_bb2(int bb1_Index, int bb2_Index)
{
  basic_block bb2;
  std::list<int> predBB; 
  std::list<int>::iterator predBB_iter; 

  /* get BB predecessor Basic block  */
  bb2 = db_get_bb(bb2_Index);
  /* Put bb2 predecessors basic block indexes list to predBB List  */
  db_build_basic_block_predecessors_List(bb2,predBB);
  predBB.pop_front();

  /* find bb1 on predecessors list  */
  predBB_iter = std::find(predBB.begin(), predBB.end(), bb1_Index);
  
  /* return is bb1 predecessor for bb2  */
  return (predBB_iter != predBB.end());
}

/* This function returns a binary number where each bit on the mask 
   represents the problem(Take a look at enum  insn_dependency_problem_mask).
   for example, if we get as result 6(0b110) that's mean 
   we have two problems loop problem and if_then_else problem  */
static int 
db_categorize_insn_dependency_problem(insns_to_value* node,
              ifelse_data *ifelse_data_node, dependency_data * dependency_db)
{
  int problemBitwiseFlags = 0, lastBBindex;
  basic_block insnBasicblock;
  if_then_else_node* ifelesNode;
  bool ifBBhasUseDef, elseBBhasUseDef;

  std::list<int> predBB; 
  std::map<int,if_then_else_node* >::iterator bbToIfElseIterator;

  insnBasicblock = BLOCK_FOR_INSN(node->current_insn);
  bbToIfElseIterator = 
        ifelse_data_node->if_else_bb_to_node_map.find(insnBasicblock->index);
    

  /* If the insn in some IF_THE_ELSE Exit Basic block */
  if(bbToIfElseIterator != ifelse_data_node->if_else_bb_to_node_map.end())
  {
     /*rise IF_THEN_ELSE mask bit*/
     problemBitwiseFlags = (problemBitwiseFlags | 
                           IF_THEN_ELSE_DEPENDENCY_PROBLEM_MASK);
    /* Put bb2 predecessors basic block indexes list to predBB List  */
    db_build_basic_block_predecessors_List(insnBasicblock,predBB);
    /* Get closest predecessor basic block with use-def  */
    lastBBindex=db_getClosestBBwithUseDef(predBB, dependency_db);
 
    /* If there is dependency after the if/else then the value will be taken 
       from the last dependency and we don't have IF_THEN_ELSE dependency 
       problem  */
    if(lastBBindex == insnBasicblock->index)
    {
      problemBitwiseFlags = (problemBitwiseFlags & 
                             (~IF_THEN_ELSE_DEPENDENCY_PROBLEM_MASK));

      problemBitwiseFlags = (problemBitwiseFlags | 
                             LAST_DEPENDENCY_PROBLEM_MASK);

    }
    else
    {
      ifelesNode=bbToIfElseIterator->second;
      /* Check if 'if BB'/'else BB' has Use Defs(dependency)*/
      ifBBhasUseDef = db_is_bb_has_UseDefs(ifelesNode->if_dest_bb_index, 
                                                              dependency_db);
      elseBBhasUseDef = db_is_bb_has_UseDefs(ifelesNode->else_dest_bb_index,
                                                              dependency_db);

      /* If no dependency on IF_THEN_ELSE code then last Use def before
         the insn is a candidate dependency  */
      if((ifBBhasUseDef == false) && (elseBBhasUseDef==false))
      {
        problemBitwiseFlags = (problemBitwiseFlags & 
                               (~IF_THEN_ELSE_DEPENDENCY_PROBLEM_MASK));
        problemBitwiseFlags = (problemBitwiseFlags | 
                               LAST_DEPENDENCY_PROBLEM_MASK);
        
      }
      else
      {
        /* If we have dependency only in 'if' or 'else' code (not in both)
           then last Use Def before the IF_THEN_ELSE insn 
          (or bettwen if/else Basic blocks) is a candidate dependency  */
        if(ifBBhasUseDef != elseBBhasUseDef)
          problemBitwiseFlags = (problemBitwiseFlags | 
                                 LAST_DEPENDENCY_BEFORE_IF_ELSE_PROBLEM_MASK);
      }

    }
    
  }
  else /* If there is no IF_THEN_ELSE dependency  */
    problemBitwiseFlags = (problemBitwiseFlags | LAST_DEPENDENCY_PROBLEM_MASK);


return problemBitwiseFlags;

}

/* This function returns true if the dep_insn is predecessors to currentInsn 
   i.e if dep_insn in predecessors Basic block or come before currentInsn in 
  the same basic block */
static bool 
db_depInsn_predTo_currentInsn(rtx_insn* dep_insn, rtx_insn* currentInsn,
                                         dependency_data * dependency_db)
{
  basic_block dep_bb, current_bb;
  int dep_LUID, current_LUID;
  
  std::list<int>::iterator predsListIter;

  dep_bb = BLOCK_FOR_INSN(dep_insn);
  current_bb = BLOCK_FOR_INSN(currentInsn);


  /* If dep_insn and currentinsn in the same Basic block.  */
  if(dep_bb->index == current_bb->index)
  {
    /* If dep_insn come before currentInsn return true else return false  */
    dep_LUID = DF_INSN_LUID(dep_insn);
    current_LUID = DF_INSN_LUID(currentInsn);
    return(dep_LUID < current_LUID);
  }
  
  /* If dep_insn in predecessors basic block return true else return false  */
  predsListIter = std::find(dependency_db->g_preds_bb_index.begin(), 
                     dependency_db->g_preds_bb_index.end(), dep_bb->index);

  return (predsListIter != dependency_db->g_preds_bb_index.end());

}

/**/
static bool 
db_keyExistInMap(std::map<int, std::list<rtx_insn*>> theMap, int id)
{
  std::map<int,std::list<rtx_insn*>>::iterator mapIter;
 
 if(theMap.empty())
  return false;

 mapIter = theMap.find(id);
  return (mapIter != theMap.end());
}

/* This function build global Map bb_index_to_preds_defs_map : 
  (Basic block index)--> (List of BasicBlock UseDefs )  */
static void 
db_bb_index_to_preds_defs_map(rtx_insn* currentInsn, 
                                          dependency_data * dependency_db)
{
  rtx_insn *dep_insn;
  basic_block depBB;
  int depBBindex;
 
  std::map<int,std::list<rtx_insn*>>::iterator BBtoDefIter;

  /* For each reachable def (insn dependency) */
  while(!insn_defs.is_empty ())
  {
    
    dep_insn = insn_defs.pop();
    
    /* If dep_insn(reachable def) is predecessors to the 
      currentInsn (come before currentInsn ) add dep_insn to the Map  */
    if(db_depInsn_predTo_currentInsn(dep_insn, currentInsn, dependency_db))
    {
      /* Get dep_insn Basic Block index */
      depBB = BLOCK_FOR_INSN(dep_insn);
      depBBindex = depBB->index; 
      
      /* Add dependency insn to dep_insn use-def list */
      db_insert_insn_to_map_List(dependency_db->bb_index_to_preds_defs_map,
                                      depBBindex ,dep_insn);

    }
  }
}

/**/
static rtx_insn* 
db_getBBLastInsn(std::list<rtx_insn*> defInsnsList)
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
This function inserts insn on a map from the type : 
key = int value =  std::list<rtx_insn*>
the insn will be inserted on the element list (the element with key=id . list) 
if there is no such element(with key=id) it will create
a new element with key =id and will insert the insn in the new element list  */
static void 
db_insert_insn_to_map_List(std::map<int, std::list<rtx_insn*>>& theMap,
                                int id,rtx_insn* insn)
{
  std::map<int, std::list<rtx_insn*>>::iterator theMapIter;
   /* If element with key = id  exist add insn to element List  */
   if(db_keyExistInMap(theMap, id))
      {
        theMapIter = theMap.find(id);
        theMapIter->second.push_back(insn);
      }
      else
      {
        /* If no element with key= id on the map, create and add a new
           element with key= id and add the insn to the new element list  */
        std::list<rtx_insn*> newlist;
        theMap.insert (std::pair<int, std::list<rtx_insn*>>(id, newlist));
        theMapIter=theMap.find(id);
        theMapIter->second.push_back(insn);
      }
}

/* */
static void db_findAndPush_Last_Dependency_insn(insns_to_value* node,
                                              dependency_data * dependency_db)
{
  int lastBBindex;
  rtx_insn* lastInsn;
  std::list<int> predBB; 
  std::map<int,std::list<rtx_insn*>>::iterator BBtoDefIter;

  /* get closest predecessor basic block  */
  lastBBindex = db_getClosestBBwithUseDef(dependency_db->g_preds_bb_index, 
                                                               dependency_db);
  
  if(lastBBindex != -1)
  {
    /* find closest Basic block UseDef List  */
    BBtoDefIter = dependency_db->bb_index_to_preds_defs_map.find(lastBBindex);
    /* get last use-def insn in the basic block  */
    lastInsn = db_getBBLastInsn(BBtoDefIter->second);

    /*add lastInsn to current_insn dependency list  */
    db_insert_insn_to_map_List(dependency_db->insn_uid_to_dep_insns,
                                    INSN_UID(node->current_insn) ,lastInsn);

  }

}

/* This function find out if the current insn has a dependency 
   on IF_THEN_ELSE code that's is if the IF_THEN_ELSE Exit Basic block 
   and current insn Basic block is the same Basic block, and some 
   insn in IF/ELSE Basic block write to current insn Src REG  */
static void 
db_findAndPush_IfThenElse_Dependency_insns(insns_to_value* node, 
               ifelse_data *ifelse_data_node, dependency_data * dependency_db)
{

  std::map<int,if_then_else_node* >::iterator bbToIfElseIterator;
  std::map<int,std::list<rtx_insn*>>::iterator BBtoDefIter;
  basic_block insnBasicblock;
  if_then_else_node* ifelesNode;
  bool ifBBhasUseDef, elseBBhasUseDef;
  rtx_insn* dep_insn; 


  /* Find the IF_THEN_ELSE node that his Exit Basic Block
     is the same Basic block as Current insn  */
  insnBasicblock = BLOCK_FOR_INSN(node->current_insn);
  bbToIfElseIterator = ifelse_data_node->if_else_bb_to_node_map.find(
                                                       insnBasicblock->index);

  /* We must have 'if then else BasicBlock'  */
  gcc_assert(
       bbToIfElseIterator != ifelse_data_node->if_else_bb_to_node_map.end());

  ifelesNode = bbToIfElseIterator->second;
  /* check if 'if BB'/'else BB' has Use Defs(dependency)*/
  ifBBhasUseDef = db_is_bb_has_UseDefs(ifelesNode->if_dest_bb_index,
                                                              dependency_db);
  elseBBhasUseDef = db_is_bb_has_UseDefs(ifelesNode->else_dest_bb_index,
                                                              dependency_db);

  /* If the 'IF Basic block' has UseDef (that's is insns that write to 
     the current insn SRC REG)
     find the last UseDef and push it to the dependency list */
  if(ifBBhasUseDef)
  {
    BBtoDefIter = dependency_db->bb_index_to_preds_defs_map.find(
                                                ifelesNode->if_dest_bb_index);
    dep_insn = db_getBBLastInsn(BBtoDefIter->second);
    db_insert_insn_to_map_List(dependency_db->insn_uid_to_dep_insns,
                                    INSN_UID(node->current_insn), dep_insn);

  }

  /* If the 'ELSE Basic block' has UseDef (that's is insns that write to
     the current insn SRC REG)
     find the last UseDef and push it to the dependency list  */
  if(elseBBhasUseDef)
  {
    BBtoDefIter = dependency_db->bb_index_to_preds_defs_map.find(
                                            ifelesNode->else_dest_bb_index);
    dep_insn = db_getBBLastInsn(BBtoDefIter->second);
    db_insert_insn_to_map_List(dependency_db->insn_uid_to_dep_insns, 
                                    INSN_UID(node->current_insn) ,dep_insn);

  }

}

/* This function add last dependncy before the if_then_else for
   node exepression insn .
   the dependency will be added into insn_uid_to_dep_insns Map */
static void 
db_findAndPush_Last_Dependency_insn_before_IfThenElse(insns_to_value* node, 
             ifelse_data *ifelse_data_node, dependency_data * dependency_db)
{

  std::map<int,if_then_else_node* >::iterator bbToIfElseIterator;

  std::map<int,std::list<rtx_insn*>>::iterator BBtoDefIter;
  std::list<int> IfElsePredsBBs;
  basic_block insnBasicblock, ifElseBB;

  if_then_else_node* ifelesNode;
  bool ifBBhasUseDef, elseBBhasUseDef;
  rtx_insn* dep_insn; 
  int lastBBindex;

  
  /* Find IF_THEN_ELSE insn  */
  insnBasicblock = BLOCK_FOR_INSN(node->current_insn);
  bbToIfElseIterator = ifelse_data_node->if_else_bb_to_node_map.find(
                                                       insnBasicblock->index);
  gcc_assert(
       bbToIfElseIterator != ifelse_data_node->if_else_bb_to_node_map.end());

  ifelesNode = bbToIfElseIterator->second;

  /* Make sorted list with IF_THEN_ELSE predecessors basic blocks  */
  ifElseBB=BLOCK_FOR_INSN(ifelesNode->insn);
  db_build_basic_block_predecessors_List(ifElseBB,IfElsePredsBBs);


  /* Get closest predecessor basic block with use def  */
  lastBBindex = db_getClosestBBwithUseDef(IfElsePredsBBs, dependency_db);
  
  /* If we found predecessor basic block with use def  */
  if(lastBBindex != -1)
  {
     /* Find closest Basic block UseDef List  */
    BBtoDefIter = dependency_db->bb_index_to_preds_defs_map.find(lastBBindex);
    /* Get last use-def insn in the basic block  */
    dep_insn = db_getBBLastInsn(BBtoDefIter->second);

    /* Add dep_insn to current_insn dependency list  */
    db_insert_insn_to_map_List(dependency_db->insn_uid_to_dep_insns, 
                                    INSN_UID(node->current_insn) ,dep_insn);
  }

}

/* This function finding out expression value dependency insns and add new 
   entry on exprID_To_DependencyInsns map where 
   key = (expression Id) value = (candidate insns dependency List)  */
static void 
db_insn_dependency_data(insns_to_value* node,ifelse_data *ifelse_data_node,
                        dependency_data * dependency_db)
{
  bool ThereIsReachingDefs;
  int dependency_problems;
  std::map<int, std::list<rtx_insn*>>::iterator InsnToDepIter;

  /* if this insn is processed in the past then we have cyclic dependency
     cleat dependency and dont push the dependency and the node will be 
     converted to extremum node  */
  if(db_keyExistInMap(dependency_db->insn_uid_to_dep_insns,
                                                INSN_UID(node->current_insn)))
  {
    /* find the insn dependency list  */
    InsnToDepIter = dependency_db->insn_uid_to_dep_insns.find(
                                                INSN_UID(node->current_insn));
    /* delete the insn dependency list  */
    InsnToDepIter->second.clear();

    return;

  }
  
  /* Build reaching defs vector(insn_defs) and updates 
     ThereIsReachingDefs value.
     (if there reaching defs ThereIsReachingDefs value will be true )  */
  ThereIsReachingDefs = db_build_expr_reaching_defs_Data(node);

  /* If there reaching defs  */
  if(ThereIsReachingDefs)
  {
    /* Clear previous data  */
    dependency_db->g_preds_bb_index.clear();
    /*  Build predecessor  basic blocks  global list for the current insn  */
    db_build_basic_block_predecessors_List(BLOCK_FOR_INSN(node->current_insn),
                                          dependency_db->g_preds_bb_index);
    
    /* Build global Map bb_index_to_preds_defs_map :
       (Basic block index)--> (List of BasicBlock UseDefs)  */
    db_bb_index_to_preds_defs_map(node->current_insn, dependency_db);


    /* Analyse and categorize dependency problem */
    dependency_problems = db_categorize_insn_dependency_problem(node,
                                            ifelse_data_node, dependency_db);

	/* if we had more the one IF_THEN_ELSE insn on if then else
     dependency problem we deal with the current REG as un-supported insn
	   that's is  :
	   1)  mark the node as not-supported
     2)  don't push the if_then_esle dependency
	   3)  put the upper bound to machine word size extremum value
         (as Shortcut we put on the REG operands Upper bound list
	       element with  machine word size extremum value and on eval the
         REG value will be machine word size extremum value )  

         NOTE : we can support more than one if then eles that has no
                nested if then else  */
      if((dependency_problems & IF_THEN_ELSE_DEPENDENCY_PROBLEM_MASK) ==
                                        IF_THEN_ELSE_DEPENDENCY_PROBLEM_MASK)
      {
        if(ifelse_data_node->if_then_else_insns_list.size() > 1)
        {
		      /* Push  machine word size extremum value on the operands List  */
          node->operands_upper_bound.push_back(MACHINE_WORD_MAX_VALUE);

          /* Mark node as un-supported  */
          node->is_supported = false;
		      /* Exit from function  */
          return;
        } /* if there more than one IF_THEN_ELSE  */
      }/* IF_THEN_ELSE_DEPENDENCY_PROBLEM_MASK  */



    /* If the dependency problem category is the Last dependency problem
       find and push the last dependency insn to insn dependency List  */
    if((dependency_problems & LAST_DEPENDENCY_PROBLEM_MASK) ==
        LAST_DEPENDENCY_PROBLEM_MASK)
    {
      db_findAndPush_Last_Dependency_insn(node, dependency_db);
    }/* if LAST_DEPENDENCY_PROBLEM_MASK */

    /* If the dependency problem category is IF THEN ELSE dependency problem 
       find and push the IF/ELSE dependency insn to insn dependency List  */
    if((dependency_problems & IF_THEN_ELSE_DEPENDENCY_PROBLEM_MASK) ==
       IF_THEN_ELSE_DEPENDENCY_PROBLEM_MASK)
      db_findAndPush_IfThenElse_Dependency_insns(node, ifelse_data_node, 
                                                 dependency_db);
    
    /* If the dependency problem category is the last dependency insn before
       IF THEN ELSE problem find and push the last dependency  problem befor
      IF/ELSE dependency insn to insn dependency List  */
    if((dependency_problems & LAST_DEPENDENCY_BEFORE_IF_ELSE_PROBLEM_MASK) ==
        LAST_DEPENDENCY_BEFORE_IF_ELSE_PROBLEM_MASK)
      db_findAndPush_Last_Dependency_insn_before_IfThenElse(node,
                                             ifelse_data_node, dependency_db);


   /* Clear insn dependency data  */
   db_clear_insn_Data(dependency_db);
  }/* if there reaching defs  */

}

/* This function push the node candidadte dependencies to the stack  */
static void 
db_Push_candidate_insns_dependency_to_stack(insns_to_value* node, 
                                             dependency_data * dependency_db)
{

  std::map<int, std::list<rtx_insn*>>::iterator InsnToDepIter;
  std::list<rtx_insn*> dep_list;
  std::list<rtx_insn*>::iterator depListIter;
  rtx_insn* dep_insn;
  insns_to_value *dep_insn_Node;

  /* If the node had candidate dependency  */
  if(db_keyExistInMap(
          dependency_db->insn_uid_to_dep_insns, INSN_UID(node->current_insn)))
  {
    /* Get current insn dependency List  */
    InsnToDepIter = dependency_db->insn_uid_to_dep_insns.find(
                                                INSN_UID(node->current_insn));

    dep_list=InsnToDepIter->second;

    /* For each dependency */
    for(depListIter=dep_list.begin(); depListIter != dep_list.end();
                                                                ++depListIter)
    {
      /* Creat node for dep_insn */
      dep_insn = *depListIter;
      dep_insn_Node = db_create_node(dep_insn,
                                   single_set(dep_insn), 0, node, D_BTM_INSN);
    
      /* Push node to stacks  */
      stack_manage_push(STACK_MANAGE_ORIGIN,
                                             dep_insn_Node);

      stack_manage_push(STACK_MANAGE_TEMP, dep_insn_Node);
    } /* for  */
  } /* IF  */
}


/* In this function, will build an Insn value dependency graph,
   we sort the graph on the stack(STACK_MANAGE_ORIGIN) for the eval Phase,
   where nodes on the stack head should be evaluated before nodes on the
   bottom of the stack.  */
void 
db_build_value_dependency_graph(rtx_insn* insn) 
{
  /* Get RTX Database as reference to clear them at the end of the function  */
  std::map<int,basic_block>& bb_index_to_bb_map = db_get_bb_index_to_bb_map();
  std::map<int,rtx_insn*>& uid_to_insn_map = db_get_uid_to_insn_map();
  std::map<int,std::list<rtx_insn*>>::iterator BBtoPredsIter;


  insns_to_value *insnNode, *currentNode;
  int dep_type;

  ifelse_data *ifelse_data_node = new ifelse_data;
  dependency_data * dependency_db = new dependency_data;
  /* Build ret extension global maps  */
  db_build_rtx_extension_data();
  /* build and map if_then_else insns data where the main data is 
     maping part of IF_THEN_ELSE basic blocks to ifelse node containing 
     the if basic block , else basic block , exit basic block that this 
     block part of it  */
  db_build_ifelse_data(ifelse_data_node);


  /* Creat node for insn  */
  insnNode = db_create_node(insn, NULL, 0, NULL, D_BTM_INSN);

  /* Push the node to the stack and the temporary stack  */
  stack_manage_push(STACK_MANAGE_ORIGIN, insnNode);
  stack_manage_push(STACK_MANAGE_TEMP, insnNode);
  
  /* While TEMP stack not empty */
  while(!stack_manage_is_empty(STACK_MANAGE_TEMP))
  {
    /* Get stack Top */
    currentNode = stack_manage_top(STACK_MANAGE_TEMP);
    /* If the stack top is visited pop it from the stack */
    if(currentNode->is_visited)
    {
       stack_manage_pop(STACK_MANAGE_TEMP);
    }
    else /* If the stack top in not visited*/
    {
       currentNode->is_visited=true;

      /* If we support the CurrentNode expression code */
      if(currentNode->is_supported)
      {

        /* Categorize the Node dependency
           We have three cases:

            Case 1: we did not have a dependency problem 
                   (the expression is constant). in that case do nothing 
                   (only mark node as visited)

            Case 2: the value of the node expression depends on his operands.
                    in that case push his operands to the stack 
                    (and mark the node as visited)

            Case 3: the node expression is REG. in that case find candidate 
                    insn dependency (insns that the REG value depend on them) 
                  and push them to the stack (and mark the node as visited)  */

        dep_type = db_get_dependency_type(currentNode);

        /* Case 2 : Operands dependency
           Build node for each dependency and push the node to the stack  */
        if(dep_type == OPERAND_DEPENDENCY_PROBLEM)
          db_push_operands(currentNode);

        /* Case 3 : Insn dependency
           Build candidate insn dependency Data  
           and push candidate dependency to the stack (creat node for
           each dependency)  */
        if(dep_type == INSN_DEPENDENCY_PROBLEM)
        {
          db_insn_dependency_data(currentNode, ifelse_data_node ,
                                                               dependency_db);
          db_Push_candidate_insns_dependency_to_stack(currentNode, 
                                                               dependency_db);
        } /* if INSN_DEPENDENCY_PROBLEM  */

      } /* if is_supported  */
      
    }/* else  */

  }/* while  */

/* free and clear data  */

  /* free g_preds_bb_index */
  dependency_db->g_preds_bb_index.clear(); 

  /* free bb_index_to_preds_defs_map */
  for(BBtoPredsIter = dependency_db->bb_index_to_preds_defs_map.begin(); 
      BBtoPredsIter != dependency_db->bb_index_to_preds_defs_map.end(); ++BBtoPredsIter)
      BBtoPredsIter->second.clear();
  dependency_db->bb_index_to_preds_defs_map.clear();

db_clear_insn_Data(dependency_db);
db_clear_function_Data(ifelse_data_node,dependency_db);
free(ifelse_data_node);
free(dependency_db);



}


 

/*** FREE DATA ***/
void db_clear_function_Data(ifelse_data *ifelse_data_node, 
                                          dependency_data * dependency_db)
{

  std::map<int,basic_block>& bb_index_to_bb_map = db_get_bb_index_to_bb_map();
  std::map<int,rtx_insn*>& uid_to_insn_map = db_get_uid_to_insn_map();
  std::map<int, if_then_else_node* >::iterator nodeIter;

  /* free if_then_else_insns_list */
  ifelse_data_node->if_then_else_insns_list.clear();

  /* free bb_to_if_else_node map */
  for(nodeIter = ifelse_data_node->bb_to_if_else_node_map.begin(); 
  nodeIter!= ifelse_data_node->bb_to_if_else_node_map.end(); ++nodeIter)
    free(nodeIter->second);

  ifelse_data_node->bb_to_if_else_node_map.clear();

  /* free if_else_bb_to_node_map  
     Note: we free the nodes on 'free bb_to_if_else_node_map map' */
  ifelse_data_node->if_else_bb_to_node_map.clear();

  /*free bb_index_to_bb_map*/
  bb_index_to_bb_map.clear();

  /*free uid_to_insn_map */
  uid_to_insn_map.clear();

  /*free insn_uid_to_dep_insns*/
  dependency_db->insn_uid_to_dep_insns.clear();

}/* db_clear_function_Data  */

/* */
void db_clear_insn_Data(dependency_data * dependency_db){

  std::map<int,std::list<rtx_insn*>>::iterator BBtoPredsIter;

  /* free g_preds_bb_index */
  dependency_db->g_preds_bb_index.clear(); 

  /* free bb_index_to_preds_defs_map */
  for(BBtoPredsIter = dependency_db->bb_index_to_preds_defs_map.begin(); 
      BBtoPredsIter != dependency_db->bb_index_to_preds_defs_map.end(); ++BBtoPredsIter)
      BBtoPredsIter->second.clear();
  dependency_db->bb_index_to_preds_defs_map.clear();
 
}/* db_clear_insn_Data  */

void
db_free_node(insns_to_value* node)
{
  node->operands_upper_bound.clear();
  free(node);
}