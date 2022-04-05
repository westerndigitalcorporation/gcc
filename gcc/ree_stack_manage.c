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

/* This file manage two global generic stacks , origin stack and temp stack */

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
#include <pthread.h>
#include "ree_stack_manage.h"

#ifndef GCC_REE_STACK_MANAGE
#define GCC_REE_STACK_MANAGE

namespace stm
{

  /* This function returns chosen stack by reference 
     input:
       1)stack_choise : the stack that we want to work with it
         stack_choise should be one of :
           1.1) (enum val=0) STACK_MANAGE_ORIGIN : origin stack 
           1.2) (enum val=1) STACK_MANAGE_TEMP : temp stack  */
  template <typename ItemType>
  std::stack<ItemType*>& stack_manage_get(int stack_choice)
  {
    if(stack_choice == STACK_MANAGE_TEMP)
    {
     static std::stack<ItemType*> stack_manage_temp;
     return stack_manage_temp;
    }

    /* stack_choice == STACK_MANAGE_ORIGIN  */
    static std::stack<ItemType*> stack_manage_origin;
    return stack_manage_origin;
  }


  
  /* This function push the node to the chosen stack
     Input:
       1) node
       2)stack_choise : the stack that we want to work with it
         stack_choise should be one of :
         2.1) (enum val=0) STACK_MANAGE_ORIGIN : origin stack 
         2.2) (enum val=1) STACK_MANAGE_TEMP : temp stack  */
  template <typename ItemType> 
  void 
  stack_manage_push(int stack_choice, ItemType * node)
  {
    std::stack<ItemType*>& st = stack_manage_get<ItemType>(stack_choice);
    st.push(node);
  }


  /* Returns true if the chosen stack is empty 
     Input:
     1)stack_choise : the stack that we want to work with it
       stack_choise should be one of :
         1.1) (enum val=0) STACK_MANAGE_ORIGIN : origin stack 
         1.2) (enum val=1) STACK_MANAGE_TEMP : temp stack  */
  template <typename ItemType> 
  bool 
  stack_manage_is_empty(int stack_choice)
  {
    std::stack<ItemType*>& st = stack_manage_get<ItemType>(stack_choice);
    return st.empty();
  }


  /* Returns the chosen stack head
     Input:
     1)stack_choise : the stack that we want to work with it
       stack_choise should be one of :
         1.1) (enum val=0) STACK_MANAGE_ORIGIN : origin stack 
         1.2) (enum val=1) STACK_MANAGE_TEMP : temp stack  */
  template <typename ItemType> 
  ItemType* 
  stack_manage_top(int stack_choice)
  {
    std::stack<ItemType*>& st = stack_manage_get<ItemType>(stack_choice);
    return st.top();
  }


  
  /* This function pop the node from the chosen stack  
     input:
     1)stack_choise : the stack that we want to work with it
       stack_choise should be one of :
         1.1) (enum val=0) STACK_MANAGE_ORIGIN : origin stack 
         1.2) (enum val=1) STACK_MANAGE_TEMP : temp stack  */ 
  template <typename ItemType> 
  void 
  stack_manage_pop(int stack_choice)
  {
    std::stack<ItemType*>& st = stack_manage_get<ItemType>(stack_choice);
    st.pop();

  }


  /* 
  /* pop and return the stack top (stack head)
     input:
     1)stack_choise : the stack that we want to work with it
        stack_choise should be one of :
          1.1) (enum val=0) STACK_MANAGE_ORIGIN : origin stack 
          1.2) (enum val=1) STACK_MANAGE_TEMP : temp stack  */
  template <typename ItemType> 
  ItemType* 
  stack_manage_pop_and_return(int stack_choice)
  {
    std::stack<ItemType*>& st = stack_manage_get<ItemType>(stack_choice);
    ItemType* node = st.top();
    st.pop();
    return node;
  }



}/* name space stm */

#endif /* GCC_REE_STACK_MANAGE */
  