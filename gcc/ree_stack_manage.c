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
 


  /* Origin stack  */
  static std::stack<insns_to_value*> stack_manage_origin;
  
  /* Temp stack  */
  static std::stack<insns_to_value*> stack_manage_temp;


  /* This function returns chosen stack by reference 
     input:
       1)stack_choise : the stack that we want to work with it
         stack_choise should be one of :
           1.1) (enum val=0) STACK_MANAGE_ORIGIN : origin stack 
           1.2) (enum val=1) STACK_MANAGE_TEMP : temp stack  */
  static std::stack<insns_to_value*>& 
  stack_manage_get(stack_manage_enum stack_choice)
  {
    if(stack_choice == STACK_MANAGE_TEMP)
    {
      static std::stack<insns_to_value*> stack_manage_temp;
      return stack_manage_temp;
    }
    else
    {
      /* stack_choice == STACK_MANAGE_ORIGIN  */
      static std::stack<insns_to_value*> stack_manage_origin;
      return stack_manage_origin;
    }
  }

  
  /* This function push the node to the chosen stack
     Input:
       1) node
       2)stack_choise : the stack that we want to work with it
         stack_choise should be one of :
         2.1) (enum val=0) STACK_MANAGE_ORIGIN : origin stack 
         2.2) (enum val=1) STACK_MANAGE_TEMP : temp stack  */
  void 
  stack_manage_push(stack_manage_enum stack_choice, insns_to_value * node)
  {
    std::stack<insns_to_value*>& st = stack_manage_get(stack_choice);
    st.push(node);
  }


  /* Returns true if the chosen stack is empty 
     Input:
     1)stack_choise : the stack that we want to work with it
       stack_choise should be one of :
         1.1) (enum val=0) STACK_MANAGE_ORIGIN : origin stack 
         1.2) (enum val=1) STACK_MANAGE_TEMP : temp stack  */
  bool 
  stack_manage_is_empty(stack_manage_enum stack_choice)
  {
    std::stack<insns_to_value*>& st = stack_manage_get(stack_choice);
    return st.empty();
  }


  /* Returns the chosen stack head
     Input:
     1)stack_choise : the stack that we want to work with it
       stack_choise should be one of :
         1.1) (enum val=0) STACK_MANAGE_ORIGIN : origin stack 
         1.2) (enum val=1) STACK_MANAGE_TEMP : temp stack  */
  insns_to_value* 
  stack_manage_top(stack_manage_enum stack_choice)
  {
    std::stack<insns_to_value*>& st = stack_manage_get(stack_choice);
    return st.top();
  }

  
  /* This function pop the node from the chosen stack  
     input:
     1)stack_choise : the stack that we want to work with it
       stack_choise should be one of :
         1.1) (enum val=0) STACK_MANAGE_ORIGIN : origin stack 
         1.2) (enum val=1) STACK_MANAGE_TEMP : temp stack  */ 
  void 
  stack_manage_pop(stack_manage_enum stack_choice)
  {
    std::stack<insns_to_value*>& st = stack_manage_get(stack_choice);
    st.pop();

  }


  /* pop and return the stack top (stack head)
     input:
     1)stack_choise : the stack that we want to work with it
        stack_choise should be one of :
          1.1) (enum val=0) STACK_MANAGE_ORIGIN : origin stack 
          1.2) (enum val=1) STACK_MANAGE_TEMP : temp stack  */
  insns_to_value* 
  stack_manage_pop_and_return_top(stack_manage_enum stack_choice)
  {
    std::stack<insns_to_value*>& st = stack_manage_get(stack_choice);
    insns_to_value* node = st.top();
    st.pop();
    return node;
  }


  /* Return the stack size  */
  int 
  stack_manage_size(stack_manage_enum stack_choice)
  {
    std::stack<insns_to_value*>& st = stack_manage_get(stack_choice);
    return st.size();

  }

  