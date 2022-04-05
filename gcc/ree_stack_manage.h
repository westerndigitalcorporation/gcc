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

 

/* This file manage two global generic stacks , origin stack and temp stack  */



#include <stack>


/* Used to chose a stack to work with it  */
typedef enum stack_list{
	STACK_MANAGE_ORIGIN = 0,
	STACK_MANAGE_TEMP = 1
}stack_manage_list;



namespace stm
{
  /* Origin stack  */
  template <typename ItemType> 
  std::stack<ItemType*> stack_manage_origin;
  
  /* Temp stack  */
  template <typename ItemType> 
  std::stack<ItemType*> stack_manage_temp;


  /* Returns the chosen stack by reference  */
  template <typename ItemType> 
  std::stack<ItemType*>& stack_manage_get(int stack_choice);

 
  template <typename ItemType> 
  void stack_manage_push(int stack_choice, ItemType * node);


  
  /* This function pop node from the chosen stack
     Input:
     1)stack_choise : the stack that we want to work with it
        stack_choise should be one of :
          1.1) (enum val=0) STACK_MANAGE_ORIGIN : origin stack 
          1.2) (enum val=1) STACK_MANAGE_TEMP : temp stack  */
  template <typename ItemType> 
  void stack_manage_pop(int stack_choice);


  /* Return true if the chosen stack is empty
     1)stack_choise : the stack that we want to work with it
       stack_choise should be one of :
         1.1) (enum val=0) STACK_MANAGE_ORIGIN : origin stack 
         1.2) (enum val=1) STACK_MANAGE_TEMP : temp stack  */
  bool stack_manage_is_empty(int stack_choice);


  /* Return chosen stack head
     Input:
     1)stack_choise : the stack that we want to work with it
       stack_choise should be one of :
         1.1) (enum val=0) STACK_MANAGE_ORIGIN : origin stack 
         1.2) (enum val=1) STACK_MANAGE_TEMP : temp stack  */
  template <typename ItemType> 
  ItemType* stack_manage_top(int stack_choice);


  /* 
   Pop and return the stack top (stack head)
   Input:
   1)stack_choise : the stack that we want to work with it
       stack_choise should be one of :
         1.1) (enum val=0) STACK_MANAGE_ORIGIN : origin stack 
         1.2) (enum val=1) STACK_MANAGE_TEMP : temp stack  */
  template <typename ItemType> 
  ItemType* 
  stack_manage_pop_and_return(int stack_choice);

 } /* namespace stm  */