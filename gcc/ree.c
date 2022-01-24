/* Redundant Extension Elimination pass for the GNU compiler.
   Copyright (C) 2010-2021 Free Software Foundation, Inc.
   Contributed by Ilya Enkovich (ilya.enkovich@intel.com)

   Based on the Redundant Zero-extension elimination pass contributed by
   Sriraman Tallam (tmsriram@google.com) and Silvius Rus (rus@google.com).

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
<http://www.gnu.org/licenses/>.  */


/* Problem Description :
   --------------------
   This pass is intended to remove redundant extension instructions.
   Such instructions appear for different reasons.  We expect some of
   them due to implicit zero-extension in 64-bit registers after writing
   to their lower 32-bit half (e.g. for the x86-64 architecture).
   Another possible reason is a type cast which follows a load (for
   instance a register restore) and which can be combined into a single
   instruction, and for which earlier local passes, e.g. the combiner,
   weren't able to optimize.

   How does this pass work  ?
   --------------------------

   This pass is run after register allocation.  Hence, all registers that
   this pass deals with are hard registers.  This pass first looks for an
   extension instruction that could possibly be redundant.  Such extension
   instructions show up in RTL with the pattern  :
   (set (reg:<SWI248> x) (any_extend:<SWI248> (reg:<SWI124> x))),
   where x can be any hard register.
   Now, this pass tries to eliminate this instruction by merging the
   extension with the definitions of register x.  For instance, if
   one of the definitions of register x was  :
   (set (reg:SI x) (plus:SI (reg:SI z1) (reg:SI z2))),
   followed by extension  :
   (set (reg:DI x) (zero_extend:DI (reg:SI x)))
   then the combination converts this into :
   (set (reg:DI x) (zero_extend:DI (plus:SI (reg:SI z1) (reg:SI z2)))).
   If all the merged definitions are recognizable assembly instructions,
   the extension is effectively eliminated.

   For example, for the x86-64 architecture, implicit zero-extensions
   are captured with appropriate patterns in the i386.md file.  Hence,
   these merged definition can be matched to a single assembly instruction.
   The original extension instruction is then deleted if all the
   definitions can be merged.

   However, there are cases where the definition instruction cannot be
   merged with an extension.  Examples are CALL instructions.  In such
   cases, the original extension is not redundant and this pass does
   not delete it.

   Handling conditional moves :
   ----------------------------

   Architectures like x86-64 support conditional moves whose semantics for
   extension differ from the other instructions.  For instance, the
   instruction *cmov ebx, eax*
   zero-extends eax onto rax only when the move from ebx to eax happens.
   Otherwise, eax may not be zero-extended.  Consider conditional moves as
   RTL instructions of the form
   (set (reg:SI x) (if_then_else (cond) (reg:SI y) (reg:SI z))).
   This pass tries to merge an extension with a conditional move by
   actually merging the definitions of y and z with an extension and then
   converting the conditional move into :
   (set (reg:DI x) (if_then_else (cond) (reg:DI y) (reg:DI z))).
   Since registers y and z are extended, register x will also be extended
   after the conditional move.  Note that this step has to be done
   transitively since the definition of a conditional copy can be
   another conditional copy.

   Motivating Example I :
   ---------------------
   For this program :
   **********************************************
   bad_code.c

   int mask[1000];

   int foo(unsigned x)
   {
     if (x < 10)
       x = x * 45;
     else
       x = x * 78;
     return mask[x];
   }
   **********************************************

   $ gcc -O2 bad_code.c
     ........
     400315:       b8 4e 00 00 00          mov    $0x4e,%eax
     40031a:       0f af f8                imul   %eax,%edi
     40031d:       89 ff                   mov    %edi,%edi - useless extension
     40031f:       8b 04 bd 60 19 40 00    mov    0x401960(,%rdi,4),%eax
     400326:       c3                      retq
     ......
     400330:       ba 2d 00 00 00          mov    $0x2d,%edx
     400335:       0f af fa                imul   %edx,%edi
     400338:       89 ff                   mov    %edi,%edi - useless extension
     40033a:       8b 04 bd 60 19 40 00    mov    0x401960(,%rdi,4),%eax
     400341:       c3                      retq

   $ gcc -O2 -free bad_code.c
     ......
     400315:       6b ff 4e                imul   $0x4e,%edi,%edi
     400318:       8b 04 bd 40 19 40 00    mov    0x401940(,%rdi,4),%eax
     40031f:       c3                      retq
     400320:       6b ff 2d                imul   $0x2d,%edi,%edi
     400323:       8b 04 bd 40 19 40 00    mov    0x401940(,%rdi,4),%eax
     40032a:       c3                      retq

   Motivating Example II :
   ---------------------

   Here is an example with a conditional move.

   For this program :
   **********************************************

   unsigned long long foo(unsigned x , unsigned y)
   {
     unsigned z;
     if (x > 100)
       z = x + y;
     else
       z = x - y;
     return (unsigned long long)(z);
   }

   $ gcc -O2 bad_code.c
     ............
     400360:       8d 14 3e                lea    (%rsi,%rdi,1),%edx
     400363:       89 f8                   mov    %edi,%eax
     400365:       29 f0                   sub    %esi,%eax
     400367:       83 ff 65                cmp    $0x65,%edi
     40036a:       0f 43 c2                cmovae %edx,%eax
     40036d:       89 c0                   mov    %eax,%eax - useless extension
     40036f:       c3                      retq

   $ gcc -O2 -free bad_code.c
     .............
     400360:       89 fa                   mov    %edi,%edx
     400362:       8d 04 3e                lea    (%rsi,%rdi,1),%eax
     400365:       29 f2                   sub    %esi,%edx
     400367:       83 ff 65                cmp    $0x65,%edi
     40036a:       89 d6                   mov    %edx,%esi
     40036c:       48 0f 42 c6             cmovb  %rsi,%rax
     400370:       c3                      retq

  Motivating Example III :
  ---------------------

  Here is an example with a type cast.

  For this program :
  **********************************************

  void test(int size, unsigned char *in, unsigned char *out)
  {
    int i;
    unsigned char xr, xg, xy=0;

    for (i = 0; i < size; i++) {
      xr = *in++;
      xg = *in++;
      xy = (unsigned char) ((19595*xr + 38470*xg) >> 16);
      *out++ = xy;
    }
  }

  $ gcc -O2 bad_code.c
    ............
    10:   0f b6 0e                movzbl (%rsi),%ecx
    13:   0f b6 46 01             movzbl 0x1(%rsi),%eax
    17:   48 83 c6 02             add    $0x2,%rsi
    1b:   0f b6 c9                movzbl %cl,%ecx - useless extension
    1e:   0f b6 c0                movzbl %al,%eax - useless extension
    21:   69 c9 8b 4c 00 00       imul   $0x4c8b,%ecx,%ecx
    27:   69 c0 46 96 00 00       imul   $0x9646,%eax,%eax

   $ gcc -O2 -free bad_code.c
     .............
    10:   0f b6 0e                movzbl (%rsi),%ecx
    13:   0f b6 46 01             movzbl 0x1(%rsi),%eax
    17:   48 83 c6 02             add    $0x2,%rsi
    1b:   69 c9 8b 4c 00 00       imul   $0x4c8b,%ecx,%ecx
    21:   69 c0 46 96 00 00       imul   $0x9646,%eax,%eax

   Usefulness :
   ----------

   The original redundant zero-extension elimination pass reported reduction
   of the dynamic instruction count of a compression benchmark by 2.8% and
   improvement of its run time by about 1%.

   The additional performance gain with the enhanced pass is mostly expected
   on in-order architectures where redundancy cannot be compensated by out of
   order execution.  Measurements showed up to 10% performance gain (reduced
   run time) on EEMBC 2.0 benchmarks on Atom processor with geomean performance
   gain 1%.  */

/*
patch main function : insn_is_reachable_and_removable
authour : Ibrahim Qashqoush
 






Problem Description :
  --------------------
This patch removing redundant extensions instructions by calculating the instruction
value if the value is reachable, or the maximum value that the instruction can get if
the instruction value is not reachable, and checking if the (value) or (maximum value)
is less than maximum value that the un-extended REG mode can reach
For example:
(insn 53 52 54 (set (reg: QI 15 a5 [91])
   (const_int 1 [0x1]))  
(nil))
(insn 54 53 55 (set (reg:SI 15 a5 [orig:80 _9 ] [80])
   (zero_extend:SI (reg:QI 15 a5 [91])))
  (nil))


QI Mode maximum value ~ 127 (the unextended REG)
Reg a5[91] value=1
The second insn is removable due to 1<127 

How does this patch work?
--------------------------
Phase1:
We found all IF_THEN_ELSE instructions on the current function and build a map From 
IF_THEN_ELSE Exit basic block index to IF_THEN_ELSE node. where Exit basic block is the
basic block that will be executed after the IF_THEN_ELSE, and the IF_THEN_ELSE node
contains IF/ELSE basic blocks index

If(condition)
{
//code
<--If basic block
}
Else
{
//code
<--else basic block

}
<-- IF_THEN_ELSE Exit basic block


Phase1 Limitations:
We currently don’t support :
1.Nested IF_THEN_ELSE and we cannot calculate all the IF_THEN_ELSE Exit basic block if we have Nested IF_THEN_ELSE 

Phase2:
-------
Calculating the expressions(instruction) value OR max value  
We calculate the value of the expression by recursively calculating the 
expression sub-expressions (Src operands) and simulating the expression code.
If the sub-expression is constant the sub-expression value will be the constant value.
If the sub-expression is Src REG for the current expression find all last dependency 
instructions calculate their value and update the src REG value as the maximum value 
of all the last dependency Instructions.
If at least one subexpression is not supported like call instruction or MEM,
or if we have nested IF_THEN_ELSE, or if We have a negative sub-expression or more than one IF_THEN_ELSE in the function,
calculate the maximum value that the expression value can reach.
Where the maximum value of sub-expression is the maximum value that can fit in the expression(father) Destination REG 
Phase3:
Removing extensions instructions
If the extended expression  value less than the extended expression destination value remove the extensions instructions


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



#define MAX_INT 2147483647

#define D_BTM_INSN 0
#define D_BTM_EXPR 1
#define RTX_INSN_NULL (rtx_insn*)0
#define HAS_NESTED_IF 1
#define HAS_NESTED_ELSE 10
#define HAS_NESTED_IF_ELSE 11

/* The main struct (insns_to_value)
  the struct represent insn/subinsn info 
  type :
    we have two types:.
      D_BTM_INSN : if the node represent insn (rtx_insn)
      D_BTM_EXPR : if the node represent operand(sub insn) (rtx)
  father :
    if the node is insn here father could be :
      * Null if it the first insn (the insn that we want to calculate its value).
      * REG operand if the insn is could be the last insn that modife the REG value.
    if the Node is oprand here father could be
      * insn if it the insn operand
      * operand if it sub sub insns (operand operand).
  current_insn : 
    the insn that the Node represent.
  operands_value:
    hold sons value
  code:
    current insn code i.e REG,PLUS,CONST..
  value:
    current insn value
  operandNum:
    used to calculate the id start from 1, 
    if current node is insn operanNum value will be 0
  id:
    unique id to verify if we visted this insn before.
  valid_value :
    true if the value is valid 
  is_supported:
    true if we support the insn


  */
struct insns_to_value
{
  /* The instruction */
  int type;
  insns_to_value * father;
  rtx_insn *current_insn;
  rtx current_expr;
  std::list<int> operands_value;
  rtx_code code;
  int value;
  int opernadNum;
  int id;
  bool valid_value;
  bool is_supported;
};


struct if_then_else_node
{
  /* IF_THEN_ELSE_NODE that the node is represinting */
  rtx_insn* insn;

  /* condition=true Basic Block index */
  int if_bb_index;

  /* condition=false Basic Block index*/
  int else_bb_index;

  /* Next excuted BasicBlock after executing if/else Basic Blocks index*/
  int exit_bb_index;

  /* the last ELSE basic block in case we have nested IF_then_else on The ELSE index*/
  int else_end_bb_index;

  /* the last IF basic block in case we have nested IF_then_else on The IF index*/
  int if_end_bb_index;

  /* unique id (insn id)*/
  int id;

  /* if there nested IF_THEN_ELSE*/
  bool hasNested;

  /* we done calculating node data*/
  bool isValid;
};

/* we didn't support yet two if then else that write to the same REG 
  and all the basic blocks bettwen the two if then else have no insn that write to REG also we not support nested
  if then else yet so on that case we will return max/min value range   */
bool functionHaveMoreThanOneIfThenElse;

/* IF_THEN_ELSE EXIT BasicBlock to IF and ELSE Basic Blocks */
std::map<int, if_then_else_node*> ExitBBtoIfElseBBMap;

/*  IF_THEN_ELSE Basic Block to IF_THEN_ELSE Node map */
std::map<int, if_then_else_node* > bbToBranch;

/* insn id to insn map */
std::map<int,rtx_insn*> uidToInsnMap;

/* Basic block index to BasicBlock map */
std::map<int,basic_block> bbIndexToBBmap;

/* visited insns list to avoid circular REG dependencies deadlock */
std::list<int> g_list_visited_insns;    

std::list<int>::iterator g_list_visited_insns_it;
std::list<basic_block>::iterator bb_it;

/* Basic block predecessors */
auto_vec<basic_block > bb_preds_vec;

/*insn use-def chain*/
auto_vec<rtx_insn *> insn_defs;

bool hasNegativExpr;

/* if_then_else_node constructor */
static if_then_else_node *
build_if_then_else_node(rtx_insn * ins)
{
 if_then_else_node *node = new  if_then_else_node;
 node->insn=ins;
 node->isValid=false;
 node->id=INSN_UID(ins);
 node-> exit_bb_index = -1;
 node-> else_end_bb_index = -1;
 node-> if_end_bb_index = -1;
 node-> if_bb_index = -1;
 node-> else_bb_index=-1;
 return node;
}


/* return ExitBBtoIfElseBBMap map by reference */
std::map<int,if_then_else_node*>& 
GetTableExitBBtoIfElseBBMap()
{
  static std::map<int,if_then_else_node*> ExitBBtoIfElseBBMap;
  return ExitBBtoIfElseBBMap;
}

/* return bbIndexToBBmap map by reference */
std::map<int,basic_block>& 
GetTableBB()
{
  static std::map<int,basic_block> bbIndexToBBmap;
  return bbIndexToBBmap;
}

/* return uidToInsnMap map by reference */
std::map<int,rtx_insn*>& 
GetTableInsn()
{
  static std::map<int,rtx_insn*> uidToInsnMap;
  return uidToInsnMap;
}

/* return bbToBranch map by reference */
std::map<int,if_then_else_node*>& 
GetTableIfElseNode()
{
  static std::map<int,if_then_else_node*> bbToBranch;
  return bbToBranch;
}

/* input: insn id
   output : insn */
static rtx_insn*
get_insn(int id){
  std::map<int,rtx_insn*>& uidToInsnMap=GetTableInsn();
  std::map<int, rtx_insn* >::iterator theMapIt;
  theMapIt=uidToInsnMap.find(id);
  return theMapIt->second;

}

/* input : basic block index 
   output : basic block */
static basic_block
get_bb(int id){
    std::map<int,basic_block>& bbIndexToBBmap= GetTableBB();
    std::map<int, basic_block >::iterator theMapIt;
    theMapIt=bbIndexToBBmap.find(id);
    if(theMapIt==bbIndexToBBmap.end())
      return NULL;
    return theMapIt->second;
}

/* sorted list of curent and  predecessors basic blocks
  input : Basic block (current)
  output: sorted list containing current Basic block index and her predecessors basic blocks index */
static std::list<int> 
get_bb_preds(basic_block bb ){
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


/* This structure represents a candidate for elimination.  */
struct ext_cand
{
  /* The expression.  */
  const_rtx expr;

  /* The kind of extension.  */
  enum rtx_code code;

  /* The destination mode.  */
  machine_mode mode;

  /* The instruction where it lives.  */
  rtx_insn *insn;
};



static int max_insn_uid;






/* Update or remove REG_EQUAL or REG_EQUIV notes for INSN.  */

static bool
update_reg_equal_equiv_notes (rtx_insn *insn, machine_mode new_mode,
            machine_mode old_mode, enum rtx_code code)
{
  rtx *loc = &REG_NOTES (insn);
  while (*loc)
    {
      enum reg_note kind = REG_NOTE_KIND (*loc);
      if (kind == REG_EQUAL || kind == REG_EQUIV)
  {
    rtx orig_src = XEXP (*loc, 0);
    /* Update equivalency constants.  Recall that RTL constants are
       sign-extended.  */
    if (GET_CODE (orig_src) == CONST_INT
        && HWI_COMPUTABLE_MODE_P (new_mode))
      {
        if (INTVAL (orig_src) >= 0 || code == SIGN_EXTEND)
    /* Nothing needed.  */;
        else
    {
      /* Zero-extend the negative constant by masking out the
         bits outside the source mode.  */
      rtx new_const_int
        = gen_int_mode (INTVAL (orig_src)
            & GET_MODE_MASK (old_mode),
            new_mode);
      if (!validate_change (insn, &XEXP (*loc, 0),
          new_const_int, true))
        return false;
    }
        loc = &XEXP (*loc, 1);
      }
    /* Drop all other notes, they assume a wrong mode.  */
    else if (!validate_change (insn, loc, XEXP (*loc, 1), true))
      return false;
  }
      else
  loc = &XEXP (*loc, 1);
    }
  return true;
}

/* Given a insn (CURR_INSN), an extension candidate for removal (CAND)
   and a pointer to the SET rtx (ORIG_SET) that needs to be modified,
   this code modifies the SET rtx to a new SET rtx that extends the
   right hand expression into a register on the left hand side.  Note
   that multiple assumptions are made about the nature of the set that
   needs to be true for this to work and is called from merge_def_and_ext.

   Original :
   (set (reg a) (expression))

   Transform :
   (set (reg a) (any_extend (expression)))

   Special Cases :
   If the expression is a constant or another extension, then directly
   assign it to the register.  */

static bool
combine_set_extension (ext_cand *cand, rtx_insn *curr_insn, rtx *orig_set)
{

  rtx orig_src = SET_SRC (*orig_set);
  machine_mode orig_mode = GET_MODE (SET_DEST (*orig_set));
  rtx new_set;
  rtx cand_pat = single_set (cand->insn);

  /* If the extension's source/destination registers are not the same
     then we need to change the original load to reference the destination
     of the extension.  Then we need to emit a copy from that destination
     to the original destination of the load.  */
  rtx new_reg;
  bool copy_needed
    = (REGNO (SET_DEST (cand_pat)) != REGNO (XEXP (SET_SRC (cand_pat), 0)));
  if (copy_needed)
    new_reg = gen_rtx_REG (cand->mode, REGNO (SET_DEST (cand_pat)));
  else
    new_reg = gen_rtx_REG (cand->mode, REGNO (SET_DEST (*orig_set)));

  /* Merge constants by directly moving the constant into the register under
     some conditions.  Recall that RTL constants are sign-extended.  */
  if (GET_CODE (orig_src) == CONST_INT
      && HWI_COMPUTABLE_MODE_P (cand->mode))
    {
      if (INTVAL (orig_src) >= 0 || cand->code == SIGN_EXTEND)
  new_set = gen_rtx_SET (new_reg, orig_src);
      else
  {
    /* Zero-extend the negative constant by masking out the bits outside
       the source mode.  */
    rtx new_const_int
      = gen_int_mode (INTVAL (orig_src) & GET_MODE_MASK (orig_mode),
          GET_MODE (new_reg));
    new_set = gen_rtx_SET (new_reg, new_const_int);
  }
    }
  else if (GET_MODE (orig_src) == VOIDmode)
    {
      /* This is mostly due to a call insn that should not be optimized.  */
      return false;
    }
  else if (GET_CODE (orig_src) == cand->code)
    {
      /* Here is a sequence of two extensions.  Try to merge them.  */
      rtx temp_extension
  = gen_rtx_fmt_e (cand->code, cand->mode, XEXP (orig_src, 0));
      rtx simplified_temp_extension = simplify_rtx (temp_extension);
      if (simplified_temp_extension)
        temp_extension = simplified_temp_extension;
      new_set = gen_rtx_SET (new_reg, temp_extension);
    }
  else if (GET_CODE (orig_src) == IF_THEN_ELSE)
    {
      /* Only IF_THEN_ELSE of phi-type copies are combined.  Otherwise,
         in general, IF_THEN_ELSE should not be combined.  */
      return false;
    }
  else
    {
      /* This is the normal case.  */
      rtx temp_extension
  = gen_rtx_fmt_e (cand->code, cand->mode, orig_src);
      rtx simplified_temp_extension = simplify_rtx (temp_extension);
      if (simplified_temp_extension)
        temp_extension = simplified_temp_extension;
      new_set = gen_rtx_SET (new_reg, temp_extension);
    }

  /* This change is a part of a group of changes.  Hence,
     validate_change will not try to commit the change.  */
  if (validate_change (curr_insn, orig_set, new_set, true)
      && update_reg_equal_equiv_notes (curr_insn, cand->mode, orig_mode,
               cand->code))
    {
      if (dump_file)
        {
          fprintf (dump_file,
       "Tentatively merged extension with definition %s:\n",
       (copy_needed) ? "(copy needed)" : "");
          print_rtl_single (dump_file, curr_insn);
        }
      return true;
    }

  return false;
}

/* Treat if_then_else insns, where the operands of both branches
   are registers, as copies.  For instance,
   Original :
   (set (reg:SI a) (if_then_else (cond) (reg:SI b) (reg:SI c)))
   Transformed :
   (set (reg:DI a) (if_then_else (cond) (reg:DI b) (reg:DI c)))
   DEF_INSN is the if_then_else insn.  */

static bool
transform_ifelse (ext_cand *cand, rtx_insn *def_insn)
{
  rtx set_insn = PATTERN (def_insn);
  rtx srcreg, dstreg, srcreg2;
  rtx map_srcreg, map_dstreg, map_srcreg2;
  rtx ifexpr;
  rtx cond;
  rtx new_set;

  gcc_assert (GET_CODE (set_insn) == SET);

  cond = XEXP (SET_SRC (set_insn), 0);
  dstreg = SET_DEST (set_insn);
  srcreg = XEXP (SET_SRC (set_insn), 1);
  srcreg2 = XEXP (SET_SRC (set_insn), 2);
  /* If the conditional move already has the right or wider mode,
     there is nothing to do.  */
  if (GET_MODE_UNIT_SIZE (GET_MODE (dstreg))
      >= GET_MODE_UNIT_SIZE (cand->mode))
    return true;

  map_srcreg = gen_rtx_REG (cand->mode, REGNO (srcreg));
  map_srcreg2 = gen_rtx_REG (cand->mode, REGNO (srcreg2));
  map_dstreg = gen_rtx_REG (cand->mode, REGNO (dstreg));
  ifexpr = gen_rtx_IF_THEN_ELSE (cand->mode, cond, map_srcreg, map_srcreg2);
  new_set = gen_rtx_SET (map_dstreg, ifexpr);

  if (validate_change (def_insn, &PATTERN (def_insn), new_set, true)
      && update_reg_equal_equiv_notes (def_insn, cand->mode, GET_MODE (dstreg),
               cand->code))
    {
      if (dump_file)
        {
          fprintf (dump_file,
       "Mode of conditional move instruction extended:\n");
          print_rtl_single (dump_file, def_insn);
        }
      return true;
    }

  return false;
}

/* Get all the reaching definitions of an instruction.  The definitions are
   desired for REG used in INSN.  Return the definition list or NULL if a
   definition is missing.  If DEST is non-NULL, additionally push the INSN
   of the definitions onto DEST.  */
static struct df_link *
get_defs (rtx_insn *insn, rtx reg, vec<rtx_insn *> *dest)
{
  df_ref use;
  struct df_link *ref_chain, *ref_link;

  FOR_EACH_INSN_USE (use, insn)
    {
      if (GET_CODE (DF_REF_REG (use)) == SUBREG)
        return NULL;
      if (REGNO (DF_REF_REG (use)) == REGNO (reg))
  break;
    }

  gcc_assert (use != NULL);

  ref_chain = DF_REF_CHAIN (use);

  for (ref_link = ref_chain; ref_link; ref_link = ref_link->next)
    {
      /* Problem getting some definition for this instruction.  */
      if (ref_link->ref == NULL)
        return NULL;
      if (DF_REF_INSN_INFO (ref_link->ref) == NULL)
        return NULL;
      /* As global regs are assumed to be defined at each function call
   dataflow can report a call_insn as being a definition of REG.
   But we can't do anything with that in this pass so proceed only
   if the instruction really sets REG in a way that can be deduced
   from the RTL structure.  */
    if (global_regs[REGNO (reg)]
    && !set_of (reg, DF_REF_INSN (ref_link->ref)))
  return NULL;
    }

  if (dest)
    for (ref_link = ref_chain; ref_link; ref_link = ref_link->next)
      dest->safe_push (DF_REF_INSN (ref_link->ref));

  return ref_chain;
}

/* Get all the reaching uses of an instruction.  The uses are desired for REG
   set in INSN.  Return use list or NULL if a use is missing or irregular.  */

static struct df_link *
get_uses (rtx_insn *insn, rtx reg)
{
  df_ref def;
  struct df_link *ref_chain, *ref_link;

  FOR_EACH_INSN_DEF (def, insn)
    if (REGNO (DF_REF_REG (def)) == REGNO (reg))
      break;

  gcc_assert (def != NULL);

  ref_chain = DF_REF_CHAIN (def);

  for (ref_link = ref_chain; ref_link; ref_link = ref_link->next)
    {
      /* Problem getting some use for this instruction.  */
      if (ref_link->ref == NULL)
        return NULL;
      if (DF_REF_CLASS (ref_link->ref) != DF_REF_REGULAR)
  return NULL;
    }

  return ref_chain;
}

/* Return true if INSN is
     (SET (reg REGNO (def_reg)) (if_then_else (cond) (REG x1) (REG x2)))
   and store x1 and x2 in REG_1 and REG_2.  */

static bool
is_cond_copy_insn (rtx_insn *insn, rtx *reg1, rtx *reg2)
{
  rtx expr = single_set (insn);

  if (expr != NULL_RTX
      && GET_CODE (expr) == SET
      && GET_CODE (SET_DEST (expr)) == REG
      && GET_CODE (SET_SRC (expr))  == IF_THEN_ELSE
      && GET_CODE (XEXP (SET_SRC (expr), 1)) == REG
      && GET_CODE (XEXP (SET_SRC (expr), 2)) == REG)
    {
      *reg1 = XEXP (SET_SRC (expr), 1);
      *reg2 = XEXP (SET_SRC (expr), 2);
      return true;
    }

  return false;
}

enum ext_modified_kind
{
  /* The insn hasn't been modified by ree pass yet.  */
  EXT_MODIFIED_NONE,
  /* Changed into zero extension.  */
  EXT_MODIFIED_ZEXT,
  /* Changed into sign extension.  */
  EXT_MODIFIED_SEXT
};

struct ATTRIBUTE_PACKED ext_modified
{
  /* Mode from which ree has zero or sign extended the destination.  */
  ENUM_BITFIELD(machine_mode) mode : 8;

  /* Kind of modification of the insn.  */
  ENUM_BITFIELD(ext_modified_kind) kind : 2;

  unsigned int do_not_reextend : 1;

  /* True if the insn is scheduled to be deleted.  */
  unsigned int deleted : 1;
};

/* Vectors used by combine_reaching_defs and its helpers.  */
class ext_state
{
public:
  /* In order to avoid constant alloc/free, we keep these
     4 vectors live through the entire find_and_remove_re and just
     truncate them each time.  */
  auto_vec<rtx_insn *> defs_list;
  auto_vec<rtx_insn *> copies_list;
  auto_vec<rtx_insn *> modified_list;
  auto_vec<rtx_insn *> work_list;

  /* For instructions that have been successfully modified, this is
     the original mode from which the insn is extending and
     kind of extension.  */
  struct ext_modified *modified;
};

/* Reaching Definitions of the extended register could be conditional copies
   or regular definitions.  This function separates the two types into two
   lists, STATE->DEFS_LIST and STATE->COPIES_LIST.  This is necessary because,
   if a reaching definition is a conditional copy, merging the extension with
   this definition is wrong.  Conditional copies are merged by transitively
   merging their definitions.  The defs_list is populated with all the reaching
   definitions of the extension instruction (EXTEND_INSN) which must be merged
   with an extension.  The copies_list contains all the conditional moves that
   will later be extended into a wider mode conditional move if all the merges
   are successful.  The function returns false upon failure, true upon
   success.  */

static bool
make_defs_and_copies_lists (rtx_insn *extend_insn, const_rtx set_pat,
          ext_state *state)
{
  rtx src_reg = XEXP (SET_SRC (set_pat), 0);//iq: accses Operand 0 
  bool *is_insn_visited;
  bool ret = true;

  state->work_list.truncate (0);

  /* Initialize the work list.  */
  if (!get_defs (extend_insn, src_reg, &state->work_list))
    return false;

  is_insn_visited = XCNEWVEC (bool, max_insn_uid);

  /* Perform transitive closure for conditional copies.  */
  while (!state->work_list.is_empty ())
    {
      rtx_insn *def_insn = state->work_list.pop ();
      rtx reg1, reg2;

      gcc_assert (INSN_UID (def_insn) < max_insn_uid);

      if (is_insn_visited[INSN_UID (def_insn)])
  continue;
      is_insn_visited[INSN_UID (def_insn)] = true;

      if (is_cond_copy_insn (def_insn, &reg1, &reg2))
  {
    /* Push it onto the copy list first.  */
    state->copies_list.safe_push (def_insn);

    /* Now perform the transitive closure.  */
    if (!get_defs (def_insn, reg1, &state->work_list)
        || !get_defs (def_insn, reg2, &state->work_list))
      {
        ret = false;//true and call ebraheem phi func

        break;
      }
        }
      else
  state->defs_list.safe_push (def_insn);
    }

  XDELETEVEC (is_insn_visited);

  return ret;
}

/* If DEF_INSN has single SET expression with a register
   destination, possibly buried inside a PARALLEL, return
   the address of the SET expression, else return NULL.
   This is similar to single_set, except that single_set
   allows multiple SETs when all but one is dead.  */
static rtx *
get_sub_rtx (rtx_insn *def_insn)
{
  enum rtx_code code = GET_CODE (PATTERN (def_insn));
  rtx *sub_rtx = NULL;

  if (code == PARALLEL)
    {
      for (int i = 0; i < XVECLEN (PATTERN (def_insn), 0); i++)
        {
          rtx s_expr = XVECEXP (PATTERN (def_insn), 0, i);
          if (GET_CODE (s_expr) != SET)
            continue;
    if (!REG_P (SET_DEST (s_expr)))
      continue;

          if (sub_rtx == NULL)
            sub_rtx = &XVECEXP (PATTERN (def_insn), 0, i);
          else
            {
              /* PARALLEL with multiple SETs.  */
              return NULL;
            }
        }
    }
  else if (code == SET)
    {
  rtx s_expr = PATTERN (def_insn);
  if (REG_P (SET_DEST (s_expr)))
    sub_rtx = &PATTERN (def_insn);
    }

  return sub_rtx;
}

/* Merge the DEF_INSN with an extension.  Calls combine_set_extension
   on the SET pattern.  */

static bool
merge_def_and_ext (ext_cand *cand, rtx_insn *def_insn, ext_state *state)
{
  machine_mode ext_src_mode;
  rtx *sub_rtx;

  ext_src_mode = GET_MODE (XEXP (SET_SRC (cand->expr), 0));
  sub_rtx = get_sub_rtx (def_insn);

  if (sub_rtx == NULL)
    return false;

  if (GET_MODE (SET_DEST (*sub_rtx)) == ext_src_mode
    || ((state->modified[INSN_UID (def_insn)].kind
         == (cand->code == ZERO_EXTEND
       ? EXT_MODIFIED_ZEXT : EXT_MODIFIED_SEXT))
        && state->modified[INSN_UID (def_insn)].mode
     == ext_src_mode))
    {
      if (GET_MODE_UNIT_SIZE (GET_MODE (SET_DEST (*sub_rtx)))
    >= GET_MODE_UNIT_SIZE (cand->mode))
  return true;
      /* If def_insn is already scheduled to be deleted, don't attempt
   to modify it.  */
      if (state->modified[INSN_UID (def_insn)].deleted)
  return false;
      if (combine_set_extension (cand, def_insn, sub_rtx))
  {
    if (state->modified[INSN_UID (def_insn)].kind == EXT_MODIFIED_NONE)
      state->modified[INSN_UID (def_insn)].mode = ext_src_mode;
    return true;
  }
    }

  return false;
}

static rtx_insn* getBBLastInsanModifiedDestReg(int lastDependBBindex, std::list<rtx_insn*> defInsnsList, rtx_insn *currentInsn, insns_to_value * node);

/* get index of first src operand for RTX_EXTRA class  */
static int
firstOperandSrcForRTX_EXTRA(insns_to_value *node){

  if(node->code == SUBREG)
    return 0;
  return 1;
}

/* get index of first src operand*/
static int
firstOperandSrc(insns_to_value * node){
  if(GET_RTX_CLASS (node->code) == RTX_EXTRA)
    return firstOperandSrcForRTX_EXTRA(node);

  if((GET_RTX_CLASS (node->code) == RTX_COMM_ARITH)||(GET_RTX_CLASS (node->code) == RTX_UNARY)||
    (GET_RTX_CLASS (node->code) == RTX_BIN_ARITH)||(GET_RTX_CLASS (node->code) == RTX_AUTOINC))
    return 0;

  return 1;

}


/* Given SRC, which should be one or more extensions of a REG, strip
   away the extensions and return the REG.  */

static inline rtx
get_extended_src_reg (rtx src)
{
  while (GET_CODE (src) == SIGN_EXTEND || GET_CODE (src) == ZERO_EXTEND)
    src = XEXP (src, 0);
  gcc_assert (REG_P (src));
  return src;
}

/* return operands number for given expression */
static int 
getNumberOfOperands(rtx expr){
  rtx_code code=GET_CODE(expr);
  return GET_RTX_LENGTH(code);
}

/* return true if insn part of loop else return false */
static bool 
isPartOfLoop(rtx_insn *insn)
{
  basic_block bb=BLOCK_FOR_INSN(insn);
  if(bb->loop_father==NULL)
    return false;
  return true;
}

/* casting current insn to rtx */
static rtx 
getExpr(insns_to_value * node){
  if(node->type == D_BTM_INSN)
    return single_set(node->current_insn);
  return node->current_expr;
}


/*
return true if the insn marked as visited
return false if the insn did not marked as visited
*/
static bool
InsansIsVisited(rtx_insn *curr_insn)
{
  int currentInsnId;
  currentInsnId=INSN_UID(curr_insn);

  /* if there is no visited insns yet return false */
  if (g_list_visited_insns.empty())
     return false;

  /* if current insn id dosnt exist in the list return false*/
  g_list_visited_insns_it = std::find(g_list_visited_insns.begin(), g_list_visited_insns.end(), currentInsnId);
  if (g_list_visited_insns_it == g_list_visited_insns.end())
    return false;

  return true;

}
 

/* return unique ID to operand */
static int
calcOpernadId(insns_to_value * node){
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

/* calculate node insn/expr ID*/
static int 
calcId(insns_to_value * node)
{
  int idin;
  if(node->type == D_BTM_INSN){
    idin= INSN_UID(node->current_insn);
    return idin;
  }
  return  calcOpernadId(node);
}

/*
return true if the insn marked as visited
return false if the insn did not marked as visited
*/
static bool
OperandIsVisited(insns_to_value * node)
{
  /* if there is no visited insns yet return false */
  if (g_list_visited_insns.empty())
     return false;

  /* if current insn id dosnt exist in the list return false */
  g_list_visited_insns_it = std::find(g_list_visited_insns.begin(), g_list_visited_insns.end(), node->id);
  if (g_list_visited_insns_it == g_list_visited_insns.end())
    return false;

  return true;
}


/* mark operand expr as visited*/
static void
markOperandAsVisited(insns_to_value * node){

if(!OperandIsVisited(node))
  g_list_visited_insns.push_back(node->id);
}

/* mark insn as visited */
static void
markInsnsAsVisited(rtx_insn *curr_insn)
{
  int currentInsnId=INSN_UID(curr_insn);

  if(!InsansIsVisited(curr_insn))
    g_list_visited_insns.push_back(currentInsnId);
}

/* insn_to_value_node constructor */
static insns_to_value*
build_insn_to_value_node(rtx_insn *curr_insn,rtx curr_expr,int opernadNum ,insns_to_value *father, int type)
{
 insns_to_value *node = new  insns_to_value;//(insns_to_value *) malloc(sizeof(insns_to_value));
rtx expr;
node->type = type;
node->father = father;
node->current_expr = curr_expr;
node->current_insn = curr_insn;
expr = getExpr(node);
node->current_expr = expr;
node->code = GET_CODE(expr);
node->opernadNum = opernadNum;
node->valid_value = false;
node->is_supported = true;
node->value = MAX_INT;
node->id = calcId(node);

return node;
}


/* return the source destination register*/
static rtx 
getDestReg(insns_to_value* node){
  insns_to_value* father = node->father;
  insns_to_value* result;
  bool flag=true;
  
  /*get destination expression*/
  while((father!=NULL)&&(flag)){
    if(getNumberOfOperands(father->current_expr)>1)
      flag=false;
    result=father;
    father = father->father;
  }
  
  /*if destinations is exist return destination register*/
  if(!flag)
    return XEXP(result->current_expr,0);
  return NULL_RTX;

}


static int 
getRegMaxValue(machine_mode mode)
{
  
 switch (mode)
    {
      /* bit */
      case BImode:
        return 1;
      /* if it partial Quarter integer or Quarter integer */
      case QImode:
        return 127;
      /* if it partial half integer or half integer */
      case HImode:
        return 32767;
      /* Single Integer */ 
      case SImode:
        return 2147483647;

      /*Double Intege */
        /* NOTE : we currently supports only 32'bit*/
      case DImode:
        return 9223372036854775807;

    default:
      return 9223372036854775807;
    }
}

/* return max value that could be writen to destination Register*/
static int 
calcMaxValue(insns_to_value* node)
{
  rtx expr;
  
  expr = getDestReg(node);
  if(expr !=NULL_RTX){
    machine_mode mode=GET_MODE(expr);
    return  getRegMaxValue(mode);
   }

  return MAX_INT;
 
}
 
/* return true if the key is exist */
static bool 
keyExistInBBtoBranch(int id){
  std::map<int, if_then_else_node* >& bbToBranch=GetTableIfElseNode();
  std::map<int, if_then_else_node*>::iterator theMapIt;
 
 if(bbToBranch.empty())
  return false;

 theMapIt = bbToBranch.find(id);
  return (theMapIt != bbToBranch.end());
}
 

/* insert new if_then_else_node to BBtoBranch map*/
static void
insertNewNodeToBBtoBranch(int id, if_then_else_node* node){
  std::map<int, if_then_else_node* >& bbToBranch=GetTableIfElseNode();
  std::map<int, if_then_else_node*>::iterator theMapIt;

  if(!keyExistInBBtoBranch(id))
  /* push new node with empty list */
  bbToBranch.insert (std::pair<int, if_then_else_node*>(id,node)); 

}

/* return true if the key exist in the map  (used fo usedef to bb map) */
static bool 
keyExistInMap(std::map<int, std::list<rtx_insn*>> theMap, int id){
  std::map<int,std::list<rtx_insn*>>::iterator theMapIt;
 
 if(theMap.empty())
  return false;

 theMapIt = theMap.find(id);
  return (theMapIt != theMap.end());
}

/*insert new insn to the mapped list  */
static void 
insertToMapList(std::map<int, std::list<rtx_insn*>> &theMap,int id, rtx_insn * insn){

  std::map<int,std::list<rtx_insn*>>::iterator theMapIt;
  
  /* find BasicBlock definsns List*/
  theMapIt=theMap.find(id);
  /* push insn to BB List */
  theMapIt->second.push_back(insn);
}

/* insert new use def insn to basic block list */
static void 
insertNewNodeToMap(std::map<int, std::list<rtx_insn*>> &theMap,int id, rtx_insn * insn){
  std::map<int,std::list<rtx_insn*>>::iterator theMapIt;
  std::list<rtx_insn*> newlist;
  /* push new node with empty list */
  theMap.insert (std::pair<int, std::list<rtx_insn*>>(id,newlist));
  /* go to new node location */
  theMapIt=theMap.find(id);
  /* insert the insn to the new node List*/
  theMapIt->second.push_back(insn);

}

/* creat map key= basic block index value = list of insns in the basic block that write to the REG */
static std::map<int/*BB ID*/,std::list<rtx_insn*> /*def insns in BB with ID =BB_ID*/>
mapUseDefToBBid( rtx_insn* current_insn ){ 

  std::map<int, std::list<rtx_insn*>> theMap;
  basic_block currentBB=BLOCK_FOR_INSN(current_insn),defBB;
  rtx_insn *def_insn;
  int currentBbIndex=currentBB->index, defBbIndex;
  std::map<int,std::list<rtx_insn*>>::iterator theMapIt;
  /* for each reching defination */
  while(!insn_defs.is_empty ()){
    def_insn=insn_defs.pop();
    /* get reaching def basic block index */
    defBB=BLOCK_FOR_INSN(def_insn);
    defBbIndex=defBB->index;

    /* if it's first time we have reaching def from this basic block creat new pair with key=basic block index
       and push the reching def on the pair list and push the pair on the  map.
       else find the pair with key= basic block index and add the reching def to the list */
    if(keyExistInMap(theMap,defBbIndex))
      insertToMapList(theMap, defBbIndex, def_insn);
    else
      insertNewNodeToMap(theMap,defBbIndex,def_insn);
  }
 return theMap;
}

/*
  input : pair first=reg index second= insn
  output : map key = basic block index, value = reg use def on the basic block*/
std::map<int,std::list<rtx_insn*>>
getDefsPerBasicBlock(insns_to_value * node)
{
  std::map<int,std::list<rtx_insn*>> BBsInsns;
  insn_defs.truncate (0);

  /* save all reaching defination that write to REG(node->current_expr) to insn_defs vector
    and check if the vector is  not empty */
  if (get_defs (node->current_insn,node->current_expr /*src_reg*/, &insn_defs)!=NULL)// get all insns dependency as list //def use chain
  {
    /* insert the insns into map 
      (Basic block Index) --> ( Basic Block def use Insns)*/
    BBsInsns=mapUseDefToBBid(node->current_insn);
  }
  
  return BBsInsns;
}



/* get predecessors closest BB index that contains Use-defs insns, return -1 if not found*/
static int 
getClosestBBwithUseDef(std::map<int,std::list<rtx_insn*>> defsToBBmap,std::list<int> predsBBindex){
  
  std::map<int,std::list<rtx_insn*>>::iterator theMapIt;
  int bbId;

/* predsBBindex is sorted list that have predecessors basic block indexs (sorted by distance from current insn basic block) 
   each time we get next predecessor basic block index and check if we have reching defination 
   on that basic block if the basic block contain reching def return here index else continue to next BB
   if no predecessor basic block that contain reching def return -1 */
  while(!predsBBindex.empty()){

    bbId=predsBBindex.front();

    if(keyExistInMap(defsToBBmap,bbId))
    {
      theMapIt=defsToBBmap.find(bbId);
      if(!(theMapIt->second).empty())
        return bbId;

    }

    predsBBindex.pop_front();

  }
return -1; /* not found*/ 
}


/*
  input : list of insns that write to the REG on the Basic block
  output : last insn write to the REG on the Basic Block
*/
static rtx_insn* 
getBBLastInsn(std::list<rtx_insn*> defInsnsList){
  int maxId,curId;
  rtx_insn *cinsn,*result;
  std::list<rtx_insn *>::iterator defListIter;

  /*if list not empty mark first elemet as the soloution  */
  if(defInsnsList.size() > 0){
    cinsn= *(defInsnsList.begin());
    maxId=DF_INSN_LUID(cinsn);
    result=cinsn;
   }
  /*for each element in the list , if the element LUID greter than the solution LUID
    update the solution to be current element */

  for(defListIter=defInsnsList.begin(); defListIter != defInsnsList.end(); ++defListIter)
  {
    cinsn= *defListIter;

    curId=DF_INSN_LUID(cinsn);
    if(curId>maxId){
      maxId=curId;
      result=cinsn;
    }

  }
  return result;
}


/* input: current insn(currentInsn) , node of the current insn(node) ,
 list of all insns (from current  basic blocks) that write to REG(defInsnsList).

  we try to find last insn that write to REG in the current basic block  if all the insns come after the current insn 
  (There Luid greater than the current insn) we chose the closest predecessor basic block(to the current) and return the 
  last insn from the predecessor Basic block*/
static rtx_insn* 
getBBLastInsnBeforCurrentInsn(  std::list<rtx_insn*> defInsnsList, rtx_insn *currentInsn, insns_to_value * node){
  int maxId, curId, mainInsnId, lastDependBBindex;
  rtx_insn *cinsn,*result;
  bool firstTime=true;
  std::list<rtx_insn *>::iterator defListIter;
  std::list<int> predsBBindex;
  std::map<int,std::list<rtx_insn*>> defsToBBmap;
  std::map<int,std::list<rtx_insn*>>::iterator defsToBBmapIter;


  /*get current insn LUID*/
  mainInsnId = DF_INSN_LUID(currentInsn);
  /*for each def in current basic block*/
  for(defListIter=defInsnsList.begin(); defListIter != defInsnsList.end(); ++defListIter)
  {
    cinsn= *defListIter;
    /*if it's first time find first element with LUID less than current insn LUID(mainInsnId)*/
    if(firstTime){
      do /*do while we don't have insn with LUID less than current insn LUID*/
      {
        if((defListIter != defInsnsList.end())){
          maxId=DF_INSN_LUID(cinsn);
          result=cinsn;
         }

          /* if all the insns in the basic block come after the current insn return last insn in the closest pred basic block*/
         if((defListIter == defInsnsList.end())&&(maxId >=  mainInsnId))
          { 
            defsToBBmap=getDefsPerBasicBlock(node);
            /* get sorted list with current/pred BBs*/
            predsBBindex=get_bb_preds(BLOCK_FOR_INSN(currentInsn));
            predsBBindex.pop_front();
            /* get closest BB with index*/
            lastDependBBindex=getClosestBBwithUseDef(defsToBBmap,predsBBindex);
            /*if there is no insn in the predecessors basic blocks that write to REG  */
            if(lastDependBBindex == -1)
              return NULL;
            /* to get closest BB use-def insns list*/
            defsToBBmapIter = defsToBBmap.find(lastDependBBindex);
            /* last reachable insn */
            result=getBBLastInsn(defsToBBmapIter->second); 

            return result;

          }
          else
          {
            if(defListIter ==  defInsnsList.end())
               return result;
          }
         ++defListIter;

         cinsn= *defListIter;

      }while((maxId >= mainInsnId));

      firstTime=false;
    }
    if(defListIter ==  defInsnsList.end())
      return result;
    /*if we found a solution with LUID less than current insn LUID(mainInsnId) 
    and we didn't check all the elements in the list yet, check if there element 
    with LUID greater than the solution LIUD and less than current insn LUID and update the solution */
    curId=DF_INSN_LUID(cinsn);
    /* if cinsn come before current insn and it the last insn till now */
    if((curId>maxId)&&(curId<mainInsnId)){
      maxId=curId;
      result=cinsn;
    }

  }
 
  return result;
}

/* This function return insns that could be the last insns modifies 
*/
static rtx_insn* 
getBBLastInsanModifiedDestReg(int lastDependBBindex, std::list<rtx_insn*> defInsnsList, rtx_insn *currentInsn, insns_to_value * node){
  basic_block currentInsnBB, defBb;
 
  currentInsnBB=BLOCK_FOR_INSN(currentInsn);

    if(lastDependBBindex != currentInsnBB->index)
      return getBBLastInsn(defInsnsList);
    else
      return getBBLastInsnBeforCurrentInsn(defInsnsList, currentInsn, node);

}


/* 
 This function return the last insns that could modifie the REG value
 we push to the stack only src REG or reg use def insns  , here we search the last insn could write to this REG.

 if the current Basic Block is EXIT basic block for IF_THEN_ELSE insn, we push to the list (and the stack later)
 last insn modifie the REG on IF basic block, and last insn Modifie the REG on Else Basic Block also */

static std::list < rtx_insn * > 
getLastDependedInsns(insns_to_value * node)
{
  int numOfOperands, i, firstSrcOp, lastDependBBindex, predBBindex,resultBBindex=-1;
  rtx expr, source;
  rtx_code code;
  basic_block currentBB;
  bool register_depend_on_if_then_else=false;

  /* for single source reg  (if we have more than src this function will be called more than one time)*/
  std::list < rtx_insn* > regInsns;
  std::list < rtx_insn* > emptyList;

  /* map key= Basic block value = insn on Basic Block that modifie the REG value */
  std::map<int,std::list<rtx_insn*>> defsToBBmap;
  std::map<int,std::list<rtx_insn*>>::iterator defsToBBmapIter;

  /*if then else Exit basic block to Basic block map */
  std::map<int, if_then_else_node* >::iterator ExitBBtoIfElseIterator;
  std::map<int, if_then_else_node*>& ExitBBtoIfElseBBMap = GetTableExitBBtoIfElseBBMap();

  /* List hold predecessors basick blocks index */
  std::list<int> predsBBindex;
  rtx_insn *currentInsn, *resultInsn;
  currentInsn = node->current_insn;


   
 
  /* get expr and expr operands count */
  expr=getExpr(node);
  numOfOperands=getNumberOfOperands(expr);
  /* get first src. if there no src reg, it will return 0*/
  firstSrcOp=firstOperandSrc(node->father);
  currentBB=BLOCK_FOR_INSN(node->current_insn);

   /* check if we have src operand , (operandnum start from 1 if we have some insn like subreg firstSrcOp
    will have value 0 and we will not excute the if code) */
  if(node->opernadNum > firstSrcOp)
  {
    defsToBBmap=getDefsPerBasicBlock(node);
    /* get sorted list with current/pred BBs */
    predsBBindex=get_bb_preds(currentBB);
    /* get closest BB with index */
    lastDependBBindex=getClosestBBwithUseDef(defsToBBmap,predsBBindex);
    /* to get closest BB use-def insns list*/
    defsToBBmapIter = defsToBBmap.find(lastDependBBindex);
    /* the wanted insn */
    if(lastDependBBindex != -1){
      /* get last depend insn */
      resultInsn=getBBLastInsanModifiedDestReg(lastDependBBindex, defsToBBmapIter->second, currentInsn, node);
        if(resultInsn != NULL){
          regInsns.push_back(resultInsn);
          resultBBindex = BLOCK_FOR_INSN(resultInsn)->index;

        }
    }
    /* if current insn is exit bassic block for some if then else and no rechig defination on the same basic block */
    ExitBBtoIfElseIterator = ExitBBtoIfElseBBMap.find(currentBB->index);
    if((ExitBBtoIfElseIterator != ExitBBtoIfElseBBMap.end())&&(resultBBindex!=currentBB->index)){

      /* if in the "if" basic block there is insn that write to the REG add it to the list*/
      defsToBBmapIter = defsToBBmap.find(ExitBBtoIfElseIterator->second->if_bb_index);
      if(defsToBBmapIter !=defsToBBmap.end() ){
        resultInsn=getBBLastInsanModifiedDestReg(ExitBBtoIfElseIterator->second->if_bb_index, defsToBBmapIter->second, currentInsn, node);
        if(resultInsn != NULL){
          regInsns.push_back(resultInsn);
          }
      }

      /* if in the "else" basic block there is insn that write to the REG add it to the list*/
      defsToBBmapIter = defsToBBmap.find(ExitBBtoIfElseIterator->second->else_bb_index);
      if(defsToBBmapIter !=defsToBBmap.end() ){
        resultInsn=getBBLastInsanModifiedDestReg(ExitBBtoIfElseIterator->second->else_bb_index, defsToBBmapIter->second, currentInsn, node);
        if(resultInsn != NULL){
          regInsns.push_back(resultInsn);
        }
      }

    }
          

  }

  /* we didn't support yet two if then else that write to the same REG 
  and all the basic blocks bettwen the two if then else have no insn that write to REG 

  also we not support nested if then else yet so on that case we will return max/min value range 
  as result REG operand list will be empty and we will calculate max value 
  (another problem if we have zero extend on the if then else)
  if we have nested if then else we dont now also all the IF_THEN_ELSE ExitBasickBlock 
  because the nested if then else will divid the if/else basic block to more than one basic block
  and to now the exit basic block we should find the jump  on the nested exit basic block
  so for now we return max value if we have more than one if then else
   */
  
  if(functionHaveMoreThanOneIfThenElse)
       return emptyList;  
  
    return regInsns;
   
}

/* return true if we support the insn else return false(if insn not supported we will put max value) */
static bool
isSupported(rtx_code  code){
 if((code == MEM)||(code == CALL_INSN) || (code == CALL) )
    return false;
  return true;
  
}

/*return true if current operand is src operand*/
static bool 
isSrcOperand(insns_to_value * node, int opNum){
  if((opNum<firstOperandSrc(node)))
    return false;
  return true;

}

/* creat new node for insn and push the node to stack*/
static insns_to_value *
pushInsnToStack(rtx_insn * currentInsn,insns_to_value * father,rtx curr_expr,int opNum,int type, std::stack<insns_to_value*> &stack){
  insns_to_value *node;

  node=build_insn_to_value_node(currentInsn,curr_expr,opNum,father,type);
  stack.push(node);
  return node;
}

/* creat node for each src operand and push them to the stack*/
static void 
pushOperandsToStack(insns_to_value * currentNode, std::stack<insns_to_value*> &stack){
   int i, numOfOperands;
   rtx  expr;
   insns_to_value *node;
   bool flag=true;
  expr=getExpr(currentNode);
  numOfOperands=getNumberOfOperands(expr);
  /* check if we support the all operands if at least one operand not supported
     mark current expression as not supported to be calculated as max value*/
  for(i=0; i<numOfOperands; i++){
    /* check if the current operand is src operand */
    if(isSrcOperand(currentNode,i)){
      rtx subExpr=XEXP(expr, i);
      /*check if we support the current operand*/
      if(!isSupported(GET_CODE(subExpr))){
        flag=false;
        currentNode->is_supported=false;
        break;
      }
    }
  }

  /*if all operands supported creat node for each operand and push them to the stack*/
  if(flag){
    for(i=0; i<numOfOperands; i++){ 
      rtx subExpr=XEXP(expr, i);
      if(isSrcOperand(currentNode,i))
        node=pushInsnToStack(currentNode->current_insn, currentNode, subExpr, i+1, D_BTM_EXPR, stack);

    }
  }
   
}


/* update current node value and push the value to father list*/
static void 
updateNodesValue(insns_to_value* node, long long int value ){
  int intValue;
  if(value > MAX_INT)
    intValue = MAX_INT;
  else
    intValue=value;

  node->value=intValue;
  if(node->father != NULL)
    node->father->operands_value.push_back(intValue);
}

/* 
    This function calculate insn vlaue or return the maximum value that could be
    the insn value

*/
static long long int
valuate(insns_to_value* node){
  std::list<int>::iterator opvlistIter;
  long long int value=0;
  long long int max_reg_value,max_operand_value;
  /*if the insn is not const and dosnt have sons
    if we found that we canot valuate the insn/expr we will not push here operands/use def insn
    in that case we will return here max value depent on pred dest reg */
  if(node->operands_value.size()==0  ){
    max_reg_value = calcMaxValue(node);
    updateNodesValue(node, max_reg_value);
    return max_reg_value;
  }
  /* intlize operands value */
  int i;
  long long int x,y; 

  if(node->operands_value.size()>1){
    x = * (node->operands_value.begin());
    y = * (++node->operands_value.begin());
  }
  opvlistIter = node->operands_value.begin();


//operands_value
  switch(node->code){

    case REG:
      value = MAX_INT;
      max_reg_value = calcMaxValue(node);
      if(max_reg_value < value)
        value = max_reg_value;
      if(node->operands_value.size() > 0){
        max_operand_value = * (node->operands_value.begin());
        for(opvlistIter = node->operands_value.begin(); opvlistIter != node->operands_value.end(); ++opvlistIter ){
          if( max_operand_value < (*opvlistIter))
            max_operand_value =  (*opvlistIter);

        }
      }
      if(max_operand_value < value )
        value = max_operand_value;
      
      updateNodesValue(node, value);
      break;

    case ZERO_EXTEND:
    case SET:
    case SUBREG:
    case SIGN_EXTEND:
    case TRUNCATE:
      value = *opvlistIter;
      updateNodesValue(node, value);
      break;

    case PLUS:
      value=0;
      for(opvlistIter = node->operands_value.begin(); opvlistIter != node->operands_value.end(); ++opvlistIter ){
        value=value + *opvlistIter;
      }
      updateNodesValue(node, value);
      break;

    case MINUS:
      value =x-y;
      updateNodesValue(node, value);
      break;

    case MULT:
      value=1;
      for(opvlistIter = node->operands_value.begin(); opvlistIter != node->operands_value.end(); ++opvlistIter ){
        value = value * (*opvlistIter);
       }
      updateNodesValue(node, value);
      break;
       
    case DIV:
      value =x/y;
      updateNodesValue(node, value);
      break;

    case MOD:
      value =x%y;
      updateNodesValue(node, value);
      break;

    case AND :
      value =x&y;
      updateNodesValue(node, value);
      break;

    case IOR:
      value =x|y;
      updateNodesValue(node, value);
      break;

    case XOR:
      value =x ^ y;
      updateNodesValue(node, value);
      break;

    case NOT:
      value = ~(*opvlistIter);
      updateNodesValue(node, value);
      break;


    case NEG:
      value =  - (*opvlistIter);
      updateNodesValue(node, value );
      break;

    case ASHIFT:
      value = x << y;
      updateNodesValue(node, value );
      break;

    case LSHIFTRT:
      value = x >> y;
      updateNodesValue(node, value );
      break;

    case ASHIFTRT:
      value = x >> y;
      if ((x < 0 ) && (y > 0))
        value = ( x >> y | ~(~0U >> y));//x>>y | 0xffff>>y
     updateNodesValue(node, value );
     break;


     default:
      value = calcMaxValue(node);
      updateNodesValue(node, value);
      break;
  }



  return value;

}


 /*
  this function calculate insn value or push to the stack the insns sons 
  son can be:
    *insn operand 
    * insn that could be the last insns modifie the REG if current node is REG
  IF the node is NOT VISITED:
    * we put here sons on the stack and mofie there father to point to the current node 
    * if the node insn is REG we found last insns could modifie the REG and push them to the stack 
      and mofie there father to point to the current node (the REG) 
       * if the insn that that modifie the REG is vistid means we have circular dep 
         getLastDependedInsns function will not add this insn
         then next time will marked as visted and it will still on the top of the stack
  If the node is VISITED :
    if the node is:
      *REG : we calculate the max value could the register have
      * NOT REG : we will calculate the value see calcArithmetic function
*/
static bool 
calcValue(insns_to_value* node,std::stack<insns_to_value*> &stack){
  int value;
  std::list <rtx_insn* > lastInsnM;
  std::list <rtx_insn*>::iterator itlastInsnM;
  std::list<int> readyInsnsId;
  std::list<int>::iterator readyInsnsIdIter;

  rtx_insn* newInsn;

  bool visited;
  bool all_last_insns_is_supported=true;
  if(node->type == D_BTM_INSN)
    visited=InsansIsVisited(node->current_insn);
  else
    visited=OperandIsVisited(node);

  insns_to_value *cNode;
  rtx expr, *pInsn;
  /* if insn is visited before */
  if(visited)
  {
    /*if constant value that is an integer. i.e CONST_INT,CONST_WIDE_INT,CONST_POLY_INT,CONST_FIXED,CONST_DOUBLE,CONST_VECTOR, CONST, HIGH */
    if((GET_RTX_CLASS (node->code) == RTX_CONST_OBJ)){
     
      /* get node insn as rtx */
      expr=getExpr(node);
      /* get the value of first operand as integer (const value as intger) */
      value= XINT(expr, 0);
      /* update Node value */
      node->value = value;
      /* if it's the first insn pushed to the stack (the main insn) mark it as valid */
      if(stack.size()==1)
      {
         node->valid_value=true;
      }
      else
      {
        /* push value to father list */
         node->father->operands_value.push_back(value);

      }

     
    }
    else
    {
     /* if insn is NOT constant  */
     value = valuate(node);

    }


    return true;
  }
  else /* insn dose not marked as visited */
    {
       
        /* if the insn is REG push all last depended insn to the stack*/
        if(node->code == REG){
          lastInsnM = getLastDependedInsns(node);
          for(itlastInsnM=lastInsnM.begin(); itlastInsnM != lastInsnM.end(); ++itlastInsnM){
            newInsn = *itlastInsnM;
            expr=single_set(newInsn);
            if(!isSupported(GET_CODE(expr))){
              all_last_insns_is_supported=false;
              node->is_supported=false;
              break;
            }

          }

          /*if we have dependency with unsupported insn like CALL_INSN we will not push any dependency and as result 
            REG will have MAX value*/
          if(all_last_insns_is_supported){
          
            readyInsnsId.clear();
            /* for each last depended insn */
            for(itlastInsnM=lastInsnM.begin(); itlastInsnM != lastInsnM.end(); ++itlastInsnM){
              newInsn = *itlastInsnM;
                if(newInsn!=NULL_RTX){
                  expr=single_set(newInsn);
               
                  readyInsnsIdIter = std::find(readyInsnsId.begin(), readyInsnsId.end(), INSN_UID(newInsn));
                  /* if insn not in the ready list (to avoid push the same insn as son more than once*/
                  if (readyInsnsIdIter == readyInsnsId.end()){
                    /* add to ready list*/
                    readyInsnsId.push_back(INSN_UID(newInsn));
                    /* creat Node for the new insn and push it to stack*/
                    cNode = pushInsnToStack(newInsn, node, expr, 0, D_BTM_INSN, stack);
                    /* next iteration operands will puted to the stack*/

                  }
                }
          
            }
          }

        }
        else
        {
          pushOperandsToStack(node, stack);
        }
  
      
      return false;
    }

}

/* this function return 
 0 if there is no nesten IF_THEN_ELSE
 1 if there only in IF BASic Block nested IF_THEN_ELSE
 10 if there only in else BASic Block nested IF_THEN_ELSE
 11   in both if/else BASic Block nested IF_THEN_ELSE
*/
static int
has_nested_if_then_else(rtx_insn* insn){
  int result =0;

  bool getnextinsn;
  basic_block else_bb, if_bb;
  rtx_insn* insnTemp; 
  rtx_insn * insnnext;

  /* get IF_THEN_ELSE label_ref */
  rtx label_ref=XEXP(XEXP(single_set(insn),1),1); 
  /*label ref point to code_label get the code_label insn*/
  rtx code_label=XEXP(label_ref,0); 
  /* get next insn*/
  rtx insn_after_label_2= XEXP(code_label,1);


  /* get first insn on ELSE BasicBlock i.e (condition == false) BasicBlock */
  if(insn_after_label_2 != NULL_RTX){
    while((GET_CODE(insn_after_label_2)!=INSN) && (GET_CODE(insn_after_label_2)!=CALL_INSN) && (GET_CODE(insn_after_label_2)!=JUMP_INSN)) {
      /* get next insn (XEXP(this,1) return next insn [rtl.h])*/
      insn_after_label_2=XEXP(insn_after_label_2,1); 
        if(insn_after_label_2 == NULL_RTX)
          break;
    }
  }

  /*if there IF_THEN_ELSE insn in ELSE Basick block return put in result 10 */
  if(insn_after_label_2 != NULL_RTX){
    else_bb = BLOCK_FOR_INSN(insn_after_label_2);
     FOR_BB_INSNS (else_bb, insnTemp)
      {
        if(GET_CODE(insnTemp) == JUMP_INSN)
          if(GET_CODE(XEXP(single_set(insnTemp),1)) == IF_THEN_ELSE ){
            result=10;
            break;
          }
      }
  }

  /* get first insns on the IF BasicBlock i.e (condition ==true) BasicBlock */
  insnnext=NEXT_INSN(insn);
  if(insnnext != NULL){
    while( (GET_CODE(insnnext)!=INSN) && (GET_CODE(insnnext)!=CALL_INSN) && (GET_CODE(insnnext)!=JUMP_INSN) ){
      insnnext=NEXT_INSN(insnnext);
      if(insnnext == NULL)
         break; 
    }
  } 

  /*if there IF_THEN_ELSE insn in IF Basick block add 1 to result and break */
  if(insnnext != NULL){
    if_bb =BLOCK_FOR_INSN(insnnext);
      FOR_BB_INSNS (if_bb, insnnext)
        {
          if(GET_CODE(insnnext) == JUMP_INSN)
            if(GET_CODE(XEXP(single_set(insnnext),1)) == IF_THEN_ELSE ){
              result=result+1;
              break;
            }
        }
  }


return result;
}





/* 
 input : jump if_then_else insn without nested jump

 return std::pair<dest BB ,list<if/else basic blocks>>. 
*/

static std::list<int>
getBranchDestBlocksIndexs(rtx_insn* insn){
  std::list<int> DestBlockIndexs;

  
  rtx_insn * insnnext;
  /*get if then else label_ref*/
  rtx label_ref=XEXP(XEXP(single_set(insn),1),1); 
  /* get code label */
  rtx code_label=XEXP(label_ref,0); 

  /* find first insn after code label (else basic block) */
  rtx insn_after_label_2= XEXP(code_label,1);
  if(insn_after_label_2 != NULL_RTX){
    while((GET_CODE(insn_after_label_2)!=INSN) && (GET_CODE(insn_after_label_2)!=CALL_INSN) && (GET_CODE(insn_after_label_2)!=JUMP_INSN)){
      insn_after_label_2=XEXP(insn_after_label_2,1);//insn_after_label_1 = NEXT INSN 
        if(insn_after_label_2 == NULL_RTX)
          break;
    }
  }
  /* push else basic block index to list*/
  if(insn_after_label_2 != NULL_RTX)
    DestBlockIndexs.push_back(BLOCK_FOR_INSN(insn_after_label_2)->index);

  /*find first insn in if_basic block*/
  insnnext=NEXT_INSN(insn);
  if(insnnext != NULL){
    while( (GET_CODE(insnnext)!=INSN) && (GET_CODE(insnnext)!=CALL_INSN) && (GET_CODE(insnnext)!=JUMP_INSN) ){
      insnnext=NEXT_INSN(insnnext);
      if(insnnext == NULL)
        break; 
    }
  } 

  /* push if_basic block index to list*/
  if(insnnext != NULL)
    DestBlockIndexs.push_back(BLOCK_FOR_INSN(insnnext)->index);
  
  /* return the list */
  return DestBlockIndexs;
}

/*this function build Basic Block to IF_THEN_ELSE map
  input: list of if_then_else insns*/
static void 
build_BB_to_branch_map(std::list <rtx_insn* >& ifThenElseInsnsList){
  int bbIndex;
  std::list <rtx_insn* >::iterator IfElseListIter;
  
  /* get global bbToBranch by reference */
  std::map<int, if_then_else_node* >& bbToBranch=GetTableIfElseNode();
   
   /* for each if_then_else insn */
  for(IfElseListIter=ifThenElseInsnsList.begin(); IfElseListIter != ifThenElseInsnsList.end(); ++IfElseListIter){
    /* get if then else basic block index */
    bbIndex=BLOCK_FOR_INSN(*IfElseListIter)->index;
    /* creat new if_then_else node*/
    if_then_else_node* ifelsenode=build_if_then_else_node( *IfElseListIter);
    /* add the node to map key= basic block index value= if_then_else node*/
    insertNewNodeToBBtoBranch(bbIndex, ifelsenode);
  }

}
/*
this function should get as paramter sample jump like:

(jump_insn 75 24 76 (set (pc)
        (label_ref 36)) 239 {jump}
     (nil)

if given jump not sample jump we return -1 
else it return  index of jump destination basic block
*/
static int
getJumpDestBBindex(rtx_insn* jmpinsn){
  std::map<int,rtx_insn*>& uidToInsnMap=GetTableInsn();
  std::map<int,rtx_insn*>::iterator insnsMapIter;
  rtx_insn* insnnext;

  /* check if we have sample jump i.e not IF_THEN_ELSE */
  rtx label_ref = XEXP(single_set(jmpinsn),1); 
  if(GET_CODE(label_ref) != LABEL_REF)
     return -1;

  /* get code label as rtx_insn */
  rtx code_label = XEXP(label_ref,0);
  insnsMapIter = uidToInsnMap.find(INSN_UID(code_label));
  insnnext = insnsMapIter->second;

  /*find first insn after code label
   actually sample jump destination can't be to JUMP_INSN or CALL_INSN because compiler can optimize it and replace
    the jump insn */
  while((GET_CODE(insnnext)!=INSN) && (GET_CODE(insnnext)!=CALL_INSN) && (GET_CODE(insnnext)!=JUMP_INSN)){
    insnnext=NEXT_INSN(insnnext);
    if(insnnext == NULL)
       break; 
  }
   /*if not return jump*/
    if(insnnext != NULL)
      return BLOCK_FOR_INSN(insnnext)->index;
    return -1;
}


/* return jump insn on basic block with index=bbIndex and null if there is no jump insn */
static rtx_insn*
find_jump_insn(int bbIndex){
  rtx_insn* bbinsn;
  basic_block bb;
  bb= get_bb(bbIndex);

  if(bb!=NULL){
    FOR_BB_INSNS (bb, bbinsn)
    {
      if(GET_CODE(bbinsn) == JUMP_INSN)
      {
       return  bbinsn;
      }
    }
  }
  return NULL;
}

/* input if/else basic block index , and if_then_else insn
   this function update if_then_else node*/
static void
updateNodeIfElseBolckIndex(std::list<int>& DestBlockIndexs, rtx_insn* insn){

  std::map<int, if_then_else_node* >& bbToBranch=GetTableIfElseNode();
  int bbIndex;
  std::list<int>::iterator destBBiter;
  std::map<int,if_then_else_node*>::iterator theMapIt;
  if_then_else_node * ifElseNode;

  /*find if_then_else node in the map*/
  destBBiter = DestBlockIndexs.begin();
  bbIndex=BLOCK_FOR_INSN(insn)->index;
  theMapIt=bbToBranch.find(bbIndex); 
  
  /*update the node*/
  ifElseNode=theMapIt->second;
  ifElseNode->else_bb_index = *(DestBlockIndexs.begin());
  ifElseNode->if_bb_index = *(++DestBlockIndexs.begin());
}




/* this function build IF_THEN_ELSE_EXIT Basic block to  IF/ELSE basic blocks map to be used in calcvalue function
 */
static void
save_if_then_else_data(){
  
  std::map<int, if_then_else_node* >& bbToBranch=GetTableIfElseNode();
  std::map<int, if_then_else_node*>& ExitBBtoIfElseBBMap = GetTableExitBBtoIfElseBBMap();
  /*this list contains IF_THEN_ELSE insns*/
  std::list <rtx_insn* > ifThenElseInsnsList;
  std::list <rtx_insn* >::iterator IfElseListIter;

  std::list<int> DestBlockIndexs;
  std::list<int> doneIfElseList;

  std::list<int>::iterator doneIter;
  std::map<int, if_then_else_node* >::iterator nodeIter;
 
  int if_bb_index, else_bb_index, jump_dest;
  if_then_else_node *currentNode, *newNode;
  basic_block bb;
  rtx_insn *insn, *jump_insn;
  rtx expr;
  rtx_code code;

  /* initialize IF_THEN_ELSE insn  list*/
  FOR_EACH_BB_FN (bb, cfun)
  {
    FOR_BB_INSNS (bb, insn)
    {
      if(GET_CODE(insn) == JUMP_INSN)
          if(GET_CODE(XEXP(single_set(insn),1)) == IF_THEN_ELSE )
            ifThenElseInsnsList.push_back(insn);
    }
  }

  /* if we have more than one IF_THEN_ELSE insn*/
  if(ifThenElseInsnsList.size() > 1)
    functionHaveMoreThanOneIfThenElse = true;
  else
    functionHaveMoreThanOneIfThenElse = false;

  /* create node for each IF_THEN_ELSE insn ,
     and initialize bbToBranch map (key = Basic Block index)--> (value = IF_THEN_ELSE node) */
  build_BB_to_branch_map(ifThenElseInsnsList);

  /* find if/else Basic blocks and update the nodes */
  for(IfElseListIter=ifThenElseInsnsList.begin(); IfElseListIter != ifThenElseInsnsList.end(); ++IfElseListIter){
    if(GET_CODE(*IfElseListIter) == JUMP_INSN ){
      /* get if/else blocks index */
      DestBlockIndexs = getBranchDestBlocksIndexs(*IfElseListIter);
      /* found if_then_else node on bbToBranch and update if/else basic block index (if_bb_index/else_bb_index) */
      updateNodeIfElseBolckIndex(DestBlockIndexs, *IfElseListIter);
    }
  }


/* find EXIT basick block for each IF_THEN_ELSE NODE
   if the IF_THEN_ELSE have nested IF_THEN_ELSE find first the nested EXIT Basick block
   then found the curren node EXIT basic block by searching  jump insn on the nested EXIT Basic block
   if there is no jump insns in both  IF BB and ELSE bb  EXIT BB will be else BB or the  nested If then EXIT*/
  for(IfElseListIter=ifThenElseInsnsList.begin(); IfElseListIter != ifThenElseInsnsList.end(); ++IfElseListIter){
  
    /* get current if_then_else node */ 
    nodeIter=bbToBranch.find(BLOCK_FOR_INSN(*IfElseListIter)->index);
    currentNode=nodeIter->second;
  
    /* if current node is not valid */
    if(!currentNode->isValid){

      /*
      nestedIfElseFlag =0 mean no nesten IF_THEN_ELSE
      nestedIfElseFlag = 1 mean we have only in IF BASic Block nested IF_THEN_ELSE
      nestedIfElseFlag =10  mean we have only in else BASic Block nested IF_THEN_ELSE
      nestedIfElseFlag =11  mean we have  in both if/else BASic Block nested IF_THEN_ELSE
      */
      int nestedIfElseFlag = has_nested_if_then_else(currentNode->insn);

      /* if no nested if_then_else */
      if(nestedIfElseFlag==0){
    
        doneIter= std::find(doneIfElseList.begin(), doneIfElseList.end(), currentNode->id);
        /* if this node dosent done yet */
        if (doneIter == doneIfElseList.end()){

          /* find else and if Basick blocks indxes and push them to the node */
          if (currentNode->if_bb_index != -1){
            jump_insn = find_jump_insn(currentNode->if_bb_index);
            if(jump_insn != NULL){
              jump_dest = getJumpDestBBindex(jump_insn);
              currentNode->exit_bb_index=jump_dest;
              currentNode->isValid=true;
            }
         
          }

          if ((currentNode->else_bb_index != -1 )&&(currentNode->exit_bb_index == -1)){
              jump_insn = find_jump_insn(currentNode->else_bb_index);
              if(jump_insn != NULL){
                jump_dest = getJumpDestBBindex(jump_insn);
                currentNode->exit_bb_index=jump_dest;
                currentNode->isValid=true;
              }
         
          }

          /* if the insn IF and not IF_THEN_ELSE */
          if(currentNode->exit_bb_index == -1)
            currentNode->exit_bb_index=currentNode->else_bb_index;
          /* Mark as valid and done */
          currentNode->isValid=true;
          doneIfElseList.push_back(currentNode->id);

          /* add to ExitBBtoIfElseBBMap MAP*/
          ExitBBtoIfElseBBMap.insert (std::pair<int, if_then_else_node*>(currentNode->exit_bb_index,currentNode)); 

       
      

        }
      }



    }
  }
}

/* This function initializing bbIndexToBBmap uidToInsnMap */
static void
initialize_uidToInsnAndIndexToBB_Maps()
{
std::map<int,basic_block>& bbIndexToBBmap= GetTableBB();
std::map<int,rtx_insn*>& uidToInsnMap=GetTableInsn();
basic_block bb;
std::map<int,basic_block>::iterator kh_iter;
rtx_insn *insn;
int id;
  /* for each basic block (bb) except Entry/Exit BasicBlocks */
  FOR_EACH_BB_FN (bb, cfun)
  {
    basic_block newbb=bb;
    /*insert to map (Basic block index)-->(Basic Block) */
    bbIndexToBBmap.insert (std::pair<int, basic_block >(newbb->index,newbb));
    /* for each insn in bb*/
    FOR_BB_INSNS (bb, insn)
    {
      id=INSN_UID(insn);
      /*insert to map (insn UID)-->(insn) */
      uidToInsnMap.insert (std::pair<int, rtx_insn* >(id,insn));
    }
  }
}

static bool
insn_is_removable(insns_to_value* node){
  rtx dest;
  machine_mode mode;
  int modeMaxValue;

  if(hasNegativExpr)
    return false;
  if(node->code == SET){
    if(GET_CODE(SET_SRC(node->current_expr ))== ZERO_EXTEND){
      dest=XEXP(SET_SRC(node->current_expr ),0);
      mode = GET_MODE(dest);
      modeMaxValue=getRegMaxValue(mode);
      /* we currently support code that only use positive values without minus (small-large) */
      if((modeMaxValue >= node->value )&&(node->value >= 0))
        return true;
    }
    
  }
  return false;

}
static void free_global_data(){
  std::map<int, if_then_else_node* >& bbToBranch=GetTableIfElseNode();
  std::map<int, if_then_else_node*>& ExitBBtoIfElseBBMap = GetTableExitBBtoIfElseBBMap();
  std::map<int, if_then_else_node* >::iterator nodeIter;

  /*free bbToBranch*/
  for(nodeIter = bbToBranch.begin(); nodeIter!=bbToBranch.end();++nodeIter)
  {
    nodeIter->second=NULL;
    free(nodeIter->second);
  }
  bbToBranch.clear();

  /*free ExitBBtoIfElseBBMap*/
  for(nodeIter = ExitBBtoIfElseBBMap.begin(); nodeIter!=ExitBBtoIfElseBBMap.end();++nodeIter)
    if(nodeIter->second != NULL){
      nodeIter->second=NULL;
      free(nodeIter->second);
    }
  ExitBBtoIfElseBBMap.clear();

  /*clear g_list_visited_insns*/
  g_list_visited_insns.clear();
  /* clear uidToInsnMap*/
  uidToInsnMap.clear();
  /* clear bbIndexToBBmap */
  bbIndexToBBmap.clear();
  /* clear insn_defs */
  insn_defs.truncate (0);



}
/* 
 input : insn
 output: calculate insn value or max value that the insn could have and return true if the insn is removable*/
static bool 
insn_is_reachable_and_removable(rtx_insn *curr_insn){
  bool is_removable;
  std::map<int, if_then_else_node* >& bbToBranch=GetTableIfElseNode();
  std::map<int,rtx_insn*>& uidToInsnMap=GetTableInsn();
  std::map<int,basic_block>& bbIndexToBBmap= GetTableBB();
  std::list<int>::iterator opvlistIter;


  std::stack<insns_to_value*> stack;
  insns_to_value *currentNode;
  /* Reset Global Maps */
  hasNegativExpr=false;
  bbToBranch=std::map<int, if_then_else_node* >();
  uidToInsnMap= std::map<int,rtx_insn*>();
  bbIndexToBBmap=std::map<int,basic_block>();
  bbToBranch.clear();
  uidToInsnMap.clear();
  bbIndexToBBmap.clear();

/* building data structure for if-then-else jump_insns */
  initialize_uidToInsnAndIndexToBB_Maps();
  save_if_then_else_data();

  /*each node visted two times
   first time we put to the stack here sons (sub insns)
   we we visit it second time means here sons have valid value the we can calc here value
   when stack is empty mean we calculate all here sons and we calculte here value*/
  stack.push(build_insn_to_value_node(curr_insn, NULL, 0, NULL, D_BTM_INSN));
  while(!stack.empty())
  {
    currentNode=stack.top();
    if(!calcValue(currentNode,stack))
    {
      if(currentNode->type == D_BTM_INSN)
       markInsnsAsVisited(currentNode->current_insn);  //
      else
        markOperandAsVisited(currentNode);
    }
    else
    {
      /*we dont have value range (lower/uper bound) yet so if there non constant expression that have or could
       have negative value we will not remove the zero extend becouse we cannot valuate the range yet*/
      if((currentNode->operands_value.size()>1)&&(currentNode->value <0))
        hasNegativExpr=true;
      /*if current insn is reg that could have more than one value (i.e use-def)*/
      if((currentNode->code == REG)&&(currentNode->operands_value.size()>1))
      {
        for(opvlistIter = currentNode->operands_value.begin(); opvlistIter != currentNode->operands_value.end(); ++opvlistIter ){
          if( (*opvlistIter) < 0){
            hasNegativExpr=true;
            break;
          }
        }

      }
      
      /*free node and pop it*/
      currentNode->operands_value.clear();
      free(currentNode);
      stack.pop();
    }

  }

  is_removable=insn_is_removable(currentNode);
  /*free global data*/
  free_global_data();
 

  /*first insns visited twice means it have valid value.
   (the only way to pop node from stack to visit the node twice)*/
  return is_removable;
}



/* This function goes through all reaching defs of the source
   of the candidate for elimination (CAND) and tries to combine
   the extension with the definition instruction.  The changes
   are made as a group so that even if one definition cannot be
   merged, all reaching definitions end up not being merged.
   When a conditional copy is encountered, merging is attempted
   transitively on its definitions.  It returns true upon success
   and false upon failure.  */

static bool
combine_reaching_defs (ext_cand *cand, const_rtx set_pat, ext_state *state)
{
  rtx_insn *def_insn;
  bool merge_successful = true;
  int i;
  int defs_ix;
  bool outcome;

  state->defs_list.truncate (0);
  state->copies_list.truncate (0);
 
  outcome = make_defs_and_copies_lists (cand->insn, set_pat, state);

  if (!outcome)
    return false;
  if(insn_is_reachable_and_removable(cand->insn))
    return true;

  /* If the destination operand of the extension is a different
     register than the source operand, then additional restrictions
     are needed.  Note we have to handle cases where we have nested
     extensions in the source operand.

     Candidate insns are known to be single_sets, via the test in
     find_removable_extensions.  So we continue to use single_set here
     rather than get_sub_rtx.  */
  rtx set = single_set (cand->insn);
  bool copy_needed
    = (REGNO (SET_DEST (set)) != REGNO (get_extended_src_reg (SET_SRC (set))));
  if (copy_needed)
    {
      /* Considering transformation of
   (set (reg1) (expression))
   ...
   (set (reg2) (any_extend (reg1)))

   into

   (set (reg2) (any_extend (expression)))
   (set (reg1) (reg2))
   ...  */

      /* In theory we could handle more than one reaching def, it
   just makes the code to update the insn stream more complex.  */
      if (state->defs_list.length () != 1)
  return false;

      /* We don't have the structure described above if there are
   conditional moves in between the def and the candidate,
   and we will not handle them correctly.  See PR68194.  */
      if (state->copies_list.length () > 0)
  return false;

      /* We require the candidate not already be modified.  It may,
   for example have been changed from a (sign_extend (reg))
   into (zero_extend (sign_extend (reg))).

   Handling that case shouldn't be terribly difficult, but the code
   here and the code to emit copies would need auditing.  Until
   we see a need, this is the safe thing to do.  */
      if (state->modified[INSN_UID (cand->insn)].kind != EXT_MODIFIED_NONE)
  return false;

      machine_mode dst_mode = GET_MODE (SET_DEST (set));
      rtx src_reg = get_extended_src_reg (SET_SRC (set));

      /* Ensure we can use the src_reg in dst_mode (needed for
   the (set (reg1) (reg2)) insn mentioned above).  */
      if (!targetm.hard_regno_mode_ok (REGNO (src_reg), dst_mode))
  return false;

      /* Ensure the number of hard registers of the copy match.  */
      if (hard_regno_nregs (REGNO (src_reg), dst_mode) != REG_NREGS (src_reg))
  return false;

      /* There's only one reaching def.  */
      rtx_insn *def_insn = state->defs_list[0];

      /* The defining statement must not have been modified either.  */
      if (state->modified[INSN_UID (def_insn)].kind != EXT_MODIFIED_NONE)
  return false;


//int destval=find_data_source(cand->insn);

      /* The defining statement and candidate insn must be in the same block.
   This is merely to keep the test for safety and updating the insn
   stream simple.  Also ensure that within the block the candidate
   follows the defining insn.  */
      basic_block bb = BLOCK_FOR_INSN (cand->insn);
      if (bb != BLOCK_FOR_INSN (def_insn)
    || DF_INSN_LUID (def_insn) > DF_INSN_LUID (cand->insn))
  return false;

      /* If there is an overlap between the destination of DEF_INSN and
   CAND->insn, then this transformation is not safe.  Note we have
   to test in the widened mode.  */
      rtx *dest_sub_rtx = get_sub_rtx (def_insn);
      if (dest_sub_rtx == NULL)
  return false;

      rtx tmp_reg = gen_rtx_REG (GET_MODE (SET_DEST (set)),
         REGNO (SET_DEST (*dest_sub_rtx)));
      if (reg_overlap_mentioned_p (tmp_reg, SET_DEST (set)))
  return false;

      /* On RISC machines we must make sure that changing the mode of SRC_REG
   as destination register will not affect its reaching uses, which may
   read its value in a larger mode because DEF_INSN implicitly sets it
   in word mode.  */
      poly_int64 prec
  = GET_MODE_PRECISION (GET_MODE (SET_DEST (*dest_sub_rtx)));
      if (WORD_REGISTER_OPERATIONS && known_lt (prec, BITS_PER_WORD))
  {
    struct df_link *uses = get_uses (def_insn, src_reg);
    if (!uses)
      return false;

    for (df_link *use = uses; use; use = use->next)
      if (paradoxical_subreg_p (GET_MODE (*DF_REF_LOC (use->ref)),
              GET_MODE (SET_DEST (*dest_sub_rtx))))
        return false;
  }

      /* The destination register of the extension insn must not be
   used or set between the def_insn and cand->insn exclusive.  */
      if (reg_used_between_p (SET_DEST (set), def_insn, cand->insn)
    || reg_set_between_p (SET_DEST (set), def_insn, cand->insn))
  return false;

      /* We must be able to copy between the two registers.   Generate,
   recognize and verify constraints of the copy.  Also fail if this
   generated more than one insn.

         This generates garbage since we throw away the insn when we're
   done, only to recreate it later if this test was successful. 

   Make sure to get the mode from the extension (cand->insn).  This
   is different than in the code to emit the copy as we have not
   modified the defining insn yet.  */
      start_sequence ();
      rtx new_dst = gen_rtx_REG (GET_MODE (SET_DEST (set)),
                                 REGNO (get_extended_src_reg (SET_SRC (set))));
      rtx new_src = gen_rtx_REG (GET_MODE (SET_DEST (set)),
                                 REGNO (SET_DEST (set)));
      emit_move_insn (new_dst, new_src);

      rtx_insn *insn = get_insns ();
      end_sequence ();
      if (NEXT_INSN (insn))
  return false;
      if (recog_memoized (insn) == -1)
  return false;
      extract_insn (insn);
      if (!constrain_operands (1, get_preferred_alternatives (insn, bb)))
  return false;

      while (REG_P (SET_SRC (*dest_sub_rtx))
       && (REGNO (SET_SRC (*dest_sub_rtx)) == REGNO (SET_DEST (set))))
  {
    /* Considering transformation of
       (set (reg2) (expression))
       ...
       (set (reg1) (reg2))
       ...
       (set (reg2) (any_extend (reg1)))

       into

       (set (reg2) (any_extend (expression)))
       (set (reg1) (reg2))
       ...  */
    struct df_link *defs
      = get_defs (def_insn, SET_SRC (*dest_sub_rtx), NULL);
    if (defs == NULL || defs->next)
      break;

    /* There is only one reaching def.  */
    rtx_insn *def_insn2 = DF_REF_INSN (defs->ref);

    /* The defining statement must not have been modified either.  */
    if (state->modified[INSN_UID (def_insn2)].kind != EXT_MODIFIED_NONE)
      break;

    /* The def_insn2 and candidate insn must be in the same
       block and def_insn follows def_insn2.  */
    if (bb != BLOCK_FOR_INSN (def_insn2)
        || DF_INSN_LUID (def_insn2) > DF_INSN_LUID (def_insn))
      break;

    rtx *dest_sub_rtx2 = get_sub_rtx (def_insn2);
    if (dest_sub_rtx2 == NULL)
      break;

    /* On RISC machines we must make sure that changing the mode of
       SRC_REG as destination register will not affect its reaching
       uses, which may read its value in a larger mode because DEF_INSN
       implicitly sets it in word mode.  */
    if (WORD_REGISTER_OPERATIONS && known_lt (prec, BITS_PER_WORD))
      {
        struct df_link *uses = get_uses (def_insn2, SET_DEST (set));
        if (!uses)
    break;

        df_link *use;
        rtx dest2 = SET_DEST (*dest_sub_rtx2);
        for (use = uses; use; use = use->next)
    if (paradoxical_subreg_p (GET_MODE (*DF_REF_LOC (use->ref)),
            GET_MODE (dest2)))
      break;
        if (use)
    break;
      }

    /* The destination register of the extension insn must not be
       used or set between the def_insn2 and def_insn exclusive.
       Likewise for the other reg, i.e. check both reg1 and reg2
       in the above comment.  */
    if (reg_used_between_p (SET_DEST (set), def_insn2, def_insn)
        || reg_set_between_p (SET_DEST (set), def_insn2, def_insn)
        || reg_used_between_p (src_reg, def_insn2, def_insn)
        || reg_set_between_p (src_reg, def_insn2, def_insn))
      break;

    state->defs_list[0] = def_insn2;
    break;
  }
    }

  /* If cand->insn has been already modified, update cand->mode to a wider
     mode if possible, or punt.  */
  if (state->modified[INSN_UID (cand->insn)].kind != EXT_MODIFIED_NONE)
    {
      machine_mode mode;

      if (state->modified[INSN_UID (cand->insn)].kind
    != (cand->code == ZERO_EXTEND
        ? EXT_MODIFIED_ZEXT : EXT_MODIFIED_SEXT)
    || state->modified[INSN_UID (cand->insn)].mode != cand->mode
    || (set == NULL_RTX))
  return false;
      mode = GET_MODE (SET_DEST (set));
      gcc_assert (GET_MODE_UNIT_SIZE (mode)
      >= GET_MODE_UNIT_SIZE (cand->mode));
      cand->mode = mode;
    }

  merge_successful = true;

  /* Go through the defs vector and try to merge all the definitions
     in this vector.  */
  state->modified_list.truncate (0);
  FOR_EACH_VEC_ELT (state->defs_list, defs_ix, def_insn)
    {
      if (merge_def_and_ext (cand, def_insn, state))
  state->modified_list.safe_push (def_insn);
      else
        {
          merge_successful = false;
          break;
        }
    }

  /* Now go through the conditional copies vector and try to merge all
     the copies in this vector.  */
  if (merge_successful)
    {
      FOR_EACH_VEC_ELT (state->copies_list, i, def_insn)
        {
          if (transform_ifelse (cand, def_insn))
      state->modified_list.safe_push (def_insn);
          else
            {
              merge_successful = false;
              break;
            }
        }
    }

  if (merge_successful)
    {
      /* Commit the changes here if possible
   FIXME: It's an all-or-nothing scenario.  Even if only one definition
   cannot be merged, we entirely give up.  In the future, we should allow
   extensions to be partially eliminated along those paths where the
   definitions could be merged.  */
      if (apply_change_group ())
        {
          if (dump_file)
            fprintf (dump_file, "All merges were successful.\n");

    FOR_EACH_VEC_ELT (state->modified_list, i, def_insn)
      {
        ext_modified *modified = &state->modified[INSN_UID (def_insn)];
        if (modified->kind == EXT_MODIFIED_NONE)
    modified->kind = (cand->code == ZERO_EXTEND ? EXT_MODIFIED_ZEXT
                        : EXT_MODIFIED_SEXT);

        if (copy_needed)
    modified->do_not_reextend = 1;
      }
          return true;
        }
      else
        {
          /* Changes need not be cancelled explicitly as apply_change_group
             does it.  Print list of definitions in the dump_file for debug
             purposes.  This extension cannot be deleted.  */
          if (dump_file)
            {
        fprintf (dump_file,
           "Merge cancelled, non-mergeable definitions:\n");
        FOR_EACH_VEC_ELT (state->modified_list, i, def_insn)
          print_rtl_single (dump_file, def_insn);
            }
        }
    }
  else
    {
      /* Cancel any changes that have been made so far.  */
      cancel_changes (0);
    }

  return false;
}

/* Add an extension pattern that could be eliminated.  */

static void
add_removable_extension (const_rtx expr, rtx_insn *insn,
       vec<ext_cand> *insn_list,
       unsigned *def_map,
       bitmap init_regs)
{
  enum rtx_code code;
  machine_mode mode;
  unsigned int idx;
  rtx src, dest;

  /* We are looking for SET (REG N) (ANY_EXTEND (REG N)).  */
  if (GET_CODE (expr) != SET)
    return;

  src = SET_SRC (expr);
  code = GET_CODE (src);
  dest = SET_DEST (expr);
  mode = GET_MODE (dest);

  if (REG_P (dest)
      && (code == SIGN_EXTEND || code == ZERO_EXTEND)
      && REG_P (XEXP (src, 0)))
    {
      rtx reg = XEXP (src, 0);
      struct df_link *defs, *def;
      ext_cand *cand;

      /* Zero-extension of an undefined value is partly defined (it's
   completely undefined for sign-extension, though).  So if there exists
   a path from the entry to this zero-extension that leaves this register
   uninitialized, removing the extension could change the behavior of
   correct programs.  So first, check it is not the case.  */
      if (code == ZERO_EXTEND && !bitmap_bit_p (init_regs, REGNO (reg)))
  {
    if (dump_file)
      {
        fprintf (dump_file, "Cannot eliminate extension:\n");
        print_rtl_single (dump_file, insn);
        fprintf (dump_file, " because it can operate on uninitialized"
                " data\n");
      }
    return;
  }

      /* Second, make sure we can get all the reaching definitions.  */
      defs = get_defs (insn, reg, NULL);
      if (!defs)
  {
    if (dump_file)
      {
        fprintf (dump_file, "Cannot eliminate extension:\n");
        print_rtl_single (dump_file, insn);
        fprintf (dump_file, " because of missing definition(s)\n");
      }
    return;
  }

      /* Third, make sure the reaching definitions don't feed another and
   different extension.  FIXME: this obviously can be improved.  */
      for (def = defs; def; def = def->next)
  if ((idx = def_map[INSN_UID (DF_REF_INSN (def->ref))])
      && idx != -1U
      && (cand = &(*insn_list)[idx - 1])
      && cand->code != code)
    {
      if (dump_file)
        {
          fprintf (dump_file, "Cannot eliminate extension:\n");
    print_rtl_single (dump_file, insn);
          fprintf (dump_file, " because of other extension\n");
        }
      return;
    }
  /* For vector mode extensions, ensure that all uses of the
     XEXP (src, 0) register are in insn or debug insns, as unlike
     integral extensions lowpart subreg of the sign/zero extended
     register are not equal to the original register, so we have
     to change all uses or none and the current code isn't able
     to change them all at once in one transaction.  */
  else if (VECTOR_MODE_P (GET_MODE (XEXP (src, 0))))
    {
      if (idx == 0)
        {
    struct df_link *ref_chain, *ref_link;

    ref_chain = DF_REF_CHAIN (def->ref);
    for (ref_link = ref_chain; ref_link; ref_link = ref_link->next)
      {
        if (ref_link->ref == NULL
      || DF_REF_INSN_INFO (ref_link->ref) == NULL)
          {
      idx = -1U;
      break;
          }
        rtx_insn *use_insn = DF_REF_INSN (ref_link->ref);
        if (use_insn != insn && !DEBUG_INSN_P (use_insn))
          {
      idx = -1U;
      break;
          }
      }
    if (idx == -1U)
      def_map[INSN_UID (DF_REF_INSN (def->ref))] = idx;
        }
      if (idx == -1U)
        {
    if (dump_file)
      {
        fprintf (dump_file, "Cannot eliminate extension:\n");
        print_rtl_single (dump_file, insn);
        fprintf (dump_file,
           " because some vector uses aren't extension\n");
      }
    return;
        }
    }

      /* Fourth, if the extended version occupies more registers than the
   original and the source of the extension is the same hard register
   as the destination of the extension, then we cannot eliminate
   the extension without deep analysis, so just punt.

   We allow this when the registers are different because the
   code in combine_reaching_defs will handle that case correctly.  */
      if (hard_regno_nregs (REGNO (dest), mode) != REG_NREGS (reg)
    && reg_overlap_mentioned_p (dest, reg))
  return;

      /* Then add the candidate to the list and insert the reaching definitions
         into the definition map.  */
      ext_cand e = {expr, code, mode, insn};
      insn_list->safe_push (e);
      idx = insn_list->length ();

      for (def = defs; def; def = def->next)
  def_map[INSN_UID (DF_REF_INSN (def->ref))] = idx;
    }
}

/* Traverse the instruction stream looking for extensions and return the
   list of candidates.  */

static vec<ext_cand>
find_removable_extensions (void)
{
  vec<ext_cand> insn_list = vNULL;
  basic_block bb;
  rtx_insn *insn;
  rtx set;
  unsigned *def_map = XCNEWVEC (unsigned, max_insn_uid);
  bitmap_head init, kill, gen, tmp;

  bitmap_initialize (&init, NULL);
  bitmap_initialize (&kill, NULL);
  bitmap_initialize (&gen, NULL);
  bitmap_initialize (&tmp, NULL);

  FOR_EACH_BB_FN (bb, cfun)
    {
      bitmap_copy (&init, DF_MIR_IN (bb));
      bitmap_clear (&kill);
      bitmap_clear (&gen);

      FOR_BB_INSNS (bb, insn)//<<<<<<----- 
  {
    if (NONDEBUG_INSN_P (insn))
      {
        set = single_set (insn);
        if (set != NULL_RTX)
    add_removable_extension (set, insn, &insn_list, def_map,
           &init);
        df_mir_simulate_one_insn (bb, insn, &kill, &gen);
        bitmap_ior_and_compl (&tmp, &gen, &init, &kill);
        bitmap_copy (&init, &tmp);
      }
  }
    }

  XDELETEVEC (def_map);

  return insn_list;
}

/* This is the main function that checks the insn stream for redundant
   extensions and tries to remove them if possible.  */

static void
find_and_remove_re (void)
{
  ext_cand *curr_cand;
  rtx_insn *curr_insn = NULL;
  int num_re_opportunities = 0, num_realized = 0, i;
  vec<ext_cand> reinsn_list;
  auto_vec<rtx_insn *> reinsn_del_list;
  auto_vec<rtx_insn *> reinsn_copy_list;

  /* Construct DU chain to get all reaching definitions of each
     extension instruction.  */
  df_set_flags (DF_RD_PRUNE_DEAD_DEFS);
  df_chain_add_problem (DF_UD_CHAIN + DF_DU_CHAIN);
  df_mir_add_problem ();
  df_analyze ();
  df_set_flags (DF_DEFER_INSN_RESCAN);

  max_insn_uid = get_max_uid ();
  reinsn_list = find_removable_extensions ();

  ext_state state;
  if (reinsn_list.is_empty ())
    state.modified = NULL;
  else
    state.modified = XCNEWVEC (struct ext_modified, max_insn_uid);

  FOR_EACH_VEC_ELT (reinsn_list, i, curr_cand)
    {
      num_re_opportunities++;

      /* Try to combine the extension with the definition.  */
      if (dump_file)
        {
          fprintf (dump_file, "Trying to eliminate extension:\n");
          print_rtl_single (dump_file, curr_cand->insn);
        }

      if (combine_reaching_defs (curr_cand, curr_cand->expr, &state))
        {
          if (dump_file)
            fprintf (dump_file, "Eliminated the extension.\n");
          num_realized++;
    /* If the RHS of the current candidate is not (extend (reg)), then
       we do not allow the optimization of extensions where
       the source and destination registers do not match.  Thus
       checking REG_P here is correct.  */
    rtx set = single_set (curr_cand->insn);
    if (REG_P (XEXP (SET_SRC (set), 0))
        && (REGNO (SET_DEST (set)) != REGNO (XEXP (SET_SRC (set), 0))))
      {
              reinsn_copy_list.safe_push (curr_cand->insn);
              reinsn_copy_list.safe_push (state.defs_list[0]);
      }
    reinsn_del_list.safe_push (curr_cand->insn);
    state.modified[INSN_UID (curr_cand->insn)].deleted = 1;
        }
    }

  /* The copy list contains pairs of insns which describe copies we
     need to insert into the INSN stream.

     The first insn in each pair is the extension insn, from which
     we derive the source and destination of the copy.

     The second insn in each pair is the memory reference where the
     extension will ultimately happen.  We emit the new copy
     immediately after this insn.

     It may first appear that the arguments for the copy are reversed.
     Remember that the memory reference will be changed to refer to the
     destination of the extention.  So we're actually emitting a copy
     from the new destination to the old destination.  */
  for (unsigned int i = 0; i < reinsn_copy_list.length (); i += 2)
    {
      rtx_insn *curr_insn = reinsn_copy_list[i];
      rtx_insn *def_insn = reinsn_copy_list[i + 1];

      /* Use the mode of the destination of the defining insn
   for the mode of the copy.  This is necessary if the
   defining insn was used to eliminate a second extension
   that was wider than the first.  */
      rtx sub_rtx = *get_sub_rtx (def_insn);
      rtx set = single_set (curr_insn);
      rtx new_dst = gen_rtx_REG (GET_MODE (SET_DEST (sub_rtx)),
         REGNO (XEXP (SET_SRC (set), 0)));
      rtx new_src = gen_rtx_REG (GET_MODE (SET_DEST (sub_rtx)),
         REGNO (SET_DEST (set)));
      rtx new_set = gen_rtx_SET (new_dst, new_src);
      emit_insn_after (new_set, def_insn);
    }

  /* Delete all useless extensions here in one sweep.  */
  FOR_EACH_VEC_ELT (reinsn_del_list, i, curr_insn)
    delete_insn (curr_insn);

  reinsn_list.release ();
  XDELETEVEC (state.modified);

  if (dump_file && num_re_opportunities > 0)
    fprintf (dump_file, "Elimination opportunities = %d realized = %d\n",
       num_re_opportunities, num_realized);
}

/* Find and remove redundant extensions.  */

static unsigned int
rest_of_handle_ree (void)
{
  find_and_remove_re ();
  return 0;
}

namespace {

const pass_data pass_data_ree =
{
  RTL_PASS, /* type */
  "ree", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_REE, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_df_finish, /* todo_flags_finish */
};

class pass_ree : public rtl_opt_pass
{
public:
  pass_ree (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_ree, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *) { return (optimize > 0 && flag_ree); }
  virtual unsigned int execute (function *) { return rest_of_handle_ree (); }

}; // class pass_ree

} // anon namespace

rtl_opt_pass *
make_pass_ree (gcc::context *ctxt)
{
  return new pass_ree (ctxt);
}




 