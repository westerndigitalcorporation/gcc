/* { dg-do compile } */
/* { dg-options "-c -Os -fdump-rtl-ree -free" } */



extern void foo(unsigned int val1, unsigned int val2, unsigned char index);
extern   int go(unsigned char x);

int main(int argc, char** argv)
{
int z=(int)argv[0];
int y,w,x,e;
e=1;
x=220;
if(z>1)
{
  y=go((unsigned char)x+1);
  x=170;
  e=e+42;
}

x=e;

w = (unsigned char)x;//<<<<<---DELETED
foo(1, 2, w);
x=go((unsigned char)x+w);
}
/* { dg-final { scan-rtl-dump "ree_eval found unnecessary zext" "ree" } } */

