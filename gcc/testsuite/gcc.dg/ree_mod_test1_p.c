/* { dg-do compile } */
/* { dg-options "-c -Os -fdump-rtl-ree -free" } */



extern void foo(unsigned int val1, unsigned int val2, unsigned char index);
extern   char go(unsigned char x);
extern   int bar(int x);


int main(int argc, char** argv)
{
int z=(int)argv[0];
int y,w,x;

x=bar(z);
if(z>1)
{
  y=go((unsigned char)x+1);
    x=bar(454);;


}
else
{
  y=go((unsigned char)x);
 
 }
 x=x%7;

 w = (unsigned char)x;
foo(1, 2, w);
x=go((unsigned char)x+w);
}

/* { dg-final { scan-rtl-dump "ree_eval found unnecessary zext" "ree" } }  */