/*
 * parse.c - Parse source file.
 */

#include <stdio.h>
#include "util.h"
#include "symbol.h"
#include "absyn.h"
#include "errormsg.h"
#include "parse.h"

extern int yyparse(void);
extern void SEM_transProg(A_exp exp);
extern A_exp absyn_root;

/* parse source file fname; 
   return abstract syntax data structure */
A_exp parse(string fname) 
{EM_reset(fname);
 if (yyparse() == 0) /* parsing worked */
   return absyn_root;
 else{ 
 	exit(123);
 	return NULL;
 }

}

int main(int argc, char **argv) {
 if (argc!=2) {fprintf(stderr,"usage: a.out filename\n"); exit(1);}
 A_exp prog = parse(argv[1]);
 pr_exp(stderr, prog, 0);
 fprintf(stderr, "\n");
 SEM_transProg(prog);
 printf("\n");
 return 0;
}
