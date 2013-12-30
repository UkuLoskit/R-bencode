// #ifdef BENDEBUG

#include <Rinternals.h>
#include <stdio.h>

#ifdef BENDEBUG
#define PRN(X, args...) printf(X, args)
#else
#define PRN(X, args...) ;
#endif

#define DIGIT(X) ((X) >= '0' && (X) <= '9')


SEXP reversePairList( SEXP head ){
  if( !isPairList( head ) ) error("Wrong argument type to reversePairList: %s", type2str(TYPEOF(head)));
  SEXP tail = CDR(head), tmp;
  SETCDR(head, R_NilValue);
  while( tail != R_NilValue ){
    tmp = CDR(tail);
    SETCDR(tail, head);
    head = tail;
    tail = tmp;
  }
  return head;
}

