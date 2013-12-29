/*
 *  bencode decoder and encoder
 *  Author: Vitalie Spinu
 *  Copyright (C) 2013--2014  Vitalie Spinu
 *
 *  This program is free software; you can redistribute it and/or modify it
 *  under the terms of the GNU General Public License as published by the Free
 *  Software Foundation; either version 2 of the License, or (at your option)
 *  any later version.
 *
 *  This program is distributed in the hope that it will be useful, but WITHOUT
 *  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 *  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
 *  more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */
// C ref: http://www.tutorialspoint.com/ansi_c/c_function_references.htm
// http://www.tutorialspoint.com/cprogramming/c_strings.htm
// bencode: https://wiki.theory.org/BitTorrentSpecification
// simple dynamic arrays: http://stackoverflow.com/questions/3536153/c-dynamically-growing-array

#include <Rinternals.h>
#include <stdio.h>
#define DIGIT(X) ((X) >= '0' && (X) <= '9')

//#define BENDEBUG 

#ifdef BENDEBUG
#define PRN(X, args...) printf(X, args)
#else
#define PRN(X, args...) ;
#endif

static char *err_wrong_enc = "Wrong encoding '%c'. Allowed values are i (int),l (list),d (dict) or a digit (string).";

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

int parse_i( const char **c ){
  if(  **c != 'i' ) error("integer parsing should start with 'i'. Encountered '%c'.", **c);
  (*c)++; 

  int ans=0, sig = 1;
  if( **c == '-') {sig = -1; (*c)++; }
  else if( **c == '+' ) (*c)++;
  while( **c && DIGIT(**c) ) { ans = ans * 10 + (**c - '0'); (*c)++; }

  if( **c != 'e' ) error("Invalid terminator of integer value '%c'", **c);
  (*c)++;
  PRN("out of i: %c\n", **c);
  return ans;
}

SEXP parse_s( const char **c ){
  // return Schalar String
  if(! DIGIT(**c)) error("string parsing should start with an integer descriptor. Encountered '%c'", **c);

  long long len = 0;
  while( **c && DIGIT(**c) ) { len = len * 10 + (**c - '0'); (*c)++; }
  
  if( **c != ':' ) error("invalid separation '%c' after string length descriptor (%d). Must be ':'.", **c, len);
  if (len > INT_MAX)
    error("R character strings are limited to 2^31-1 bytes");

  char newc[len + 1];
  R_len_t i;
  for ( i = 0; i != len; i++ ){
    if( *((*c)++) ) newc[i] = **c;
    else error("input string terminated unexpectedly");
  }
  newc[len] = 0;

  (*c)++;
  PRN("out of s: %c\n", **c);
  return ScalarString(mkChar(newc));
}

SEXP parse_l( const char **c ){
  // return VECTORSXP
  if(  **c != 'l' ) error("list parsing should start with 'l'. Encountered '%c'.", **c);
  (*c)++;
  
  SEXP ans = R_NilValue;
  int same = 1, prev = **c, len = 0;
  if( DIGIT(prev) ) prev = 's';
  
  while( **c && **c != 'e' ){
    len ++;

    switch( **c ){
    case 'i':
      ans = CONS(ScalarInteger(parse_i(c)), ans);
      same = same && prev == 'i';
      prev = 'i';
      break;
    case 'd':
      error("Dictionaries are not implemented yet");
      break;
    case 'l':
      ans = CONS(parse_l(c), ans);
      same = same && prev == 'l';
      prev = 'l';
      break;
    default:
      if( DIGIT(**c) ){
        PRN("prev: %c\n", prev);
        ans = CONS(parse_s(c), ans);
        same = same && prev == 's';
        prev = 's';
      } else {
        error(err_wrong_enc, **c);
      }
    }
  }

  
  PRN("same: %d\n", same);
  if ( **c ) (*c)++;
  PRN("out of l: %c\n", **c);

  ans = reversePairList(ans);
  
  if( same ){
    // convert to appropriate vector
    int type, i = 0;
    SEXP newans;
    switch( prev ){
    case 'i':
      PROTECT(newans = allocVector(INTSXP, len));
      for( i = 0; ans != R_NilValue; i++, ans = CDR(ans)){
        INTEGER(newans)[i] = INTEGER(CAR(ans))[0];
      }
      UNPROTECT(1);
      return(newans);
    case 's':
      PROTECT(newans = allocVector(STRSXP, len));
      for( i = 0; ans != R_NilValue; i++, ans = CDR(ans)){
        SET_STRING_ELT( newans, i, STRING_ELT(CAR(ans), 0) );
      }
      UNPROTECT(1);
      return(newans);
    default:
      return(PairToVectorList(ans));
    }
  }
  return PairToVectorList(ans);
}

SEXP decode1( SEXP str ) {

    if( ! isString(str) ) error("bcencode: input must be a character string");
    const char *c = CHAR(STRING_ELT(str, 0));

    SEXP ans = R_NilValue;

    while( *c ){
      /* PRN("*c %c\n", *c); */
      switch( *c ){
      case 'i':
        ans = CONS(ScalarInteger(parse_i(&c)), ans);
        break;
      case 'd':
        error("dictionaries are not implemented yet");
        break;
      case 'l':
        ans = CONS(parse_l(&c), ans);
        break;
      default:
        if( DIGIT(*c) ){
          ans = CONS(parse_s(&c), ans);
        } else {
          error("Wrong encoding '%c'. Allowed values are i,l,d or a digit.", *c);
        }
      }
     }
    if( CDR(ans) == R_NilValue ) return(CAR(ans));
    else return ans;
}

// from http://stackoverflow.com/a/7458516/453735

// Rf_lengthgets in Rinternals.h; implemented in builtin.c:lengthgets. The
// returned pointer needs to be PROTECTed, so one pattern is

/* SEXP myList; */
/* PROTECT_INDEX ipx; */
/* PROTECT_WITH_INDEX(myList = allocVector( VECSXP, 1 ), &ipx); */
/* REPROTECT(mylist = Rf_lengthgets(mylist, 100), ipx); */

/* // If one were growing a list based on some unknown stopping condition, the */
/* // approach might be like in R, with pre-allocate and fill followed by */
/* // extension; the following is psuedo-code: */


/* const int BUF_SIZE = 100; */
/* PROTECT_INDEX ipx; */
/* SEXP myList; */
/* int i, someCondition = 1; */

/* PROTECT_WITH_INDEX(myList=allocVector(VECSXP, BUF_SIZE), &ipx); */
/* for (i = 0; some_condition; ++i) { */
/*     if (Rf_length(myList) == i) { */
/*         const int len = Rf_length(myList) + BUF_SIZE; */
/*         REPROTECT(myList = Rf_lengthgets(mYlist, BUF_SIZE), &ipx); */
/*     } */
/*     PROTECT(result = some_calculation(); */
/*             SET_VECTOR_ELT(myList, i, result); */
/*             UNPROTECT(1); */
/*             // set some_condition */
/*             } */
/*         lengthgets(myList, i); // no need to re-PROTECT; we're leaving C */
/*     UNPROTECT(1) */
/*         return myList; */

/*  } */


// Local Variables:
// tags-table-list: ("/home/vitoshka/bin/R-devel/TAGS")
// End:
