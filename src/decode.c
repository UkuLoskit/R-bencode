#include "bencode.h"

SEXP parse_switch( const char **c );
/* static char *err_wrong_enc = "Wrong encoding '%c'. Allowed values are i (int),l (list),d (dict) or a digit (string)."; */

SEXP parse_i( const char **c ){
  PRN("in i: %c\n", **c);

  if(  **c != 'i' ) error("integer parsing should start with 'i'. Encountered '%c'.", **c);
  (*c)++; 

  int ans=0, sig = 1;
  if( **c == '-') {sig = -1; (*c)++; }
  else if( **c == '+' ) (*c)++;
  while( **c && DIGIT(**c) ) { ans = ans * 10 + (**c - '0'); (*c)++; }

  if( **c != 'e' ) error("Invalid terminator of integer value '%c'", **c);
  (*c)++;
  PRN("out of i: %c\n", **c);
  return ScalarInteger(ans);
}

// return Schalar String
SEXP parse_s( const char **c ){

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
  return mkString(newc);
}

// return VECTORSXP
SEXP parse_d( const char **c ){
  
  if(  **c != 'd' ) error("Dict should start with 'd'. Encountered '%c'.", **c);
  (*c)++;
  
  SEXP K = R_NilValue, D = R_NilValue, tt;
  int len = 0;
  
  while( **c != 'e' ){
    if( !**c ) error("Dict decoding terminated prematurely");
    if( ! DIGIT(**c) ) error("Dict keys must be strings");

    len++;
    K = CONS(parse_s(c), K);
    D = CONS(parse_switch(c), D);
  }
  (*c)++;

  SEXP ans;

  if( len == 0 ){

    ans = allocVector(VECSXP, 0);
    setAttrib(ans, R_ClassSymbol, mkString("bendict"));
    
  } else {

    K = reversePairList(K);
    D = reversePairList(D);

    SEXP nms;
    int i = 0;
  
    PROTECT(nms = allocVector(STRSXP, len));
    PROTECT(ans = allocVector(VECSXP, len));
    for( i = 0; K != R_NilValue; i++, K = CDR(K), D = CDR(D) ){
      SET_STRING_ELT(nms, i, STRING_ELT(CAR(K), 0));
      SET_VECTOR_ELT(ans, i, CAR(D));
    }
    setAttrib(ans, R_NamesSymbol, nms);
    setAttrib(ans, R_ClassSymbol, mkString("bendict"));
    UNPROTECT(2);
  }

  PRN("out of d: %c\n", **c)

  return(ans);
}

// return VECTORSXP
SEXP parse_l( const char **c ){

  if(  **c != 'l' ) error("List parsing should start with 'l'. Encountered '%c'.", **c);
  (*c)++;
  
  SEXP ans = R_NilValue;
  int same = 0, prev = **c, len = 0;

  if ( **c == 'i' ) { same = 1; prev = 'i'; }
  else if ( DIGIT(**c) ) { same = 1; prev = 's';}
  
  while( **c != 'e' ){
    if( !**c ) error("List parsing terminated prematurely.");
    len ++;
    same = same && ((prev == 's' && DIGIT(**c)) || (prev == 'i' && **c == 'i'));
    ans = CONS(parse_switch(c), ans);
  }

  if ( **c ) (*c)++;
  PRN("out of l: %c\n", **c);
  PRN("same: %d\n", same);

  if( len == 0){

    return(allocVector(VECSXP, 0));
    
  } else {
    
    ans = reversePairList(ans);
    
    if( ! same ) {
      
      return PairToVectorList(ans);
      
    } else {
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
  }
}



SEXP parse_switch( const char **c ){
  switch( **c ){
  case 'i':
    return(parse_i(c));
  case 'd':
    return(parse_d(c));
  case 'l':
    return(parse_l(c));
  default:
    if( DIGIT(**c) ) return(parse_s(c));
    else error("Wrong encoding '%c'. Allowed values are i, l, d or a digit.", **c);
  }
}


SEXP C_bdecode( SEXP str ) {

  if( ! isString(str) ) error("bcencode: input must be a character string");
  const char *c = CHAR(STRING_ELT(str, 0));

  SEXP ans = R_NilValue;

  while( *c ){
    ans = CONS(parse_switch(&c), ans);
  }
  // if one element return that element
  if( CDR(ans) == R_NilValue )
    return( CAR(ans) );
  else
    return( reversePairList(ans) );
}
