##' Bencode encoder and decoder.
##'
##' Functions to convert R objects from/to bencoded representation.
##'
##'
##' \code{bencode} converts any \code{dictlike} R object as bencode
##' dictionary. Integer non scalar vectors are converted into a list of bencode
##' integers. All other non scalar vectors will be converted to character and
##' then to bencode list. See examples.
##'
##' \code{bdecode} can operate on strings or connections. String parser is
##' implemented at C level and is very fast. Due to R's awful C level connection
##' API, decoding from the connection is implemented in R and will probably be
##' very slow for large data.
##' 
##' @seealso \code{\link{is.dictlike}}, \code{\link{bendict}}
##'
##'  For bencode protocol see:
##' 	\url{http://en.wikipedia.org/wiki/Bencode}
##' @export
##' @examples 
##' bencode(1L) # scalar integer
##' ## [1] "i1e"
##' bencode(1) # scalar double, converted to string
##' ## [1] "1:1"
##' bencode(1:2) # integer vector converted into list of ints
##' ## [1] "li1ei2ee"
##' 
##' bencode("abc") # scalar string
##' ## [1] "3:abc"
##' bencode(c("a", "b", "c")) # character vector
##' ## [1] "l1:a1:b1:ce"
##'  
##' bencode(list(a = 1L, b = 2L, c = 3L)) ## dictlike
##' ## [1] "d1:ai1e1:bi2e1:ci3ee"
##' bencode(bendict(a = 1L, b = 2L, c = 3L)) ## bendict, same as above
##' ## [1] "d1:ai1e1:bi2e1:ci3ee"
##' bencode(list(a = 1L, b = 2L, 3L)) ## not dictlike, names ignored
##' ## [1] "li1ei2ei3ee"
##'  
##' bencode(bendict(a1 = bendict(a2 = 2, b2 = 1:3), b1 = 1L)) ## lists are parsed recursively
##' ## [1] "d2:a1d2:a21:22:b2li1ei2ei3eee2:b1i1ee"
##'  
##' bd <- bendict(a1 = bendict(a2 = 2, b2 = 1:3), b1 = 1L)
##' identical(bd, bdecode(bencode(bd))) # numeric a2 is converted to a string
##' ## [1] FALSE
##'  
##' bd2 <- bendict(a1 = bendict(a2 = "2", b2 = 1:3), b1 = 1L)
##' identical(bd2, bdecode(bencode(bd2)))
##' ## [1] TRUE
bencode <- function(obj){
    if( is.dictlike(obj) ){
        if( length(obj) == 0L ) "de"
        else {
            nms <- .enc_string(names(obj), FALSE)
            obj <- unlist(lapply(obj, bencode), recursive = FALSE)
            sprintf("d%se", paste0(nms, obj, collapse = ""))
        }
    } else if(is.factor(obj)){
        bencode(as.character(obj))
    } else {
        switch(typeof(obj),
               pairlist =,
               list = sprintf("l%se", paste0(unlist(lapply(obj, bencode)), collapse = "")), 
               integer = {
                   out <- paste0("i", obj, "e", collapse = "")
                   if( length(obj) > 1 ) sprintf("l%se", out)
                   else out
               }, 
               character =, { #default
                   obj <- as.character(obj)
                   if(length(obj) > 1) sprintf("l%se", .enc_string(obj))
                   else .enc_string(obj)
               })
    }
}

##' @rdname bencode
##' @param obj An R object.
##' @useDynLib bencode C_bdecode
##' @export
bdecode <- function(obj){
    if(inherits(obj, "connection")){
        .decode_con(obj)
    } else if(is.character(obj)){
        .Call("C_bdecode", obj)
    } else
        stop("OBJ must be a character vector or a connection")
}

## we cannot read at C level, R doesn't allow that
.read_till_char <- function(con, char){
    accum <- list()
    c <- readChar(con, 1L)
    while(c != char){
        accum[length(accum) + 1L] <- c
        c <- readChar(con, 1L)
    }
    do.call(paste0, accum)
}

.decode_i <- function(con){
    out <- as.numeric(.read_till_char(con, "e"))
    if(is.na(out)) stop("bdecode error while reading integer.")
    out
}

.decode_s <- function(con, first_char = ""){
    len <- paste0(first_char, .read_till_char(con, ":"))
    len <- as.integer(len)
    if(is.na(len)) stop("bdecode error while reading string's len.")
    readChar(con, len)
}

.decode_l <- function(con){
    out <- list()
    same <- T
    el <- .decode_con(con)
    to <- typeof(el)
    while(!is.null(el)){
        out[[length(out) + 1L]] <- el
        el <- .decode_con(con)
        if(!is.null(el))
            same <- same && typeof(el) == to
    }
    if(same)
        out <- unlist(out, recursive=F)
    out
}

.decode_d <- function(con){
    out <- list()
    key <- NULL
    while(!is.null(el <- .decode_con(con))){
        if(is.null(key)) key <- el
        else {
            out[[as.character(key)]] <- el
            key <- NULL
        }
    }
    as.bendict(out)
}

.decode_con <- function(con){
    ## should be in C, but R's API sucks pickles
    c <- readChar(con, 1L)
    if (length(c) == 0L) {
        condition <- simpleError("End of input")
        class(condition) <- c("endOfInput", class(condition))
        signalCondition(condition)
    }
    else if (c == "e")
        NULL ## termination
    else if (c == "d")
        .decode_d(con)
    else if (c == "i")
        .decode_i(con)
    else if (c == "l")
        .decode_l(con)
    else {
        .decode_s(con, c)
    }
}

.enc_string <- function(str, collapse = TRUE){
    ## all empty objects are encoded as NULL
    if(length(str) == 0) "4:NULL"
    else {
        out <- paste0(nchar(str), ":", str)
        if(collapse) paste(out, collapse = "")
        else out
    }
}

