##' Manipulate bencode dictionary object.
##'
##' Bendict class is a convenience wrapper around named list to allow custom
##' printer.
##'
##'   \code{bendict}: Constructor similar to \code{list(...)}.
##' 
##'   \code{is.bendict}: TRUE if the object is a \code{bendict}.
##'
##'   \code{is.dictlike}: TRUE if object is a \code{bendict} or can be treated
##' as such. An object is \code{dictlike} if it's a vector or list and all its
##' names have positive length.
##'
##'   \code{as.bendict}: Convert to \code{bendict} object.
##' 
##' @param ... Key value pairs
##' @param obj An R object
##' @export
##' @examples
##'
##' (x <- bendict(op = "eval", code = "log(5)"))
##'
##' is.bendict(x)
##'
##' as.bendict(list(A = 5, B = "C"))
##' as.bendict(c(A = 5, B = "C")) # same
##'
##' is.dictlike(c(1, 2)) # FALSE
##' is.dictlike(c(A = 1, B = 2)) # TRUE
##' is.dictlike(c(1, B = 2)) # TRUE (not all are named)
bendict <- function(...){
    obj <- list(...)
    as.bendict(obj)
}

##' @rdname bendict
##' @export
is.bendict <- function(obj){
    class(obj) == "bendict"
}

##' @rdname bendict
##' @export
is.dictlike <- function(obj){
    is.bendict(obj) || ( is.vector(obj) &&
                          !is.null(nms <- names(obj)) &&
                          all(nzchar(nms)) )
}

##' @rdname bendict
##' @export
as.bendict <- function(obj){
    if(is.bendict(obj)){
        obj
    } else {
        obj <- as.list(obj)
        if(length(obj) >= 1){
            nms <- names(obj)
            if( is.null(nms) || !all(nzchar(nms)) )
                stop("Missing names detected. Only named objects can be converted to BenDict.")
            nms <- sort(unique(nms))
            if(length(nms) < length(obj)) stop("Names are not unique.")
            obj <- obj[nms]
        }
        class(obj) <- "bendict"
        obj
    }
}

##' @method print bendict
##' @export
print.bendict <- function(x, ...){
    if(length(x) == 0)
        cat("Empty BENDICT\n")
    else {
        cat("BENDICT:\n")
        str(unclass(x), no.list = T)
    }
}
