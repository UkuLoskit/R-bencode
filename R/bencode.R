
### DECODER
ben_decode <- function(str){
    if(!is.character(str)) stop("str must be a character vector")
    .Call("decode1", str)
}

### ENCODER
ben_encode <- function(obj){

    enc_s <- function(str, collapse = TRUE){
        out <- paste0(nchar(str), ":", str)
        if(collapse) paste(out, collapse = "")
        else out
    }

    if( class(obj) == "bendict" ){
        nms <- enc_s(names(obj), FALSE)
        obj <- unlist(lapply(obj, ben_encode), recursive = FALSE)
        sprintf("d%se", paste0(nms, obj, collapse = ""))
    } else {
        switch(typeof(obj),
               pairlist =,
               list = sprintf("l%se", paste0(unlist(lapply(obj, ben_encode)), collapse = "")), 
               integer = sprintf("l%se", paste0("i", obj, "e", collapse = "")),
               character = sprintf("l%se", enc_s(obj)),
               { ## default
                   obj <- as.character(obj)
                   if(length(obj)) sprintf("l%se", enc_s(obj))
                   else enc_s(obj)
               })
    }
}

### CONSTRUCTOR
bendict <- function(obj){
    obj <- as.list(obj)
    nms <- names(obj)
    if( is.null(nms) ) stop("No names detected. Only named objects can be converted to BenDict.")
    nms <- sort(unique(nms))
    if(length(nms) < length(obj)) stop("Names are not unique.")
    obj <- obj[nms]
    class(obj) <- "bendict"
    obj
}

setMethod("print", "bendict",
          function(x, ...){
              cat("BENDICT:\n")
              str(unclass(x), no.list = T)
          })

