
bencode <- function(str){
    .Call("decode1", str)
}

setMethod("print", "bendict",
          function(x, ...){
              cat("BENDICT:\n")
              str(unclass(x), no.list = T)
          })
