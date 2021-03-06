% Generated by roxygen2 (4.0.2): do not edit by hand
\name{bencode}
\alias{bdecode}
\alias{bencode}
\title{Bencode encoder and decoder.}
\usage{
bencode(obj)

bdecode(obj)
}
\arguments{
\item{obj}{An R object.}
}
\description{
Functions to convert R objects from/to bencoded representation.
}
\details{
\code{bencode} converts any \code{dictlike} R object as bencode
dictionary. Integer non scalar vectors are converted into a list of bencode
integers. All other non scalar vectors will be converted to character and
then to bencode list. See examples.

\code{bdecode} can operate on strings or connections. String parser is
implemented at C level and is very fast. Due to R's awful C level connection
API, decoding from the connection is implemented in R and will probably be
very slow for large data.
}
\examples{
bencode(1L) # scalar integer
## [1] "i1e"
bencode(1) # scalar double, converted to string
## [1] "1:1"
bencode(1:2) # integer vector converted into list of ints
## [1] "li1ei2ee"

bencode("abc") # scalar string
## [1] "3:abc"
bencode(c("a", "b", "c")) # character vector
## [1] "l1:a1:b1:ce"

bencode(list(a = 1L, b = 2L, c = 3L)) ## dictlike
## [1] "d1:ai1e1:bi2e1:ci3ee"
bencode(bendict(a = 1L, b = 2L, c = 3L)) ## bendict, same as above
## [1] "d1:ai1e1:bi2e1:ci3ee"
bencode(list(a = 1L, b = 2L, 3L)) ## not dictlike, names ignored
## [1] "li1ei2ei3ee"

bencode(bendict(a1 = bendict(a2 = 2, b2 = 1:3), b1 = 1L)) ## lists are parsed recursively
## [1] "d2:a1d2:a21:22:b2li1ei2ei3eee2:b1i1ee"

bd <- bendict(a1 = bendict(a2 = 2, b2 = 1:3), b1 = 1L)
identical(bd, bdecode(bencode(bd))) # numeric a2 is converted to a string
## [1] FALSE

bd2 <- bendict(a1 = bendict(a2 = "2", b2 = 1:3), b1 = 1L)
identical(bd2, bdecode(bencode(bd2)))
## [1] TRUE
}
\seealso{
\code{\link{is.dictlike}}, \code{\link{bendict}}

 For bencode protocol see:
	\url{http://en.wikipedia.org/wiki/Bencode}
}

