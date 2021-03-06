% Generated by roxygen2 (4.0.2): do not edit by hand
\name{bendict}
\alias{as.bendict}
\alias{bendict}
\alias{is.bendict}
\alias{is.dictlike}
\title{Manipulate bencode dictionary object.}
\usage{
bendict(...)

is.bendict(obj)

is.dictlike(obj)

as.bendict(obj)
}
\arguments{
\item{...}{Key value pairs}

\item{obj}{An R object}
}
\description{
Bendict class is a convenience wrapper around named list to allow custom
printer.
}
\details{
\code{bendict}: Constructor similar to \code{list(...)}.

\code{is.bendict}: TRUE if the object is a \code{bendict}.

\code{is.dictlike}: TRUE if object is a \code{bendict} or can be treated
as such. An object is \code{dictlike} if it's a vector or list and all its
names have positive length.

\code{as.bendict}: Convert to \code{bendict} object.
}
\examples{
(x <- bendict(op = "eval", code = "log(5)"))

is.bendict(x)

as.bendict(list(A = 5, B = "C"))
as.bendict(c(A = 5, B = "C")) # same

is.dictlike(c(1, 2)) # FALSE
is.dictlike(c(A = 1, B = 2)) # TRUE
is.dictlike(c(1, B = 2)) # TRUE (not all are named)
}

