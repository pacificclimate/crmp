\name{crmp-class}
\Rdversion{1.1}
\docType{class}
\alias{crmp-class}
\alias{[,crmp,integer,character-method}
\alias{[,crmp,integer,integer-method}
\alias{[,crmp,integer,missing-method}
\alias{[,crmp,missing,character-method}
\alias{[,crmp,missing,integer-method}
\alias{[,crmp,POSIXt,POSIXt-method}
\alias{coerce<-,crmp,data.frame-method}
\alias{coerce,crmp,data.frame-method}
\alias{initialize,crmp-method}

\title{Class \code{"crmp"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("crmp", x, check.names, ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"list"} ~~ }
    \item{\code{station.id}:}{Object of class \code{"character"} ~~ }
    \item{\code{station.name}:}{Object of class \code{"character"} ~~ }
    \item{\code{network}:}{Object of class \code{"character"} ~~ }
    \item{\code{flags}:}{Object of class \code{"list"} ~~ }
    \item{\code{cf:convention}:}{Object of class \code{"character"} ~~ }
    \item{\code{cf:units}:}{Object of class \code{"character"} ~~ }
    \item{\code{cf:long_description}:}{Object of class \code{"character"} ~~ }
    \item{\code{cf:cell_method}:}{Object of class \code{"character"} ~~ }
    \item{\code{cf:standard_name}:}{Object of class \code{"character"} ~~ }
    \item{\code{time}:}{Object of class \code{"POSIXct"} ~~ }
    \item{\code{names}:}{Object of class \code{"character"} ~~ }
    \item{\code{row.names}:}{Object of class \code{"data.frameRowLabels"} ~~ }
    \item{\code{.S3Class}:}{Object of class \code{"character"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{data.frame}"}, directly.
Class \code{"\linkS4class{list}"}, by class "data.frame", distance 2.
Class \code{"\linkS4class{oldClass}"}, by class "data.frame", distance 2.
Class \code{"\linkS4class{vector}"}, by class "data.frame", distance 3.
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "crmp", i = "integer", j = "character")}: ... }
    \item{[}{\code{signature(x = "crmp", i = "integer", j = "integer")}: ... }
    \item{[}{\code{signature(x = "crmp", i = "integer", j = "missing")}: ... }
    \item{[}{\code{signature(x = "crmp", i = "missing", j = "character")}: ... }
    \item{[}{\code{signature(x = "crmp", i = "missing", j = "integer")}: ... }
    \item{[}{\code{signature(x = "crmp", i = "POSIXt", j = "POSIXt")}: ... }
    \item{coerce<-}{\code{signature(from = "crmp", to = "data.frame")}: ... }
    \item{coerce}{\code{signature(from = "crmp", to = "data.frame")}: ... }
    \item{initialize}{\code{signature(.Object = "crmp")}: ... }
	 }
}
\references{
%%  ~~put references to the literature/web site here~~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
}
\examples{
showClass("crmp")
}
\keyword{classes}
