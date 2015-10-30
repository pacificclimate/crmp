\name{crmp.verify}
\alias{crmp.verify}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{crmp.verify}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
crmp.verify(object)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
%%     ~~Describe \code{object} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (object) 
{
    do.check <- function() {
        stopifnot(inherits(object, "crmp"))
        for (a in c("station.id", "network")) {
            if (is.null(slot(object, a)) || slot(object, a) == 
                "") 
                stop()
        }
        stopifnot(slot(object, "cf:convention") \%in\% paste("1.", 
            seq(0, 4), sep = ""))
        len <- dim(object)[2]
        for (a in c("cf:units", "cf:standard_name", "cf:cell_method", 
            "cf:long_description")) {
            stopifnot(length(slot(object, a)) == len)
        }
        t <- slot(object, "time")
        stopifnot(all(diff(t) > 0))
        s <- names(object)
        doesnt.comply <- grepl("^[^A-Za-z]", s)
        if (any(doesnt.comply)) 
            stop("Failed check 3")
        doesnt.comply <- grepl("[^A-Za-z0-9_]", s)
        if (any(doesnt.comply)) 
            stop("Failed check 3")
        stopifnot(all(sapply(slot(object, "flags"), class) == 
            "flag"))
        stopifnot(all(sapply(slot(object, "flags"), nrow) == 
            nrow(object)))
    }
    result <- try(do.check(), silent = T)
    if (inherits(result, "try-error")) 
        return(F)
    else return(T)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
