\name{hist.crmp}
\alias{hist.crmp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{hist.crmp}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
\method{hist}{crmp}(x, var = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{var}{
%%     ~~Describe \code{var} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (x, var = NULL, ...) 
{
    if (!is.null(var)) {
        var.names <- list(var)
    }
    else {
        nvars <- ncol(x) - 1
        par(mfrow = get.square.plot.dims(nvars))
        var.names <- colnames(x)
    }
    for (var.name in var.names) {
        if ((!var.name \%in\% names(x)) || all(is.na(x[, var.name]))) 
            next
        if (!is.numeric(x[, var.name])) 
            next
        title <- paste("Histogram of", var.name, "at station", 
            slot(x, "station.id"))
        hist(x[, var.name], xlab = var.name, ylab = "Number of observations", 
            main = title, ...)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
