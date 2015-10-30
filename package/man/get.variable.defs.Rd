\name{get.variable.defs}
\alias{get.variable.defs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{get.variable.defs}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
get.variable.defs(network)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{network}{
%%     ~~Describe \code{network} here~~
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
function (network) 
{
    network.file <- paste(sep = ".", network, "xform")
    do.call(data, list(network.file, package = "crmp"))
    var.defs <- get(network.file)
    var.names <- var.defs$network.var.name
    rownames(var.defs) <- var.names
    i <- which(names(var.defs) == "network.var.name")
    var.defs[, -i]
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
