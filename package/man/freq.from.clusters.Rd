\name{freq.from.clusters}
\alias{freq.from.clusters}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{freq.from.clusters}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
freq.from.clusters(cluster, thresh = 0.7)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cluster}{
%%     ~~Describe \code{cluster} here~~
}
  \item{thresh}{
%%     ~~Describe \code{thresh} here~~
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
function (cluster, thresh = 0.7) 
{
    if (is.na(cluster)) 
        return(NA)
    stopifnot(thresh > 0, thresh < 1)
    i <- which(cluster$weights > thresh)
    if (length(i > 0)) {
        rv <- switch(cluster$clusters[i], `60` = "1-minute", 
            `120` = "2-minute", `300` = "5-minute", `900` = "15-minute", 
            `1800` = "30-minute", `3600` = "1-hourly", `10800` = "3-hourly", 
            `21600` = "6-hourly", `43200` = "12-hourly", `86400` = "daily", 
            "irregular")
    }
    else rv <- "irregular"
    rv
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
