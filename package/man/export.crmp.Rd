\name{export.crmp}
\alias{export.crmp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{export.crmp}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
export.crmp(obj, dir, force = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
%%     ~~Describe \code{obj} here~~
}
  \item{dir}{
%%     ~~Describe \code{dir} here~~
}
  \item{force}{
%%     ~~Describe \code{force} here~~
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
function (obj, dir, force = F) 
{
    fname <- paste(sep = ".", slot(obj, "station.id"), "robject")
    print(paste("Exporting data from station", slot(obj, "station.id")))
    if (file.exists(fname)) 
        if (!force) 
            stop(paste("Cannot export to file", fname, "because it already exists"))
    save(obj, file = file.path(dir, fname))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
