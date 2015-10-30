\name{search.sql.crmp}
\alias{search.sql.crmp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Search CRMP}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Given a set of station identifying information, return a crmp object 
representing the data at that station.
}
\usage{
search.sql.crmp(con, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{con}{
%%     ~~Describe \code{con} here~~
A PostgreSQLConnection object with read permissions on the CRMP database.
}
  \item{...}{
%%     ~~Describe \code{...} here~~
Sufficient information to uniquely identify a station or history id.
Valid variables are hist.id, stn.id, net.id, net.name. native.id, stn.name.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
Identification is attempted using the following order:\cr
\tabular{rl}{
  \tab hist.id\cr
  \tab stn.id\cr
  \tab native.id and net.id\cr
  \tab native.id and net.name\cr
  \tab native.id\cr
  \tab stn.name and net.id\cr
  \tab stn.name and net.name\cr
  \tab stn.name
}
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
A crmp object.
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
\code{\link{read.sql.crmp}}
}
