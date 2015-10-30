\name{ks.test.station.temperature}
\alias{ks.test.station.temperature}
\alias{ks.test.station.precip}
\alias{ks.test.one.var}

\title{ks.test.station.*}

\description{
Calculates the Kolmogorov-Smirnov (KS) statistic comparing variable's fitted distribution to
its emperical distribution
}
\usage{
ks.test.station.temperature(obj)
ks.test.station.precip(obj)
ks.test.one.var(obj, var.name)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{An instantiation of the crmp class}
  \item{var.name}{name of a single variable upon which to calculate the
    KS statistic}
}
\details{
  ks.test.station.temperature matches all variables with attributes
  \itemize{
    \item cf:standard_name == temperature
  }
    
  ks.test.station.precip matches all variables with attributes
  \itemize{
    \item cf:standard_name == precipitation
    \item cf:cell_method == time: sum
  }

  ks.test.one.var may be used to programmatically test a single variable without knowning
  whether it is temperature or precip.
}
\value{
  Returns a list, one item for each variable which matches the
  requirements.  For example, if a station records tmin and tmax,
  ks.test.station.temperature will return a list of two values.
  Each item of the list is the result from the \link{ks.test} function
  or NA if ks.test raises an error.

  ks.test.one will only return a single result.
}
\references{
  \url{http://www.york.ac.uk/depts/maths/tables/pdf.htm}
}

\seealso{
  \link{is.significant}
  \link{ks.test}
}

\examples{
\dontrun{con <- dbConnect("PostgreSQL", "crmp")}
\dontrun{x <- read.sql.crmp(con, 1000)}
\dontrun{all.results <- lapply(names(x), ks.test.one.var, obj=x)}
}
