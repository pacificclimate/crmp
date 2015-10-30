\name{which.is.tmin}

\alias{which.is.tmin}
\alias{which.is.tmax}
\alias{which.is.tmean}
\alias{which.is.precip}

\title{which.is.*}
\description{Convenience functions for locating the data column which
  represents certain canonical climate varaibles (Tmin, Tmax, Tmean, and Precip)
}
\usage{
which.is.tmin(obj)
which.is.tmax(obj)
which.is.tmean(obj)
which.is.precip(obj)
}
\arguments{
  \item{obj}{An instantiation of the crmp class}
}
\details{This family of functions just works by searching the
  cf:standard_name and cf:cell_method slots and returning the index for
  which variables match.  This search is according to the following:

  \tabular{rl}{
    tmin \tab standard.names == "air_temperature" & cell.methods == "time:
    minimum"\cr
    tmax \tab standard.names == "air_temperature" & cell.methods == "time:
    maximum"\cr
    tmean \tab standard.names == "air_temperature" &
    (cell.methods == "time: point" | cell.methods == "time: mean")\cr
    precip \tab standard.names == "lwe_thickness_of_precipitation_amount"\cr
  }

}
\value{
  Returns an integer vector (possibly of length zero, if no matches are
  found) which is the index of the columns(s) that match the specification.
}
\note{
These functions really are just for convenience and are not highly
robust.  They should be used with care and caution.
}
