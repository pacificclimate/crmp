\name{crmp.get.start.date}
\alias{crmp.get.start.date}
\alias{crmp.get.end.date}

\title{crmp.get.*.date}
\description{get the min and max dates for a station's timeseries}
\usage{
crmp.get.start.date(obj)
crmp.get.end.date(obj)
}
\arguments{
  \item{obj}{An instantiation of the crmp class}
}
\value{Returns a single POSIXt value which represents the min or max
  date for which there is an observation in the station's timeseries.}
