\name{crmp.stns.dwithin.*}
\alias{crmp.stns.dwithin.decimal.degrees}
\alias{crmp.stns.dwithin.meters}
\title{Spatial searching for CRMP stations}

\description{Function to return a list of crmp station id's within a
  certain distance of a location.
}
\usage{
crmp.stns.dwithin.decimal.degrees(con, lat, lon, network = NULL, dis)
crmp.stns.dwithin.metres(con, lat, lon, network = NULL, dis, srs = "3005")
}
\arguments{
  \item{con}{ a connection object as produced by ‘dbConnect’ within the RPostgreSQL library}
  \item{lat}{Numeric latitude coordinate in decimal degrees}
  \item{lon}{Numeric longitude coordinate in decimal degrees (don't forget to add the negative if the longitide is West of 0)}
  \item{network}{ String of network name within CRMP database (example: "EC")}
  \item{dis}{Numerical value that determines cutoff distance. For
    function: crmp.stns.dwithin.decimal.degrees, dis is assumed to be in
    decimal degrees, For function: crmp.stns.dwithin.metres, dis is
    assumed to be in metres.}
}
\value{Returns a list of crmp station ids. The rational behind this is
  you can perform an lapply over the returned list & get each station
  via read.sql.crmp}
\author{
  Hailey Eckstrand
}

\examples{
\dontrun{
library(rgeos)
library(rgdal)
library(RPostgreSQL)
library(crmp)
con <- dbConnect(PostgreSQL(), dbname='crmp', user='my_user', password='my_password')
crmp.stns.dwithin.decimal.degrees(con, lat=49.2, lon=-123, dis=1.5)
crmp.stns.dwithin.decimal.degrees(con, lat=49.2, lon=-123, dis=1.5, network="EC")
crmp.stns.dwithin.metres(con, lat=49.2, lon=-123, dis=10000)
crmp.stns.dwithin.metres(con, lat=49.2, lon=-123, dis=10000,
network="EC")
}
}