\name{crmp.stns.within.shp}
\alias{crmp.stns.within.shp}
\title{Spatial searching for CRMP stations}

\description{Function to return a list of crmp station id's within a shapefile.
}
\usage{ crmp.stns.within.shp(con, shp.filename, network = NULL) }
\arguments{
  \item{con}{ a connection object as produced by ‘dbConnect’ within the
    RPostgreSQL library}
  \item{shp.filename}{a string of a shapefile location
    (ex. '/home/data/gis/basedata/base_layers/columbia_boundary.shp'). The
    shapefile needs to have a projection file (a .prj)}
  \item{network}{String of network name within CRMP database (example: "EC")}
}
\value{Returns a list of crmp station ids. The rational behind this is
  you can perform an lapply over the returned list & get each station
  via read.sql.crmp
}
\references{
}
\author{
  Hailey Eckstrand
}
\note{For this function to work, you need to bring in 3 additional libraries: rgdal, rgeos, RPostgreSQL}

\examples{
\dontrun{
library(rgeos)
library(rgdal)
library(RPostgreSQL)
library(crmp)
con <- dbConnect(PostgreSQL(), dbname='crmp', user='my_user', password='my_password')
crmp.stns.within.shp(con, shp.filename='/home/data/gis/basedata/base_layers/columbia_boundary.shp')
crmp.stns.within.shp(con,
shp.filename='/home/data/gis/basedata/base_layers/columbia_boundary.shp',
network="EC")
}
}