library(rgdal)
library(rgeos)

## parameters are to be of class SpatialPointsDataFrame
## returns the number of CRMP stations which are independent
## (i.e. NOT co-located with an EC station)
## threshold is in kilometers
spatially.independent <- function(ec.stations, crmp.stations, threshold=1) {
  stopifnot(inherits(ec.stations, "SpatialPointsDataFrame"),
            inherits(crmp.stations, 'SpatialPointsDataFrame'))
  d <- spDists(ec.stations, crmp.stations, longlat=T)
  matches <- d > threshold
  n <- apply(matches, 2, all)
  length(which(n))
}

## stations parameter should be a SpatialPointsDataFrame
## counts groups of clusters that are no farther than distance.threshold
## from another station
count.clusters <- function(stations, distance.threshold, projection=CRS("+init=epsg:3005"), do.plot=F) {
  ## Project the coordinates to a CRS with units matching the units of the distance.threshold
  projected <- spTransform(stations, projection)
  clusters <- gBuffer(projected, width=distance.threshold)

  if (do.plot) {
    plot(clusters);
    points(projected, pch=20)
  }

  length(clusters@polygons[[1]]@Polygons)
}

## stations and anchors should be a SpatialPoints (or SpatialPointsDataFrame) object
## stations can be either a superset or disjoint set of anchors
## distance.threshold is in the units of the projection
count.clusters.with.anchor.stations <- function(stations, anchors, distance.threshold, projection=CRS("+init=epsg:3005"), do.plot=F) {
  anchors <- as(anchors, 'SpatialPoints')
  stations <- spTransform(stations, projection); anchors <- spTransform(anchors, projection)
  all.stations <- gUnion(stations, anchors)
  clusters <- gBuffer(all.stations, width=distance.threshold)

  clusters <- explode(clusters)
  result <- over(clusters, anchors)

  if (do.plot) {
    plot(clusters)
    points(all.stations, pch=20)
    points(anchors, col='red', pch=20)
  }

  length(na.exclude(result))
}

## Take a SpatialPolygons object which contains a multipolygon and split the
## multipolygon into single polygon object.
## Equivalent of disaggregate/explode/multipolygon to singlepolygon
explode <- function(spatial.polygons, ids=NULL) {
  n <- length(spatial.polygons@polygons[[1]]@Polygons)

  if (is.null(ids))
    ids <- 1:n

  mk.poly <- function(p, id) Polygons(list(p), id)

  single.polys <- mapply(mk.poly, spatial.polygons@polygons[[1]]@Polygons, ids)
  SpatialPolygons(single.polys, proj4string=spatial.polygons@proj4string)
}

library(RUnit)

mk.fixtures <- function(data.path = "/home/data/projects/crmp/archive", data.file='prov_met_net_nov2010') {
  data <- readOGR(data.path, data.file, stringsAsFactors=F)

  ## Subset 1: 
  ## These are actually reversed... but this is an area where there are 4 EC stations and 1 CRMP station
  ## just pretend like it's 1 EC to 4 CRMP stations for the purposes of the test
  select.stations <- c('1016640', '1017254', 'E231478', '1015105', '1018935')
  sub <- data[ data@data$'Station_Co' %in% select.stations, ]
  i <- which(sub@data$Network == "Environment Canada")
  j <- which(sub@data$Network != "Environment Canada")
  rv <- list(list(ec=sub[j,], crmp=sub[i,]))

  ## Subset 2:
  ## 2 EC and 2 CRMP stations
  select.stations <- c('1018620', '1018598', 'E231478', 'E264882')
  subset <- data[ data@data$'Station_Co' %in% select.stations, ]
  i <- which(subset@data$Network == "Environment Canada")
  j <- which(subset@data$Network != "Environment Canada")
  rv <- append(rv, list(list(ec=subset[i,], crmp=subset[j,])))

  ## Subset 3:
  ## 2 EC and 2 CRMP stations + and additional cluster with 2 more EC stations
  select.stations <- c('1018620', '1018598', 'E231478', 'E264882', '1022571', '1012573')
  subset <- data[ data@data$'Station_Co' %in% select.stations, ]
  i <- which(subset@data$Network == "Environment Canada")
  j <- which(subset@data$Network != "Environment Canada")
  rv <- append(rv, list(list(ec=subset[i,], crmp=subset[j,])))

  return(rv)
}

test.spatially.independent <- function() {

  fixtures <- mk.fixtures()

  ## Three tests near Victoria
  ## The (faux) EC station is 7.2, 11.4, 15.9, and 25.5 km away from the other stations
  ## So, place the threshold at various radii around the EC station
  fix <- fixtures[[1]]
  cases <- list(# Radius, expected
                c(5, 4),
                c(8, 3),
                c(30, 0))

  for (case in cases) {
    rv <- spatially.independent(fix$ec, fix$crmp, case[1])
    checkEquals(rv, case[2])
  }

  ## Open these up in QGIS to figure out why.
  fix <- fixtures[[2]]
  cases <- list(c(10, 2),
                c(16, 1),
                c(22, 0))
  for (case in cases) {
    rv <- spatially.independent(fix$ec, fix$crmp, case[1])
    checkEquals(rv, case[2])
  }
}

do.not.test.all <- function() {
  data.path <- "/home/data/projects/crmp/archive"
  data <- readOGR(data.path, 'prov_met_net_nov2010', stringsAsFactors=F)

  i <-which(data@data$Network == "Environment Canada")
  j <- which(data@data$Network != "Environment Canada")
  spatially.independent(data[i,], data[j,], 1)
}

test.count.clusters <- function() {
  data <- mk.fixtures()[[2]]
  fix <- gUnion(data$ec, data$crmp)

  width <- 10000 #10km
  checkEquals(count.clusters(fix, width), 2)
}

test.count.clusters.with.anchor.stations <- function() {
  f <- 'count.clusters.with.anchor.stations'
  fixtures <- mk.fixtures()

  ec <- fixtures[[2]]$ec
  crmp <- fixtures[[2]]$crmp
  all <- gUnion(ec, crmp)
  ec.plus <- fixtures[[3]]$ec
  all.plus <- gUnion(crmp, ec.plus)

  cases <- list(list(all.plus, ec.plus, 10000, 3),
                list(all, ec, 1000, 2), # Small buffer results in one cluster per station
                list(all, ec, 30000, 1), # Large buffer results in only one cluster (for all stations)
                ## These two should be the same... submitting stations as either superset or disjoint set
                list(all, ec, 10000, 2),
                list(crmp, ec, 10000, 2)
                )
  
  for (case in cases) {
    args <- append(case[1:3], list(do.plot=T))
    expected <- case[[4]]
    rv <- do.call(f, args)
    checkEquals(rv, expected)
  }
}
