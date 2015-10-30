library(ncdf4)
library(seas)
library(MASS)
library(parallel)
library(RUnit)

setClass("crmp",
         representation('data.frame',
                        station.id="character",
                        station.name="character",
                        network="character",
                        flags="list",
                        'cf:convention'="character",
                        'cf:units'="character",
                        'cf:long_description'="character",
                        'cf:cell_method'="character",
                        'cf:standard_name'="character",
                        time="POSIXct"),
         contains="data.frame"
         )

crmp.init <- function(.Object, x, check.names=T, ...) {

  args <- list(...)

  if (! missing('x')) {
    x <- data.frame(x, check.names=check.names)
    slot(.Object, 'row.names') <- rownames(x)
    slot(.Object, '.Data') <- x
  }
  
  n <- ncol(.Object)
  
  defaults <- list('cf:convention'='1.4', 'cf:units'=rep('', n), 'cf:standard_name'=rep('', n), 'cf:long_description'=rep('', n), 'cf:cell_method'=rep('', n))

  set.slots <- unique(c(names(args), names(defaults)))
  for (arg in set.slots) {
    rv <- try(slot(.Object, arg) <- args[[arg]], silent=T)
    if (inherits(rv, 'try-error')) {
      slot(.Object, arg) <- defaults[[arg]]
    }
  }
  .Object
}

setMethod("initialize",
          "crmp",
          crmp.init
          )

# Checks to ensure that an object of the crmp class meets the specifications for the crmp class
# 1: inherits from crmp
# 2: attributes:
#    a: 'station.id' and 'network' are non-empty strings
#    b: 'cf:convention' maps to a valid version string of CF (1.0 - 1.4 at present)
#    c: 'cf:units', 'cf:standard_name', 'cf:cell_method', and 'cf:long_description' are all present and of length [number of rows/variables]
#    d: 'time' is guaranteed to be monotonically increasing
# 3: row names all comply with CF conventions for names
# 4: all members of the 'flags' slot are of type flag

crmp.verify <- function(object) {
  do.check <- function() {
    ## 1
    stopifnot(inherits(object, "crmp"))

    ## 2a
    for (a in c("station.id", "network")) {
      if (is.null(slot(object, a)) || slot(object, a) == "")
        stop()
    }

    ## 2b
    stopifnot(slot(object, "cf:convention") %in% paste("1.", seq(0, 4), sep=""))

    ## 2c
    len <- dim(object)[2]
    for (a in c("cf:units", "cf:standard_name", "cf:cell_method", "cf:long_description")) {
      stopifnot(length(slot(object, a)) == len)
    }

    ## 2d
    t <- slot(object, 'time')
    stopifnot(all(diff(t) > 0))

    ## 3
    s <- names(object)
    doesnt.comply <- grepl("^[^A-Za-z]", s) # Starts with a non alpha character
    if (any(doesnt.comply)) stop("Failed check 3")
    doesnt.comply <- grepl("[^A-Za-z0-9_]", s) # Contains a non-alphanumeric (or underscore) character
    if (any(doesnt.comply)) stop("Failed check 3")

    ## 4
    stopifnot(all(sapply(slot(object, 'flags'), class) == 'flag'))
    stopifnot(all(sapply(slot(object, 'flags'), nrow) == nrow(object)))
  }
  result <- try(do.check(), silent=T)

  if (inherits(result, "try-error"))
    return(F)
  else
    return(T)
}
setValidity("crmp", crmp.verify)

as.data.frame.crmp <- function(from) {
  li <- from@.Data
  names(li) <- from@names
  do.call('data.frame', li)
}
setAs('crmp', 'data.frame', as.data.frame.crmp)

crmp.to.ncdf <- function(data, out.dir=getwd()) {
  if (! inherits(data, "crmp"))
    stop("Type error: data frame must be of class \"crmp\"")

  origin <- as.POSIXct("1970/01/01", tz="GMT")

  print("Extracting dimension extents")
  t <- slot(data, 'time')
  t <- ncdim_def("time", "days since 1970-01-01 00:00:00", as.numeric(t - origin, units="days"), unlim=T)

  stn.id <- slot(data, "station.id")
  network <- slot(data, "network")

  vars <- mapply(ncvar_def, name=names(data), units=slot(data, 'cf:units'),
                 longname=slot(data, 'cf:long_description'),
                 MoreArgs=list(dim=list(t)), SIMPLIFY=F)

  print("Opening the nc file and writing the variables")
  outfile <- file.path(out.dir, paste(sep="", stn.id, ".nc"))
  nc <- nc_create(outfile, vars)
  # Write values to the netcdf variables
  vals <- if (length(vars) == 1) list(data[,names(vars)]) else data[,names(vars)] ## Avoid mapply recycling if vars == 1
  mapply(ncvar_put, varid=vars, vals=vals, MoreArgs=list(nc=nc, verbose=F))

  # Write optional attributes to the variable descriptions
  i <- sapply(names(vars), function (v) {which(names(data) == v)})
  cf.sn <- slot(data, "cf:standard_name")[i]

  # Global attributes
  ncatt_put(nc, 0, "Conventions", paste("CF-", slot(data, "cf:convention"), sep=""))
  ncatt_put(nc, 0, "station_id", stn.id)
  ncatt_put(nc, 0, "network", network)

  mapply(ncatt_put, varid=names(vars), attval=cf.sn, MoreArgs=list(nc=nc, attname="standard_name"))
  cf.cm <- slot(data, "cf:cell_method")[i]
  mapply(ncatt_put, varid=names(vars), attval=cf.cm, MoreArgs=list(nc=nc, attname="cell_method"))

  # Additional attributes for CF compliance
  ncatt_put(nc=nc, varid="time", attname="calendar", attval="julian")
  ncatt_put(nc=nc, varid="time", attname="long_name", attval="time")

  nc
}
#setAs('crmp', 'ncdf4', function(from) {crmp.to.ncdf(from)})

which.are.defined <- function(var.names, var.defs) {
  which(sapply(var.names, "%in%", rownames(var.defs)))
}

## Take a crmp frame and remove varaibles which have no
## definition in the var.defs table (i.e. something that comes out of
## get.variable.defs())
remove.nondefined.vars <- function(obj, var.defs) {
  stopifnot(inherits(obj, 'crmp'))

  ## Remove variables which have no definition
  are.defined <- which.are.defined(names(obj), var.defs)
  obj[,are.defined,drop=F]
}

set.crmp.attributes <- function(crmp, var.defs, network) {
  var.names <- names(crmp)
  add.attributes <- list(network = network,
                         "cf:units" = var.defs[var.names,"network.unit"],
                         "cf:standard_name" = var.defs[var.names,"cf.var.name"],
                         "cf:long_description" = var.defs[var.names,"network.long.description"],
                         "cf:cell_method" = var.defs[var.names,"cf.cell.method"],
                         "cf:convention" = "1.4")
  slots(crmp) <- add.attributes

  # Make the variable names CF-1.4 compatible
  names(crmp) <- convert.to.cf.1.4.names(var.names) 
  crmp
}

remove.duplicate.times <- function(obj) {
  stopifnot(inherits(obj, 'crmp'))
  i <- which(diff(obj@time) == 0) + 1
  if (length(i) > 0)
    obj[-i,,drop=F]
  else
    obj
}

remove.NA.vars <- function(obj) {
  stopifnot(inherits(obj, 'crmp'))
  i <- which(sapply(obj, function(x) {all(is.na(x))}))
  obj[,-i,drop=F]
}

get.square.plot.dims <- function(n) {
  plot.rows <- ceiling(sqrt(n))
  plot.cols <- ceiling(n / plot.rows)
  c(plot.rows, plot.cols)
}

plot.crmp <- function(x, var=NULL, ...) {
  if (! is.null(var)) {
    var.names <- list(var)
  }
  else { # If var is null, just plot all of the variables on the same plot
    nvars <- ncol(x)
    par(mfrow=get.square.plot.dims(nvars))
    var.names <- colnames(x)
  }
  for (var.name in var.names) {
    if (! var.name %in% names(x))
      next
    title <- paste("Station:", slot(x, "station.id"), var.name)
    subtitle <- paste("Network:", x@network, "Station name:", x@station.name)
    t <- slot(x, 'time'); y <- x[,var.name]
    if (all(is.na(y)))
      warning(paste("No data exists for station: ", slot(x, "station.id"), ", variable: ", var.name, ". Skipping the plot.", sep=""))
    else
      plot(t, y, xlab="time", ylab=var.name, main=title, sub=subtitle, ...)
  }
}

# var is the name of _one_variable to plot
boxplot.crmp <- function(x, var=NULL, horizontal=F, ...) {

  if (! is.null(var)) {
    var.names <- list(var)
  }
  else { # If var is null, just plot all of the variables on the same plot
    nvars <- ncol(x) - 1
    if (horizontal) {
      par(mfrow=c(nvars, 1))
    } else {
      par(mfrow=c(1, nvars))
    }
    var.names <- colnames(x)
  }
  for (var.name in var.names) {
    # Don't try to plot it if there's no data, or the variable doesn't exist
    if ((! var.name %in% names(x)) || all(is.na(x[,var.name])))
      next

    # Some variables are factors, like OBSTYPE or flags.  Don't plot these
    if (! is.numeric(x[,var.name]))
      next

    title <- paste("Station:", slot(x, "station.id"), var.name)
    boxplot(x[,var.name], ylab=var.name, names=var.name, horizontal=horizontal, main=title, ...)
  }
}

# var is the name of _one_variable to plot
hist.crmp <- function(x, var=NULL, ...) {
  if (! is.null(var)) {
    var.names <- list(var)
  } else {
    nvars <- ncol(x) - 1
    par(mfrow=get.square.plot.dims(nvars))
    var.names <- colnames(x)
  }
  for (var.name in var.names) {

    # Don't try to plot it if there's no data, or the variable doesn't exist
    if ((! var.name %in% names(x)) || all(is.na(x[,var.name])))
      next

    # Some variables are factors, like OBSTYPE or flags.  Don't plot these
    if (! is.numeric(x[,var.name]))
      next

    title <- paste("Histogram of", var.name, "at station", slot(x, "station.id"))
    hist(x[,var.name], xlab=var.name, ylab="Number of observations", main=title,  ...)
  }
}

summary.crmp <- function(object, ...) {
  lapply(slot(object, '.Data'), summary)
}

## FIXME: Need to handle the flags here
order.crmp <- function(obj) {
  stopifnot(inherits(obj, 'crmp'))
  t <- slot(obj, 'time')
  i <- order(t)
  new.flags <- lapply(obj@flags, '[', i, drop=F)
  obj@flags <- new.flags
  obj[i,,drop=F]
}

na.omit.POSIXt <- function(x) {
  i <- which(!is.na(x))
  x[i]
}

## Takes a crmp object and fills in the temporal gaps with NA values
## Makes some assumptions about time scale... supports daily or hourly data
## (based on results of crmp.get.observation.frequency)
## Returns a crmp object with gaps filled in
## Note that infilling can only _expand_ the range of time for the object
## If you specify start.date and end.date that are mutually exclusive from
## the object's date.range, this function will infill to the maximal extent
## possible
crmp.infill <- function(obj, start.date=NULL, end.date=NULL) {
  stopifnot(inherits(obj, 'crmp'))
  stopifnot( is.null(start.date) || inherits(start.date, 'POSIXt') )
  stopifnot( is.null(end.date) || inherits(end.date, 'POSIXt') )

  ## Infilling can only _expand_ the temporal range of the object... it cannot subset
  start.date <- min(start.date, crmp.get.start.date(obj))
  end.date <- max(end.date, crmp.get.end.date(obj))
  
  if (start.date > end.date)
    stop(start.date, ">", end.date)

  if (nrow(obj) <= 1)
    return(obj)

  ## If start and end dates are provided, prepend them if necessary and infill from/to them
  ## FIMXE: This makes the function kind of messy... this should be replaced
  ## by a robust implementation of rbind.crmp()
  if (!is.null(start.date) && start.date < crmp.get.start.date(obj)) {
    one.row <- as.data.frame(array(rep(NA, ncol(obj)), dim=c(1, ncol(obj))))
    names(one.row) <- names(obj)
    new.t <- slot(obj, 'time')
    zone <- attr(new.t, 'tzone')
    new.t <- c(start.date, new.t); attr(new.t, 'tzone') <- zone

    to.preserve <- c('.Data', 'time')
    to.copy <- setdiff(slotNames(obj), to.preserve)

    args <- append(list('crmp', rbind(one.row, slot(obj, '.Data')), time=new.t), slots(obj)[to.copy])
    obj <- do.call('new', args)
  }
  if (!is.null(end.date) && end.date > crmp.get.end.date(obj)) {
    one.row <- one.row <- as.data.frame(array(rep(NA, ncol(obj)), dim=c(1, ncol(obj))))
    names(one.row) <- names(obj)
    new.t <- slot(obj, 'time')
    zone <- attr(new.t, 'tzone')
    new.t <- c(new.t, end.date); attr(new.t, 'tzone') <- zone

    to.preserve <- c('.Data', 'time')
    to.copy <- setdiff(slotNames(obj), to.preserve)

    args <- append(list('crmp', rbind(slot(obj, '.Data'), one.row), time=new.t), slots(obj)[to.copy])
    obj <- do.call('new', args)
  }

  ## Find the requency and the units of our indicies
  freq <- crmp.get.observation.frequency(obj)
  units <- switch(as.character(freq),
                  '24'="days",
                  '1'="hours",
                  '0'=return(obj),
                  stop("Unhandleable frequency", freq)
                  )

  t0 <- trunc(crmp.get.start.date(obj), units)
  tn <- as.numeric(crmp.get.end.date(obj) - t0, units=units) + 1
  new.t <- na.omit(slot(obj, 'time')) # FIXME: there should _not_ be any NAs in POSIXct field

  ## Calculate the t indicies
  ti <- as.numeric(new.t - t0, units=units) + 1

  ## Transfer the data to the new vector
  rv <- data.frame(array(dim=c(tn, length(obj)), dimnames=list(NULL, names(obj)) ))
  t.na.i <- which(! is.na(slot(obj, 'time'))) # FIXME: again... fix this at the root level
  rv[ti,] <- obj[t.na.i,]
  rv <- new('crmp', rv,
            time=seq(t0, by=units, length.out=tn))

  ## Copy attributes
  to.preserve <- c('.Data', 'row.names', 'names', 'time')
  to.copy <- setdiff(slotNames(obj), to.preserve)
  slots(rv) <- slots(obj)[to.copy]
  rv
}

# Make a timeseries plot of temperature showing all of the zero observations in a different colour
# Plot the summer zeros in red and the rest of the zeros in blue
tzero.plot.crmp <- function(obj, ...) {
  x <- slot(obj, 'time'); y <- obj$PRES.TEMP
  s <- mkseas(x[which(y == 0)], width="DJF")
  title <- paste("Station:", slot(obj, "station.id"))
  plot(x, y, pch='.', ylab="Temperature (degrees C)", xlab="time", main=title)
  i <- which(s == "JJA")
  points(x[which(y == 0)][i], rep(0, length(i)), col="red", pch="o")
  i <- which(s != "JJA")
  points(x[which(y == 0)][i], rep(0, length(i)), col="blue", pch="o")
}

## Functions to find variable names (since we standardize only on the CF metadata)
# Takes a crmp data frame and returns the index to the variable in question (or logical() if it isn't found)
which.is.tmax <- function(obj) {
  standard.names <- slot(obj, "cf:standard_name")
  cell.methods   <- slot(obj, "cf:cell_method")
  return(which(standard.names == "air_temperature" & cell.methods == "time: maximum"))
}

which.is.tmin <- function(obj) {
  standard.names <- slot(obj, "cf:standard_name")
  cell.methods   <- slot(obj, "cf:cell_method")
  return(which(standard.names == "air_temperature" & cell.methods == "time: minimum"))
}

which.is.tmean <- function(obj) {
  standard.names <- slot(obj, "cf:standard_name")
  cell.methods   <- slot(obj, "cf:cell_method")
  return(which(standard.names == "air_temperature" &
               (cell.methods == "time: point" | cell.methods == "time: mean")))
}

which.is.precip <- function(obj) {
  standard.names <- slot(obj, "cf:standard_name")
  cell.methods   <- slot(obj, "cf:cell_method")
  return(which(standard.names == "lwe_thickness_of_precipitation_amount"))
}

# Input vector of booleans
# Returns a vector of integers representing the _length_ of each consequtive sequence of True values
sequential <- function(v) {
  if (! any(v, na.rm=T)) return(0)
  vect <- which(v)
  diff(which(c(T, diff(vect) != 1, T)))
}

# WMO "3 and 5 rule": No more than 3 consecutive and no more than 5 total missing entries from a monthly period
# Returns True if the timeseries is valid (i.e. if the about condition is _not_ met)
# Returns False if the timeseries fails the 3 and 5 rule (i.e. the above condition _is_ met)
# Note that the calling function is responsible for passing in only one month's worth of data...
# no validity checking is done on the part of this function
month.meets.3.5.rule <- function(x) {
  stopifnot(is.numeric(x) || is.logical(x))

  # Indicies of missing data
  i <- which(
             if (is.numeric(x)) is.na(x)
             else is.na(x | NA) #logical.. c(T, F, NA) | NA  == c(T, NA, NA)
             )
  
  if (length(i) > 5)
    return(F)

  # Represents the length of each sequence of missing data
  s <- diff(which(c(T, diff(i) != 1, T)))
  # Which sequenses match our length critereon
  matches <- which(s > 3)
  if (length(matches) > 0)
    return(F)
  else
    return(T)
}

# Returns the length of the longest subseries of x that meets the 3 and 5 rule
# x is a vector were FALSE represent missing data
# See page 86-87 of my notebook (8/31/2010) for diagrams of this algorithm
# Test it against the function maximal.3.5.rule.n.squared which is much slower, but much simpler to understand
# Algorithm runs in linear time (worst case... all holes) to constant time (best case... fewer holes)
# FIXME: What is the expected behaviour for a vector of ALL FALSE values?  Is it 0, or is it 3?
maximal.3.5.rule <- function(x, return.indicies=F) {
  stopifnot(is.logical(x))
  my.max <- 0
  best.i <- c(0,0)
  holes <- which((!x) | is.na(x))

  ## If there are 3 or less holes, this problem is very simple
  if (length(holes) <= 3) {
    if (return.indicies) {
      return(list(value=length(x), indicies=c(1, length(x))))
    }
    else return(length(x))
  }

  # Window initialization
  x0 <- 1
  holes <- c(holes, length(x) + 1) # The final "hole" is the end of the data... ensures that sequence of True's on the end are counted

  # Check each window of five holes
  # A window starts on index _after_ the previous hole (x0),
  # includes 5 holes (holes[1:k] k <=5), and stops _before_ the 6th hole
  # For each window, we pass the sequence of bools to longest.matching.series()
  # (which is ~ O(k), but k ~ constant) to get the maximum
  for (first.hole.i in seq(length(holes)-1)) {
 
    # No more than 6 holes, but no more holes than exist
    hi <- holes[first.hole.i : (first.hole.i + min(5, length(holes) - first.hole.i))] # indicies of holes to send
    hni <- hi[length(hi)] # index of last hole
    sub.x <- x[x0:hni] # the subset of x to send

    this.max <- longest.matching.series(sub.x, (hi-x0+1), T)
    ## Save it if it's better
    if (this.max$value >= my.max) {
      my.max <- this.max$value
      best.i <- x0 + this.max$indicies - 1
    }
    x0 <- hi[1] + 1 # Move up the beginning of the next window
  }
  # Return the results
  if (return.indicies)
    return(list(value=my.max, indicies=best.i))
  else
    return(my.max)
}

# Don't use this.  This is only a simplified (and much, much slower... O(n**2))
# implimentation of the function above.  Only to be used for verification
maximal.3.5.rule.n.squared <- function(x) {
  stopifnot(is.logical(x))
  n <- length(x)

  indicies <- expand.grid(i=seq(n), j=seq(n))

  fun <- function(index) {
    sub.x <- x[ index['i'] : index['j'] ]

    if (! more.than.three.in.a.row.missing(sub.x) &&
        length(which(!sub.x)) <= 5)
      return(length(sub.x))
    else
      return(min(3, length(sub.x)))
  }
  rv <- apply(indicies, 1, fun)
  max(rv)
}

# Caller is responsible for passing x as _only_ the values between the first break and the 6th break
# indicies parameter is a list of indicies of each (of the 5) break in the sequence plus the 6th break which ends it
longest.matching.series <- function(x, holes, return.indicies=F) {
  n <- length(holes)

  if (length(holes) == 6 || is.na(x[holes[n]]))
    holes[n] <- holes[n] - 1 # The 6th (or final) holes is not actually elligible for inclusion in a sequence

  indicies <- sapply(holes, seq, from=1)
  lengths <- unlist(lapply(indicies, function(i) {get.3.5.length(x[i])}))

  i <- which.max(lengths)

  if (return.indicies) {
    return(list(value=lengths[i], indicies=range(indicies[i])))
  } else {
    return(lengths[i])
  }
}

# This should only be called from functions where the sequence is guaranteed to already
# have no more than 5 total values missing (e.g. longest.matching.series)
get.3.5.length <- function(x) {
  if (more.than.three.in.a.row.missing(x))
    return(0)
  else
    return(length(x))
}

more.than.three.in.a.row.missing <- function(x) {
  stopifnot(is.logical(x))
  # Indicies of missing data
  i <- which((!x) | is.na(x))
  
  # Represents the length of each sequence of missing data
  s <- diff(which(c(T, diff(i) != 1, T)))
  # Which sequenses match our length critereon
  matches <- which(s > 3)
  if (length(matches) > 0)
    return(T)
  else
    return(F)
}

monthDays <- function (time) {
  time <- as.POSIXlt(time)
  time$mday[] <- time$sec[] <- time$min <- time$hour <- 0
  time$mon <- time$mon + 1
  return(as.POSIXlt(as.POSIXct(time))$mday)
}

## Calculating the normal code is three stages of the 3/5 rule
## 1. For hourly data, a single day must meet the 3/5 rule for all of their hourly observations
## 2. Months must meet the 3/5 rule (for their daily data) to qualify for stage 3
## 3. The monthly timeseries must pass the 3/5 rule for the normal period
## If variable name is omitted, it will calculated for where the station has temp and precip
## Handles two special variable names "ANY_TEMP" (i.e. tmean | (tmin ^ tmax))
## and "PRECIP_AND_ANY_TEMP" (i.e. (precip ^ (tmean | (tmin ^ tmax))))if (is.null(var.name))
## returns NA if infilling fails
normal.code.crmp <- function(obj, var.name="PRECIP_AND_ANY_TEMP", return.period=F) {
  stopifnot(inherits(obj, "crmp"), var.name %in% c(names(obj), 'ANY_TEMP', 'PRECIP_AND_ANY_TEMP'))

  if (nrow(obj) == 0) {
    if (return.period) return(list(code=NA, period=NA))
    else return(NA)
  }
  
  mk.date <- function(yyyymm, flag) {
    mday <- monthDays(as.POSIXlt(paste(yyyymm, "01", sep='/'), tz='GMT'))
    d <- if (flag == 'start') 1 else mday
    as.POSIXct(paste(yyyymm, d, sep='/'), tz='GMT')
  }

  ## Infill the gaps in the timeseries
  obs.freq <- crmp.get.observation.frequency(obj)
  filled <- try(crmp.infill(obj), silent=T)

  if (inherits(filled, 'try-error')) {
    warning("Could not infill data for station ", slot(obj, 'station.id'))
    return(NA)
  } else {
    obj <- filled
  }

  t <- slot(obj, 'time')

  ## Pick out where data is present
  data.present <-
    switch(var.name,
           ## has.temp.and.preceip returns a boolean, but month.meets.3.5.rule takes a numeric
           ## c(T, F) | NA -> c(T, NA)... as.numeric(c(T, NA)) -> c(1, NA)
           'PRECIP_AND_ANY_TEMP'= as.numeric(crmp.has.temp.and.precip(obj) | NA),
           'ANY_TEMP'=as.numeric(crmp.has.temp(obj) | NA),
           obj[,var.name]
           )

  if (all(is.na(data.present))) {
    if (return.period) {return(list(code=NA, period=NA))}
    else {return(NA)}
  }

  ## If hourly data, do a first pass of 3/5 rule for each day
  if (obs.freq == 1) {
    tf <- as.factor(format(t, "%Y/%m/%d"))
    qualifying.days <- tapply(data.present, tf, month.meets.3.5.rule)
    t <- as.POSIXct(levels(tf), tz="GMT")
  }
  else ## it's just daily data so _all_ of the days pass
    qualifying.days <- data.present

  ## Create a time-series factor of the months
  tf <- as.factor(format(t, "%Y/%m"))

  ## Do a two-step 3 and 5 rule
  ## Deterimine which months qualify and then find the maximal
  ## length of qualifying months
  qualifying.months <- tapply(qualifying.days, tf, month.meets.3.5.rule)
  qualifying.period <- maximal.3.5.rule(qualifying.months, return.indicies=T)
  my.max <- qualifying.period$value/ 12

  period <- mapply(mk.date, names(qualifying.months[qualifying.period$indicies]), c('start', 'end'), SIMPLIFY=F)
  names(period) <- c('start', 'end')

  ## Construct the return value
  normal.periods <- list(A=30, B=25, C=20, D=15, E=10, F=5, G=0)
  rv <- NA
  for (code in names(normal.periods)) {
    thresh <- normal.periods[[code]]
    if (my.max > thresh) {
      rv <- code; break
    }
  }

  if (return.period)
    rv <- list(code=rv, period=period)
  return(rv)
}

export <- function(obj, dir) {
  UseMethod("export")
}

export.crmp <- function(obj, dir, force=F) {
  fname <- paste(sep=".", slot(obj, "station.id"), "robject")
  print(paste("Exporting data from station", slot(obj, "station.id")))

  if (file.exists(fname))
    if (! force)
      stop(paste("Cannot export to file", fname, "because it already exists"))

  save(obj, file=file.path(dir, fname))
}

import.crmp <- function(filename) {
  load(filename)
  # It's not clear what the name of the variable was when it was exported
  # But there's only one other variable in this environment right now (filename)
  # just just use ls() and remove filename, and the object is whatever is left
  obj <- get(ls()[- which(ls() == "filename")])
  return(obj)
}

crmp.get.start.date <- function(obj) {
  stopifnot(inherits(obj, "crmp"))
  stopifnot(nrow(obj) > 0)
  min(slot(obj, 'time'), na.rm=T)
}

crmp.get.end.date <- function(obj) {
  stopifnot(inherits(obj, "crmp"))
  stopifnot(nrow(obj) > 0)
  max(slot(obj, 'time'), na.rm=T)
}

## Attempts to determine the frequency of observation
## (which is generally hourly or daily) by counting
## frequencies between timestamps and returning (in hours)
## the time difference with the highest frequency
## E.g. if t=1, 2, 3, 4, 28, then the function would return 1 (hourly)
## Returns 0 if there are no observations
crmp.get.observation.frequency <- function(obj) {
  stopifnot(inherits(obj, "crmp"))

  if (nrow(obj) <= 1)
    return(0)

  t <- sort(slot(obj, 'time'))
  dt <- as.numeric(diff(t), units='hours')
  frequencies <- tapply(dt, dt, length)
  i <- which.max(frequencies)
  return(as.numeric(names(frequencies)[[i]]))
}

## Depends on Dave's get.time.clusters() function
freq.from.clusters <- function(cluster, thresh=.70) {
  if (is.na(cluster)) return(NA)
  stopifnot(thresh > 0, thresh < 1)
  i <- which(cluster$weights > thresh)
  if (length(i > 0)) {
    rv <- switch(cluster$clusters[i],
                 `60`='1-minute', `120`='2-minute', `300`='5-minute', `900`='15-minute', `1800`='30-minute',
                 `3600`='1-hourly', `10800`='3-hourly', `21600`='6-hourly', `43200`='12-hourly', `86400`='daily',
                 'irregular')
  }
  else
    rv <- 'irregular'
  rv
}

## Assuming the potential for full temporal coverage, this function
## returns the percentage of observations present between the first
## and final observations (or start.date/end.date if given)
## Handles two special variable names "ANY_TEMP" (i.e. tmean | (tmin ^ tmax))
## and "PRECIP_AND_ANY_TEMP" (i.e. (precip ^ (tmean | (tmin ^ tmax))))
crmp.fraction.of.data.present <- function(obj, var.name='PRECIP_AND_ANY_TEMP', start.date=NULL, end.date=NULL) {
  stopifnot(inherits(obj, "crmp"))

  if (nrow(obj) == 0)
    return(0)

  ## Use start.date and end.date if available, otherwise use the first/last available
  if (is.null(start.date)) start.date <- crmp.get.start.date(obj)
  if (is.null(end.date))   end.date   <- crmp.get.end.date(obj)
  stopifnot(start.date <= end.date)

  filled <- try(crmp.infill(obj))
  if (inherits(filled, 'try-error')) {
    warning("Could not infill data for station ", slot(obj, 'station.id'))
    return(NA)
  } else {
    obj <- filled
  }
    
  possible.obs <- nrow(obj)
  i <- which(slot(obj, 'time') >= start.date & slot(obj, 'time') <= end.date)

  ## Pick out where data are present
  data.present <-
    switch(var.name,
           ## has.temp.and.preceip returns a boolean, but month.meets.3.5.rule takes a numeric
           ## c(T, F) | NA -> c(T, NA)... as.numeric(c(T, NA)) -> c(1, NA)
           'PRECIP_AND_ANY_TEMP'= crmp.has.temp.and.precip(obj[i,]),
           'ANY_TEMP'=crmp.has.temp(obj[i,]),
           ! is.na(obj[i,var.name])
           )

  num.obs <- length(which(data.present))
  num.obs / possible.obs
}

# Returns the maximum amount of time that had passed between recorded observations
# for a particular variable (var.name)
crmp.longest.data.gap <- function(obj, var.name='PRECIP_AND_ANY_TEMP') {
  stopifnot(inherits(obj, "crmp"))
  stopifnot(var.name %in% c('PRECIP_AND_ANY_TEMP', 'ANY_TEMP', names(obj)))
 
  ## Pick out where data are present
  data.present <-
    switch(var.name,
           ## has.temp.and.preceip returns a boolean, but month.meets.3.5.rule takes a numeric
           ## c(T, F) | NA -> c(T, NA)... as.numeric(c(T, NA)) -> c(1, NA)
           'PRECIP_AND_ANY_TEMP'= crmp.has.temp.and.precip(obj), ## FIXME: problem here 0110031.robject
           'ANY_TEMP'=crmp.has.temp(obj),
           ! is.na(obj[[var.name]])
           )
  
  if (!any(data.present))
    return(NA)

  i <- which(data.present)
  t <- sort(slot(obj, 'time')[i])
  if (length(t) < 2) return(NA)
  max(diff(t))
}

crmp.get.vars <- function(obj) {
  names(obj)
}

## Returns a boolean vector for the times at which the station has
## ((tmax ^ tmin) | tmean) ^ precip
crmp.has.temp.and.precip <- function(obj) {
  stopifnot(inherits(obj, "crmp"))
  vars <- c('tmax', 'tmin', 'tmean', 'precip')
  fs <- paste('which.is.', vars, sep='')
  indicies <- sapply(fs, do.call, list(obj))
  names(indicies) <- vars

  ## Replace the zero-length results with 0
  indicies <- sapply(indicies, function(x) {if (length(x) == 0) 0 else x})
  nvar <- sapply(indicies, length)

  ## If we don't have either precip, or some temp measurements, then the station
  ## effectively has no data
  if (!(((nvar['tmax'] && nvar['tmin']) || nvar['tmean']) && nvar['precip']))
    return(rep(F, nrow(obj)))
  if (nrow(obj) == 0)
    return(logical())

  ## Each effective variable can have zero, one or more than one variable which represents it
  calc.nas <- function(x) {
    if (length(x) == 1 && x == 0) rep(F, nrow(obj))
    else if (length(x) == 1) {! is.na(obj[,x,drop=F])}
    else apply(obj[,x], 1,
               function(y) {!(all(is.na(y)))}
               )
  }
  ## Not NA values
  nna <- sapply(indicies, calc.nas)
  dim(nna) <- c(nrow(obj), 4)
  colnames(nna) <- vars

  ## Must have precip and temp (either tmean or both t(min/max))
  return ( as.logical(((nna[,'tmax'] & nna[,'tmin']) | nna[,'tmean'] )
          &
          nna[,'precip'])
          )
}

## Returns a boolean vector for the times at which the station has
## ((tmax ^ tmin) | tmean)
## Since I have spent a lot of time rooting out bugs in crmp.has.temp.and.prec
## this uses a hack of just copying the object, filling in all precip with fake values
## and calling crmp.has.temp.and.precip.
crmp.has.temp <- function(obj) {
  stopifnot(inherits(obj, "crmp"))
  
  df <- as.data.frame(obj)
  i <- which.is.precip(obj)

  ## Either fill in _all_ of the precip with "data" since we only care about temp
  if (length(i) > 0) {
    df[,i] <- 0

    to.preserve <- c('.Data')
    to.copy <- setdiff(slotNames(obj), to.preserve)
    args <- append(list('crmp', df), slots(obj)[to.copy])
    obj <- do.call('new', args)
  } else { # OR create precip variable and add it to the object if it does not exist
    df$prec <- rep(0, nrow(obj))    
    i <- ncol(obj) + 1
    obj <- new('crmp', df,
               'cf:standard_name'= c(slot(obj, 'cf:standard_name'), 'lwe_thickness_of_precipitation_amount'),
               'cf:cell_method'=c(slot(obj, 'cf:cell_method'), 'time: sum'),
               time=slot(obj, 'time')
               )
  }  
  crmp.has.temp.and.precip(obj)
}

# Maximum liklihood fitting of normal distribution
# Takes as arguments a crmp object and the variable name
# Returns the KS statistic for comparison against a normal distribution

crmp.fit.normal <- function(obj, var.name, do.plot=T) {
  stopifnot(inherits(obj, 'crmp'), var.name %in% names(obj))
  print(paste("Trying to fit", var.name, "for station", slot(obj, 'station.id')))
  v <- na.omit(obj[[var.name]])
  i <- order(v)
  v <- v[i]
  if (length(v) <= 1) return(NA)
  fit <- fitdistr(v, 'normal')
  if (do.plot) {
    hist(v, freq=F, col=8)
    lines(v, dnorm(v, fit$estimate['mean'], fit$estimate['sd']), xlim=range(v))
    plot(ecdf(v))
    lines(ecdf(rnorm(length(v), fit$estimate['mean'], fit$estimate['sd'])), col='red')
  }
  rv <- try(ks.test(v, "pnorm", fit$estimate['mean'], fit$estimate['sd']))
  if (class(rv) != 'try-error') {rv$n <- length(v); return(rv)}
  else return(NA)
}

crmp.fit.gamma <- function(obj, var.name, do.plot=T) {
  stopifnot(inherits(obj, 'crmp'), var.name %in% names(obj))
  print(paste("Trying to fit a gamma distribution to variable", var.name, "for station", slot(obj, 'station.id')))
  v <- na.omit(obj[[var.name]])
  wet.days <- v[v > 1]
  i <- order(wet.days)
  wet.days <- wet.days[i]
  if (length(wet.days) <= 1) return(NA)
  fit <- try(fitdistr(wet.days, 'gamma'))
  if (inherits(fit, 'try-error')) return(NA)
  if (do.plot) {
    hist(wet.days, freq=F, col=8)
    lines(v, dgamma(v, fit$estimate['shape'], fit$estimate['rate']), xlim=range(v))
    plot(ecdf(wet.days))
    lines(ecdf(rgamma(length(wet.days), fit$estimate['shape'], fit$estimate['rate'])), col='red')
  }
  rv <- try(ks.test(wet.days, "pgamma", fit$estimate['shape'], fit$estimate['rate']))
  if (class(rv) != 'try-error') {rv$n <- length(wet.days); return(rv)}
  else return(NA)
}

## Convert strings to CF-1.4 naming conventions
## string must begin with a letter and be composed of letters, digits and underscores
## http://cf-pcmdi.llnl.gov/documents/cf-conventions/1.4/ch02s03.html
convert.to.cf.1.4.names <- function(string) {
  p <- "[^A-Za-z0-9_]"
  string <- gsub(p, '', string)
  p <- "^[^A-Za-z]*(.*)$"
  gsub(p, '\\1', string)
}

## the.map is a named list such as list(from.name="to.name", ...)
## any of "the.names" not found in the.map will be returned unchanged
map.names <- function(the.names, the.map) {
  replace.f <- function(x) {
    if (x %in% names(the.map)) the.map[[x]]
    else x
  }
  sapply(the.names, replace.f, USE.NAMES=F, simplify=F)
}

'[.crmp.by.time' <- function(x, i, j, drop=F) {

  if (missing(i) || missing(j))
    return(NextMethod('['))
  if (! (inherits(i, 'POSIXt') && inherits(j, 'POSIXt')))
    return(NextMethod('['))

  t <- slot(x, 'time')
  i <- which(t >= i & t <= j)
  x[i,,drop=drop]
}
setMethod('[', signature(x='crmp', i='POSIXt', j='POSIXt'), `[.crmp.by.time`)

'[.crmp' <- function(x, i, j, drop = if (missing(i)) TRUE else ncol(x) == 1) {

  df <- as.data.frame(x@.Data)[i, j, drop] ## returns data.frame

  if (drop && is.null(dim(df)))
    return(df)

  #### Subset the slots
  ## slots on the i axis
  t <- slot(x, 'time')
  rn <- slot(x, 'row.names')
  flags <- x@flags
  if (!missing(i)) {
    t <- t[i]; rn <- rn[i];
    flags <- lapply(X=flags, FUN=`[.flag.by.row`, i=i, drop = F)
  }
  ## slots on the j axis
  j.slot.names <- c('cf:units', 'cf:long_description', 'cf:cell_method', 'cf:standard_name', 'names')
  j.slots <- slots(x)[j.slot.names]
  if (!missing(j))
    j.slots <- lapply(j.slots, '[', i=j)

  to.preserve <- c('.Data', 'time', 'flags', j.slot.names)

  to.copy <- setdiff(slotNames(x), to.preserve)
  args <- append(append(list('crmp', df, time=t, flags=flags, row.names=rn), slots(x)[to.copy]), j.slots)
  do.call('new', args)
}
setMethod('[', signature(x='crmp', i='integer', j='integer'), `[.crmp`)
setMethod('[', signature(x='crmp', i='missing', j='integer'), `[.crmp`)
setMethod('[', signature(x='crmp', i='integer', j='missing'), `[.crmp`)

'[.crmp.by.name' <- function(x, i, j, drop = if (missing(i)) TRUE else ncol(x) == 1) {
  if (missing(i))
    i <- 1:nrow(x)
  j <- sapply(j, function(n) {which(n == names(x))})
  x[i, j, drop=drop]
}
setMethod('[', signature(x='crmp', i='integer', j='character'), `[.crmp.by.name`)
setMethod('[', signature(x='crmp', i='missing', j='character'), `[.crmp.by.name`)

is.na.crmp <- function(x) {
  is.na(as.data.frame(x))
}

## setMethod('[', signature(x='crmp', i='ANY', j='missing'),
##           function(x, i, j, ..., drop=NA) {
##             print("BOGUS")
##             ##blah <- callNextMethod()
##             return("FUCK YOU")
##           }
##           )

## `[[.crmp` <- function(x, i, j, ..., drop=NA) {
##             print("BOGUS")
##             #callNextMethod()
##           }
