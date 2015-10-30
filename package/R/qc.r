library(seas)

crmp.qc1 <-function(data, thresh, plot.dir="/tmp") {
  if (! inherits(data, "crmp"))
    stop("Type error: data frame must be of class \"crmp\"")

  print(paste("Performing Quality Control on station:", slot(data, "station.id")))
  # Feel free to add additional procedures here, once they have been coded
  procedures <- list(#list(fun="qc.replace.na.values", name="Replace NAs", args=NULL),
                     list(fun="qc.filter.by.threshold", name="Filter by threshold", args=list(thresh)),
                     list(fun="qc.filter.diurnal.temp.range", name="Filter by large diurnal temperature", args=NULL),
                     list(fun="qc.filter.bad.tmin.tmax", name="Filter because Tmin > TMax", args=NULL),
                     list(fun="qc.filter.persistant", name="Filter because of persistant values", args=NULL),
                     list(fun="qc.filter.summer.zeros", name="Filter because T = 0 during the summer", args=NULL)
                     )
  if (! is.null(plot.dir)) {
    out.file <- file.path(plot.dir, paste(sep="-", slot(data, "station.id"), "qc.png"))
  }

  # Initialize the return value
  # If for some reason _all_ QC procedures fail, the data will be returned unaltered
  qc.data <- qc.replace.na.values(data)

  for (proc in procedures) {
    print(paste("--step", proc$name))

    args <- c(list(qc.data), proc$args)
    qc.flags <- try(do.call(proc$fun, args), silent=T)

    if (inherits(qc.flags, 'try-error')) {
      warning(paste(proc$fun, "failed on station", slot(qc.data, "station.id")))
      next
    }

    if (! is.null(plot.dir)) {
      out.file <- file.path(plot.dir, paste(sep="-", slot(qc.data, "station.id"), proc$fun, "%d.png"))
      png(out.file, width=960, height=960)
      plot.filter.results(qc.data, qc.flags, main=paste("Results of QA/QC operation", proc$name))
      dev.off()
    }
    
    slot(qc.data, 'flags') <- append(slot(qc.data, 'flags'), list(qc.flags))

  }
  print(paste("--finished QC.  Now plotting"))
  if (! is.null(plot.dir)) {
    png(file.path(plot.dir, paste(sep="-", slot(qc.data, "station.id"), "final.qc.results.png")), width=960, height=960)
    plot(qc.data)
    dev.off()
  }
  print("--finished plotting and am returning the results")
  return(qc.data)
}

qc.replace.na.values <- function(frame, na.values=c(-99, -6999, 6999)) {
  stopifnot(inherits(frame, "crmp"))

  df <- as(frame, 'data.frame')
  m <- as(df, 'matrix') # %in% doesn't work very well for data.frames
  m[m %in% na.values] <- NA
  df <- as.data.frame(m)

  to.preserve <- c('.Data')
  to.copy <- setdiff(slotNames(frame), to.preserve)
  args <- append(list('crmp', df), slots(frame)[to.copy])

  do.call(new, args)
}

qc.filter.diurnal.temp.range <- function(frame, max.range=60) {
  stopifnot(inherits(frame, "crmp"))

  tmin.i <- which.is.tmin(frame); tmax.i <- which.is.tmax(frame)
  if (length(tmin.i) != 1 || length(tmax.i) != 1) {
    warning("Skipping the diurnal.temp.range filter because both tmax and tmin are not available")
    return(new('flag', qc.operation='diurnal.temp.range'))
  }

  rv <- rep(FALSE, nrow(frame))
  failures <- which(abs(frame[,tmax.i] - frame[,tmin.i]) > max.range)
  if (! is.null(dim(failures)))
    rv[failures] <- TRUE
  new('flag', data.frame(rv), qc.operation='diurnal.temp.range', applicable.vars=names(frame)[c(tmin.i, tmax.i)])
}

qc.filter.bad.tmin.tmax <- function(frame) {
  stopifnot(inherits(frame, "crmp"))

  tmin.i <- which.is.tmin(frame); tmax.i <- which.is.tmax(frame)
  if (length(tmin.i) != 1 || length(tmax.i) != 1) {
    warning("Skipping the bad.tmin.max filter because both tmax and tmin are not available")
    return(new('flag', qc.operation='bad.tmin.tmax'))
  }

  rv <- rep(FALSE, nrow(frame))
  failures <- which(frame[,tmin.i] > frame[,tmax.i])
  if (! is.null(dim(failures)))
    rv[failures] <- TRUE
  new('flag', data.frame(rv), qc.operation='bad.tmin.tmax', applicable.vars=names(frame)[c(tmin.i, tmax.i)])
}

qc.filter.by.threshold <- function(frame, thresh=NULL) {
  stopifnot(inherits(frame, "crmp"))
  if (is.null(thresh)) {
    data(thresholds)
    thresh <- thresholds
  }
  thresh <- thresh[slot(frame, "cf:standard_name"),]
  failures <- mapply(function(x, mn, mx) {x <- as.numeric(x); x < mn | x > mx},
                     frame, thresh[,'min.valid'], thresh[,'max.valid'], SIMPLIFY=T)
  failures <- failures

  if (! is.null(dim(failures))) {
    new('flag',
        as.data.frame(failures),
        qc.operation='threshold',
        qc.args=list(thresholds=thresh),
        applicable.vars=names(frame)
        )
  }
  else
    new('flag',
        qc.operation='threshold',
        qc.args=list(thresholds=thresh),
        applicable.vars=names(frame)
        )
}

## If redline.length is NULL, the function looks up per-variable
## persistance values from the variable definition files
## Otherwise if redline.length is of length one it just repeats it n times
## FIXME: Should not apply to max_* variables for hourly observations
qc.filter.persistant <- function(frame, redline.length=NULL) {
  stopifnot(inherits(frame, "crmp"))

  if (is.null(redline.length)) {
    data(thresholds)
    p.values <- thresholds[,'persist',drop=F]
  } else if (length(redline.length) == 1) {
    p.values <- rep(redline.length, length(frame) - 1)
  } else {
    p.values <- redline.length
  }
  p.values <- rbind(p.values, POSIXct=Inf)

  persistance <- mapply(detect.redlines, frame, p.values[slot(frame, 'cf:standard_name'),])

  ## Initialize the return object with all False
  flags <- replace(frame, 1:length(frame), F)

  for (n in names(persistance)) {
    i <- persistance[[n]]

    ## persistance checking should not apply to where precip/wind speed = 0
    j <- which(n == names(frame))
    cf.sn <- slot(frame, "cf:standard_name")[j]

    if (grepl("(precipitation_amount|wind_speed)", cf.sn) &&
        length(i) > 0
        ) {
      non.zeros <- which(frame[i, n] != 0)
      i <- i[non.zeros]
    }

    if (length(i) > 0) {
      flags[i, n] <- T
    }
  }
  new('flag', flags,
      qc.operation='persistant',
      qc.args=list(redline.length=redline.length),
      applicable.vars=names(frame)
      )
}

qc.filter.summer.zeros <- function(frame) {
  stopifnot(inherits(frame, "crmp"))
  temp.i <- c(which.is.tmean(frame), which.is.tmax(frame), which.is.tmin(frame))
  if (length(temp.i) < 1) {
    warning("Skipping qc.filter.summer.zeros because data frame doesn't have temp variable available")
    return(new('flag', qc.operation='summer.zeros'))
  }
  flags <- data.frame(array(FALSE, dim=c(nrow(frame), length(temp.i))))
  names(flags) <- names(frame)[temp.i]

  for (ti in temp.i) {
    x <- data.frame(date=slot(frame, 'time'))
    y <- frame[,ti]
    s <- mkseas(x, width="DJF")
    i <- which(s == "JJA" & y == 0)
    if (length(i) > 0)
      flags[i, ti] <- TRUE
  }
  new('flag',
      flags,
      qc.operation='summer.zeros',
      applicable.vars=names(frame)[temp.i]
      )
}

# This function searches a vector for sequences where the value remains the same
# It returns the indicies corresponding to values for which there is a sequence of length greater
# than len where the value remains the same
# This is intended for finding bad sensor data where a sensor is not function and/or the detected
# quanitity is out of range of the sensor limits.
detect.redlines <- function(v, len=20) {
  # Differences between each measurement
  d <- diff(v)
  # Indices of repeat measurements
  i <- which(d == 0)
  # Represents the length-1 of each sequence of repeat meausurements
  s <- diff(which(c(T, diff(i) != 1, T)))
  # Which sequenses match our length critereon
  matches <- which(s >= len)

  if (length(matches) == 0) {
    return(NULL)
  }
  # Use this as an index into our indices of repeat measurements
  match.i <- sapply(matches, function (m) {sum(s[1:m-1]) + 1})
  lengths <- s[matches]
  # And finally return the indicies which correspond to the redline-ed sequenses
  unlist(mapply(seq, i[match.i], i[match.i] + lengths, SIMPLIFY=F ))
}

get.filtered.points <- function(flags) {
  if (length(flags) == 0) logical()
  else
    which(flags == T, arr.ind=T)
}

get.filtered.points.factor <- function(flags) {
  if (length(flags) == 0) as.factor(NULL)
  else
    as.factor(flags == T)
}

plot.filter.results <- function(orig, flags, col="red", main="Results of QA/QC operations", sub=NULL, plot.dev.call=NULL, ...) {
  stopifnot(inherits(orig, "crmp"), inherits(flags, "flag"))

  # If a call object is passed in by plot.dev.call, the device call is made before each plot and turned off after each plot
  filtered <- get.filtered.points(flags)

  if (length(filtered) == 0) return(NULL)

  t <- slot(orig, 'time')[filtered[,"row"]]

  # Set up a data frame to which we can plot changes for each variable
  values <- apply(filtered, 1, function(i) {orig[i[1], i[2]]})
  data <- cbind(filtered, value=values)
  dimnames(data) <- list(NULL, c("", "var.id", "value"))
  data <- as.data.frame(data, stringsAsFactors=F)
  data$t <- t

  if (is.null(sub)) {
    sub <- paste("Station", slot(orig, "station.id"))
  }

  do.plot <- function(subframe) {
    var.name <- names(orig)[as.numeric(subframe$var.id[1])]
    if (! var.name %in% slot(flags, 'applicable.vars')) return

    if (is.call(plot.dev.call)) eval(plot.dev.call)

    plot(slot(orig, 'time'), orig[[var.name]], xlab="time", ylab=var.name, main=main, sub=sub, ...)
    points(subframe$t, subframe$value, col=col, ...)

    if (is.call(plot.dev.call)) dev.off()
  }

  by(data, data$var.id, do.plot)
  NULL
}

