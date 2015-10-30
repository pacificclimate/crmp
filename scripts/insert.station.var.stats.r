library(crmp)
library(parallel)
library(RPostgreSQL)

options(mc.cores=16)

# q should be a query by which one can obtain the (database) station_ids for which to calculate stats
run.it <- function(con=dbConnect('PostgreSQL', 'crmp'), q='SELECT DISTINCT station_id FROM obs_raw NATURAL JOIN meta_history') {

  ## Get all of the station ids for which we have observations
  print(q)
  res <- dbSendQuery(con, q)
  stns <- fetch(res, -1)[,'station_id']

  print("Starting calculations")
  rv <- mclapply(stns, iterate.wrapper)

  print(paste("Inserted a total of", sum(unlist(rv)), "rows"))
}

iterate.wrapper <- function(db.stn.id) {
  ## Kind of bad to start a new connection in the function... but if we're running multithreaded we can't use the same connection
  con <- dbConnect('PostgreSQL', 'crmp')
  y <- try(load.stn.and.iterate(con, db.stn.id))
  dbDisconnect(con)  
  if (inherits(y, 'try-error')) 0 else y
}

load.stn.and.iterate <- function(con, db.stn.id) {
  obj <- read.sql.crmp(con, db.stn.id)
  periods <- c('all', 'best', '71-00')
  vars <- c('temp', 'both', names(obj))

  ## Call calc.and.insert.stn.info for every period/variable combo
  args <- expand.grid(period=periods, param=vars)
  n.rows.affected <- apply(args, 1, function(x) {
    data.insert <- try(do.call(calc.and.insert.stn.info, append(as.list(x), list(obj=obj, db.stn.id=db.stn.id), after=0)))

    if (inherits(data.insert, 'try-error')) {
      print(paste('Failed to calculate stats for db_id:', db.stn.id, paste(x, collapse=':'))); return(0)
    } else {
      q <- paste('INSERT INTO stats_station_var', as(data.insert, 'character'))
      print(q)

      res <- try(dbSendQuery(con, q), silent=T)
      if (inherits(res, 'try-error'))
        affected.rows <- 0
      else 
        affected.rows <- dbGetRowsAffected(res)
      return(affected.rows)
    }
  })
  if (! is.numeric(n.rows.affected)) {
    print(paste("ERROR ERROR ERROR", n.rows.affected))
    return(0)
  }
  print(paste("Inserted", sum(n.rows.affected, na.rm=T), "for station", obj@network, obj@station.id, obj@station.name))

  return(sum(n.rows.affected, na.rm=T))
}

## period can be "all"|"best"|"71-00"
## + all calculates info over the entire record of the station
## + best calculates info over the maximal period which meets the 3/5 rule (i.e. the station's normal period)
## + 71-00 clips the station record to 1971-2000
## param can be "temp"|"both"|[any station variable]
calc.and.insert.stn.info <- function(obj, db.stn.id, period="best", param="both") {

  print(paste("Calculating station stats for station:", obj@network, obj@station.id, obj@station.name, period, param))

  var.name <- switch(param,
                     temp="ANY_TEMP",
                     both="PRECIP_AND_ANY_TEMP",
                     if (param %in% names(obj)) param else stop("param parameter must be one of temp|both|[any station var].  Instead got", param)
                     )

  subset <- try(
  if (period == '71-00') {
    start.date <- as.POSIXct("1971/01/01", tz="GMT")
    end.date <- as.POSIXct("2001/01/01", tz="GMT")
    subset <- obj[start.date, end.date, drop=F]
    crmp.infill(subset, start.date=start.date, end.date=end.date)
  } else if (period == 'best') {

    ## This could fail in which case it's caught above, but we won't be able
    ## to really say anything about this station's "best" period
    norm.rv <- normal.code.crmp(obj, var.name, return.period=T)
    if (is.na(norm.rv$code)) stop("Could not find the best normal period")
    start.date <- norm.rv$period$start
    end.date <- norm.rv$period$end
    obj[start.date, end.date]
 
  } else if (period == 'all') {
    obj
  } else {
    stop("period parameter must be one of all|best|71-00. Instead got", period)
  })
  ## FIXME: If this fails, we'll just end up inserting a blank line (with keys).  Is that OK?
  if (inherits(subset, 'try-error')) warning("F'ed upedness:", obj@network, obj@station.id, obj@station.name, period, param)

  ks.stat <- try(ks.test.one.var(subset, var.name), silent=T)

  vlist <- list(
                'wmo_code'            = try(normal.code.crmp(subset, var.name, return.period=F), silent=T),
                'ks_significant'      = try(is.significant(ks.stat), silent=T),
                'ks_stat'             = try(ks.stat[['statistic']], silent=T),
                'fractional_coverage' = try(crmp.fraction.of.data.present(subset, var.name), silent=T),
                'longest_gap_days'    = try(as.numeric(crmp.longest.data.gap(subset, var.name), units='days'), silent=T)
                )

  values <- lapply(vlist, function(x) if (inherits(x, 'try-error')) as.numeric(NA) else x)
  values <- as.data.frame(values, stringsAsFactors=F)

  ## Insert is slightly different for composite vars like ANY_TEMP and a regular station var
  ## table requires either a valid vars_id or one of (ANY_TEMP|PRECIP_AND_ANY_TEMP)
  if (var.name %in% c('ANY_TEMP','PRECIP_AND_ANY_TEMP')) {
    keys <- data.frame(stringsAsFactors=F, 'station_id'=as.numeric(db.stn.id), 'other_var'=var.name, 'time_period'=period)
    di <- new('dataInsert', cbind(keys, values))
  }
  else {
    vars.sql <- paste(sep='', '(SELECT vars_id FROM meta_station NATURAL JOIN meta_network NATURAL JOIN meta_vars WHERE net_var_name = \'', param, '\' AND station_id = ', db.stn.id, ')'); attr(vars.sql, 'verbatim') <- T

    keys <- data.frame(stringsAsFactors=F, 'station_id'=as.numeric(db.stn.id), 'vars_id'=vars.sql, 'time_period'=period)
    di <- new('dataInsert', cbind(keys, values))
  }
  flush(stdout())
  return(di)
}

is.subset <- function(x, y)
  all(intersect(x, y) == x)

## args <- commandArgs(trailingOnly=T)
## for (a in args) {
##   print(a)
##   eval(parse(text=a))
## }

## required.args <- c('cache.dir', 'period', 'param')
## semi.optional.args <- c('output.dir', 'output.file') # One (and only one) must be provided

## if ( ! is.subset(required.args, ls())) {
##   stop("Did not receive all of the required args:", paste(collapse="|", required.args))
## }

## if (!exists('output.dir'))
##   output.dir <- NULL
## if (!exists('output.file'))
##   output.file <- NULL

