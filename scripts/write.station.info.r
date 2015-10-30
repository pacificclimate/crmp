library(parallel)
options(mc.cores=8)
source('../crmp-class.r', chdir=T)

run.it <- function(cache.dir, output.dir=NULL, output.file=NULL, period="best", param="both") {

  ## Must provide either output.dir or output.file (full path), but not both
  stopifnot(xor(is.null(output.dir), is.null(output.file)))
  
  files <- list.files(cache.dir, pattern="robject$", full.names=T)

  summary <- list()
  for (f in files) {
    summary[[f]] <- tryCatch(get.stn.info(f, period, param),
                             error = paste("Failed on file", f),
                             finally = gc()
                             )
  }

  ## We're creating a table, and if our output isn't table-like, then we have problems
  n <- unique(lapply(summary, length))
  if (length(n) != 1) {
    print("Some calls to get.stn.info returned different lengths")
    browser()
  }

  for (name in names(summary[[1]])) {
    if (grepl("date", name))
      assign(name, sapply(lapply(summary, '[[', name), as.numeric))
    else if (grepl("vars", name))
      assign(name, unlist(lapply(lapply(summary, '[[', 'vars'), paste, collapse=' ')))
    else
      assign(name, unlist(lapply(summary, '[[', name), recursive=F))
  }

  ## Unlisting manages to break the date classes
  attributes(start.date)          <- list(class="POSIXct", tzone="GMT")
  attributes(end.date)            <- list(class="POSIXct", tzone="GMT")

  for (v in names(summary[[1]])) {
    print(paste(v, length(get(v))))
  }

  df <- data.frame(station.id=station.id,
                   network=network,
                   start.date=strftime(start.date, tz="GMT"),
                   end.date=strftime(end.date, tz="GMT"),
                   fractional.coverage=fractional.coverage,
                   frequency=frequency,
                   wmo.code=wmo.code,
                   vars=vars)

  if (is.null(output.file))
    output.file <- file.path(output.dir, paste('station.metadata', period, param, 'csv', sep='.'))
  
  if (!file.exists(dirname(output.file)))
    dir.create(dirname(output.file), recursive=T)

  write.csv(df, output.file, row.names=F)
}

error.rv <- function(obj) {  
  rv <- append(as.list(c(attr(obj, 'station.id'), attr(obj, 'network'), rep(NA, 5))), list(crmp.get.vars(obj), NA))
  names(rv) <- c('station.id', 'network', 'start.date', 'end.date', 'fractional.coverage',
                 'longest.gap', 'frequency', 'vars', 'wmo.code')
  rv
}

## period can be "all"|"best"|"71-00"
## + all calculates info over the entire record of the station
## + best calculates info over the maximal period which meets the 3/5 rule (i.e. the station's normal period)
## + 71-00 clips the station record to 1971-2000
## param can be "temp"|"prec"|"both"
get.stn.info <- function(file, period="best", param="both") {
  print(paste("Importing:", file))
  obj <- import.crmp(file)
  freq <- crmp.get.observation.frequency(obj)
  if (! freq %in% c(1, 24)) return(error.rv(obj))

  var.name <- switch(param,
                     temp="ANY_TEMP",
                     both="PRECIP_AND_ANY_TEMP",
                     prec= if(length(i <- which.is.precip(obj)) == 0) return(error.rv(obj)) else names(obj)[i][1],
                     stop("param parameter must be one of temp|prec|both.  Instead got", param)
                     )

  if (period == '71-00') {
    start.date <- as.POSIXct("1971/01/01", tz="GMT")
    end.date <- as.POSIXct("2001/01/01", tz="GMT")
    obj <- obj[start.date, end.date, drop=F]
    obj <- crmp.infill(obj, start.date=start.date, end.date=end.date)
  } else if (period == 'best') {
    norm.rv <- normal.code.crmp(obj, var.name, return.period=T)
    if (is.na(norm.rv$code)) return(error.rv(obj))
    start.date <- norm.rv$period$start
    end.date <- norm.rv$period$end
    obj <- obj[start.date, end.date]
  } else if (period == 'all') {
    start.date <- crmp.get.start.date(obj)
    end.date <- crmp.get.end.date(obj)
  } else {
    stop("period parameter must be one of all|best|71-00. Instead got", period)
  }

  norm.rv <- normal.code.crmp(obj, var.name, return.period=F)
  if (is.na(norm.rv)) return(error.rv(obj))

  rv <- list(
              station.id=attr(obj, "station.id"),
              network=attr(obj, "network"),
              start.date=start.date,
              end.date=end.date,
              fractional.coverage=crmp.fraction.of.data.present(obj, var.name),
              longest.gap=as.numeric(crmp.longest.data.gap(obj, var.name), units='days'),
              frequency=freq,
              vars=crmp.get.vars(obj),
              wmo.code=norm.rv
             )
  rm(obj); gc()
  rv
}

is.subset <- function(x, y)
  all(intersect(x, y) == x)

args <- commandArgs(trailingOnly=T)
for (a in args) {
  print(a)
  eval(parse(text=a))
}

required.args <- c('cache.dir', 'period', 'param')
semi.optional.args <- c('output.dir', 'output.file') # One (and only one) must be provided

if ( ! is.subset(required.args, ls())) {
  stop("Did not receive all of the required args:", paste(collapse="|", required.args))
}

if (!exists('output.dir'))
  output.dir <- NULL
if (!exists('output.file'))
  output.file <- NULL

run.it(cache.dir, output.dir=output.dir, output.file=output.file, period=period, param=param)
