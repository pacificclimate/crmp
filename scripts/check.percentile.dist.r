## Script for Dave Rodenhuis
## The intent is to fit a distribution (normal for temperature, gamma for precip on "wet days") for the observations
## at a single station and flag stations which have empirical distributions that are significantly different from
## their fitted distributions.

is.subset <- function(x, y)
  all(intersect(x, y) == x)

args <- commandArgs(trailingOnly=T)
for (a in args) {
  print(a)
  eval(parse(text=a))
}

required.args <- c('cache.dir', 'plot.dir', 'metadata.file')

if(! is.subset(required.args, ls())) {
  stop("Did not receive all of the required args:", paste(collapse="|", required.args))
}

source(chdir=T, '../crmp.load.data.r')
source(chdir=T, '../dist.check.r')
options(cores=4)

meta <- read.csv(metadata.file)
files <- file.path(cache.dir, paste(meta$stnid[meta$network == 'EC'], '.robject', sep=''))
files <- files[file.exists(files)]

## Main function
## Runs for either temp or precip and iterates over each cached station and calculates the ks stat for
## each matching variable in each station

do.it <- function(ks.func, fit.func, var.label='Temp') {
  rv <- mclapply(files, ks.func)
  rv <- unlist(rv, recursive=F)

  significant <- na.omit(unlist(lapply(rv, is.significant)))
  ks.stat <- unlist(lapply(rv, function(x) {if (inherits(x, 'htest')) x$statistic else NULL}))

  close.to.95 <- abs(sum(significant) / length(significant) - .95)
  #stopifnot(close.to.95 < .02) # Should be within 2% of the 95% conf interval

  ## Plot the distribution of KS statistics for all stations
  if (! file.exists(plot.dir))
    dir.create(recursive=T, file.path(plot.dir, c('EC', 'crmp')))

  plot.file <- paste('ks.test.', var.label, '.pdf', sep='')
  pdf(file.path(plot.dir, 'EC', plot.file))
  plot(ecdf(ks.stat), main=paste("CDF of KS-test results from fitting a distribution to", var.label), xlab="KS value", ylab="P")
  dev.off()

  dubious.stations <- which(! significant)
  for (stn in dubious.stations) {
    name <- names(significant)[stn]
    stn.id <- strsplit(name, '.', fixed=T)[[1]][1]
    var.name <- strsplit(name, '.', fixed=T)[[1]][2]
    file <- file.path(cache.dir, paste(stn.id, '.robject', sep=''))
    obj <- import.crmp(file)

    pdf(file.path(plot.dir, 'EC', paste(sep='.', stn.id, var.label, 'pdf')))
    plot(obj, var.name)
    fit.func(obj, var.name, T)
    dev.off()
  }

  ## Now do _all_ of the stations
  files <- list.files(cache.dir, pattern='robject$', full.name=T)
  rv <- mclapply(files, ks.func)
  rv <- unlist(rv, recursive=F)

  significant <- na.omit(unlist(lapply(rv, is.significant)))
  ks.stat <- unlist(lapply(rv, function(x) {if (inherits(x, 'htest')) x$statistic else NULL}))

  pdf(file.path(plot.dir, 'crmp', plot.file))
  plot(ecdf(ks.stat), main=paste("CDF of KS-test results from fitting a distribution to", var.label), xlab="KS value", ylab="P")
  dev.off()

  for (i in 1:length(rv)) {
    if (is.na(rv[[i]]) || is.na(is.significant(rv[[i]])))
      next
    name <- names(rv)[i]
    stn.id <- strsplit(name, '.', fixed=T)[[1]][1]
    var.name <- strsplit(name, '.', fixed=T)[[1]][2]
    file <- file.path(cache.dir, paste(stn.id, '.robject', sep=''))
    obj <- import.crmp(file)

    subdir <- if(is.significant(rv[[i]])) 'significant' else 'dubious'
    pdf(file.path(plot.dir, 'crmp', subdir, paste(sep='.', stn.id, var.label, 'pdf')))
    plot(obj, var.name)
    fit.func(obj, var.name, T)
    dev.off()
  }
}

mapply(do.it,
       list(ks.test.station.precip, ks.test.station.temperature),
       list(crmp.fit.gamma, crmp.fit.normal),
       list("Precip", "Temp")
       )
