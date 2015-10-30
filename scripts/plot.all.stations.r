library(multicore); options(cores=8)
library(Cairo)

source('../crmp.load.data.r')

run.it <- function(cache.dir, plots.dir) {
  files <- list.files(cache.dir, pattern="robject$", full.names=T)
  mclapply(files, function(file) {obj <- import.crmp(file); do.station.plots(obj, plots.dir)})
}

do.station.plots <- function(crmp.frame, plots.dir) {
  stopifnot(inherits(crmp.frame, "crmp"))
  plot.functions <- list(timeseries=plot, boxplot=boxplot, histogram=hist)
  stn.id <- attr(crmp.frame, 'station.id')
  print(paste("Plotting station", stn.id))

  for (plot.type in names(plot.functions)) {
    fun <- plot.functions[[plot.type]]

    # Ensure that the directory exists
    subplot.dir <- file.path(plots.dir, plot.type)
    if (! file.exists(subplot.dir))
      dir.create(recursive=T, subplot.dir)

    # Example plot.file: plots.dir/timeseries/mtwells.timeseries.png
    plot.file <- file.path(subplot.dir, paste(sep=".", stn.id, plot.type, "png"))

    CairoPNG(file=plot.file, width=10, height=10, dpi=100, units="in")
    the.try <- try(fun(crmp.frame))
    if (inherits(the.try, 'try-error')) {
      warning("Error plotting", plot.type, "for station", stn.id)
    }
    dev.off()
  }
}
