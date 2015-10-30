source("qa.qc1.r")
source("crmp.load.data.r")

library(Cairo)
library(multicore)
options(cores=8)

saved.dir <- "/home/hiebert/interim_data/cam/netcdfs"
plot.dir <- "/home/hiebert/interim_data/cam/qc.all.data-2010.09.08"
qc.dir <- file.path(saved.dir, "qc")
failures <- list()

files <- list.files(path=saved.dir, pattern="robject$", full.names=T)
thresh <- get.variable.thresholds()

run.it <- function() {
  mclapply(files, f)
}

# Returns True on success, False on failure and NA if the file was skipped
f <- function(filename) {
  result <-
    try(
        {
          data <- import.crmp(filename)
          stn.id <- attr(data, "station.id")

          #if (file.exists(file.path(qc.dir, paste(sep=".", stn.id, "robject")))) {
          #  return(NA)
          #}

          subplot.dir <- file.path(plot.dir, stn.id)

          # If we've already done the qc on this one, please skip it
          if (file.exists(subplot.dir))
            return(NA)
          else {
            print(paste("Processing data from station", stn.id))
            dir.create(subplot.dir, recursive=T)
          }

          qc.data <- try(crmp.qc1(data, thresh, subplot.dir))
          if (inherits(qc.data, "try-error"))
            return(F)

          plot.file <- file.path(subplot.dir, paste(sep=".", "boxplot", stn.id, "png"))
          CairoPNG(file=plot.file, width=10, height=10, dpi=100, units="in")
          boxplot(qc.data)
          dev.off()

          plot.file <- file.path(subplot.dir, paste(sep=".", "histogram", stn.id, "png"))
          CairoPNG(file=plot.file, width=10, height=10, dpi=100, units="in")
          hist(qc.data)
          dev.off()

          plot.file <- file.path(subplot.dir, paste(sep=".", "time-series", stn.id, "png"))
          CairoPNG(file=plot.file, width=10, height=10, dpi=100, units="in")
          plot(qc.data)
          dev.off()
          export(qc.data, qc.dir)

          # Everything seemed to work out OK.  Return True
          return(T)
        }, silent=T # try block
        )

  if (inherits(result, "try-error"))
    return(F)
}
