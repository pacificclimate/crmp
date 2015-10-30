source("../crmp.load.data.r", chdir=T)
source("../qa.qc1.r", chdir=T)

library(parallel)
options(mc.cores=8)

args <- commandArgs(trailingOnly=T)
for (a in args) {
  print(a)
  eval(parse(text=a))
}

required.args <- c("data.dir", "out.dir")

if(length(intersect(required.args, ls())) == length(required.args)) {

  map <- c("mofr", "MoFR/access_export_removed_dead_stations",
           "mofr.research", "MoFR/Research_Stations",
           "moe", "MoE",
           "mot", "MoT/wx_data",
           "rta", "RioTintoAlcan",
           "moag", "MoAg",
           "bch", "BCH_STATIONS/new_data",
           "snowpillow", "SNOWPILLOW_PCIC_V2/new_data",
           "ec", "CDCD_2007/new_data",
           "motie","data/MoTI/data/electronic",
           "motim","data/MoTI/data/manual")
  map <- array(map, dim=c(2, length(map) / 2), dimnames=list(c("abbv", "path"), NULL))

  for(i in 1:ncol(map)) {
    dir <- paste(map['abbv', i], 'data.dir', sep='.')
    assign(dir, file.path(data.dir, map['path', i]))
  }

  if (! file.exists(out.dir))
    dir.create(out.dir, recursive=T)

  ## Load EC data from BC stations
  canada.stns <- list.stations.BCH(ec.data.dir)
  bc.stns <- read.csv(file.path(ec.data.dir, "../bc_stnlist.csv"))[,'stnid']
  stns <- intersect(canada.stns, bc.stns)

  chunk.size <- 500
  i.min <- 1
  while (i.min < length(stns)) {
    i.max <- min(i.min + chunk.size, length(stns))
    sub.stns <- stns[i.min:i.max]
    data.list <- read.crmp.BCH(ec.data.dir, network='EC', stn.list=sub.stns)
    data.list <- mclapply(data.list, qc.replace.na.values)
    mclapply(data.list, export, out.dir)
    i.min <- i.max + 1
    rm(data.list)
  }

  ## Load all BCH and SNOWPILLOW data
  print("Network BCH")
  data.list <- read.crmp.BCH(bch.data.dir, network='BCH')
  data.list <- mclapply(data.list, qc.replace.na.values)
  mclapply(data.list, export, out.dir)
  data.list <- mclapply(data.list, qc.replace.na.values)

  print("Network SNOWPILILOW")
  data.list <- read.crmp.BCH(snowpillow.data.dir, network='SNOWPILLOW')
  mclapply(data.list, export, out.dir)
  rm(data.list)

  ## Load all of the MoAg data
  print("Network MoAG")
  file <- file.path(moag.data.dir, "WeatherData.csv")
  data.list <- read.crmp.MoAg(file)
  lapply(data.list, export, out.dir)
  rm(data.list)

  ## Load all of the MoFR data
  print("Network MoFR")
  files <- list.files(path=mofr.data.dir, full.names=T, pattern="(Hourly|Daily)Weather.*txt")
  for (file in files) {
    data.list <- read.crmp.MoFR(file)
    mclapply(data.list, export, out.dir)
  }
  rm(data.list)
  gc()

  ## Load all of the MoFR Research data
  print("Network MoFR Researc")
  files <- list.files(path=mofr.research.data.dir, full.names=T, pattern="csv$")
  mclapply(files, function(f) {export(read.crmp.MoFR.research(f), out.dir)})

  ## Load all of the MoE data
  print("Network MoE")
  regions <- MoE.get.regions(moe.data.dir)
  do.one <- function(reg) {
    data.list <- read.crmp.MoE(reg)
    mclapply(data.list, export, out.dir)
  }
  lapply(regions, do.one)

  ## Load all of the RioTintoAlcan data
  print("Network RTA")
  files <- list.files(rta.data.dir, patter="met$", full.names=T)
  for (file in files) {
    data <- read.crmp.RTA(file)
    if (! is.null(data)) {
      data <- qc.replace.na.values(data)
      export(data, out.dir)
    }
  }

  ## Load all of the MoTIe data
  print("Network MoTIe")
  files <- list.files(motie.data.dir, full.names=T)
  for (file in files) {
        data <- read.crmp.MoTIe(file)
        if (! is.null(data)) {
          data <- qc.replace.na.values(data)
          export(data,out.dir) 
    }
  }
  
  ## Load all of the MoTIm data
  print("Network MoTIm")
  files <- list.files(motim.data.dir, full.names=T)
  for (file in files) {
        data <- read.crmp.MoTIm(file)
        if (! is.null(data)) {
          data <- qc.replace.na.values(data)
          export(data,out.dir)
    }
  }
}
