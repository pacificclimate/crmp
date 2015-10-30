## CRMP Load data consists of R functions developed to import station data from the CRMP 
## Network into the R object form that we are using presently. This file also contains
## various functions designed to support the importation of data.  There are also several
## testing functions useful for algorithm checking for particular stations with non-standard
## data formats.
##
##
## This was developed by James Hiebert at PCIC throughout 2010-2011
##
## Modifications were made most recently by Faron Anslow at PCIC on 4 May, 2011 
##
##---------------------------------------------------------------------------------------------

source('package/R/flag-class.r')
source('package/R/crmp-class.r')
source('package/R/variable.defs.r')

# Use parallel if possible
error <- try(library(parallel))
if (inherits(error, 'try-error')) {
  my.lapply <- lapply
} else {
  my.lapply <- mclapply
  options(mc.cores=8)
}

get.variable.thresholds <- function() {
  var.defs <- read.csv("var_tables/thresholds.csv", as.is=T)
  rownames(var.defs) <- var.defs$cf.standard.name
  var.defs[,c("min.valid", "max.valid")]
}

get.variable.persistence.values <- function() {
  var.defs <- read.csv("var_tables/thresholds.csv", as.is=T)
  rownames(var.defs) <- var.defs$cf.standard.name
  var.defs["persist"]
}

which.are.defined <- function(var.names, var.defs) {
  which(sapply(var.names, "%in%", rownames(var.defs)))
}

## This function opens up one of the per-station MoT observation files, reads the data,
## performs some very basic organization and returns a data.frame
## The return value should have the following properties:
## + retval is of class crmp
## + all variables names will be compliant with CF-metadata names (version is in the attribute cf:convention)
## + retval will have several attributes (cf:units cf:standard_name cf:long_description)
##   which are all lists that correspond to the rows
## Status: Successfully creates formal crmp object
read.crmp.MoTI <- function(filename, na.values=c(-99, -6999), obs.tz="PST") {
  sep <- ","
  datetime.format <-"%Y/%m/%d %H:%M:%S"
  if (grepl("txt$", filename)) {sep <- "\t"; datetime.format <- "%Y/%m/%d %H:%M"}
  if (grepl("csv$", filename)) {sep <- ","}

  network <- "MoTI"
  var.defs <- get.variable.defs(network)

  classes <- list(CODE='factor', DATETIME='character', 'OBS.TYPE'='factor')
  data <- read.csv(filename, sep=sep, colClasses=classes)

  # Create a proper timestamp field from the DATETIME field
  # Retardedly, the input is not in a standard format... at midnight the time is omitted
  i <- grep("^\\d{4}/\\d{2}/\\d{2}$", data$DATETIME, perl=T)
  data$DATETIME[i] <- paste(data$DATETIME[i], "00:00:00")
  time <- as.POSIXct(data$DATETIME, format=datetime.format, tz=obs.tz)

  # The CODE "variable" should be the same for all obs... just add it as an attribute to the data frame
  if (length(levels(data$CODE)) > 1)
    stop("There are more than one station's worth of observations in this data")
  stn.id <- levels(data$CODE)[1]

  rv <- new('crmp', data, station.id=stn.id, time=time)
  rv <- remove.nondefined.vars(rv, var.defs)
  rv <- set.crmp.attributes(rv, var.defs, network)
  order.crmp(rv)
}

list.stations.MoTI.slow <- function(data.dir='/home/data/projects/crmp/MoT/wx_data') {

  files <- list.files(data.dir, pattern='csv$', full.names=T)
  rv <- sapply(files,
               function(f) {levels(read.csv(f, colClasses=list(CODE="factor"))$CODE)},
               USE.NAMES=F)

  files <- list.files(data.dir, pattern='txt$', full.names=T)
  rv2 <- sapply(files,
                function(f) {levels(read.csv(f, sep='\t', colClasses=list(CODE="factor"))$CODE)},
                USE.NAMES=F)
  c(rv, rv2)
}

list.stations.MoTI <- function(data.dir='/home/data/projects/crmp/MoT/wx_data') {
  files <- list.files(data.dir, pattern='(csv|txt)$')
  gsub('^([0-9]+) .+$', '\\1', files)
}

## This function is designed for the newer release of the MoTI electronic data
## another import will be used for the manually observed data and will be called
## MoTIm. These are per station files with a lot of flagging entries for the
## Variables.  For now, these flags will get appended to the crmp object in the flag
## field as a list.

read.crmp.MoTIe <- function(filename,na.values=c('-99','-6999','null','299.7'),obs.tz="America/Vancouver"){
  network <- "MoTIe"	#Corresponds to the electronically recorded MoTI data
  var.defs <- get.variable.defs(network)
  ## The station ID will just be the first 5 elemnts of the file part of the filename
  stn.id <- get.stn.id.MoTI(filename)
  sep <- "\t"
  data <- read.table(filename, header=T, sep=sep, na.strings=na.values)

  datetime.format <- "%Y-%m-%d %H:%M:%S"
  time <- as.POSIXct(strptime(data$OBSERVATION_DATETIME, datetime.format, tz=obs.tz))

  flag.names <- c("AIR_TEMP1_CONFIDENCE_LEVEL", "AIR_TEMP2_CONFIDENCE_LEVEL","RELATIVE_HUMIDITY1_CONF_LEVEL","DEW_POINT_CONFIDENCE_LEVEL","ATMOS_PRESSURE_CONF_LEVEL","PRECIP_GAUGE_CONFIDENCE_LEVEL","PRECIP_NEW_CONFIDENCE_LEVEL")
  data.flags <- data[,flag.names]
  dataFlags <- new('flag',data.flags,qc.operation='As Supplied',
                   applicable.vars=c('CURRENT_AIR_TEMPERATURE1', 'CURRENT_AIR_TEMPERATURE2', 'RELATIVE_HUMIDITY1', 'DEW_POINT', 'ATMOSPHERIC_PRESSURE', 'PRECIPITATION_GAUGE_TOTAL', 'PRECIPITATION_NEW'))

  have.defs <- which.are.defined(names(data), var.defs)
  data <- data[,have.defs]
  n <- names(data)
  add.attributes <- list(station.id = stn.id,
                         network = network,
                         flags=list(dataFlags),
                         "cf:units" = var.defs[n,"network.unit"],
                         "cf:standard_name" = var.defs[n,"cf.var.name"],
                         "cf:long_description" = var.defs[n,"network.long.description"],
                         "cf:cell_method" = var.defs[n,"cf.cell.method"],
                         "cf:convention" = "1.4",
                         time = time)
  data <- do.call(new, append(list("crmp", data), add.attributes))
  # Make the variable names CF-1.4 compatible
  names(data) <- convert.to.cf.1.4.names(names(data))
  order.crmp(data)
}

## Takes a full path filename to an MoTI station file and returns the station id
## Assumes a fully numeric station id with a txt extension
get.stn.id.MoTI <- function(filename){
  sub('^.*\\/([0-9]+)\\.txt$', '\\1', filename)
}

## This function is designed for the newer release of the MoTI manual data These 
## are per station files with a lot of flagging entries for the Variables.  
## For now, these flags will get appended to the crmp object in the flag
## field as a list.

read.crmp.MoTIm <- function(filename,na.values=c('-99','-6999','null','299.7'),obs.tz="America/Vancouver"){
  network <- "MoTIm"	#Corresponds to the manualy recorded MoTI data
  var.defs <- get.variable.defs(network)
  #The station ID will just be the first 5 elemnts of the file part of the filename
  stn.id <- get.stn.id.MoTI(filename)
  sep <- "\t"
  data <- read.table(filename,header=T,sep=sep,na.strings=na.values)

  datetime.format <- "%Y-%m-%d %H:%M:%S"
  time <- as.POSIXct(strptime(data$OBSERVATION_DATETIME, datetime.format, tz=obs.tz))

  ## FIXME: I don't think that the 'TYPE_CODE's are actually flags ~JMH
  flag.names <- grep(value=T, "TYPE_CODE", names(data))
  data.flags <- data[,flag.names]
  dataFlags <- new('flag',data.flags,qc.operation='As Supplied')
  
  have.defs <- which.are.defined(names(data), var.defs)
  data <- data[,have.defs]
  n <- names(data)
  add.attributes <- list(station.id = stn.id,
                         network = network,
                         flags=list(dataFlags),
                         "cf:units" = var.defs[n,"network.unit"],
                         "cf:standard_name" = var.defs[n,"cf.var.name"],
                         "cf:long_description" = var.defs[n,"network.long.description"],
                         "cf:cell_method" = var.defs[n,"cf.cell.method"],
                         "cf:convention" = "1.4",
                         time = time)
  data <- do.call(new, append(list("crmp", data), add.attributes))
  ## Make the variable names CF-1.4 compatible
  names(data) <- convert.to.cf.1.4.names(names(data))
  order.crmp(data)
}

##  The FRBC data came as an access database which was exported to per station, per variable files
##  The file lengths are different for the different variables for the same station, so will need to 
##  Build a time scale by finding min and max time and then insert records onto this time scale.
read.crmp.FRBC <- function(stn.id, data.dir='/home/data/projects/crmp/data/FRBCClimate/',
                           na.values=c(-99, -6999, 6999),obs.tz="GMT") {
  network <- "FRBC"
  var.defs <- get.variable.defs(network)
  ## We have a precip file and a temeprature file.  Want to merge the
  ## Two timeseries into a single r object with T and P.
  tempfilename <- file.path(data.dir, paste(stn.id,'_temp.txt',sep=""))
  precipfilename <- file.path(data.dir, paste(stn.id,'_precip.txt',sep=""))
  pdata <- read.table(precipfilename, colClasses=c('factor', 'character', 'factor', 'numeric'), col.names=c('stn.id', 'time', '', 'datum'), sep=',')
  tdata <- read.table(tempfilename, colClasses=c('factor', 'character', 'factor', 'numeric'), col.names=c('stn.id', 'time', '', 'datum'), sep=',')
  datetime.format <- "%m/%d/%Y %H:%M:%S"
  ptime <- as.POSIXct(strptime(pdata$time, datetime.format, tz=obs.tz))
  ttime <- as.POSIXct(strptime(tdata$time, datetime.format, tz=obs.tz))
  tdata <- tdata[,'datum']
  pdata <- pdata[,'datum']
  pdata <- diff(pdata)
  pdata <- replace(pdata, pdata < 0, 0) # get rid of negative precip (gauge emptying)
  ptime <- ptime[2:length(ptime)]

  ## Throw everything in one data frame, then we'll tapply over time as the factor
  data <- data.frame('TEMP_MEAN' = c(tdata, rep(NA, length(pdata))),
                     'PRECIP_TOTAL' = c(rep(NA, length(tdata)), pdata),
                     'TIME' = c(ttime, ptime))
  data$TIME <- round(data$TIME, 'hour')
  data <- data[order(data$TIME),]

  ## Mean (temp) or sum (prec) readings within the same hour
  tf <- as.factor(as.character(data$TIME))
  temp <- tapply(data[,'TEMP_MEAN'], tf, mean, na.rm=T)
  prec <- tapply(data[,'PRECIP_TOTAL'], tf, sum, na.rm=T)
  time <- as.POSIXct(levels(tf), tz=obs.tz)

  ## Construct the return object
  new('crmp', data.frame('TEMP_MEAN'=as.numeric(temp),
                         'PRECIP_TOTAL'=as.numeric(prec)),
      time=time,
      station.id=stn.id,
      network=network,
      "cf:units" = var.defs[,"network.unit"],
      "cf:standard_name" = var.defs[,"cf.var.name"],
      "cf:long_description" = var.defs[,"network.long.description"],
      "cf:cell_method" = var.defs[,"cf.cell.method"],
      "cf:convention" = "1.4"
      )
}

## Now its time for the MoFR_Research_II stations.  These were supplied by Michaela Waterhouse and Bill Floyd in 
## a variety of excel spreadsheets with varying amounts of data, formats, and even variable units.  I (Faron) 
## subsetted these and exported them to CSV with variables of interest to CRMP.  I've also brought the units into
## a common form for the sake of universal import into the CRMP R data structure and eventually into the CRMP database.
## The EC buoys were downloaded from the web in May, 2011 and contain all available data up until that point.
## The import function will be written to bring in as much of that data as possible even if it's not needed in
## the immediate future.  These are simple, one station per file data sets, so should be easy to import.
read.crmp.MoFR.research.II <- function(filename,na.values=c(-99, -6999, 6999), obs.tz="GMT") {
  network <- 'MoFR_research'
  stn.id <- strsplit(filename,' ')[[1]][1]
  stn.id <- strsplit(stn.id,'/')
  stn.id <- stn.id[[1]][length(stn.id[[1]])]
  var.defs <- get.variable.defs(paste(network,"_II",sep=""))
  astn <- read.csv(filename,header=TRUE,quote="\"")

  for (v in na.values)
    astn <- replace(astn, astn == v, NA)

  time <- as.POSIXct(strptime(as.character(as.matrix(astn$Date)),"%y-%m-%d",tz=obs.tz))
  flag.names <- c("QC_Tm","QC_Tx","QC_Tn","QC_Precip","QC_RHm","QC_RHx","QC_RHn","QC_Wind_m","QC_Wind_x","QC_Wind_n","QC_Wdir")
  data.flags <- as.data.frame(astn[,flag.names])
  dataFlags <- new('flag',data.flags,qc.operation='As Supplied',
                   applicable.vars=c("Tm","Tx","Tn","Precip","RHm","RHx","RHn","Wind_m","Wind_x","Wind_n","Wdir_m"))
  have.defs <- which.are.defined(names(astn), var.defs)
  astn <- astn[,have.defs]
  n <- names(astn)
  add.attributes <- list(station.id = as.character(stn.id),
                         network = network,
                         flags=list(dataFlags),
                         "cf:units" = var.defs[n,"network.unit"],
                         "cf:standard_name" = var.defs[n,"cf.var.name"],
                         "cf:long_description" = var.defs[n,"network.long.description"],
                         "cf:cell_method" = var.defs[n,"cf.cell.method"],
                         "cf:convention" = "1.4",
                         time = time)
  astn <- do.call(new, append(list("crmp", astn), add.attributes))
  names(astn) <- convert.to.cf.1.4.names(names(astn))
  return(astn)
}


## The EC buoys were downloaded from the web in May, 2011 and contain all available data up until that point.
## The import function will be written to bring in as much of that data as possible even if it's not needed in
## the immediate future.  These are simple, one station per file data sets, so should be easy to import.
read.crmp.EC.buoys <- function(filename,na.values=c(-99, -6999, 6999), obs.tz="GMT") {
  network <- 'EC_Buoy'
  var.defs <- get.variable.defs(network)
  astn <- read.csv(filename,header=TRUE,quote="\"")

  for (v in na.values)
    astn <- replace(astn, astn == v, NA)

  stn_id <- as.character(astn$STN_ID[1])
  time <- as.POSIXct(strptime(astn$DATE,"%m/%d/%Y %H:%M",tz=obs.tz))
  flag.names <- c("Q_FLAG" )
  data.flags <- as.data.frame(astn[,flag.names])
  dataFlags <- new('flag',data.flags,qc.operation='As Supplied',
                   applicable.vars=c("VCAR","VTPK","WDIR1","WSPD1","GSPD1","WDIR2","WSPD2","GSPD2","ATMS1","ATMS2","DRYT","SSTP"))
  have.defs <- which.are.defined(names(astn), var.defs)
  astn <- astn[,have.defs]
  n <- names(astn)
  add.attributes <- list(station.id = as.character(stn_id),
                         network = network,
                         flags=list(dataFlags),
                         "cf:units" = var.defs[n,"network.unit"],
                         "cf:standard_name" = var.defs[n,"cf.var.name"],
                         "cf:long_description" = var.defs[n,"network.long.description"],
                         "cf:cell_method" = var.defs[n,"cf.cell.method"],
                         "cf:convention" = "1.4",
                         time = time)
  astn <- do.call(new, append(list("crmp", astn), add.attributes))
  names(astn) <- convert.to.cf.1.4.names(names(astn))
  return(astn)
}

get.buoy.meta <- function(buoydir='/home/data/projects/crmp/data/EC_Buoy_Data',obs.tz="GMT") {
  ## Just want to loop throught the buoy data and put together the meta data for it based on the contents of the files.
  ## Idea is to make a data frame with columns: station ID, lat, long, elevation, start date, end date.
  
  ## List out the files to open
  filelist <- list.files(buoydir, pattern='csv$', full.names=T)
  nstations=length(filelist)
  
  ## Create a data frame to stick the meta data into
  buoy_meta <- as.data.frame(matrix(data=NA,nrow=nstations,ncol=6))
  buoy_meta[,5] <- as.POSIXct(buoy_meta[,5])
  buoy_meta[,6] <- as.POSIXct(buoy_meta[,6])
  c("STN_ID","LATITUDE","LONGITUDE","ELEVATION","START_DATE","END_DATE") -> colnames(buoy_meta) 
  acounter <- 0
  for (filename in filelist) {
    acounter <- acounter + 1
    astn <- read.csv(filename,header=TRUE,quote="\"")
    time <- as.POSIXct(strptime(astn$DATE,"%m/%d/%Y %H:%M",tz=obs.tz))
    buoy_meta$STN_ID[acounter] <- as.character(astn$STN_ID[1])
    buoy_meta$LATITUDE[acounter] <- astn$LATITUDE[1]
    buoy_meta$LONGITUDE[acounter] <- astn$LONGITUDE[1]
    buoy_meta$ELEVATION[acounter] <- 0
    buoy_meta$START_DATE[acounter] <- as.POSIXct(min(time,na.rm=TRUE))
    buoy_meta$END_DATE[acounter] <- as.POSIXct(max(time,na.rm=TRUE))
  }
  return(buoy_meta)

}

## The MoE AIRPROD is a semi-mystery dataset which seems to contain a *lot* of station data over roughly
## 1000 stations that appear to be independent of the EC data.  These cover years starting in the '50s
## and running through the mid 1990s.  Thus, these are highly valuable stations to add to the EC data
## They are arranged in a single file with all data for a given station in successive rows.  So, should be
## easy to pull apart and write out to crmp objects.  This will return a list of crmp objects which can 
## be written to R objects at a later step.

## This function accesses the automatically recorded data which are found in the file CLIMATE.R1
read.crmp.MoE.AIRPROD2 <- function(na.values=c(-99, -6999, 6999), obs.tz="GMT") {
  network <- 'MoE_AP'
  var.defs <- get.variable.defs('MoE_AIRPROD2')
  filename <- '/home/data/projects/crmp/data/MoE_AIRPROD1/data/CLIMATE.R2'
  data <- read.csv(filename,header=TRUE,quote="\"", colClasses=list(STATIONNO='factor', 'SAMPLE_DATE'='character'))

  do.one.station <- function(astn) {
    stationnumber <- as.character(astn[1, 'STATIONNO'])
    print(paste("STATION", stationnumber))

    if(nrow(astn) <= 1) {
      print(sprintf("Station %s has only 1 row and I'm skipping it", stationnumber))
      return(NULL)
    }

    ## Format the time from yymmdd to desired POSIXct
    time <- as.POSIXct(strptime(astn$SAMPLE_DATE,"%y%m%d",tz=obs.tz))

    ## Remove vars and flags for which there is no data
    for (v in names(astn)) {
      if (all(is.na(astn[,v]))) {
        astn[,v] <- NULL
      }
    }
    
    ## Deal with the flags
    flag.names <- names(astn)[grep('CODE$', names(astn))]
    data.flags <- apply(astn[,flag.names,drop=F], 1, gsub, pattern='^ *([^ ]) *$', replacement='\\1') # Trim whitespace
    if (length(flag.names) > 1) data.flags <- t(data.flags)
    data.flags <- as.data.frame(data.flags)
    if (nrow(astn) == 1) data.flags <- t(data.flags) # Apply has a retarded special case for legth 1 where it transposes the result *roll*
    names(data.flags) <- flag.names
    names(data.flags) <- sub('^(.*)CODE$', '\\1', names(data.flags))
    names(data.flags) <- sub('HUM', 'HUM1', names(data.flags))
    data.flags <- replace(data.flags, data.flags == '', NA)
    dataFlags <- new('flag',data.flags,qc.operation='As Supplied',
                     applicable.vars=names(data.flags))
    if (ncol(dataFlags) > 10)
      browser()

    ## Pull out the data itself
    have.defs <- which.are.defined(names(astn), var.defs)
    astn <- astn[,have.defs,drop=F]

    ## Yes, this does happen
    if (ncol(astn) < 1)
      return(NULL)

    ## Everything seems to be stored as ints (value * 10)
    i <- i <- names(astn) %in% c("MINTMP", "MAXTMP", "PPT")
    astn[,i] <- astn[,i] / 10.0

    n <- names(astn)
    add.attributes <- list(station.id = stationnumber,
                           network = network,
                           flags=list(dataFlags),
                           "cf:units" = var.defs[n,"network.unit"],
                           "cf:standard_name" = var.defs[n,"cf.var.name"],
                           "cf:long_description" = var.defs[n,"network.long.description"],
                           "cf:cell_method" = var.defs[n,"cf.cell.method"],
                           "cf:convention" = "1.4",
                           time = time)
    astn <- do.call(new, append(list("crmp", astn), add.attributes))
    astn <- order.crmp(astn)
    astn <- remove.duplicate.times(astn)
    ## Make the variable names CF-1.4 compatible
    names(astn) <- convert.to.cf.1.4.names(names(astn))
    gc()
    astn
  }

  by(data, data[,'STATIONNO'], function(x) {try(do.one.station(x))}, simplify=F)
}

## This function accesses the manually recorded data which are found in the file CLIMATE.R1
read.crmp.MoE.AIRPROD1 <- function(na.values=c(-99, -6999, 6999), obs.tz="GMT") {
  network <- 'MoE_AP'
  var.defs <- get.variable.defs('MoE_AIRPROD1')
  filename <- '/home/data/projects/crmp/data/MoE_AIRPROD1/data/CLIMATE.R1'
  data <- read.csv(filename,header=TRUE,quote="\"", colClasses=list(STATIONNO='factor', 'SAMPLE_DATE'='character'))

  hours <- replace(data[,'SAMPLE_HOUR'], is.na(data[,'SAMPLE_HOUR']), 0)
  minutes <- replace(data[,'SAMPLE_MINUTE'], is.na(data[,'SAMPLE_MINUTE']), 0)
  data[,'time.string'] <- paste(data[,'SAMPLE_DATE'], ' ', sprintf('%02d:%02d', hours, minutes), sep='')
  
  do.one.station <- function(astn) {

    astn <- unique(astn) # There are duplicate entries in the AP1 data
    stationnumber <- as.character(astn[1, 'STATIONNO'])

    tformat <- "%y%m%d %H:%M"
    time <- as.POSIXct(strptime(astn[,'time.string'], format = tformat, tz = obs.tz))
    if (any(is.na(time)))
      browser()

    flag.names <- c("ACCPPT1CDE","ACCPPT2CDE","SNOWCODE")
    data.flags <- astn[,flag.names]
    names(data.flags) <- c('ACCPPT1','ACCPPT2','SNOWDEPTH')
    dataFlags <- new('flag',data.flags,qc.operation='As Supplied',
                     applicable.vars=c('ACCPPT1','ACCPPT2','SNOWDEPTH'))
    have.defs <- which.are.defined(names(astn), var.defs)
    astn <- astn[,have.defs]
    n <- names(astn)
    add.attributes <- list(station.id = as.character(stationnumber),
                           network = network,
                           flags=list(dataFlags),
                           "cf:units" = var.defs[n,"network.unit"],
                           "cf:standard_name" = var.defs[n,"cf.var.name"],
                           "cf:long_description" = var.defs[n,"network.long.description"],
                           "cf:cell_method" = var.defs[n,"cf.cell.method"],
                           "cf:convention" = "1.4",
                           time = time)
    astn <- do.call(new, append(list("crmp", astn), add.attributes))
    ## Make the variable names CF-1.4 compatible
    names(astn) <- convert.to.cf.1.4.names(names(astn))
    order.crmp(astn)    
  }
  
  by(data, data[,'STATIONNO'], do.one.station, simplify=F)
}

ftest.MoE_AIRPROD.cumppt <- function(astn) {

  dtime <- diff(astn@time)/(3600*24)
  dur1=astn$DURATION1[2:length(astn$DURATION1)]
  dur2=astn$DURATION2[2:length(astn$DURATION2)] 
  compdata <- cbind(dtime,dur1,dur2)
  compdata <- compdata[which(dtime!= 0),]
  if (length(which(dtime!=0))>1) {
    dcompare <- cbind(compdata[,1]-compdata[,2],compdata[,1]-compdata[,3])
  }
  else {
    dcompare <- cbind(compdata[1]-compdata[2],compdata[1]-compdata[3])    
  }
  ## Get number of errors.  Add one for start of record 
  nerrors <- length(which(abs(dcompare[,1])>2))+ length(which(abs(dcompare[,2])>2)) + 1
}

count.MoE_AIRPROD.cumppt.errors <- function(crmp_list) {
  nstns <- length(crmp_list)
  error_list <- vector("list",nstns)
  acounter <- 0
  for (astn in crmp_list) {
    acounter <- acounter+1
    anerror <- try(error_list[[acounter]] <- test.MoE_AIRPROD.cumppt(astn))
    if (inherits(anerror,'try-error')) {
      print('try error')
    } 
  }
  return(error_list)
}

## MoE data is "organized" per variable per region
## each variable has a directory with one csv file per region and all of the
## stations thrown into one file
## Hence all the script-fu to rearrange everything into per stations (all vars) data.frames
read.crmp.MoE <- function(region, na.values=c(-99, -6999, 6999), obs.tz="GMT") {

  network <- "MoE"
  var.defs <- get.variable.defs(network)

  print(paste("Loading data from region:", region))
  files <- sapply(MoE.get.variables(), MoE.find.data.file, region)
  names(files) <- MoE.get.variables()

  ## MoE.find.data.file will return NULL (and print a warning) for variables where it can't find the file
  ## This unlist will vectorize the file list to be used in file.info and _also_ get rid of the NULL results for us
  files <- unlist(files, use.names=T)

  ## Don't bother reading data files for which we have no definition
  have.def <- which.are.defined(names(files), var.defs)
  files <- files[have.def]

  files <- files[which(file.info(files)$size > 0)] # Eliminate the files which have no data
  data <- sapply(files, read.csv, header=F, simplify=T, strip.white=T,
                 col.names=c("region.id", "region.name", "station.id", "station.name", "string.time", "numerical.time", "var.name", "value", "flags"),
                 as.is=c("string.time", "flags"),
                 colClasses=list(flags="character"))

  all.stations <- unique(unlist(apply(data, 2, function(x) {levels(x$station.id)})))
  values.by.var <- apply(data, 2, function(x) {tapply(x$value, x$station.id, identity)})
  times.by.var <- apply(data, 2, function(x) {tapply(x$string.time, x$station.id, as.POSIXct, format="%d%b%Y:%H:%M:%S", tz="GMT")})
  all.vars <- names(times.by.var)
  ## Returns a list with names [variable] with each element being a named vector (station.id=station.name)
  stn.name.map <- apply(data, 2, function(x) {rv <- x$station.name; names(rv) <- x$station.id; i <- duplicated(rv); rv[!i]})

  flags.by.var <- apply(data, 2, function(x) {tapply(x$flags, x$station.id, identity)})

  setup.one <- function(stn) {
    stn.name <- ""
    print(paste("Processing station", stn))
    ## For each station
    ## Select a timeseries, the values, and the flags for each variable
    ts <- lapply(all.vars, function(v) {times.by.var [[c(v, stn)]]}); names(ts) <- all.vars
    vs <- lapply(all.vars, function(v) {values.by.var[[c(v, stn)]]}); names(vs) <- all.vars
    fs <- lapply(all.vars, function(v) {flags.by.var [[c(v, stn)]]}); names(fs) <- all.vars
    all.t <- sort(Reduce(union, ts)); attributes(all.t) <- list(class=c("POSIXct", "POSIXt"), tzone="GMT")
    rv <- rf <- data.frame(array(dim=c(length(all.t), length(all.vars)), dimnames=list(time=strftime(all.t, tz="GMT"), var=all.vars)))

    ## Copy the values from the read data to the return data.frame
    for (v in all.vars) {
      if (stn %in% names(values.by.var[[v]])) { # only bother if this variable is collected by this station
        stn.name <- as.character(stn.name.map[[v]][stn]) # Grab the station name
        ti <- strftime(ts[[v]], tz="GMT")
        rv[ti,v] <- vs[[v]]  ## Might be wise to use a hash for this instead... named lists apparently have an O(n) retreival time
        rf[ti,v] <- fs[[v]]
      } else { # Otherwise, remove the variable from the frame
        rv[[v]] <- rf[[v]] <- NULL
      }
    }

    stopifnot(length(all.t) > 0, ! is.null(all.t), inherits(all.t, 'POSIXct'))

    rv <- new('crmp', rv,
              station.id = stn,
              station.name = stn.name,
              time = all.t,
              flags = list(new('flag', rf, qc.operation='from original data', applicable.vars=names(rf)))
              )

    rv <- remove.nondefined.vars(rv, var.defs)
    rv <- set.crmp.attributes(rv, var.defs, network)
    order.crmp(rv)
  }

  return(my.lapply(all.stations, setup.one))
}

read.crmp.MoAg <- function(filename, obs.tz="GMT", na.values=c(-99, -6999, 6999)) {
  if(!file.exists(filename))
    stop(paste("File does not exist:", filename))

  network <- "MoAg"
  var.defs <- get.variable.defs(network)

  data <- read.csv(filename, strip.white=T)
  data$POSIXct <- as.POSIXct(strptime(data$Date, "%m/%d/%Y", tz=obs.tz))

  ## FIXME: I can't remember why this block is even here
  if (any(grepl("Station.Number", names(data)))) {
  } else { # This is what we got straight from MoAg
    ## Do some ad-hoc corrections of the station codes
    levels(data$Station) <- mangle.MoAg.stns.codes(levels(data$Station))
    browser()
  }

  setup.one <- function(crmp) {
    stn.id <- as.character(crmp[1,"Station.Number"])
    stn.id <- gsub(' ', '', stn.id)
    stn.name <- as.character(crmp[1,"Station"])
    crmp <- replace(crmp, crmp %in% na.values, NA)
    rv <- new("crmp", crmp, station.id=stn.id, time=crmp$POSIXct, station.name=stn.name)
    rv <- remove.nondefined.vars(rv, var.defs)
    rv <- set.crmp.attributes(rv, var.defs, network)
    order.crmp(rv)
  }

  # Split the data into per-station frames
  by(data, data$Station, setup.one)
}

list.stations.MoAg <- function(data.file='/home/data/projects/crmp/MoAg/WeatherData.csv') {
  data <- read.csv(data.file, strip.white=T)
  levels(data$Station.Number)
}

mangle.MoAg.stns.codes <- function(stn.names) {
  metadata.file <- "/home/data/projects/crmp/MoAg/FarmWest-Revised.csv"
  meta <- read.csv(metadata.file)

  # We didn't receive the EC stations; remove them (since they have duplicate names with the rest of the stations we got
  i <- which(meta$Ministry == "EC")
  meta <- meta[-i,]

  # If the name doesn't have a 1-to-1 mapping with a code
  # or if the code is ' - ', just return the name (cf-compliant)
  name.to.code <- function(stn.name) {
    n <- which(stn.name == meta$Name)
    if (length(n) != 1)
      return(convert.to.cf.1.4.names(stn.name))
    code <- meta$Code[n]
    if (code == " - ")
      return(convert.to.cf.1.4.names(stn.name))
    return(code)
  }

  # Whoever exported the data left a few parentheticals which just screw up the names
  # E.g. "Pitt Meadows (GVRD)" Thanks I could have gotten that out of the "Ministry" column
  # on the spreadsheet.
  # Take them out
  no.parens <- gsub('(.*) \\(.*\\)', '\\1', stn.names)

  code.factors <- lapply(no.parens, name.to.code)
  sapply(code.factors, as.character)
}

read.crmp.RTA <- function(filename, reject.estimates=F, obs.tz="GMT") {
  if(!file.exists(filename))
    stop(paste("File does not exist:", filename))

  network <- "RTA"
  var.defs <- get.variable.defs(network)

  ## Column widths determined emperically
  widths <- c(8,2,5,3,5,3,5,3)
  col.names <- c("date", "unused", "tasmax", "tasmax.flag", "tasmin", "tasmin.flag", "pcp", "pcp.flag")
  data <- read.fwf(filename, widths=widths, col.names=col.names)
  data <- data[,-2] # remove the unused column
  t <- as.POSIXct(strptime(data$date, "%Y%m%d", tz=obs.tz))

  ## Use flags to remove missing/estimated data
  for (var in c("tasmax", "tasmin", "pcp")) {
    var.flag <- paste(sep=".", var, "flag")
    data[grepl('M', data[,var.flag]),var] <- NA
  }
  if (reject.estimates) {
    for (var in c("tasmax", "tasmin", "pcp")) {
      var.flag <- paste(sep=".", var, "flag")
      data[grepl('E', data[,var.flag]),var] <- NA
    }
  }

  stn.id <- strsplit(basename(filename), '.', fixed=T)[[1]][1]
  rv <- new('crmp', data, station.id=stn.id, time=t)
  rv <- remove.nondefined.vars(rv, var.defs)
  rv <- set.crmp.attributes(rv, var.defs, network)
  order.crmp(rv)
}

list.stations.RTA <- function(data.dir='/home/data/projects/crmp/RioTintoAlcan') {
  gsub('\\.met$', '', list.files(data.dir, pattern="met$"))
}

MoE.station.id.to.region <- function(station.id, stns.file="/home/data/projects/crmp/MoE/BC Ministry of Environment Monitoring Stations.csv") {
  stns <- read.csv(stns.file)
  i <- which(stns["STATION_ID"] == station.id)
  if (length(i) < 1) {
    stop(paste("station.id not found:", station.id))
  }
  else {
    return(stns[i, "REGION_ID"])
  }
}

list.stations.MoE <- function(stns.file="/home/data/projects/crmp/MoE/BC Ministry of Environment Monitoring Stations.csv") {
  stns <- read.csv(stns.file)
  levels(stns$"STATION_ID")
}

MoE.find.data.file <- function(var, reg.id, basedir="/home/data/projects/crmp/MoE/") {
  #reg.id <- MoE.station.id.to.region(station.id)
  file <- file.path(basedir, var, paste(sep="", var, "_Region", reg.id, "_all_history.csv"))
  if (file.exists(file)) return(file)
  else warning(paste("The filename should be ", file, "but it does not exist"))
  return()
}

# Assumes that all directories in the basedir hold a variable
# This just lists the directories and returns them
MoE.get.variables <- function(basedir="/home/data/projects/crmp/MoE/") {
  files <- list.files(basedir, full.names=T)
  # Filter out non-directories
  files <- files[which(file.info(files)$isdir == T)]
  basename(files)
}

MoE.get.stations.with.data <- function(basedir="/home/data/projects/crmp/MoE") {
  read.table(file.path(basedir, "stations.with.data.txt"))
}

MoE.get.regions <- function(basedir="/home/data/projects/crmp/MoE") {
  p <- ".*_Region([0-9]+)_.*"
  files <- list.files(basedir, full.names=F, recursive=T, pattern=p)
  as.numeric(unique(sub(p, '\\1', files)))
}

list.stations.BCH <- function(data.dir="/home/data2/projects/data_cleanup/BCH_STATIONS/new_data") {
  vars <- list.files(data.dir)
  data.files <- unlist(sapply(file.path(data.dir, vars), list.files), use.names=F)
  all.stns <- lapply(strsplit(data.files, '_', fixed=T), '[[', 1)
  stns <- unique(all.stns)
  stns
}

# Read data from any of the networks which DaveB had converted (mostly in /home/data/projects/data_cleanup)
# This includes BCH, CDCD and SNOW_PILLOW
# Takes an optional stn.id argument to read data from just one station.  This is highly recommended.
# If stn.id is NULL, it reads data from all stations
read.crmp.BCH <- function(data.dir="/home/data/projects/data_cleanup/BCH_STATIONS/new_data", network='BCH', stn.list=NULL) {

  # var.defs for SNOWPILLOW/BCH/CDCD are the same
  var.defs <- get.variable.defs(network='BCH')

  vars <- list.files(data.dir)

  # List all of the data files, pull the station code off the front and then all unique names
  if (is.null(stn.list)) {
    data.files <- unlist(sapply(file.path(data.dir, vars), list.files), use.names=F)
    all.stns <- lapply(strsplit(data.files, '_', fixed=T), '[[', 1)
    stns <- unique(all.stns)
  } else {
    stns <- stn.list
  }

  get.t <- function(frame) {
    as.character(as.POSIXct(apply(frame[c('year', 'jday')], 1, paste, collapse='/'), format="%Y/%j", tz='GMT'))
  }

  read.one.station <- function(stn) {
    print(paste("Reading data for station:", stn))
    files <- file.path(data.dir, vars, paste(stn, "_", vars, ".csv", sep=""))
    v <- which(file.exists(files))
    files <- files[v]
    data <- lapply(files, read.csv, colClasses=list(flags="character"))
    names(data) <- vars[v]
    # Get the time values that are present in any variable
    t <- sort(unique(unlist(lapply(data, get.t))))
    # Initialize the return variable and flag variable
    rv <- fg <- array(dim=c(length(t), length(vars[v])),
                      dimnames=list(time=t, var=names(data))
                      )

    for (i in v) {
      var <- vars[i]
      ti <- get.t(data[[var]])
      rv[ti,var] <- data[[c(var, var)]]
      fg[ti,var] <- data[[c(var, 'flags')]]
    }
    rv <- as.data.frame(rv)
    fg <- new('flag', as.data.frame(fg), qc.operation='from original data', applicable.vars=vars[v])

    t <- as.POSIXct(t, tz="GMT")
    new('crmp', rv, time=t, station.id=stn, flags=list(fg))
  }

  per.stn.data <- my.lapply(stns, read.one.station)
  per.stn.data <- my.lapply(per.stn.data, set.crmp.attributes, var.defs, network)
  per.stn.data <- my.lapply(per.stn.data, order.crmp)
  per.stn.data
}

list.stations.BCH <- function(data.dir='/home/data/projects/data_cleanup/BCH_STATIONS/new_data') {
  var.names <- rownames(get.variable.defs('BCH'))
  files <- basename(list.files(data.dir, pattern='csv$', recursive=T))
  p <- paste('^([^_]+)_(', paste(var.names, collapse='|'), ').csv$', sep='')
  unique(gsub(p, '\\1', grep(p, files, value=T)))
}

### Ministry of Forests and Range (MoFR) ###

# Access seems to export midnight to be 24 o'clock of the preceeding day *faceplant*
# Convert it to actually be midnight
# Input should be a vector of strings to be converted such as this one '1991101224'
# Return value is a vector of POSIXct type with NA values for unparseable/unsupported formats
convert.retarded.access.dates <- function(s) {

  ## Add formats here to support them
  ## Note that order _is_ important!  Lower has higher matching priority.
  formats <- list(## Year/month/day hour format
                  list(regex='^[0-9]{10}$', format="%Y%m%d%H"),
                  ## Access's retarded 24 o'clock format
                  list(regex='^[0-9]{8}24$', format="%Y%m%d24", access=T),
                  ## Year/month/day format
                  list(regex='^[0-9]{8}$', format="%Y%m%d"))

  ## Initialize the return value
  t <- as.POSIXct(rep(NA, length(s)))
  attr(t, 'tzone') <- 'GMT'

  for (f in formats) {
    m <- grepl(f$regex, s)
    if (any(m)) {
      i <- which(m)
      t[i] <- strptime(s[i], f$format, tz="GMT")

      # Handle the 24 o'clock special case
      try(silent=T,
          if(f$access) {
            t[i] <- t[i] + as.difftime(1, units='days')
          }
          )
    }
  }
  t
}

# Opens a file that is a text export from M$ Access files provide by MoFR
# Returns a _list_ of crmp objects, one for each station which is present in the file
read.crmp.MoFR <- function(filename, na.values=c(-99, -6999), obs.tz="GMT") {
  stopifnot(file.exists(filename))

  network <- "MoFR"
  var.defs <- get.variable.defs(network)

  data <-read.csv(filename, colClasses=list('station_code'='factor'))
  data$POSIXct <- convert.retarded.access.dates(data$"weather_date")

  setup.one <- function(crmp) {
    stn.id <- as.character(crmp[1, "station_code"])
    stn.name <- as.character(crmp[1, "station_name"])
    crmp <- replace(crmp, crmp %in% na.values, NA)
    rv <- new('crmp', crmp, station.id=stn.id, station.name=stn.name, time=crmp$POSIXct)
    rv <- remove.nondefined.vars(rv, var.defs)
    rv <- set.crmp.attributes(rv, var.defs, network)
    order.crmp(rv)
  }
  by(data, data$"station_code", setup.one)
}

list.stations.MoFR <- function(data.dir='/home/data/projects/crmp/MoFR/access_export') {
  files <- list.files(data.dir, '^(Hourly|Daily).*txt$', full.names=T)
  rv <- my.lapply(files, function(f) {levels(read.csv(f, colClasses=list('station_code'='factor'))$'station_code')})
  unique(unlist(rv))
}

read.crmp.MoFR.research <- function(filename) {
  stopifnot(file.exists(filename))
  network <- "MoFR_research"
  var.defs <- get.variable.defs(network)

  data <- read.csv(filename, check.names=F, colClasses=list(StationNo='character'), as.is="StationName")
  fmt <- "%Y/%m/%d %H:%M:%S"
  t <- as.POSIXct(strptime(data[["Date and Time, PST"]], fmt, tz="PST"))
  i <- which(is.na(t))

  # Correct for midnight values that are only in yyyy/mm/dd
  if (length(i) > 0)
    t[i] <- strptime(data[i,"Date and Time, PST"], "%Y/%m/%d", tz="PST")

  stopifnot(length(t) == dim(data)[1])

  stn.id <- as.character(data[["StationNo"]][1])
  data <- new('crmp', data, station.id=stn.id, station.name=data[["StationName"]][1], time=t, check.names=F)

  data <- remove.nondefined.vars(data, var.defs=var.defs)
  data <- set.crmp.attributes(data, var.defs, network)
  order.crmp(data)
}

list.stations.MoFR.research <- function(data.dir='/home/data/projects/crmp/MoFR/Research_Stations/') {
  files <- list.files(data.dir, 'csv$', full.names=T)
  rv <- my.lapply(files, function(f) {levels(read.csv(f, colClasses=list(StationNo='factor'))$StationNo)})
  unique(unlist(rv))
}

list.stations.MoFR.research.fast <- function(data.dir='/home/data/projects/crmp/MoFR/Research_Stations/') {
  files <- list.files(data.dir, 'csv$')
  gsub('^.*_([0-9]+).csv$', '\\1', files)
}

## End MoFR ##

crmp.write.weka <- function(obj, file) {
  cat('% 1. Title: CRMP station',
      '% 2. Source: Auto-generated by crmp.write.weka from crmp R object',
      '@RELATION crmp',
      file=file,
      append=F,
      sep='\n')
  for (n in names(obj)) {
    type <- if (n == 'POSIXct') 'string' else 'NUMERIC'
    cat('@ATTRIBUTE', n, type, '\n',
        append=T,
        file=file
        )
  }
  cat('@ATTRIBUTE date date "yyyy-MM-dd HH:mm:ss"\n', append=T, file=file)
  cat('@data\n', append=T, file=file)
  obj$date <- strftime(obj$POSIXct, '%F %T', tz='GMT')
  obj$POSIXct <- as.character(obj$POSIXct)
  write.csv(obj, col.names=NA, row.names=F, quote=T, na='?', append=T, file=file)
}

### Test code ###

test.convert.retarded.access.dates <- function() {
  cases <- list(
                # Correctly converts strptime valid dates
                list(c("2010011001", "2010011002", "2010011003", "2010011004"),
                     as.POSIXct(c("2010-01-10 01:00", "2010-01-10 02:00", "2010-01-10 03:00", "2010-01-10 04:00"), format="%F %R", tz="GMT")),
                # Correctly converts dumb access 24 o'clock dates
                list(c("2010010924", "2010011024", "2010011124"),
                     as.POSIXct(c("2010-01-10 00:00", "2010-01-11 00:00", "2010-01-12 00:00"), format="%F %R", tz="GMT")),
                # Can handle the yyyymmdd
                list(c("20100110", "20100810", "19800101"),
                     as.POSIXct(c("2010/01/10", "2010/08/10", "1980/01/01"), tz="GMT")),
                # Can do a mixture (strptime, strptime, access, and yyyymmdd)
                list(c("2010011001", "2010011002", "2010011024", "19800101"),
                     as.POSIXct(c("2010-01-10 01:00", "2010-01-10 02:00", "2010-01-11 00:00", "1980-01-01 00:00"), format="%F %R", tz="GMT")),
                # Returns NA for non-parseable dates
                list(c("Blah stuff", "2010011000", NA),
                     c(NA, as.POSIXct("2010-01-10", tz="GMT"), NA))
                )
  for (case in cases) {
    names(case) <- c("input", "expected")
    result <- convert.retarded.access.dates(case$input)
    checkTrue(all(result == case$expected, na.rm=T))
    checkEquals(which(is.na(result)), which(is.na(case$expected)))
  }
}

long.test.list.stations.MoTI <- function() {
  x <- list.stations.MoTI()
  y <- list.stations.MoTI.slow()
  checkTrue(setequal(x, y))
}

## In an ideal world, this should be correct
## but apparently there's actually data for one station
## which isn't listed in the files
do.not.test.list.stations.MoFR.research <- function() {
  x <- list.stations.MoFR.research()
  y <- list.stations.MoFR.research.fast()
  checkTrue(setequal(x, y))
}
