library(RPostgreSQL)
library(rgdal)

query <- function(con, q, log='log.txt') {
  cat(file=log, append=T, q, '\n')
  dbSendQuery(con, q)
}

get.network.id <- function(con, native.id, log='log.txt') {
  q <- paste("SELECT network_id FROM meta_network WHERE network_name = '", native.id, "'", sep='')
  rv <- query(con, q)
  df <- fetch(rv, n=-1)
  df[,'network_id']
}

get.station.id <- function(con, native.network.id, native.id, log='log.txt') {
  network.id <- get.network.id(con, native.network.id, log)
  q <- paste("SELECT station_id FROM meta_station WHERE native_id = '", native.id, "' AND network_id = ", network.id, sep='')
  rv <- query(con, q)
  df <- fetch(rv, n=-1)
  df[,'station_id']
}

## Return the native_id from a station_name and network_id
stn.name.net.id.to.stn.id <- function(con, stn.name, net.id) {
  q <- sprintf("SELECT station_id FROM meta_station NATURAL JOIN meta_history WHERE station_name = '%s' AND network_id = %s", stn.name, net.id)
  res <- query(con, q)
  rv <- fetch(res, 1)[,'station_id']
  dbClearResult(res)
  rv
}

## Return the station_id associated with a history_id
hist.id.to.stn.id <- function(con, hist.id) {
  q <- sprintf("SELECT station_id FROM meta_history WHERE history_id = %s", hist.id)
  res <- query(con, q)
  rv <- fetch(res, 1)[,'station_id']
  dbClearResult(res)
  rv
}

## Returns the first history_id associated w/ a station_id
stn.id.to.hist.id <- function(con, stn.id) {
  q <- sprintf("SELECT history_id FROM meta_history WHERE station_id = %s", stn.id)
  res <- query(con, q)
  rv <- fetch(res, 1)[,'history_id']
  dbClearResult(res)
  rv
}

## Returns the network name from an associated network id
net.id.to.net.name <- function(con, net.id) {
  q <- sprintf("SELECT network_name FROM meta_network WHERE network_id = %s", net.id)
  res <- query(con, q)
  rv <- fetch(res, 1)[,'network_name']
  dbClearResult(res)
  rv
}

## Returns the network id from an associated network name
net.name.to.net.id <- function(con, net.name) {
  q <- sprintf("SELECT network_id FROM meta_network WHERE network_name = '%s'", net.name)
  res <- query(con, q)
  rv <- fetch(res, 1)[,'network_id']
  dbClearResult(res)
  rv
}

## Returns a character vector of matching station ids from a native id
native.id.to.stn.id <- function(con, native.id) {
  q <- sprintf("SELECT station_id FROM meta_station WHERE native_id = '%s'", native.id)
  res <- query(con, q)
  rv <- fetch(res, -1)[,'station_id']
  dbClearResult(res)
  rv
}

## Returns a character vector of matching station ids from a station name
stn.name.to.stn.id <- function(con, stn.name) {
  q <- sprintf("SELECT station_id FROM meta_history NATURAL JOIN meta_station WHERE station_name = '%s'", stn.name)
  res <- query(con, q)
  rv <- fetch(res, -1)[,'station_id']
  dbClearResult(res)
  rv
}

get.stnid.or.insert <- function(con, native.stn.id, native.network.id, log='log.txt') {
  network.id <- get.network.id.or.insert(con, native.network.id, log)

  q <- paste("SELECT station_id FROM meta_station WHERE native_id = '", native.stn.id, "'", sep='')

  rv <- query(con, q)
  df <- fetch(rv, n=-1)
  if (nrow(df) == 0) {
    q <- paste("INSERT INTO meta_station (network_id, native_id) VALUES (", network.id, ", '", native.stn.id, "')", sep='')

    rv <- query(con, q)
    if (dbGetRowsAffected(rv) > 0) {
      q <- paste("SELECT station_id FROM meta_station WHERE native_id = '", native.stn.id, "'", sep='')

      rv <- query(con, q)
      df <- fetch(rv, n=-1)
    }
    else
      stop("Could not insert", native.stn.id, "into the database, but it doesn't exist, either")
  }
  df[,'station_id']
}

get.network.id.or.insert <- function(con, native.network.id, log='log.txt') {
  q <- paste("SELECT network_id FROM meta_network WHERE network_name = '", native.network.id, "'", sep='')

  rv <- query(con, q)
  df <- fetch(rv, n=-1)
  if (nrow(df) == 0) {
    q <- paste("INSERT INTO meta_network (network_name) VALUES ('", native.network.id, "')", sep='')

    rv <- query(con, q)
    if (dbGetRowsAffected(rv) > 0) {
      q <-paste("SELECT network_id FROM meta_network WHERE network_name = '", native.network.id, "'", sep='')
      
      rv <- query(con, q)
      df <- fetch(rv, n=-1)
    }
    else
      stop("Could not insert", native.network.id, "into the database, but it doesn't exist, either")
  }
  df[,'network_id']
}

get.network.name <- function(con, db.hist.id) {
  q <- paste('SELECT network_name FROM meta_history NATURAL JOIN meta_station NATURAL JOIN meta_network WHERE history_id =', db.hist.id)
  rv <- query(con, q)
  df <- fetch(rv, n=-1)
  if (nrow(df) > 0)
    return(df[1,'network_name'])
  else stop("Could not find network name corresponding to history_id", db.hist.id, "in the database")
}

get.native.id <- function(con, db.hist.id) {
  q <- paste('SELECT native_id FROM meta_history NATURAL JOIN meta_station WHERE history_id =', db.hist.id)
  rv <- query(con, q)
  df <- fetch(rv, n=-1)
  if (nrow(df) > 0)
    return(df[1,'native_id'])
  else stop("Could not find station with station_id =", db.stn.id, "in the database")
}

## Takes a crmp object and returns the vars_ids (from meta_vars) associated with this station's network
db.get.vars.ids <- function(x, con, log='log.txt') {
  net.id <- get.network.id(con, x@network)
  sapply(names(x), function(v) {
    q <- paste(sep='', "SELECT vars_id FROM meta_vars WHERE network_id = ", net.id, " AND net_var_name = '", v, "'")
    rv <- query(con, q)
    df <- fetch(rv, n=-1)
    if (nrow(df) < 1) {
      cat(file=log, append=T, "Failed to find the vars_id of variable", x@network, v)
      stop("Failed to find the vars_id of variable", x@network, v)
    } else if (nrow(df) > 1) {
      cat(file=log, append=T, "Multiple vars_ids found for", x@network, v, ":", df)
      stop("Multiple vars_ids found for", x@network, v, ":", df)
    }
    df[1,'vars_id']
  })
}

db.get.flag.id <- function(con, network.id, flag.value, log='log.txt') {
  net.id <- get.network.id.or.insert(con, network.id)
  q <- paste(sep='', "SELECT native_flag_id FROM meta_native_flag WHERE network_id = ", net.id, " AND value = '", flag.value, "'")
  rv <- query(con, q)
  df <- fetch(rv, n=-1)
  if (nrow(df) < 1) {
    cat(file=log, append=T, "Failed to find the flag_id of flag", network.id, "(", net.id, ")", flag.value)
    stop("Failed to find the flag_id of flag", network.id, "(", net.id, ")", flag.value)
  }
  df[1, 'native_flag_id']
}

db.get.flag.value <- function(con, network.name, flag.name, log='log.txt') {
  q <- sprintf("SELECT value FROM meta_native_flag JOIN meta_network USING (network_id) WHERE network_name = '%s' AND flag_name = '%s'", network.name, flag.name)
  rv <- query(con, q)
  df <- fetch(rv, -1)
  if (nrow(df) < 1) {
    cat(file=log, append=T, "Failed to find the flag_value of flag", network.name, flag.name)
    stop("Failed to find the flag_id of flag ", network.name, ' ', flag.name)
  }
  df[1, 'value']
}  

split.station.by.history <- function(x, hist.table) {
  extract.one.location <- function(hist.entry) {
    t0 <- as.POSIXct(hist.entry['sdate'])
    attr(t0, 'tzone') <- 'GMT'
    tn <- as.POSIXct(hist.entry['edate'])
    attr(tn, 'tzone') <- 'GMT'
    x[t0, tn]
  }
  apply(hist.table, 1, extract.one.location)
}


## If you know the correct history_id, you can pass that in as a parameter
## If not this function will try and look up the history_id(s)
write.sql.crmp <- function(x, con, select.vars=names(x), log='log.txt', debug=F, hist.id=NULL) {
  ## Ensure network and station variables are in the db; insert if not
  net.id <- get.network.id.or.insert(con, x@network)

  ## Find the history id
  if (is.null(hist.id)) {
    q <- sprintf("SELECT history_id, sdate, edate FROM meta_history NATURAL JOIN meta_station NATURAL JOIN meta_network
                WHERE native_id = '%s' AND network_name = '%s'", x@station.id, x@network)
    hist.id <- fetch(query(con, q), -1)

    if (nrow(hist.id) < 1)
      stop("No history id yet exists for station:", x@network, x@station.id, x@station.name)

    ## Handle the case where a station has moved and has several history_ids (i.e. locations)
    else if (nrow(hist.id) > 1) {
      split.stations <- split.station.by.history(x, hist.id)
      ## Call write.sql.crmp for each history_id
      mapply(write.sql.crmp, x=split.stations, hist.id=hist.id[,'history_id'], MoreArgs=list(con=con, select.vars=select.vars, log=log, debug=debug))
      return()
    }    
    else { ## Else nrow(hist.id) == 1 and we're golden
      hist.id <- hist.id[,'history_id']
    }
  }
  ## Everything after this point corresponds to one and only one history_id

  vars.ids <- db.get.vars.ids(x, con)
  
  #q <- "SELECT count(obs_raw_id) FROM obs_raw"
  #n0 <- fetch(query(con, q))[,]

  t0 <- Sys.time()
  dbBeginTransaction(con)
  try.rv <- try(
      for (v in select.vars) {
        i <- which(!is.na(x[,v]))
        a <- x[i,v,drop=F]

        if (nrow(a) == 0 || ncol(a) == 0)
          next

        vars.id <- vars.ids[v]
        print(paste("Using vars.id =", vars.id))

        format.time <- format(a@time, '%F %T', tz='GMT')
        df <- data.frame('history_id'=hist.id, 'vars_id'=vars.id, 'obs_time'=format.time, 'mod_time'=format(Sys.time(), tz='GMT'), datum=a[,1,drop=T])
        #f <- tempfile()
        #write.csv(df, file=f, row.names=F)

        q <- "COPY obs_raw (history_id, vars_id, obs_time, mod_time, datum) FROM STDIN"
        rv <- query(con, q)
        copy.rv <- postgresqlCopyInDataframe(con, df)
        n <- dbGetRowsAffected(rv)

        print(paste("Inserted", n, "rows into obs_raw (", hist.id, x@station.id, ",", vars.id, v, ")"))
        #unlink(f)
      }, silent=T
      )

  if (inherits(try.rv, 'try-error') || dbGetException(con)$errorNum != 0) {
    browser()
    print("Rolling back")
    print(dbGetException(con)$errorMsg)
    dbRollback(con)
  }
  else if (debug) {
    print("Debug enabled, Rolling back")
    dbRollback(con)
  }
  else {
    print("Committing")
    dbCommit(con)

    #q <- "SELECT count(obs_raw_id) FROM obs_raw"
    #np <- fetch(query(con, q))[,]
    print(paste("Inserted some rows into obs_raw in", Sys.time() - t0))
    ## FIXME: Need to make sure this works when native flags aren't found (e.g. for MoE_AIRPROD1/2)
    insert.native.flags(x, con, hist.id)
  }
  return()
}

query.variable.descriptions <- function(con, net_var_names_caseless, network_name) {

  vars <- net_var_names_caseless
  tmp_table <- paste(sep='', "(VALUES ('", paste(collapse = "'), ('", vars), "')) AS foo")
  
  ## Query the variable descriptions
  q <- sprintf("SELECT vars_id, unit, standard_name, cell_method, long_description, net_var_name as name FROM %s LEFT JOIN meta_vars NATURAL JOIN meta_network ON (foo.column1 = lower(meta_vars.net_var_name)) WHERE network_name = '%s'", tmp_table, network_name)

  res <- query(con, q)
  vars <- fetch(res, n=-1)
  rownames(vars) <- vars[,'vars_id']
  vars
}

read.sql.crmp.names <- function(con, network.name, native.station.id, freq='hour', avail.thresh=.85) {
  stn.id <- get.station.id(con, network.name, native.station.id)
  read.sql.crmp(con, stn.id)
}

## hist.id should be the database history.id and likewise for the stn.id
read.sql.crmp <- function(con, stn.id=NULL, hist.id=NULL) {
  if (is.null(hist.id)) {
    if (is.null(stn.id)) {
      stop("Must provide at least one of a PCIC station id or a PCIC history_id")
    } else hist.id <- stn.id.to.hist.id(con, stn.id)
  } else stn.id <- hist.id.to.stn.id(con, hist.id)

  network.name <- get.network.name(con, hist.id)
  native.station.id <- get.native.id(con, hist.id)

  q <- get.sql.for.data.fetch(con, stn.id)
  df <- fetch(dbSendQuery(con, q), -1)

  # strip off the time, save for later
  t <- df[,'obs_time']
  df <- df[,-1]

  rv <- df[,grep('_flag$', colnames(df), invert=T)]
  fg <- df[,grep('_flag$', colnames(df), invert=F)]

  vars <- query.variable.descriptions(con, colnames(rv), network.name)
  colnames(rv) <- vars[,'name']

  ## Convert the flag names to the numerical values
  for (flag in unique(unlist(fg))) {
    if (is.na(flag)) next
    val <- db.get.flag.value(con, network.name, flag)
    fg <- replace(fg, fg == flag, val)
  }
  ## Assign applicable vars descriptor
  applicable.vars <- gsub('_flag$', '', colnames(fg))
  applicable.vars <- query.variable.descriptions(con, applicable.vars, network.name)[,'name']

  flags <- new('flag', fg,
               qc.operation="As supplied",
               applicable.vars=applicable.vars)

  new('crmp', rv,
      station.id = native.station.id,
      network = network.name,
      'cf:units' = vars[, 'unit'],
      'cf:standard_name' = vars[,'standard_name'],
      'cf:cell_method' = vars[,'cell_method'],
      'cf:long_description' = vars[,'long_description'],
      time=t,
      flags=list(flags))
}

# This function takes:
#   -a postgres connection with read permissions on the crmp database
#   -enough identifying information to determine a history_id or station_id
#    being the first of:
#       -hist.id
#       -stn.id
#       -native.id and net.id
#       -native.id and net.name
#       -native.id
#       -stn.name and net.id
#       -stn.name and net.name
#       -stn.name
# Returns:
#   -A crmp object
search.sql.crmp <- function(con, ...) {
  expected.args <- list(hist.id=NULL, stn.id=NULL, native.id=NULL,
              net.name=NULL, net.id=NULL, stn.name=NULL)
  params <- modifyList(expected.args, list(con=con, ...))
  
  hist.id <- params$hist.id
  stn.id <- params$stn.id
  native.id <- params$native.id
  net.id <- params$net.id
  stn.name <- params$stn.name
  net.name <- params$net.name
  
  # Only history_id
  if (!is.null(hist.id)) {
    print(paste("Searching for history id", hist.id))
    return(read.sql.crmp(con, hist.id=hist.id))
  }
  # Only station_id
  else if (!is.null(stn.id)) {
    print(paste("Searching for station id", stn.id))
  }
  # native_id and network_name
  else if (!is.null(native.id) & !is.null(net.name)) {
    print(paste("Searching for native.id", native.id, "on the", net.name, "network"))
    stn.id <- get.station.id(con, net.name, native.id)
  }
  # native_id and network_id
  else if (!is.null(native.id) & !is.null(net.id)) {
    print(paste("Searching for native.id", native.id, "on network id", net.id))
    net.name <- net.id.to.net.name(con, net.id)
    stn.id <- get.station.id(con, net.name, native.id)
  }
  # attempt solely upon native_id, relies upon native_id being unique
  else if (!is.null(native.id)) {
    print(paste("Searching for native station id", native.id))
    stn.id <- native.id.to.stn.id(con, native.id)
    if (length(stn.id) > 1)
      stop(paste("Unable to determine a unique station_id from native_id", native.id, "found station ids:", stn.id))
  }
  # station_name and network_name
  else if (!is.null(stn.name) & !is.null(net.name)) {
    print(paste("Searching for station", stn.name, "on the", net.name, "network"))
    net.id <- net.name.to.net.id(con, net.name)
    stn.id <- stn.name.net.id.to.stn.id(con, stn.name, net.id)
  }
  #station_name and network_id
  else if (!is.null(stn.name) & !is.null(net.id)) {
    print(paste("Searching for station", stn.name, "on network id", net.id))
    stn.id <- stn.name.net.id.to.stn.id(con, stn.name, net.id)
  }
  # attempt solely upon station_name, relies upon name being unique and available
  else if (!is.null(stn.name)) {
    print(paste("Searching for station name", stn.name))
    stn.id <- stn.name.to.stn.id(con, stn.name)
    if (length(stn.id) > 1)
      stop(paste("Unable to determine a unique station_id from station", stn.name, "found station ids:", paste(stn.id, sep=' ', collapse=' ')))
  }
  else {
    stop("Unsufficient information was given to sucessfully determine a station or history id")
  }
  return(read.sql.crmp(con, stn.id=stn.id))
}

db.get.obs.id <- function(con, hist.id, var.id, t, log='log.txt') {
  q <- paste('SELECT obs_raw_id FROM obs_raw WHERE history_id = ', hist.id, ' AND vars_id = ', var.id, " AND obs_time = '", t, "'", sep='')
  rv <- query(con, q)
  df <- fetch(rv, n=-1)
  if (nrow(df) < 1) {
    cat(file=log, append=T, "Failed to find the obs_raw_id", hist.id, var.id, t)
    warning(paste("Failed to find the obs_raw_id", hist.id, var.id, t))
    return(NA)
  }
  df[1, 'obs_raw_id']  
}

insert.native.flags <- function(x, con, hist.id) {
  net.id <- get.network.id.or.insert(con, x@network)
  stn.id <- get.stnid.or.insert(con, x@station.id, x@network)
  fg <- x@flags
  i <- grep('(As Supplied|from original data)', sapply(fg, slot, 'qc.operation'))

  if (length(i) < 1) {
    warning(paste("Station", x@network, x@station.id, "has no native flags"))
    return()
  }

  ## Account for native flags being supplied, but no obs in x have flags or flags are NA
  if (all(as.data.frame(x@flags)=='', na.rm=T)) {
    warning(paste("Station", x@network, x@station.id, "has no native flags"))
    return()
  }
  
  fg <- fg[[ i ]]
  vars.ids <- db.get.vars.ids(x, con)

  if (is.factor(fg[[1]]))
    flag.values <- unique(unlist(c(sapply(fg, levels))))
  else
    flag.values <- unique(unlist(c(sapply(fg, unique))))

  flag.values <- flag.values[flag.values != '' & ! is.na(flag.values)]
  flag.ids <- mapply(db.get.flag.id, flag.value=flag.values, MoreArgs=list(con=con, network.id=x@network))

  for (v in fg@applicable.vars) {
    var.id <- vars.ids[v]
    for (f in flag.values) {
      fid <- flag.ids[f]
      i <- which(fg[, v] == f) ## FIXME: names and flag names don't necessarily agree...
      t <- x@time[i]
      if (length(t) < 1)
        next

      oids <- mapply(db.get.obs.id, t, MoreArgs=list(con=con, hist.id=hist.id, var.id=var.id))
      ## It's _possible_ that NA's have been flags in which case they don't exist in the database
      ## db.get.obs.id will return NA in this case, so just filter these out
      oids <- oids[!is.na(oids)]

      if (!exists('to.insert'))
        to.insert <- data.frame('obs_raw_id'=oids, 'native_flag_id'=rep(fid, length(oids)))
      else
        to.insert <- rbind(to.insert, data.frame('obs_raw_id'=oids, 'native_flag_id'=rep(fid, length(oids))))
    }
  }

  f <- tempfile()
  write.csv(to.insert, file=f, row.names=F)

  q <- "COPY obs_raw_native_flags (obs_raw_id, native_flag_id) FROM STDIN WITH CSV HEADER"
  dbBeginTransaction(con)  
  rv <- query(con, q)
  copy.rv <- postgresqlCopyIn(con, f)
  dbCommit(con)
}

as.values <- function(x, types) {
  x <- mapply(as, x, types, SIMPLIFY=F)
  y <- sapply(x,
              function(e) {
                if (length(e) == 0) 'NULL' # check for NULL or NULL that got typecast to a zero length numeric/character
                else if (is.na(e) || e == 'NA') 'NULL'
                else if (is.character(e)) if (is.na(e) || e == "") NULL else paste("E'", gsub("'", "\\\\'", e, ), "'", sep='')
                else if (is.numeric(e) && is.na(e)) 'NULL'
                else e
              }
              )
  paste("(", paste(y, collapse=', '), ")")
}


library(rgdal)
fill.history.table <- function(con,
                               metadata.dir='/home/data/projects/crmp/haileye_space',
                               metadata.file='crmp_historical_metadata',
                               log='log.txt') {

  data <- readOGR(metadata.dir, metadata.file)
  fields <- c('station_id', 'station_name', 'elev', 'lon', 'lat')
  subset <- data[,c('stnid', 'stnname', 'elev', 'network')]

  for (i in 1:nrow(subset)) {

    net <- as.character(subset[i,'network'][[1]])
    stn <- as.character(subset[i,'stnid'][[1]])
    stn.id <- get.stnid.or.insert(con, stn, net)
    

    vlist <- lapply(c("stnname", "elev"), function(X) {subset[i,][[X]]})
    vlist <-  append(append(vlist, stn.id, after=0), coordinates(subset[i,]))
    values <- as.values(vlist, c('numeric', 'character', rep('numeric', 3)))
    q <- paste("INSERT INTO meta_history (", paste(fields, collapse=', '), ") VALUES ", values)
    rv <- query(con, q)

    if (dbGetRowsAffected(rv) > 0) {
      q <- "UPDATE meta_history SET the_geom = ST_SetSRID(ST_Point(lon,lat),4326)"
      rv <- query(con, q)

    } else {
      cat(file=log, append=T, "ERROR: Could not insert into meta_history for row", i, q, "\n")
    }

  }
}

update.history.table <- function(con,
                                 metadata.dir='/home/data/projects/crmp/haileye_space',
                                 metadata.file='crmp_active_metadata',
                                 log='log.txt') {

  data <- readOGR(metadata.dir, metadata.file, stringsAsFactors=F)

  ## Only insert the stations that we don't have a history for
  q <- "SELECT native_id FROM meta_station natural join meta_network WHERE station_id NOT IN (SELECT station_id from meta_history)"
  rv <- fetch(query(con, q), n=-1)
  i <- which(data$'Station_Co' %in% rv[,'native_id'])

  fields <- c('station_id', 'station_name', 'elev', 'lon', 'lat')
  subset <- data[i,c('Station_Co', 'Station_Na', 'Elevation_', 'network')]

  for (i in 1:nrow(subset)) {

    net <- as.character(subset[i,'network'][[1]])
    stn <- as.character(subset[i,'Station_Co'][[1]])
    stn.id <- get.stnid.or.insert(con, stn, net)
    
    vlist <- lapply(c("Station_Na", "Elevation_"), function(X) {subset[i,][[X]]})
    vlist <-  append(append(vlist, stn.id, after=0), coordinates(subset[i,]))
    values <- as.values(vlist, c('numeric', 'character', rep('numeric', 3)))
    q <- paste("INSERT INTO meta_history (", paste(fields, collapse=', '), ") VALUES ", values)
    rv <- query(con, q)

    if (dbGetRowsAffected(rv) > 0) {
      q <- "UPDATE meta_history SET the_geom = ST_SetSRID(ST_Point(lon,lat),4326)"
      rv <- query(con, q)

    } else {
      cat(file=log, append=T, "ERROR: Could not insert into meta_history for row", i, q, "\n")
    }

  }
}

## add all the rest of the active points that didn't get put in the first time
update.history.table.hde <- function(con,
                                     metadata.dir='/home/data/projects/crmp/haileye_space',
                                     metadata.file='crmp_active_metadata_with_reasonable_stnids',
                                     log='log.txt') {
  
  data <- readOGR(metadata.dir, metadata.file, stringsAsFactors=F)
  
  ## Only insert the stations that we don't have a history for
  ## unsure why you did the NOT IN 
  #q <- "SELECT native_id FROM meta_station natural join meta_network WHERE station_id NOT IN (SELECT station_id from meta_history)"
  q <- "SELECT native_id FROM meta_station natural join meta_network"
  rv <- fetch(query(con, q), n=-1)
  i <- which(!data$'Station_Co' %in% rv[,'native_id'])

  fields <- c('station_id', 'station_name', 'elev', 'lon', 'lat')
  subset <- data[i,c('Station_Co', 'Station_Na', 'Elevation_', 'network')]
  #
  my.count <- 0
  for (i in 1:nrow(subset)) {
    
    net <- as.character(subset[i,'network'][[1]])
    stn <- as.character(subset[i,'Station_Co'][[1]])
    stn.id <- get.stnid.or.insert(con, stn, net)
    
    vlist <- lapply(c("Station_Na", "Elevation_"), function(X) {subset[i,][[X]]})
    vlist <-  append(append(vlist, stn.id, after=0), coordinates(subset[i,]))
    # why is there an 'E' in the statement
    # ie: values
    # [1] "( 3134, E'Mt. Seymour', 910, -122.9493778, 49.36829722 )"
    
    values <- as.values(vlist, c('numeric', 'character', rep('numeric', 3)))
    # don't you need to update the meta_station as well to record which native id goes with which?
    # STOP HERE
    q <- paste("INSERT INTO meta_history (", paste(fields, collapse=', '), ") VALUES ", values)
    rv <- query(con, q)
    if (dbGetRowsAffected(rv) == 0) {
      cat(file=log, append=T, "ERROR: Could not insert into meta_history for row", i, q, "\n")
      my.count <- my.count + dbGetRowsAffected(rv)
    }

  }
  if (my.count > 0) {
    q <- "UPDATE meta_history SET the_geom = ST_SetSRID(ST_Point(lon,lat),4326)"
    rv <- query(con, q)
  }
}

## Lazy, lazy
update.history.table.again <- function(con,
                                       metadata.dir='/home/data/projects/data_cleanup/CDCD_2007',
                                       metadata.file='bc_stnlist.csv',
                                       log='log.txt') {

  data <- read.csv(file.path(metadata.dir, metadata.file))
  data$network <- 'EC'

  ## Only insert the stations that we don't have a history for
  q <- "SELECT native_id FROM meta_station natural join meta_network WHERE station_id NOT IN (SELECT station_id from meta_history)"
  rv <- fetch(query(con, q), n=-1)
  i <- which(data$stnid %in% rv[,'native_id'])

  fields <- c('station_id', 'station_name', 'elev', 'lon', 'lat', 'sdate', 'edate')
  subset <- data[i,c('stnid', 'stnname', 'elev', 'network', 'coords.x1', 'coords.x2', 'startyear', 'endyear')]

  for (i in 1:nrow(subset)) {

    net <- as.character(subset[i,'network'][[1]])
    stn <- as.character(subset[i,'stnid'][[1]])
    stn.id <- get.stnid.or.insert(con, stn, net)
    
    vlist <- lapply(c("stnname", "elev"), function(X) {subset[i,][[X]]})
    vlist <-  append(append(vlist, stn.id, after=0), subset[i,c('coords.x1', 'coords.x2')])
    vlist <- append(vlist, paste(subset[i,'startyear'], 1, 1, sep='/'))
    vlist <- append(vlist, paste(subset[i,'endyear'], 12, 31, sep='/'))
    values <- as.values(vlist, c('numeric', 'character', rep('numeric', 3), rep('character', 2)))
    q <- paste("INSERT INTO meta_history (", paste(fields, collapse=', '), ") VALUES ", values)
    print(q)
    rv <- query(con, q)

    if (dbGetRowsAffected(rv) > 0) {
      q <- "UPDATE meta_history SET the_geom = ST_SetSRID(ST_Point(lon,lat),4326)"
      rv <- query(con, q)

    } else {
      cat(file=log, append=T, "ERROR: Could not insert into meta_history for row", i, q, "\n")
    }

  }
}

insert.metadata.frbc <- function(con,
                               metadata.file='/home/data/projects/crmp/data/FRBCClimate/FRBCClimate_Meta.csv',
                               log='log.txt') {

  network <- 'FRBC'
  data <- read.csv(metadata.file, col.names=c('stnid', 'stnname', 'lat', 'lon'))
  fields <- c('station_id', 'station_name', 'lon', 'lat', 'freq')
  ##subset <- data[,c('stnid', 'stnname', 'network')]

  for (i in 1:nrow(data)) {

    stn <- as.character(data[i,'stnid'][[1]])
    stn.id <- get.stnid.or.insert(con, stn, network)
    
    vlist <- lapply(c("stnname", "lon", "lat"), function(X) {data[i,X]})
    vlist <- append(append(vlist, stn.id, after=0), 'hourly')
    values <- as.values(vlist, c('numeric', 'character', rep('numeric', 2), 'character'))
    q <- paste("INSERT INTO meta_history (", paste(fields, collapse=', '), ") VALUES ", values)
    print(q)
    rv <- query(con, q)

    if (dbGetRowsAffected(rv) > 0) {
      q <- "UPDATE meta_history SET the_geom = ST_SetSRID(ST_Point(lon,lat),4326)"
      rv <- query(con, q)

    } else {
      cat(file=log, append=T, "ERROR: Could not insert into meta_history for row", i, q, "\n")
    }

  }
}


fill.MoTIme <- function(con,
                        metadata.file = '/home/data/projects/crmp/data/MoTI/metadata/Inventory for PCIC 110420.csv',
                        log = 'log.txt') {

  md <- read.csv(metadata.file, colClasses=list(Stn.Code='character', Name='character', Start.Date='character', End.Date='character', Lat='numeric', Lon='numeric'))
  md$Start.Date <- format(strptime(md$Start.Date, '%y%m%d'), '%Y/%m/%d', tz='GMT')
  md$End.Date <- format(strptime(md$End.Date, '%y%m%d'), '%Y/%m/%d', tz='GMT')

  for (i in 1:nrow(md)) {

    types <- c('numeric', 'character', rep('numeric', 3), rep('character', 2))
    net <- if(md[i,'Type'] %in% c('RWS', 'RAWS')) 'MoTIe' else 'MoTIm'
    stn <- md[i, 'Stn.Code']
    stn.id <- get.stnid.or.insert(con, stn, net)

    fields <- c('station_id', 'station_name', 'elev', 'lon', 'lat', 'sdate', 'edate')

    vlist <- as.list(md[i,c('Name', 'Elevation', 'Lon', 'Lat', 'Start.Date', 'End.Date')])
    vlist <-  append(vlist, stn.id, after=0)
    ina <- sapply(vlist, is.na)

    vlist <- vlist[!ina]
    fields <- fields[!ina]
    types <- types[!ina]
    values <- as.values(vlist, types)

    ## Ensure that this entry doesn't already exist
    sdate <- md[i,'Start.Date']; edate <- md[i, 'End.Date']
    q <- paste(sep='', "SELECT history_id FROM meta_history WHERE station_id = ", stn.id)
    if (! is.na(sdate)) q <- paste(q, sep='', " AND sdate = '", sdate, "'")
    if (! is.na(edate)) q <- paste(q, sep='', " AND edate = '", edate, "'")

    rv <- query(con, q)
    n <- dbGetRowsAffected(rv)
    dbClearResult(rv)
    if (n > 0) {
      cat(file=log, append=T, "History already exists for", stn.id, "(", stn, ")", sdate, edate)
      next
    }

    q <- paste("INSERT INTO meta_history (", paste(fields, collapse=', '), ") VALUES ", values)
    rv <- query(con, q)

    if (dbGetRowsAffected(rv) > 0) {
      q <- "UPDATE meta_history SET the_geom = ST_SetSRID(ST_Point(lon,lat),4326)"
      rv <- query(con, q)

    } else {
      cat(file=log, append=T, "ERROR: Could not insert into meta_history for row", i, q, "\n")
    }


  }
}

fill.var.tables <- function(con, net.names=NULL, log='log.txt') {
  if (is.null(net.names)) {
    q <- 'SELECT network_name FROM meta_network'
    rv <-  query(con, 'SELECT network_name FROM meta_network')
    net.names <- fetch(rv, n=-1)[,]
  }
  db.fields <- c('network_id', 'unit', 'standard_name', 'cell_method', 'long_description', 'net_var_name')

  for (network in net.names) {
    net.id <- get.network.id.or.insert(con, network)
    defs <- get.variable.defs(if(network %in% c('SNOWPILLOW', 'EC')) 'BCH' else network)
    defs$native.name <- rownames(defs)
    defs$net.id <- net.id

    vlist <- apply(defs[,c('net.id', 'network.unit', 'cf.var.name', 'cf.cell.method', 'network.long.description', 'native.name')], 1,
                   as.values, c('numeric', rep('character', 5)))
    q <- paste("INSERT INTO meta_vars (", paste(db.fields, collapse=', '), ") VALUES ", paste(vlist, collapse=', '))

    rv <- query(con, q)
    if (dbGetRowsAffected(rv) < 1)
      cat(file=log, append=T, 'Failed to insert', defs)
  }
  
}

obs.in.db <- function(x, con) {
  if (inherits(x, 'crmp')) {
    stn.id <- get.stnid.or.insert(con, x@station.id, x@network)
    q <- paste("SELECT count(*) FROM obs_raw WHERE station_id =", stn.id)
  } else if (inherits(x, 'character')) {
    q <- paste(sep='', "SELECT count(*) FROM obs_raw, meta_station WHERE obs_raw.station_id = meta_station.station_id AND meta_station.native_id = '", x, "'")
  }
  n <- fetch(query(con, q), n=-1)[,]
  return(n > 0)
}

flags.in.db <- function(x, con) {
  stn.id <- get.stnid.or.insert(con, x@station.id, x@network)
  q <- paste("SELECT count(fg.pcic_flag_id) FROM obs_raw AS obs, obs_raw_pcic_flags as fg WHERE obs.station_id =", stn.id, "and obs.obs_raw_id = fg.obs_raw_id" )
  n <- fetch(query(con, q, n=-1))[,]
  return(n > 0)
}

unique.times <- function(x) {
  t <- x@time
  length(t) == length(unique(t))
}

has.unique.times <- function(files) {
  sapply(files, function(f) {
    x <- import.crmp(f)
    rv <- unique.times(x)
    names(rv) <- x@station.id
    rv
  })
}

fix.sbc <- function(con) {
  q <- "SELECT station_id, native_id FROM meta_station WHERE native_id LIKE 'SBC %'"
  df <- fetch(query(con, q), n=-1)
  df$real.native.id <- gsub(' ', '', df[,'native_id'])
  df$real.station.id <- sapply(df$real.native.id, function(id) {q <- paste(sep='', "SELECT station_id FROM meta_station WHERE native_id = '", id, "'"); res <- query(con, q); fetch(res, -1)[,]})
  apply(df, 1, function(x) {
    dbBeginTransaction(con)
    q <- paste("UPDATE meta_history SET station_id =", x['real.station.id'], "WHERE station_id =", x['station_id'])
    query(con, q)
    q <- paste("DELETE FROM meta_station WHERE station_id =", x['station_id'])
    query(con, q)
    dbCommit(con)
  })
}

queries.for.joe <- c('SELECT flag_name, description FROM meta_native_flag',
                     'SELECT network_name, description FROM meta_network',
                     'SELECT station_id, network_id, native_id, FROM meta_station',
                     'SELECT station_id, station_name, lon, lat, elev, sdate, edate FROM meta_history'
                     )


load.all <- function(files) {
  for (f in files) {
    x <- import.crmp(f)
    rv <- try(write.sql.crmp(x, con))
    if (inherits(rv, 'try-error'))
      warning("Could not insert data for station", x@network, x@station.id)
  }
}

# Uses the databases's query_one_station function to return and SQL statement that will
# return an entire station table joined on time, with one column per variable and one column per flag
get.sql.for.data.fetch <- function(con, stn.id) {
  fetch(dbSendQuery(con, sprintf('SELECT query_one_station(%d)', stn.id)), -1)[,]
}

#        "SELECT daily_ts(*, .85) FROM obs_raw GROUP BY vars_id HAVING "
queries.for.joe <- function(con, out="insert_meta_native_flags.txt") {
  q <- 'SELECT flag_name, description FROM meta_native_flag'
  res <- query(con, q)
  data <- fetch(res, -1)

  s <- paste('INSERT INTO meta.native_flag (name,"desc") VALUES (\'', data[,'flag_name'], '\',\'', data[,'description'], '\')', sep='')
  write.table(file=out, sep='', s, row.names=F, col.names=F, quote=F)
}

insert.duped.moti <- function(con, dup.file='moti_dups.txt', data.dir='/home/data/projects/crmp/data/MoTI/data') {
  non.inserted <- read.csv(dup.file, header=F, col.names=c("stn.id", "network"))

  files <- list.files(file.path(data.dir, c('manual', 'electronic')), full.names=T)
  non.inserted <- read.csv('are.not.unique.txt', header=F, col.names=c("stn.id", "network"))
  browser()
  x <- file.path(data.dir, sapply(as.character(non.inserted[,'network']), switch, MoTIm='manual', MoTIe='electronic'), paste(non.inserted[,'stn.id'], '.txt', sep=''))

  files <- intersect(files, x) 
 
  for (f in files) {

    read.f <- if (grepl('electronic', f)) read.crmp.MoTIe
              else if (grepl('manual', f)) read.crmp.MoTIm
              else stop('Can\'t determine read function to use')

    data <- read.f(f)
    print(paste("Inserting station", data@network, data@station.id))
    i <- which(diff(data@time) == 0)
    data <- data[-i,]

    stn.id <- get.stnid.or.insert(con, data@station.id, data@network)
    q <- paste("SELECT count(*) as the_count FROM obs_raw WHERE station_id =", stn.id)
    rv <- query(con, q)
    n <- fetch(rv, n=-1)[1,'the_count']
    if (n > 0)
      next

    rv <- try(write.sql.crmp(data, con))
    if (inherits(rv, 'try-error'))
      warning("Could not insert data for station", data@network, data@station.id)
  }

}

replace.ec.locations <- function(con, csv.file) {
  data <- read.csv(csv.file, as.is=T, colClasses=list('Station_Code'='character'))
  data <- data[,c('Latitude','Longitude','Station_Code', 'Elevation..m.')]

  fun <- function(x) { # lat, lon, code, elev
    lat <- x[1]; lon <- x[2]; code <- x[3]; elev <- x[4]
    q <- paste(sep='', "UPDATE meta_history SET (lat, lon, elev) = (", lat, ",", lon, ",", elev, ") FROM meta_station WHERE meta_history.station_id = meta_station.station_id AND native_id = '", code, "'")
    print(q)
    rv <- query(con, q)
    print(paste("Updated", dbGetRowsAffected(rv), "rows"))
  }
  apply(data, 1, fun)
  q <- 'UPDATE meta_history SET the_geom = ST_SetSRID(ST_Point(lon,lat),4326)'
  rv <- query(con, q)
  dbGetRowsAffected(rv)
}

import.moe.ap.locations <- function(con, csv.file='/home/data/projects/crmp/data/MoE_AIRPROD1/data/CLIMATE_fix_station_name_errors.csv') {
  data <- read.csv(csv.file)
  data$lat <- data$'LAT_DEG' + data$'LAT_MIN' / 60 + data$'LAT_SEC' / 3600
  data$lon <- data$'LONG_DEG' + data$'LONG_MIN' / 60 + data$'LONG_SEC' / 3600
  data$lon <- data$lon * -1 # They record their degrees in positive degrees east or something.
  on.cent <- sapply(data$ONDATE, function(c) {if (c >= 120000) 19 else 20})
  off.cent <- sapply(data$ONDATE, function(c) {if (c >= 120000) 19 else 20})
  data$sdate <- as.POSIXct(paste(sep='', on.cent, data[,'ONDATE'], ' ', sprintf('%04d', data[,'ONTIME'])), format='%Y%m%d %H%M', tz='GTM')
  data$edate <- as.POSIXct(paste(sep='', off.cent, data[,'OFFDATE'], ' ', sprintf('%04d', data[,'OFFTIME'])), format='%Y%m%d %H%M', tz='GTM')

  do.one.station <- function(df) {
    x <- rownames(unique(df[,c('LAT_DEG', 'LAT_MIN', 'LAT_SEC', 'LONG_DEG', 'LONG_MIN', 'LONG_SEC')]))
    #vars <- unique(df[,'PARAMETER'])
    stn.name <- as.character(unique(df[,'NAME']))
    native.id <- unique(df[,'STATIONNO'])
    if((length(stn.name) != 1) || (length(native.id) != 1)) browser()
    stn.id <- get.stnid.or.insert(con, native.id, 'MoE_AP')
    ## Do insertation into meta_station
    insert.one.history <- function(row.name) {
      if (native.id == 'ANTLER') browser()
      arow <- df[row.name,]
      lat <- arow$lat; lon <- arow$lon
      elev <- arow$ELEVATION
      i <- df[,'lat'] == lat & df[,'lon'] == lon
      sdate <- min(df[i,'sdate']); edate <- max(df[i,'edate'])
      print(paste(native.id, stn.name, paste(lat, lon, sep=', '), elev, sdate, edate))
      ## Do insertion into meta_history
      values <- as.values(list(stn.id, stn.name, lon, lat, elev, sdate, edate), c('numeric', 'character', rep('numeric', 3), rep('character', 2)))
      q <- paste("INSERT INTO meta_history (station_id, station_name, lon, lat, elev, sdate, edate) VALUES", values)
      rv <- query(con, q)
      print(q)
    }
    lapply(x, insert.one.history)
  }
  dbBeginTransaction(con)
  unlist(by(data, data$STATIONNO, do.one.station))
  q <- 'UPDATE meta_history SET the_geom = ST_SetSRID(ST_Point(lon,lat),4326)'
  rv <- query(con, q)
  dbGetRowsAffected(rv)
  dbCommit(con)
}
 
update.freq <- function(con, hist.id, freq.desc) {
  if(is.na(freq.desc) || is.na(hist.id)) return
  q <- sprintf("UPDATE meta_history SET new_freq = '%s' WHERE history_id = %d", freq.desc, hist.id)
  res <- dbSendQuery(con, q)
  print(sprintf("Updated %d rows", dbGetRowsAffected(res)))
}
