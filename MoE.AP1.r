source('crmp-class.r')

## The precip data in MoE_AP1 has irregular times of observations and we need to track the dates of accumulation
## This requires some slightly different handling of certain things, in particular database inserts
setClass('MoE.AP1',
         representation('crmp'),
         contains='crmp'
         )

MoE.AP1.valid <- function(x) {
  if(all(names(x) == c("ACCPPT1", "DURATION1", "ACCPPT2", "DURATION2", "SNOWDEPTH")))
    return(T)
  else
    return('Unexpected names')
}

write.sql.MoE.AP1 <- function(x, con) {
  ## split the object into observations and time bounds
  i <- grep('^DURATION.', names(x))
  obs <- x[,-i,drop=F]
  time.bounds <- x[,i,drop=F]

  rv <- write.sql.crmp(obs, con, debug=F)

  insert.time.bounds <- function(obs, time.bounds, hist.id, var.name, bounds.name) {
    print(sprintf('insert.time.bounds(%s, %s)', var.name, bounds.name))
    ## Remove NA's from the object obs and time.bounds
    i <- which(is.na(obs[,var.name]) | is.na(time.bounds[,bounds.name]))
    my.time.bounds <- time.bounds@time[-i]

    ## It's possible that some variables have all NA's
    if (length(my.time.bounds) < 1) {
      return(NULL)
    }
    
    ## Get the observations ids
    ## We don't need the history_id(s) since we only really care about the obs_ids
    q <- sprintf("SELECT obs_raw_id, obs_time FROM obs_raw NATURAL JOIN meta_history NATURAL JOIN meta_station NATURAL JOIN meta_network NATURAL JOIN meta_vars WHERE native_id = '%s' AND network_name = '%s' AND net_var_name = '%s' AND history_id = %s AND obs_time in (%s) ORDER BY obs_time", x@station.id, x@network, var.name, hist.id, paste(collapse=', ', paste(sep='', "'", strftime(my.time.bounds, tz='GMT'), "'")))
    print(q)
    res <- query(con, q)
    df <- fetch(res, -1)
    ## Bug in RPostgreSQL where it doesn't convert the pg type "timestamp without time zone" to POSIXct properly
    obs.times <- as.POSIXct(strftime(df$'obs_time'), tz='GMT')

    ## Match df obs_time to the time.bounds obs_time
    stopifnot(all(obs.times == my.time.bounds))
    t.diff <- as.difftime(time.bounds[-i,bounds.name,drop=T], units='days')
    t.start <- my.time.bounds - t.diff
    if (any(is.na(t.start)))
      browser()
    t.end <- my.time.bounds
    q <- sprintf('INSERT INTO time_bounds VALUES (%d, \'%s\', \'%s\')', df[,'obs_raw_id'], t.start, t.end)
    sapply(q, query, con=con)
  }

  ## Find history.id's for this station
  q <- sprintf("SELECT history_id, sdate, edate FROM meta_history NATURAL JOIN meta_station NATURAL JOIN meta_network
                WHERE native_id = '%s' AND network_name = '%s'", x@station.id, x@network)
  hist.id <- fetch(query(con, q), -1)

  if (nrow(hist.id) < 1)
    stop("No history id yet exists for station:", x@network, x@station.id, x@station.name)

  ## Handle the case where a station has moved and has several history_ids (i.e. locations)
  else if (nrow(hist.id) > 1) {
    split.obs <- split.station.by.history(obs, hist.id)
    split.time <- split.station.by.history(time.bounds, hist.id)
    ## Call insert.time.bounds for each history_id and each variable
    for (h in 1:nrow(hist.id)){
      hid <- hist.id[h,'history_id']
      mapply(insert.time.bounds, hist.id=hid, var.name=c('ACCPPT1', 'ACCPPT2'),
                                 bounds.name=c('DURATION1', 'DURATION2'),
                                 MoreArgs=list(obs=split.obs[[h]], time.bounds=split.time[[h]]))
    }
    return()
  }
  else {
    mapply(insert.time.bounds, var.name=c('ACCPPT1', 'ACCPPT2'), bounds.name=c('DURATION1', 'DURATION2'),
                               MoreArgs=list(obs=obs, time.bounds=time.bounds, hist.id=hist.id[1,'history_id']))
  }
  NULL
}
