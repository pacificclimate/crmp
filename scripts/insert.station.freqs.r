library(RPostgreSQL)
source('../crmp-class.r', chdir=T)
source('../crmp.load.data.r', chdir=T)
source('../sql.r', chdir=T)

network <- 'ARDA'
con <- dbConnect("PostgreSQL", "crmp")

q <- sprintf("SELECT station_id FROM meta_history NATURAL JOIN meta_station NATURAL JOIN meta_network WHERE freq IS NULL and network_name = '%s'", network)
missing.freq <- fetch(dbSendQuery(con, q), -1)[,'station_id']

for (stn.id in missing.freq) {
  print(paste("Processing station", stn.id))
  x <- try(read.sql.crmp(con, stn.id))
  if (inherits(x, 'try-error')) {
    print(paste("Failed to load data from database for station", stn.id))
    next
  }
  freq <- try(crmp.get.observation.frequency(x))
  if (inherits(freq, 'try-error')) {
    print(paste("Failed to calculate obs freq for station", x@network, x@station.id, x@station.name))
    next
  }
  freq <- switch(as.character(freq),
                 '1'='1-hourly',
                 '12'='12-hourly',
                 '24'='daily',
                 'irregular'
                 )
  sql <- sprintf("UPDATE meta_history SET freq = '%s' WHERE station_id = %s", freq, stn.id)
  print(sql)
  res <- dbSendQuery(con, sql)
  n <- dbGetRowsAffected(res)
  print(paste("Updated", n, "rows"))
}
