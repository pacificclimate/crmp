is.subset <- function(x, y)
  all(intersect(x, y) == x)

file.type <- 'csv'
time.period <- 'all'

print(args)
args <- commandArgs(trailingOnly=T)
for (a in args) {
  print(a)
  eval(parse(text=a))
}

required.args <- c('file.type', 'time.period')

if ( ! is.subset(required.args, ls())) {
   stop("Did not receive all of the required args:", paste(collapse="|", required.args))
}

library(RPostgreSQL)
library(abind)
library(crmp)

#source('../crmp-class.r', chdir=T) # for map.names

con <- dbConnect('PostgreSQL', 'crmp')

fun <- function(standard.name, cell.method) {

  do.one <- function(condition) {
    print(condition)
    q <- sprintf("SELECT prov_name, network_name, sum(station_id is not null::integer) AS station_count FROM
                   (
                    SELECT standard_name, cell_method, station_id, network_id, the_geom AS hist_geom
                    FROM meta_vars NATURAL JOIN stats_station_var NATURAL JOIN meta_station
                    NATURAL JOIN meta_history AS mh
                    WHERE ks_significant %s AND time_period = '%s'
                                            AND standard_name = '%s'
                                            AND cell_method = '%s'
                   ) AS foo
                  RIGHT JOIN
                   (
                    SELECT prov_name, the_geom AS prov_geom, network_name, network_id
                    FROM meta_network CROSS JOIN ecoprov_albers AS eco
                   ) AS bar
                  ON foo.network_id = bar.network_id AND ST_INTERSECTS(prov_geom, ST_TRANSFORM(hist_geom, 3005))

                 GROUP BY network_name, prov_name
                 ORDER BY network_name, prov_name", condition, time.period, standard.name, cell.method)
    print(q)
    res <- dbSendQuery(con, q)
    df <- fetch(res, -1)

    ## Collapse it into a table which is network/eco_province
    df <- tapply(df$station_count, list(df$network_name, df$prov_name), sum)

    network.map <- list(BCH="BCHY", EC="ECAN", MoAg="MoAG", MoE="MOEV", MoFR="MoFR",
                        'MoFR_research'="MF-Res", MoTI="MOTI", RTA="RTAC", SNOWPILLOW="SWPIL")
    ##row.order <- c('BCHY', 'MoAG', 'MOEV', 'MF-Res', 'MoFR', 'MOTI', 'RTAC', 'SWPIL', 'SBTOT BCNetwk', 'ECAN', 'GRAND TOTAL')
    ## Re-name and re-order rows
    rownames(df) <- map.names(rownames(df), network.map)
    #df <- df[row.order]

    ## Replace NAs with '--'
    t(replace(df, is.na(df), '--'))
  }
  conditions <- c('= true', '= false', 'IS NULL')
  rv <- lapply(conditions, do.one)

  my.vector <- abind(true=rv[[1]], false=rv[[2]], none=rv[[3]], along=0)
  table <- apply(my.vector, 2:3, paste, sep='', collapse=', ')

  output.file <- paste(standard.name, gsub(': ', '_', cell.method), file.type, sep='.')
  switch(file.type,
         csv=write.csv(table, file=output.file),
## tex not implemented yet         
##         tex=cat(table.to.tex(table), file=output.file),
         stop("file.type parameters  must be either 'csv' or 'tex'")
         )
}

standard.names <- c(rep('air_temperature', 3), 'lwe_thickness_of_precipitation_amount')
cell.methods <- c('time: maximum', 'time: point', 'time: mean', 'time: sum')

mapply(fun, standard.names, cell.methods)
#table <- fun(standard.name, cell.method)

dbDisconnect(con)
