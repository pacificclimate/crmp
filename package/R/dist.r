## Calculates the KS statistic for each temperature variable of a station
ks.test.station.temperature <- function(obj) {
  i <- grep("temperature", slot(obj, 'cf:standard_name'))
  if (length(i) < 1) return(NA)
  n <- names(obj)[i]
  rv <- lapply(n, crmp.fit.normal, obj=obj, do.plot=F)
  names(rv) <- paste(slot(obj, 'station.id'), sep=".", n)
  rv
}

## Calculates the KS statistic for each summed precipitation variable
ks.test.station.precip <- function(obj) {
  i <- intersect(grep('precipitation', slot(obj, 'cf:standard_name')), grep('time: sum', slot(obj, 'cf:cell_method')))
  if (length(i) < 1) return(NA)
  n <- names(obj)[i]
  rv <- lapply(n, crmp.fit.gamma, obj=obj, do.plot=F)
  names(rv) <- paste(slot(obj, 'station.id'), sep='.', n)
  rv
}

ks.test.one.var <- function(obj, var.name) {
  stopifnot(var.name %in% names(obj))
  i <- grep(paste(sep='', '^', var.name, '$'), names(obj))
  dist <- if(grepl('precipitation', slot(obj, 'cf:standard_name')[i]) && grep('time: sum', slot(obj, 'cf:cell_method')[i])) 'gamma'
          else if (grepl('temperature', slot(obj, 'cf:standard_name')[i])) 'normal'
          else return(NA)
  f <- match.fun(paste(sep='.', 'crmp.fit', dist))
  f(obj, var.name, F)
}

## Determines whether the KS test results were statistically significant
## From table 30 here: http://www.york.ac.uk/depts/maths/tables/pdf.htm
is.significant <- function(ks) {
  if (is.na(ks) || ks$n <= 40) return(NA) # I'm too lazy to type out the table, and if a station has < 40 observations, what--really--is the point
  else ks$statistic > (1.22 / sqrt(ks$n))
}
