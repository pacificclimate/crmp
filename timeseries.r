source('sql.r')

crmp.timeseries <- function(obj, variable, start.date=NULL, end.date=NULL){
  stopifnot(inherits(obj, 'crmp'))
  stopifnot( is.null(start.date) || inherits(start.date, 'POSIXt') )
  stopifnot( is.null(end.date) || inherits(end.date, 'POSIXt') )  

  if (variable == 'tmin'){
    my.col <- which.is.tmin(obj)
  } else if (variable == 'tmax'){
    my.col <- which.is.tmax(obj)
  } else if (grepl(x=variable, pattern='prec')){
    my.col <- which.is.precip(obj)
  }
  obj <- crmp.infill(obj=obj, start.date=start.date, end.date=end.date)
  
  ## TODO: deal with NULL dates...
  
  return(obj[start.date,end.date][,my.col])
}
