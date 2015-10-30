## When instantiating the data.frame to be used for the dataInsert creation, be sure to turn off the stringsAsFactors option
setClass("dataInsert",
         representation('data.frame'), contains='data.frame'
         )

setValidity("dataInsert", function(object) {
  errors <- character(0)
  if (nrow(object) != 1) errors <- append(errors, 'dataInsert object can only have one row')
  if (any(sapply(object, class) == 'factor')) errors <- append(errors, 'No columns in a dataInsert object should be of type \'factor\'')
  ## add any other checks here
  return(if (length(errors) == 0) T else errors)
  })

## PostGresqlString: string representation of PostgreSQL type (i.e. what you print to a insert statement)
setClass("pgs", representation('character'), contains='character')
setValidity("pgs", function(object) {length(object) == 1})

## If hard.fail = T, function will raise an error if type coercions fail
## Otherwise, this function will list NULL for elements which cannot be coerced
## This function _should_ replace all NA's and empty strings with NULL in the sql insert statement
create.insert.stmt <- function(from){#, hard.fail=F) {

  this.frame <- sys.frame(sys.nframe())

  setAs('numeric', 'pgs', where=this.frame, def=function(from) {new('pgs', as.character(from))})
  setAs('character', 'pgs', where=this.frame, def=function(from) {new('pgs', if (from == '') 'NULL'
                                                                             else if (!is.null(attr(from, 'verbatim'))) from # Hack to allow passing SQL through
                                                                             else paste("E'", gsub("'", "\\\\'", from, ), "'", sep=''))}) # Escape all quotes and designate it as escaped
  setAs('logical', 'pgs', where=this.frame, def=function(from) {new('pgs', if (is.na(from)) 'NULL' else if(from) 'true' else 'false')})
  setAs('POSIXt', 'pgs', where=this.frame, def=function(from) {new('pgs', sprintf("'%s'", format(from, tz='GMT')))})

  x <- from
  ## Let the empty string represent NULL and invalid values
  x <- replace(x, is.na(x), '')
  x <- sapply(x, as, 'pgs')
  
  paste('(', paste(names(x), collapse=', '), ') VALUES (', paste(x, collapse=', '), ')')
}
setAs('dataInsert', 'character', create.insert.stmt)
