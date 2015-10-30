##### Code for QC flags ####

## FIXME: What is an empty flag???
setOldClass('data.frame')
setClass("flag",
         representation('data.frame',
                        qc.operation='character',
                        qc.args='list',
                        applicable.vars='character'),
         contains="data.frame"
         )

flag.init <- function(.Object, ...) {
  callNextMethod()
}

setMethod("initialize", "flag", flag.init)

## to initialize, just do a:
## new('flag', my.data.frame, qc.operation='QC OP NAME')

## Design choice to only be able to subset a flag in the i dimension (and have it still be a flag object)
## There's no real way to enforce that the applicable.vars attribute carries through a subset in the j dimension
## or at least there's no robust way to determine to what variables the flag applies afterwards
'[.flag.by.row' <- function(x, i, j, drop = if (missing(i)) TRUE else ncol(x) == 1) {

  df <- as.data.frame(x@.Data)[i, j, drop] ## returns data.frame

  if (drop && is.null(dim(df)))
    return(df)

  to.preserve <- c('.Data', 'row.names')

  to.copy <- setdiff(slotNames(x), to.preserve)
  args <- append(list('flag', df), slots(x)[to.copy])
  do.call('new', args)
}
setMethod('[', signature(x='flag', i='integer', j='missing'), `[.flag.by.row`)


str.flag <- function(object, ...) {
  cat(" flag [", paste(collapse=":", dim(object)), "] Operation \"", slot(object, 'qc.operation'), "\" applied to variables [",
      paste(slot(object, 'applicable.vars'), collapse=", "), "]\n", sep='')
}

int.to.boolean.bitvector <- function(x, min.length=8) {
  rv <- logical()
  x <- as.numeric(x)
  while (x > 0) {
    rv <- append(rv, as.logical(x %% 2))
    x <- floor(x / 2)
  }
  if (length(rv) < min.length) {
    rv <- c(rv, rep(F, min.length - length(rv)))
  }
  rv
}

boolean.bitvector.to.int <- function(x) {
  rv <- 0
  if (length(x) < 1) return(0)

  for (i in 1:length(x)) {
    if (x[i]) {
      rv <- rv + 2 ** (i-1)
    }
  }
  rv
}

##### End QC flags code ####
