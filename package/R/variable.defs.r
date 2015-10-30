# This function just returns a table of what's listed in the table in var_tables/<network>.xform.csv
# Column headers should include network.var.name, network.unit, network.long.description, cf.var.name, cf.unit, cf.cell.method
#src.dir <- getwd()

get.variable.defs <- function(network) {
  network.file <- paste(sep=".", network, "xform")
  do.call(data, list(network.file, package='crmp'))
  var.defs <- get(network.file)
  var.names <- var.defs$network.var.name
  rownames(var.defs) <- var.names
  i <- which(names(var.defs) == "network.var.name")
  var.defs[,-i]
}
