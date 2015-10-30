## Summarizes info in a computed station metadata file (output from write.station.info.r)
## Counts the number of stations with WMO codes A-G on a per-network basis and puts those
## in a summary tale
## Also computes to columns of how many stations in each network have < 10% temporal coverage
## and > 95% temporal coverage.
## Depending on file.type parameter, Writes the table out to a csv or LaTeX tabular

source('../crmp-class.r', chdir=T) # for map.names

is.subset <- function(x, y)
  all(intersect(x, y) == x)

args <- commandArgs(trailingOnly=T)
for (a in args) {
  print(a)
  eval(parse(text=a))
}

required.args <- c('input.file', 'output.file', 'file.type')

if ( ! is.subset(required.args, ls())) {
  stop("Did not receive all of the required args:", paste(collapse="|", required.args))
}


do.stuff <- function(infile) {
  data <- read.csv(infile)
  stopifnot( all(c('network', 'wmo.code') %in% colnames(data)) )
  wmo.codes <- c('A', 'B', 'C', 'D', 'E', 'F', 'G')
  network.map <- list(BCH="BCHY", EC="ECAN", MoAg="MoAG", MoE="MOEV", MoFR="MoFR",
                      'MoFR_research'="MF-Res", MoTI="MOTI", RTA="RTAC", SNOWPILLOW="SWPIL")
  row.order <- c('BCHY', 'MoAG', 'MOEV', 'MF-Res', 'MoFR', 'MOTI', 'RTAC', 'SWPIL', 'SBTOT BCNetwk', 'ECAN', 'GRAND TOTAL')

  count.codes <- function(x) {
    rv <- tapply(x, x, length)
    ##c(rv, 'NA'=length(which(is.na(x)))) # Include a count of the NAs
  }

  summary <- tapply(data$wmo.code, data$network, count.codes)
  df <- data.frame(row.names=wmo.codes)
  for (n in levels(data$network)) {
    df[[n]] <- summary[[n]]
  }

  count.coverage <- function(x, relation, threshold) {
    length(which(do.call(relation, list(x, threshold))))
  }

  ## Table columns
  good <- tapply(data$fractional.coverage, data$network, count.coverage, '>', .95)
  bad <-  tapply(data$fractional.coverage, data$network, count.coverage, '<', .1)
  ugly <- tapply(data$wmo.code, data$network, function(x) {length(which(is.na(x)))}) # WMO CODE = NA
  nsites <- tapply(data$wmo.code, data$network, length)
  nobs   <- nsites - ugly

  df <- rbind('Number of Sites'=nsites, df, 'Number of Obs'=nobs, '.GE.95%'=good, '.LE.10%'=bad, 'NA'=ugly)

  ## Extra table rows
  grand.total <- apply(df, 1, sum, na.rm=T)
  subtot <- grand.total - df$EC
  df <- cbind(df, 'SBTOT BCNetwk'=subtot, 'GRAND TOTAL'=grand.total)

  ## Re-name and re-order rows
  names(df) <- map.names(names(df), network.map)
  df <- df[row.order]

  ## Replace NAs with '--'
  t(replace(df, is.na(df), '--'))
}

table.to.tex <- function(df) {
  texline <- function(v) { paste(paste(replace(v, is.na(v), '--'), collapse = ' & '), "\\tabularnewline\n\\hline") }
  bold.texline <- function(v) { texline(paste('\\textbf{', v, '}'))}

  header <- c("\\begin{tabular}{|c|c||c|c|c|c|c|c|c|c||c|c|}
\\hline 
\\multicolumn{2}{|c||}{\\textbf{NETWORK}} & \\multicolumn{10}{c|}{\\textbf{NUMBER OF MET Observations }}\\tabularnewline
\\hline 
\\multirow{2}{*}{\\textbf{Name}} & \\textbf{Number} & \\multicolumn{7}{c|}{\\textbf{Period of coverage (WMO Code)}} & \\textbf{Number} & \\multicolumn{2}{c|}{\\textbf{Coverage }}\\\\
\\cline{3-9}\\cline{11-12}",
  bold.texline(c('', 'of Sites', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'of Obs', '$\\ge$95\\%', '$\\le$10\\%')),
  '\\hline')

  footer <- "\\end{tabular}"
  empty.line <- texline(rep("", ncol(df)))
  df <- df[,colnames(df) != "NA"]
  df <- cbind(rownames(df), df)
  crmp.summaries <- df[-grep("(TOT|ECAN)", rownames(df)),]

  lines <- c(header,
             apply(crmp.summaries, 1, texline), # CRMP summaries
             empty.line,
             bold.texline(df["SBTOT BCNetwk",]),
             empty.line,
             texline(df["ECAN",]),
             empty.line,
             bold.texline(df["GRAND TOTAL",]),
             footer
             )
             
  paste(collapse='\n', lines)
}

table <- do.stuff(input.file)

switch(file.type,
       csv=write.csv(table, file=output.file),
       tex=cat(table.to.tex(table), file=output.file),
       stop("file.type parameters  must be either 'csv' or 'tex'")
       )
