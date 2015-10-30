library(multicore)
options(cores=8)

source("../crmp.load.data.r", chdir=T)

is.subset <- function(x, y)
  all(intersect(x, y) == x)

args <- commandArgs(trailingOnly=T)
for (a in args) {
  print(a)
  eval(parse(text=a))
}

required.args <- c('cache.dir', 'plot.dir')

if ( ! is.subset(required.args, ls())) {
  stop("Did not receive all of the required args:", paste(collapse="|", required.args))
}

plot.subtitles <- c(tmean="Temperature (mean or hourly)",
                    tmin="Min temperature",
                    tmax="Max temperature",
                    precip="Precipitation",
                    tandp="Temperature and precipitation"
                    )
plot.title <- "Normal codes for imported CRMP stations"


fun <- function(file, var.name) {
  df <- import.crmp(file)

  if (var.name == 'tandp')
    return(normal.code.crmp(df))

  f.name <- paste('which.is', var.name, sep='.')
  i <- do.call(f.name, list(df))

  if (length(i) > 0) {
    vn <- names(df)[[i]]
    return(normal.code.crmp(df, vn))
  }
  else
    return(NA)
}

get.per.var.normal.code.factor <- function(var.name) {
  files <- list.files(path=cache.dir, full.names=T)
  as.factor(unlist(mclapply(files, fun, var.name=var.name)))
}

create.normal.code.pi.chart <- function(var.name, ...) {
  codes <- summary(get.per.var.normal.code.factor(var.name))

  # Remove NA's if they're over 50% of the total volume
  if ("NA's" %in% names(codes) && codes[["NA's"]] / sum(codes) > .5) {
    codes <- codes[- which(names(codes) == "NA's") ]
  }

  lab <- paste(names(codes), " (", codes, ")", sep="")
  pie(codes, labels=lab, col=rainbow(8), ...)
}

do.all <- function(plot.dir=getwd()) {
  if (!file.exists(plot.dir))
    dir.create(plot.dir, recursive=T)

  do.one <- function(var.name, subt) {
    fname <- file.path(plot.dir, paste(var.name, "png", sep='.'))
    print(fname)
    png(fname)
    create.normal.code.pi.chart(var.name, main=plot.title, sub=subt)
    dev.off()
    fname
  }
  mapply(do.one, names(plot.subtitles), plot.subtitles)
}

do.all(plot.dir)
