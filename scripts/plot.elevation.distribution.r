metadata.path <- "/home/data/projects/crmp/haileye_space"
metadata.files <- file.path(metadata.path, c(active="crmp_active_metadata.csv", historic="crmp_historical_metadata.csv"))
plot.file <- file.path(getwd(), "elevation_density.pdf")

args <- commandArgs(trailingOnly=T)
for (a in args) {
  print(a)
  eval(parse(text=a))
}

data <- lapply(metadata.files, read.csv)
names(data) <- c("active", "historic")

subset <- list(data$historic[data$historic$network == "EC", 'elev'],
               data$historic[data$historic$network != "EC", 'elev'],
               data$active[data$active$network == "EC", 'Elevation_'],
               data$active[data$active$network != "EC", 'Elevation_'])

dens <- lapply(subset, density, from=0)
get.range <- function(x, var) {range(x[[var]])}
xlim <- range(sapply(dens, get.range, var='x'))
ylim <- range(sapply(dens, get.range, var='y'))
colors <- c(red="EC historic", blue="CRMP historic", orange="EC active", green="CRMP active")

pdf(plot.file)
plot.default(NULL, xlim=xlim, ylim=ylim,
             main="Elevation distribution of EC and CRMP stations",
             xlab="Elevation (meters)", ylab="Probability density")

mapply(lines, dens, col=names(colors))

legend("topright", legend=colors, col=names(colors), lty=rep(1, 4))
dev.off()
