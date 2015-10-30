source("crmp.load.data.r")

load("~/tmp/mofr.all.Robject")
files <- list.files("/home/data3/projects/crmp/MoFR/access_export/", pattern="HourlyWeather.*.txt", full.names=T)

f <- function(file) {
  data <- read.crmp.MoFR(file)
  sapply(data, normal.code.crmp, "temperature")
}

codes <- sapply(files, f)

combined.codes <- as.factor(append(append(codes[[1]], codes[[2]]), codes[[3]]))
summary(combined.codes)

savehistory("normal.codes.for.MoFR.r")
