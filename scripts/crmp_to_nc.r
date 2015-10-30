## Short script that simply takes a saved crmp object and writes out a netcdf file representing the given station
## James Hiebert <hiebert@uvic.ca>
## 2011.06.01

source('../crmp-class.r', chdir=T)

required.args <- c('cached.object', 'out.dir')

args <- commandArgs(trailingOnly=T)
for (a in args) {
  print(a)
  eval(parse(text=a))
}

data <- import.crmp(cached.object)
crmp.to.ncdf(data, out.dir)

