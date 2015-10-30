## Test code ##

## 'case' should be list containg all the arguments to be passed to f,
## plus one element named 'expected' which is the expected result
check.one.case <- function(case, f) {
  args <- append(case[- which(names(case) == 'expected')], f, after=0)
  cl <- as.call(args)
  print(eval(cl))
  checkEquals(eval(cl), case$expected)
}

## 'case' should be list containg all the arguments to be passed to f
check.one.bad.case <- function(case, f) {
  args <- append(case, f, after=0)
  cl <- as.call(args)
  checkException(eval(cl))
}

test.month.meets.3.5.rule <- function() {
  cases <- list(
                c(1, 2, 3, 4, 5),
                c(1, 2, 3, 4, NA, 1, 2, 3, 4, NA, 1, 2, 3, 4, NA, 1, 2, 3, 4, NA, 1, NA, 1, NA), # 6 NAs
                c(NA, 1, 2, 3, 4, NA, 1, 2, 3, 4, NA, 1, 2, 3, 4, NA, 1, 2, 3, 4, NA, 1, NA, 1), # 6 NAs
                c(1, 2, 3, 4, NA, NA, 1, 2, 3, 4, NA, 1, 2, 3, 4, NA, 1, 2, 3, 4, NA, 1, NA, 1), # 6 NAs
                c(1, 2, 3, 4, NA, 1, 2, 3, 4, NA, 1, 2, 3, 4, NA, 1, 2, 3, 4, NA, 1, NA, 1), # 5 NAs
                c(NA, NA, NA, NA, 1, 2, 3), # 4 in a row at beginning (should fail)
                c(NA, NA, NA, 1, 2, 3), # 3 in a row at beginning (should pass)
                seq(40), # Doesn't matter whether there is more than a month's worth of data
                c(1, 2, 3, NA, NA, NA, NA, 1, 2, 3), # 4 in a row in middle (should fail)
                c(1, 2, 3, NA, NA, NA, 1, 2, 3), # 3 in a row in middle (should pass)
                c(1, 2, 3, NA, NA, NA, NA), # 4 in a row at end (should fail)
                c(1, 2, 3, NA, NA, NA) # 3 in a row at end (should pass)
                )
  expected <- list(T, F, F, F, T, F, T, T, F, T, F, T)
  results <- lapply(cases, month.meets.3.5.rule)
  mapply(checkEquals, results, expected)

  ## Test it with logical input
  results <- lapply(lapply(cases, as.logical), month.meets.3.5.rule)
  mapply(checkEquals, results, expected)

  ## Test for failures
  cases <- list(
                c(),
                list('blah', 'stuff'),
                c(1, 2, 3, 4, 5, 'a')
                )
  for (case in cases) {
    checkException(month.meets.3.5.rule(case))
  }
}

mk.fixture <- function() {
  a <- c(1, NA)
  fix <- expand.grid(a, a, a, a)
  names(fix) <- c('tmax', 'tmin', 'tmean', 'pcp')
  st.names <- c(rep("air_temperature", 3), "lwe_thickness_of_precipitation_amount")
  cell.meths <- c("time: maximum", "time: minimum", "time: mean", "time: sum")
  units <- rep('', 4)
  long.desc <- rep('', 4)
  time <- seq(as.POSIXct("2010/01/01", tz='GMT'), by='day', length.out=16)
  new("crmp", fix, station.id='test', network='test', 'cf:units'=units, 'cf:convention'='1.4',
      'cf:long_description'=long.desc,
      'cf:standard_name'=st.names,
      'cf:cell_method'=cell.meths,
      time=time)
}

test.crmp.has.temp.and.precip <- function() {
  fixture <- mk.fixture()
  expected <- c(rep(T, 5), rep(F, 11)) ## See page 130 of JMH's notebook for full expansion of logic table
  result <- crmp.has.temp.and.precip(fixture)
  checkEquals(result, expected)

  ## Test that it works for only one variable
  fix <- new('crmp', data.frame(tmean=rep(1, 10)), station.id='test', network='test',
             'cf:standard_name'='air_temperature', 'cf:cell_method'='time: mean',
             time=seq(as.POSIXct("2010/01/01", tz='GMT'), by='day', length.out=10)
             )
  expected <- rep(F, 10)
  result <- crmp.has.temp.and.precip(fix)
  checkEquals(result, expected)
  
  ## Test that it works for where there's multiple variables of one type (e.g. multiple temperature measurements)
  fix <- new('crmp', data.frame(tmean=c(NA, rep(1, 9)), tmean2=c(rep(1, 9), NA), pcp=c(rep(1, 9), NA)),
             station.id='test', network='test',
             'cf:standard_name'=c(rep("air_temperature", 2), "lwe_thickness_of_precipitation_amount"),
             'cf:cell_method'=c(rep("time: mean", 2), "time: sum"),
             time=seq(as.POSIXct("2010/01/01", tz='GMT'), by='day', length.out=10)
             )
  expected <- c(rep(T, 9), F)
  result <- crmp.has.temp.and.precip(fix)
  checkEquals(result, expected)

  ## Test that it works for only one observation
  fix <- new('crmp', data.frame(tmean=1, tmax=2, pcp=5),
             station.id='test', network='test',
             'cf:standard_name'=c(rep("air_temperature", 2), "lwe_thickness_of_precipitation_amount"),
             'cf:cell_method'=c("time: mean", "time: maximum", "time: sum"),
             time=as.POSIXct("2010/01/01", tz='GMT')
             )
  checkEquals(crmp.has.temp.and.precip(fix), T)
  fix <- new('crmp', data.frame(tmean=1, tmax=2, pcp=NA),
             station.id='test', network='test',
             'cf:standard_name'=c(rep("air_temperature", 2), "lwe_thickness_of_precipitation_amount"),
             'cf:cell_method'=c("time: mean", "time: maximum", "time: sum"),
             time=as.POSIXct("2010/01/01", tz='GMT')
             )
  checkEquals(crmp.has.temp.and.precip(fix), F)
}

test.crmp.has.temp <- function() {
  fixture <- mk.fixture()
  expected <- c(rep(T, 5), rep(F, 3), rep(T, 5), rep(F, 3)) ## See page 130 of JMH's notebook for full expansion of logic table
  result <- crmp.has.temp(fixture)
  checkEquals(result, expected)
}

test.convert.to.cf.1.4.names <- function () {
  ##          Input                                                          Expected
  cases <- c("identity",                                                     "identity",
             " front_spaces",                                                "front_spaces",
             "back_spaces  ",                                                "back_spaces",
             "spurious*&^#$%&$^@#%  characters @#$(*&#@$",                   "spuriouscharacters",
             "45leading_integer",                                            "leading_integer",
             "trailing_integer45",                                           "trailing_integer45",
             "HANDLES_CAPITALS",                                             "HANDLES_CAPITALS",
             "handles_underscores",                                          "handles_underscores",
             "_underscores_cannot_lead",                                     "underscores_cannot_lead",
             "hyphens-are-not-allowed",                                      "hyphensarenotallowed",
             "@#$@#$ 123 leading_garbage_plus_invalid_numbers_at_beginning", "leading_garbage_plus_invalid_numbers_at_beginning")
  dim(cases) <- c(2, length(cases) / 2)
  dimnames(cases) <- list(c("input", "expected"), NULL)
  results <- lapply(cases["input",], convert.to.cf.1.4.names)
  mapply(checkEquals, results, cases["expected",])
}

test.map.names <- function() {
  cases <- list(
                list(list("foo", "bar", "blah", "stuff"), list(), list("foo", "bar", "blah", "stuff")),
                list(list("foo", "bar"), list(foo="blah", bar="stuff"), list("blah", "stuff")),
                list(list("foo", "bar"), list(foo="blah", other="does nothing"), list("blah", "bar")),
                list(list(), list(map="this", other="does nothing"), list())
             )
  for (case in cases) {
    names(case) <- c("input", "map", "expected")
    print(case$input)
    print(case$map)
    print(case$expected)
    print(map.names(case$input, case$map))
    #print(paste(case$input, case$map, "->", map.names(case$input, case$map), case$expected))
    checkTrue(isTRUE(all.equal(map.names(case$input, case$map), case$expected)))
  }
}

test.more.than.three.in.a.row.missing <- function() {
  cases <- list(
                list(c(T, T, T, T),  F),
                list(c(T, F, F, F, T), F),
                list(c(T, F, F, F, F, T), T),
                list(c(F, F, F, T, T), F),
                list(c(T, F, F, F, F), T)
                )
  for (case in cases) {
    names(case) <- c("input", "expected")
    checkTrue(all(more.than.three.in.a.row.missing(case$input) == case$expected, na.rm=T))
  }
}

test.maximal.3.5.rule <- function() {
  cases <- list(
                list(c(rep(T, 10), rep(F, 6), rep(T, 3)),      13, c(1, 13)),
                list(c(rep(F , 4), rep(T, 10), rep(F, 6), rep(T, 3)), 15, c(3, 17)),
                list(c(T, T, T, F, F, F, F, F, F, rep(T, 10)), 13, c(7, 19)),
                ## This case occured in the months for station 1125865 (EC)
                list(c(rep(T, 283), F), 284, c(1, 284))
                )
  for (case in cases) {
    names(case) <- c("input", "expected", "expected.indicies")
    checkEquals(maximal.3.5.rule(case$input), case$expected)
    checkEquals(maximal.3.5.rule(case$input, T)$indicies, case$expected.indicies)
  }
}

# Warning: this test randomly selects a set of 100 values from the data
# it is therefore not repeatable (but probably more likely to find errors)
long.test.maximal.3.5.rule.by.comparison <- function() {
  options(cores=8)
  files <- list.files(path="/home/data/projects/crmp/results-class/data.cache/", full.names=T)

  # Just do it on temperature... most stations have this
  pattern <- "(TEMP <- MEAN|Temp|temperature|PRESTEMP)"

  fun <- function(file) {
    df <- import.crmp(file)

    # Select the variable name
    i <- which(grepl(pattern, names(df)))
    if (length(i) > 0) {
      var.name <- names(df)[[i]]
    }
    else
      return(NULL)

    print(paste("Testing maximal.3.5.rule for file", file))

    # just use a subset of the data... the actual value doesn't matter
    # just that the function results match
    i0 <- floor(runif(1, min=1, max=dim(df)[1]))
    x <- df[i0:(i0+100), var.name]

    x <- ! is.na(x)
    checkEquals(maximal.3.5.rule(x), maximal.3.5.rule.n.squared(x))
  }
  mclapply(files, fun)
}

perf.test.maximal.3.5.Big.O <- function(plot.dev.call=call('x11')) {
  N <- 2 ** (1:10)
  profile <- function(n, fun) {
    x <- rep(NA, n)
    Rprof()
    fun(x)
    Rprof(NULL)
    rv <- try(summaryRprof()$sampling.time, silent=T)
    if (inherits(rv, 'try-error'))
      return(0)
    else
      return(rv)
  }
  fast <- sapply(N, profile, fun=maximal.3.5.rule) # Seems to run in linear time
  slow <- sapply(N, profile, fun=maximal.3.5.rule.n.squared) # Seems to run in x**(2+)
  eval(plot.dev.call)
  plot(N, slow, type='b', col='red', ylab='time (seconds)', main='Emperical runing times for naive vs. sophisticated algorithms')
  lines(N, fast, type='b', col='blue')
  legend('topleft', c('naive', 'sophisticated'), lty=1, col=c('red', 'blue'))
  dev.off()
}

perf.test.maximal.3.5.Little.O <- function() {
  N <- 2 ** (1:10)
  profile <- function(n, fun) {
    x <- rep(T, n)
    # Put in some random holes to make it not run in zero time
    for (i in floor(runif(5, min=1, max=n)))
      x[i] <- NA

    Rprof()
    fun(x)
    Rprof(NULL)
    rv <- try(summaryRprof()$sampling.time, silent=T)
    if (inherits(rv, 'try-error'))
      return(0)
    else
      return(rv)
  }
  fast <- sapply(N, profile, fun=maximal.3.5.rule) # Seems to run in constant time
  x11()
  plot(N, fast, type='b', col='red')
}

test.remove.nondefined.vars <- function() {
  t <- seq(as.POSIXct("2010/01/01", tz="GMT"), by='day', length.out=10)
  fix <- new('crmp', data.frame(temperature=1:10, precipitation=-5:4, 'wind_speed'=rep(0,10)), time=t)
  defs <- get.variable.defs('MoFR')
  cases <- list(
  ## 1: Remove 0 variables
                list(fix, defs, expected=fix),
  ## 2: Remove 1 variable
                list(fix, defs[-1,], expected=new('crmp', data.frame(temperature=1:10, 'wind_speed'=rep(0,10)), time=t)),
  ## 3: Remove more than one variable
                list(fix, defs[-2:-1,], expected=new('crmp', data.frame('wind_speed'=rep(0,10)), time=t))
  ## 4: Remove all the variables
##                list(fix, data.frame(), expected=new('crmp', data.frame()))
                )
  lapply(cases, check.one.case, remove.nondefined.vars)
}

## test.crmp.get.observation.frequency <- function() {
##   ## Tests: No data -> 0
##   ##        Hourly -> 1
##   ##        Hourly with gaps -> 1
##   ##        Daily -> 24
##   ##        Daily with gaps -> 24
##   ##        Half hourly, half daily -> ??
##   ##        Semi-daily -> ??
##   stop("unimplemented")
## }

test.crmp.infill <- function() {
  t <- seq(as.POSIXct("2010/01/01", tz="GMT"), by='day', length.out=10)
  fix <- new('crmp', 1:10, time=t)
  cases <- list(
  ## 1: Identity (no infilling necessary)
                list(fix, start.date=NULL, end.date=NULL, expected=fix),

  ## 2: Need infilling in the middle
                list(new('crmp', 1:9, time=t[c(1:5,7:10)]),
                     start.date=NULL, end.date=NULL,
                     expected=new('crmp', c(1:5, NA, 6:9), time=t)
                     ),
  
  ## 3: start.date given, infilling required in the begining
                list(new('crmp', 1:5, time=t[6:10]),
                     start.date=t[1], end.date=NULL,
                     expected=new('crmp', c(rep(NA, 5), 1:5), time=t)
                     ),
    
  ## 4: end.date given, infilling required on the ned
                list(new('crmp', 1:5, time=t[1:5]),
                     start.date=NULL, end.date=t[10],
                     expected=new('crmp', c(1:5, rep(NA, 5)), time=t)
                     ),
                
  ## 5: start.date and end.date given.  Infilling required on both
                list(new('crmp', 1:5, time=t[3:7]),
                     start.date=t[1], end.date=t[10],
                     expected=new('crmp', c(NA, NA, 1:5, NA, NA, NA), time=t)
                     ),
  ## 6: Make sure that slots get preserved
                list(new('crmp', 1:5, time=t[3:7], station.id="20110"),
                     start.date=t[1], end.date=t[10],
                     expected=new('crmp', c(NA, NA, 1:5, NA, NA, NA), time=t, station.id="20110")
                     ),
  ## 7: start.date and end.date are both after the end of the time period
                list(new('crmp', 1:5, time=t[1:5], station.id="100"),
                     start.date=as.POSIXct("2010/02/01", tz="GMT"),
                     end.date=as.POSIXct("2010/02/10", tz="GMT"),
                     expected=new('crmp', c(1:5, rep(NA, 36)), station.id="100",
                       time=seq(t[1], by="day", to=as.POSIXct("2010/02/10", tz="GMT")))
                     )
                )
  lapply(cases, check.one.case, crmp.infill)
}

test.normal.code.crmp <- function() {
  ## 1: Clearly longer than 30 years
  t <- seq(as.POSIXct("1970/01/01", tz="GMT"), to=as.POSIXct("2001/01/01", tz="GMT"), by="day")
  fix <- new('crmp', data.frame(tmax=rep(1, length(t))), time=t)
  checkEquals(normal.code.crmp(fix, 'tmax'), 'A')

  ## 2: Edge case of _exactly_ 30 years
  t <- seq(as.POSIXct("1970/01/01", tz="GMT"), to=as.POSIXct("1999/12/31", tz="GMT"), by="day")
  fix <- new('crmp', data.frame(tmax=rep(1, length(t))), time=t)
  checkEquals(normal.code.crmp(fix, 'tmax'), 'B')

  ## 3: Edge case of no data
  t <- seq(as.POSIXct("1970/01/01", tz="GMT"), by="day", length.out=300)
  fix <- new('crmp', data.frame(tmax=numeric(0)))
  checkTrue(is.na(normal.code.crmp(fix, 'tmax')))
  fix <- new('crmp', data.frame(tmax=rep(NA, 300)), time=t)
  checkTrue(is.na(normal.code.crmp(fix, 'tmax')))
  fix <- new('crmp', data.frame(tmin=1:300), time=t)
  checkException(normal.code.crmp(fix,'tmax'))

  ## 4: special variables
  t <- seq(as.POSIXct("1970/01/01", tz="GMT"), by="day", length.out=200)
  fix <- new('crmp', data.frame(tmean=1:200), time=t,
             'cf:standard_name'='air_temperature',
             'cf:cell_method'='time: mean')
  checkEquals(normal.code.crmp(fix, 'ANY_TEMP'), 'G')
  checkTrue(is.na(normal.code.crmp(fix, 'PRECIP_AND_ANY_TEMP')))

  ## TODO: test special variables ANY_TEMP, PRECIP_AND_ANY_TEMP
  ##       test normal use cases where data has holes
}

`test.[.crmp` <- function() {
  fix <- mk.fixture()
  cases <- list(
  ## 1: Subset by variable and time
                list(fix, i=1:2, j=c("tmax", "tmin"),
                     expected=new('crmp', data.frame(tmax=c(1, NA), tmin=c(1, 1)),
                       'cf:standard_name'=rep('air_temperature', 2),
                       'cf:cell_method'=c("time: maximum", "time: minimum"),
                       'cf:long_description'=rep('', 2),
                       'cf:units'=rep('', 2),
                       'cf:convention'='1.4',
                       network='test', station.id='test',
                       time=seq(as.POSIXct("2010/01/01", tz='GMT'), by='day', length.out=2))
                     ),
  ## 2: Subset by variable only by name
                list(fix, j=c("tmax", "tmin"),
                     expected=new('crmp', data.frame(tmax=rep(c(1, NA), 8), tmin=rep(c(1, 1, NA, NA), 4)),
                       'cf:standard_name'=rep('air_temperature', 2),
                       'cf:cell_method'=c("time: maximum", "time: minimum"),
                       'cf:long_description'=rep('', 2),
                       'cf:units'=rep('', 2),
                       'cf:convention'='1.4',
                       network='test', station.id='test',
                       time=seq(as.POSIXct("2010/01/01", tz='GMT'), by='day', length.out=16))
                     ),
  ## 3: Subset by variable only by number
                list(fix, j=1:2,
                     expected=new('crmp', data.frame(tmax=rep(c(1, NA), 8), tmin=rep(c(1, 1, NA, NA), 4)),
                       'cf:standard_name'=rep('air_temperature', 2),
                       'cf:cell_method'=c("time: maximum", "time: minimum"),
                       'cf:long_description'=rep('', 2),
                       'cf:units'=rep('', 2),
                       'cf:convention'='1.4',
                       network='test', station.id='test',
                       time=seq(as.POSIXct("2010/01/01", tz='GMT'), by='day', length.out=16))
                     ),
  ## 4: Subset by time only
                list(fix, i=7:8,
                     expected=new('crmp', data.frame(tmax=c(1, NA), tmin=as.numeric(c(NA, NA)), tmean=as.numeric(c(NA, NA)), pcp=c(1, 1)),
                       'cf:standard_name'=c(rep('air_temperature', 3), 'lwe_thickness_of_precipitation_amount'),
                       'cf:cell_method'=c("time: maximum", "time: minimum", "time: mean", "time: sum"),
                       'cf:long_description'=rep('', 4),
                       'cf:units'=rep('', 4),
                       'cf:convention'='1.4',
                       network='test', station.id='test',
                       row.names=as.character(7:8),
                       time=seq(as.POSIXct("2010/01/07", tz='GMT'), by='day', length.out=2))
                     ),

  ## 5: Subset using time indicies
                list(fix, i=as.POSIXct("2010/01/07", tz='GMT'), j=as.POSIXct("2010/01/09", tz='GMT'),
                     expected=new('crmp', data.frame(tmax=c(1, NA, 1), tmin=as.numeric(c(NA, NA, 1)), tmean=as.numeric(c(NA, NA, 1)), pcp=c(1, 1, NA)),
                       'cf:standard_name'=c(rep('air_temperature', 3), 'lwe_thickness_of_precipitation_amount'),
                       'cf:cell_method'=c("time: maximum", "time: minimum", "time: mean", "time: sum"),
                       'cf:long_description'=rep('', 4),
                       'cf:units'=rep('', 4),
                       'cf:convention'='1.4',
                       network='test', station.id='test',
                       row.names=as.character(7:9),
                       time=seq(as.POSIXct("2010/01/07", tz='GMT'), by='day', length.out=3))
                     ),
  ## 6: Subset using time indicies out of range
                list(fix, i=as.POSIXct("2009/12/15", tz='GMT'), j=as.POSIXct("2010/01/02", tz='GMT'),
                     expected=new('crmp', data.frame(tmax=c(1, NA), tmin=c(1, 1), tmean=c(1, 1), pcp=c(1, 1)),
                       'cf:standard_name'=c(rep('air_temperature', 3), 'lwe_thickness_of_precipitation_amount'),
                       'cf:cell_method'=c("time: maximum", "time: minimum", "time: mean", "time: sum"),
                       'cf:long_description'=rep('', 4),
                       'cf:units'=rep('', 4),
                       'cf:convention'='1.4',
                       network='test', station.id='test',
                       time=seq(as.POSIXct("2010/01/01", tz='GMT'), by='day', length.out=2))
                     ),
  ## 7: Subset using time indicies out of range
                list(fix, i=as.POSIXct("2009/12/15", tz='GMT'), j=as.POSIXct("2009/12/30", tz='GMT'),
                     expected=new('crmp', data.frame(tmax=numeric(), tmin=numeric(), tmean=numeric(), pcp=numeric()),
                       'cf:standard_name'=c(rep('air_temperature', 3), 'lwe_thickness_of_precipitation_amount'),
                       'cf:cell_method'=c("time: maximum", "time: minimum", "time: mean", "time: sum"),
                       'cf:long_description'=rep('', 4),
                       'cf:units'=rep('', 4),
                       'cf:convention'='1.4',
                       network='test', station.id='test',
                       time=x <- as.POSIXct("2010/01/01", tz="GMT")[0])#zero length POSIXt
                     ),
  ## 8: Subset using drop=T (drop to lowest common dimension)
                list(fix, j=4,
                     expected=c(rep(1, 8), rep(NA, 8))),
  ## 9: Identity?
                list(fix, i=1:16, j=1:4, expected=fix),
  ## 10: Subset using time indicies, only one row, and drop=F (should return a crmp object)
                list(new('crmp', data.frame(tmax=1:10),
                         time=seq(as.POSIXct("2010/01/01", tz="GMT"), length.out=10, by='day')),
                     i=as.POSIXct("2010/01/02", tz="GMT"), j=as.POSIXct("2010/01/10", tz="GMT"), drop=F,
                     expected=new('crmp', data.frame(tmax=2:10), row.names=as.character(2:10),
                       time=seq(as.POSIXct("2010/01/02", tz="GMT"), length.out=9, by='day'))
                     ),
  ## 11: Subset using time indicies, only one row, and drop=T (should return a vector)              
                list(new('crmp', data.frame(tmax=1:10),
                         time=seq(as.POSIXct("2010/01/01", tz="GMT"), length.out=10, by='day')),
                     i=as.POSIXct("2010/01/02", tz="GMT"), j=as.POSIXct("2010/01/10", tz="GMT"), drop=T,
                     expected=2:10
                     )
                )
  lapply(cases, check.one.case, `[`)
}

test.crmp.get.start.end.date <- function() {
  fix <- mk.fixture()
  t0 <- as.POSIXct("2010/01/01", tz='GMT')
  tn <- t0 + as.difftime(15, units='days')
  checkEquals(crmp.get.start.date(fix), t0)
  checkEquals(crmp.get.end.date(fix), tn)

  # randomizing the times shouldn't change the result (even if that's not a valid object)
  fix@time <- fix@time[order(rnorm(16))]
  checkEquals(crmp.get.start.date(fix), t0)
  checkEquals(crmp.get.end.date(fix), tn)

  # works in the case of one obs?
  fix <- fix[1,]
  checkEquals(crmp.get.start.date(fix), crmp.get.end.date(fix))
  
  # empty object
  fix <- new('crmp')
  checkException(crmp.get.start.date(fix))
  checkException(crmp.get.end.date(fix))
}

test.crmp.longest.data.gap <- function() {
  fix <- mk.fixture()
  f <- crmp.longest.data.gap
  checkEquals(f(fix, 'tmax'), as.difftime(2, units='days'))
  checkEquals(f(fix, 'tmin'), as.difftime(3, units='days'))
  checkEquals(f(fix, 'tmean'), as.difftime(5, units='days'))
  checkEquals(f(fix, 'pcp'), as.difftime(1, units='days'))
  checkEquals(f(fix, 'PRECIP_AND_ANY_TEMP'), as.difftime(1, units='days'))
  checkEquals(f(fix, 'ANY_TEMP'), as.difftime(4, units='days'))
  fix@time[1] <- fix@time[1] - as.difftime(10.5, units='days')
  checkEquals(f(fix, 'tmax'), as.difftime(12.5, units='days'))

  checkException(f(fix, 'variable.does.not.exist'))

  # Check the case of one obs
  fix <- fix[1,]
  checkTrue(is.na(f(fix)))

  # Check the case of no obs
  fix <- new('crmp')
  checkTrue(is.na(f(fix)))  
}

test.crmp.get.vars <- function() {
  fix <- mk.fixture()
  f <- crmp.get.vars
  checkEquals(f(fix), names(fix))

  fix <- fix[,1,drop=F]
  checkEquals(f(fix), 'tmax')

  fix <- fix[,1]
  checkTrue(is.null(f(fix)))

  fix <- new('crmp')
  checkEquals(f(fix), character(0))
}

## test.set.crmp.attributes <- function() {stop("unimplemented")}
## test.order.crmp <- function() {stop("unimplemented")}
## test.crmp.fraction.of.data.present <- function() {stop("unimplemented")}
## test.crmp.fit.normal <- function() {stop("unimplemented")}
