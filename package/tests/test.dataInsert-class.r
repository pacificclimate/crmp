## Test code ##
library(RUnit)

## 'case' should be list containg all the arguments to be passed to f
check.one.bad.case <- function(case, f) {
  args <- append(case, f, after=0)
  cl <- as.call(args)
  checkException(eval(cl))
}

test.dataInsert.conversion <- function() {
  cases <- list(
                data.frame(station_id=54, native_id='1024', is_valid=T, start_date=as.POSIXct('2010/01/01', tz='GMT'), stringsAsFactors=F),
                data.frame(flag_match=F, network_name='EC', datum=5.45, mod_time=as.POSIXct('2012/12/31 10:00:00', tz='GMT'), stringsAsFactors=F)
                )
  expected <- c(
                "( station_id, native_id, is_valid, start_date ) VALUES ( 54, E'1024', true, '2010-01-01' )",
                "( flag_match, network_name, datum, mod_time ) VALUES ( false, E'EC', 5.45, '2012-12-31 10:00:00' )"
                )
  bad.cases <- list(
                    list('dataInsert', data.frame(station_id=100, native_id="Some station", stringsAsFactors=T)),
                    list('dataInsert', data.frame(station_id=100, native_id="Some station")), # implied stringsAsFactors=T
                    list('dataInsert', as.data.frame(array(dim=c(2, 2), rep(4, 4)))),
                    list('dataInsert', data.frame(f=factor(c('hi', 'mom')), x=c(2, 2))),
                    list('dataInsert', data.frame())
                    )
  cases <- lapply(cases, new, Class='dataInsert')
  cases <- lapply(cases, as, Class='character')
  mapply(checkEquals, cases, expected)

  lapply(bad.cases, check.one.bad.case, new)
}
