######  Unit tests  ######
library(RUnit)

test.flag.class <- function() {
  mt <- new('flag')
}

init.test.boolean.bitvector.conversion <- function() {
  # These should transitive
  rv <- list(list(c(F, F, F, F, F, F, F, F), 0),
             list(c(T, T, T, T, T, T, T, T), 255),
             list(c(F, T, F, F, F, F, F, F), 2),
             list(c(F, F, F, F, F, F, F, T), 128)
             )
}

test.boolean.bitvector.to.int <- function() {
  fixture <- init.test.boolean.bitvector.conversion()
  for (test in fixture) {
    names(test) <- c('input', 'expected')
    checkEquals(boolean.bitvector.to.int(test$input), test$expected)
  }
  checkEquals(boolean.bitvector.to.int(c()), 0)
}

test.int.to.boolean.bitvector <- function() {
  f <- int.to.boolean.bitvector
  fixture <- init.test.boolean.bitvector.conversion()
  for (test in fixture) {
    names(test) <- c('expected', 'input')
    checkEquals(f(test$input), test$expected)
  }
  checkEquals(f(0, min.length=4), c(F, F, F, F)) # Ensure that min.length param works
  checkEquals(f(16, min.length=4), c(F, F, F, F, T)) # min.length should not prevent the return of a longer bitvector
}
