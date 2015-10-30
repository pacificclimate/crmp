`test.slots<-` <- function() {
  fixtures <- list(new('data.frame', data.frame(blah=1:10)))
  for (fix in fixtures) {
    ## identity test
    post <- pre <- fix
    slots(post) <- slots(pre)
    checkEquals(pre, post)
  }
  ## Check named list assigment
  pre <- post <- new('data.frame', data.frame(stuff=1:10))
  names(pre) <- "var1"
  slots(post) <- list(names="var1")
  checkEquals(pre, post)
}
