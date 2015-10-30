## Return a list of all the slots of object 'x'
slots <- function(x) {
  rv <- lapply(slotNames(x), function(n) {slot(x, n)})
  names(rv) <- slotNames(x)
  rv
}

## Set all slots in object 'x' from list 'value'
`slots<-` <- function(x, value) {
  saved.slots <- slots(x)
  saved.slots[names(value)] <- value
  do.call('new', append(saved.slots, class(x), after=0))
}
