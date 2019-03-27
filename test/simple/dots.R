
baz <- function(a, b, c1, d, ...) {
  c(...)
}

foo <- function(a, b, c, ..., d)  {
  baz(...)
}


foo(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)


