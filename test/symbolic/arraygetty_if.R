# fails with "data has no dim attribute"
x <- make.symbolic(3, "int")

if (x[2] > 8) {
  1111
} else {
  2222
}
