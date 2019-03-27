
arr <- c(1:10)

for (i in c(1:4)) {
  if (i == 2) {
    arr[2] <- 9999
    break;
  }
}

arr

