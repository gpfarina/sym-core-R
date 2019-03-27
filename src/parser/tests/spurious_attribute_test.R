y = 1:6
dim(y) <- c(3,2)
x = y
dim(y)[3] = 1
dim(x) # 3 2
dim(y) # 3 2 1
