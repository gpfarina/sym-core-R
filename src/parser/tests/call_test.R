x = function(y) y+1
l = c(x, 3)     # put x into a list
q = as.call(l)  # the CALL object that will evaluate x(3)
eval(q)         # 4
