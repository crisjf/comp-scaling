scaling = function (mat, pop,th=30,delta=0) 
  
{
  d = NULL
  
  mat = as.matrix(mat)
  mat = get.list(mat)
  mat = merge (mat, pop, by.x = 1, by.y = 1)
  colnames (mat) = c("geo", "Industry", "Count", "pop")
  
  for(i in unique (mat$Industry)){
    
    xs = subset(mat, mat$Industry == i)
    if (nrow(xs[xs$Count!=delta,])<th) {
      beta <- NA
      r.sq <- NA
      std.err <- NA
    } else {
      xs$Count[xs$Count==0] <- NA

      lm = lm(log(xs$Count)~log(xs$pop))    
      beta = round (summary(lm)$coefficients[2, 1], digits = 3)
      r.sq = summary(lm)$adj.r.squared
      std.err = summary(lm)$coefficients[2, 2]
    }

    econ = unique (xs$Industry)
    
    d = rbind(d, data.frame(econ, beta, r.sq, std.err))
    
  }
  colnames (d) = c("Industry", "Beta", "r.sq", "std.err")
  return (d)
}

