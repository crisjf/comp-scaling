scaling = function (mat, pop) 
  
{
  d = NULL
  
  mat = as.matrix(mat)
  mat = get.list(mat)
  mat = merge (mat, pop, by.x = 1, by.y = 1)
  colnames (mat) = c("geo", "Industry", "Count", "pop")
  
  for(i in unique (mat$Industry)){
    
    xs = subset(mat, mat$Industry == i)
    xs$Count[xs$Count==0] <- NA
      lm = lm(log(xs$Count)~log(xs$pop))
      summary(lm)$coefficients[2, 1]
      
      beta = round (summary(lm)$coefficients[2, 1], digits = 3)
      r.sq = summary(lm)$adj.r.squared
      std.err = summary(lm)$coefficients[2, 2]
      
      econ = unique (xs$Industry)
      
      d = rbind(d, data.frame(econ, beta, r.sq, std.err))
    
  }
  colnames (d) = c("Industry", "Beta", "r.sq", "std.err")
  return (d)
}

