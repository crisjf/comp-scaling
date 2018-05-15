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

plotsegraph <- function(loc, value, sterr, wiskwidth, color = "grey", linewidth = 2) {
  
  w <- wiskwidth/2
  segments(x0 = loc, x1 = loc, y0 = value - sterr, y1 = value + sterr, col = color, 
           lwd = linewidth)
  segments(x0 = loc - w, x1 = loc + w, y0 = value + sterr, y1 = value + sterr, 
           col = color, lwd = linewidth)  # upper whiskers
  segments(x0 = loc - w, x1 = loc + w, y0 = value - sterr, y1 = value - sterr, 
           col = color, lwd = linewidth)  # lower whiskers
}

bottomQ <- function(df,th,use,delta) {
	bottom <- df[,paste0('top',100-th+5,'pc.',use)]
	for (i in seq(100-th+10,100,5)){
		col <- paste0('top',i,'pc.',use)
		bottom = bottom+df[,col]
	}
	bottom <- bottom+delta
	return(bottom)
}

topQ <- function(df,th,use,delta) {
	top <- df[,paste0('top',5,'pc.',use)]
	for (i in seq(10,th,5)){
		col <- paste0('top',i,'pc.',use)
		top = top+df[,col]
	}
	top <- top+delta
	return(top)
}


scalingDec <- function(df,outcol){
  d = NULL
  df = df[complete.cases(df),]
  for (i in unique (df$dec)) {
    
    c = subset (df, df$dec == i)
    m = get.matrix(c[, c("CBSA", "dec",outcol)])
    p = unique(c[, c("CBSA", "newpop")])
    scal = scaling(m, p)
    d = rbind (d, scal)
    
  }
  d = d[order(d$Industry),]
  colnames (d) = c("Decade", "Beta.pat", "r.sq.pat",  "std.err.pat")
  return(d)
}

scalingDecByCat <- function(df,use){
	d = NULL
	for (i in unique (df$ID)) {
		c = subset (df, df$ID == i)
		m = get.matrix(c[, c("CBSA", "ID",paste0(use,".count"))])
		p = unique(c[, c("CBSA", "newpop")])
		scal = scaling(m, p)
		d = rbind (d, scal)
	}

	d = d[order(d$Industry),]
	d$Decade = substr(d$Industry, 0, 4)
	d$Cat = trimws(substr(d$Industry, 6, 200))
	d = d[, c("Cat", "Decade", "Beta", "r.sq", "std.err")]
	colnames (d) = c("Cat", "Decade", "Beta.Pat", "r.sq.Pat", "std.err.Pat")
	return(d)
}

