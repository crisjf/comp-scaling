options(stringsAsFactors = FALSE)

loadFields <- function(delta) {
  rcol <- 'CBSA'
  acol <- 'ASJC.2D'
  xcol <- 'Nb.Papers.96.08' #Column to use as ammount of output
  ycol <- 'Year'
  aNameCol <- 'ASJC.2D.Name'

  economicActivity    <- read.csv("../1.Data/US_RegFieldYr.csv")
  economicActivity    <- economicActivity[economicActivity[ycol]==2010,]
  economicActivity    <- economicActivity[,c(rcol,'ASJC.4D',xcol)]
  economicActivity    <- merge(economicActivity,read.csv("../1.Data/US_Field.csv")[,c('ASJC.4D',acol,aNameCol)],by='ASJC.4D')

  economicActivity$ID <- paste(economicActivity[,rcol],economicActivity[,acol])
  economicActivity$Ec.Output = ave(economicActivity[,xcol], economicActivity$ID, FUN = sum)
  economicActivity <- unique(economicActivity[,c(acol,rcol,aNameCol,'Ec.Output')])
  economicActivity$Ec.Output <- economicActivity$Ec.Output+delta
  return(economicActivity)
}


loadBRAInds <- function(delta) {
  rcol <- 'rcode'
  acol <- 'icode2'
  xcol <- 'no_people'
  ycol <- 'year'
  aNameCol <- 'iname2'
  
  economicActivity    <- read.csv("../1.Data/BRA_RegIndYr.csv")
  economicActivity    <- economicActivity[economicActivity[ycol]==2010,]
  economicActivity    <- merge(economicActivity,read.csv("../1.Data/BRA_Ind.csv")[,c('icode3',acol,aNameCol)],by='icode3')
  
  economicActivity$ID <- paste(economicActivity[,rcol],economicActivity[,acol])
  economicActivity$Ec.Output = ave(economicActivity[,xcol], economicActivity$ID, FUN = sum)
  economicActivity <- unique(economicActivity[,c(acol,rcol,aNameCol,'Ec.Output')])
  economicActivity$Ec.Output <- economicActivity$Ec.Output+delta
  return(economicActivity)
}

loadRegs <- function() {
  popCol <- 'pop.2010' #Column to use as population data
  rcol <- 'CBSA'
  rNameCol <- 'CBSA.Name'
  
  region <- read.csv("../1.Data/US_RegYr.csv")[,c(rcol,popCol)]
  region <- merge(region, read.csv("../1.Data/US_Reg.csv")[,c(rcol,rNameCol)], by=rcol)
  return(region)
}

loadBRARegs <- function() {
  popCol <- 'pop'
  rcol <- 'rcode'
  rNameCol <- 'rname'
  ycol <- 'year'
  
  region <- read.csv("../1.Data/BRA_RegYr.csv")[,c(ycol,rcol,popCol)]
  region <- region[region[,ycol]==2010,][,c(rcol,popCol)]
  region <- merge(region, read.csv("../1.Data/BRA_Reg.csv")[,c(rcol,rNameCol)], by=rcol)
  return(region)
}





