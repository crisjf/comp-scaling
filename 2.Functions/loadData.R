options(stringsAsFactors = FALSE)

loadBRAParams <- function(actType) {
  rcol     <<- 'rcode'
  popCol   <<- 'pop'
  rNameCol <<- 'rname'
  if (actType=='ind') {
    acol     <<- 'icode2'
    aNameCol <<- 'iname2'  
  } else if (actType=='occ') {
    acol     <<- 'ocode2'
    aNameCol <<- 'oname2'  
  } else {
    stop(paste('Unrecognized Activity Type:',actType))
  }
}

loadUSParams <- function(actType) {
  rcol     <<- 'CBSA'
  popCol   <<- 'pop.2010'
  rNameCol <<- 'CBSA.Name'
  if (actType=='field') {
    acol     <<- 'ASJC.2D'
    aNameCol <<- 'ASJC.2D.Name'
  } else {
    stop(paste('Unrecognized Activity Type:',actType))
  }
}

.loadAct <- function(delta,rcol,acol,xcol,ycol,aNameCol,RAYfname,Afname,aColLow) {
  #If no aggregation is needed, then pass aColLow='null'
  economicActivity    <- read.csv(paste0("../1.Data/",RAYfname))
  economicActivity    <- economicActivity[economicActivity[ycol]==2010,]

  if (aColLow != 'null') {
    economicActivity    <- merge(economicActivity,read.csv(paste0("../1.Data/",Afname))[,c(aColLow,acol,aNameCol)],by=aColLow)
  } else {
    economicActivity    <- merge(economicActivity,read.csv(paste0("../1.Data/",Afname))[,c(acol,aNameCol)],by=acol)
  }
    
  economicActivity$ID <- paste(economicActivity[,rcol],economicActivity[,acol])
  economicActivity$Ec.Output = ave(economicActivity[,xcol], economicActivity$ID, FUN = sum)
  economicActivity <- unique(economicActivity[,c(acol,rcol,aNameCol,'Ec.Output')])
  economicActivity$Ec.Output <- economicActivity$Ec.Output+delta
  return(economicActivity)
}

loadFields <- function(delta) {
  rcol     <- 'CBSA'
  acol     <- 'ASJC.2D'
  xcol     <- 'Nb.Papers.96.08'
  ycol     <- 'Year'
  aNameCol <- 'ASJC.2D.Name'
  RAYfname <- 'US_RegFieldYr.csv'
  Afname   <- 'US_Field.csv'
  aColLow  <- 'ASJC.4D'

  economicActivity <- .loadAct(delta,rcol,acol,xcol,ycol,aNameCol,RAYfname,Afname,aColLow)
  return(economicActivity)
}

loadBRAInds <- function(delta) {
  rcol     <- 'rcode'
  acol     <- 'icode2'
  xcol     <- 'no_people'
  ycol     <- 'year'
  aNameCol <- 'iname2'
  RAYfname <- 'BRA_RegIndYr.csv'
  Afname   <- 'BRA_Ind.csv'
  aColLow  <- 'icode3'

  economicActivity <- .loadAct(delta,rcol,acol,xcol,ycol,aNameCol,RAYfname,Afname,aColLow)
  return(economicActivity)
}

loadBRAOccs <- function(delta) {
  rcol     <- 'rcode'
  acol     <- 'ocode2'
  xcol     <- 'no_people'
  ycol     <- 'year'
  aNameCol <- 'oname2'
  RAYfname <- 'BRA_RegOccYr.csv'
  Afname   <- 'BRA_Occ.csv'
  aColLow  <- 'ocode3'

  economicActivity <- .loadAct(delta,rcol,acol,xcol,ycol,aNameCol,RAYfname,Afname,aColLow)
  return(economicActivity)
}


loadRegs <- function() {
  popCol   <- 'pop.2010' 
  rcol     <- 'CBSA'
  rNameCol <- 'CBSA.Name'
  RYfname  <- 'US_RegYr.csv'
  Rfname   <- 'US_Reg.csv'
  
  region <- read.csv(paste0("../1.Data/",RYfname))[,c(rcol,popCol)]
  region <- merge(region, read.csv(paste0("../1.Data/",Rfname))[,c(rcol,rNameCol)], by=rcol)
  return(region)
}

loadBRARegs <- function() {
  popCol.  <- 'pop'
  rcol.    <- 'rcode'
  rNameCol <- 'rname'
  ycol     <- 'year'
  RYfname  <- 'BRA_RegYr.csv'
  Rfname   <- 'BRA_Reg.csv'
  
  region <- read.csv(paste0("../1.Data/",RYfname))[,c(ycol,rcol,popCol)]
  region <- region[region[,ycol]==2010,][,c(rcol,popCol)]
  region <- merge(region, read.csv(paste0("../1.Data/",Rfname))[,c(rcol,rNameCol)], by=rcol)
  return(region)
}





