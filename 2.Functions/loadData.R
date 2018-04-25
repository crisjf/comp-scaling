options(stringsAsFactors = FALSE)

loadBRAParams <- function(actType) {
  rcol     <<- 'rcode'
  popCol   <<- 'pop'
  rNameCol <<- 'rname'
  useDec   <<- FALSE
  if (actType=='ind') {
    acol     <<- 'icode2'
    aNameCol <<- 'iname2'  
    dirName  <<- 'BR_Ind'
  } else if (actType=='occ') {
    acol     <<- 'ocode2'
    aNameCol <<- 'oname2'
    dirName  <<- 'BR_Occ'  
  } else {
    stop(paste('Unrecognized Activity Type:',actType))
  }
}

loadUSParams <- function(actType) {
  rcol     <<- 'CBSA'
  popCol   <<- 'pop'
  rNameCol <<- 'CBSA.Name'
  if (actType=='field') {
    acol     <<- 'ASJC.2D'
    aNameCol <<- 'ASJC.2D.Name'
    dirName  <<- 'US_Field'
    year     <<- 2010
    useDec   <<- FALSE
  } else if (actType=='techs') {
    acol     <<- 'NBER.Sub.Cat'
    aNameCol <<- 'NBER.Sub.Cat.Name'
    dirName  <<- 'US_Tech'
    year     <<- 2000
    useDec   <<- TRUE
  } else if (actType=='ind') {
    acol     <<- 'NAICS.2D'
    aNameCol <<- 'NAICS.2D.Name'
    dirName  <<- 'US_Ind'
    year     <<- 2015
    useDec   <<- FALSE
  } else if (actType=='occ') {
    acol     <<- 'Occ.2D'
    aNameCol <<- 'Occ.2D.Name'
    dirName  <<- 'US_Occ'
    year     <<- 2015
    useDec   <<- FALSE
  } else {
    stop(paste('Unrecognized Activity Type:',actType))
  }
}


.loadAct <- function(delta,year,rcol,acol,xcol,ycol,aNameCol,RAYfname,Afname,aColLow) {
  #If no aggregation is needed, then pass aColLow='null' (never tested)
  #If delta=0 it will drop all zeros
  economicActivity    <- read.csv(paste0("../1.Data/",RAYfname))
  economicActivity    <- economicActivity[economicActivity[ycol]==year,]

  if (aColLow != 'null') {
    economicActivity    <- merge(economicActivity,read.csv(paste0("../1.Data/",Afname))[,c(aColLow,acol,aNameCol)],by=aColLow)
  } else {
    economicActivity    <- merge(economicActivity,unique(read.csv(paste0("../1.Data/",Afname))[,c(acol,aNameCol)]),by=acol)
  }

  economicActivity$ID <- paste(economicActivity[,rcol],economicActivity[,acol])
  economicActivity$Ec.Output = ave(economicActivity[,xcol], economicActivity$ID, FUN = sum)
  economicActivity <- unique(economicActivity[,c(acol,rcol,aNameCol,'Ec.Output')])
  economicActivity$Ec.Output <- economicActivity$Ec.Output+delta
  economicActivity <- economicActivity[economicActivity$Ec.Output!=0,]
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
  year     <- 2010

  economicActivity <- .loadAct(delta,year,rcol,acol,xcol,ycol,aNameCol,RAYfname,Afname,aColLow)
  return(economicActivity)
}

loadTechs <- function(delta) {
  rcol     <- 'CBSA'
  acol     <- 'NBER.Sub.Cat'
  xcol     <- 'pat.count'
  ycol     <- 'dec' 
  aNameCol <- 'NBER.Sub.Cat.Name'
  RAYfname <- 'US_RegTechYr.csv'
  Afname   <- 'US_Tech.csv' 
  aColLow  <- 'Class'
  year     <- 2000

  economicActivity <- .loadAct(delta,year,rcol,acol,xcol,ycol,aNameCol,RAYfname,Afname,aColLow)
  return(economicActivity)
}

loadInds <- function(delta) {
  rcol     <- 'CBSA'
  acol     <- 'NAICS.2D'
  xcol     <- 'GDP'
  ycol     <- 'Year'
  aNameCol <- 'NAICS.2D.Name'
  RAYfname <- 'US_RegInd2DYr.csv'
  Afname   <- 'US_Ind.csv'
  aColLow  <- 'null'
  year     <- 2015

  economicActivity <- .loadAct(delta,year,rcol,acol,xcol,ycol,aNameCol,RAYfname,Afname,aColLow)
  return(economicActivity)
}

loadOccs <- function(delta) {
  rcol     <- 'CBSA'
  acol     <- 'Occ.2D'
  xcol     <- 'Nb.Emp'
  ycol     <- 'Year'
  aNameCol <- 'Occ.2D.Name'
  RAYfname <- 'US_RegOcc2DYr.csv'
  Afname   <- 'US_Occ.csv'
  aColLow  <- 'null'
  year     <- 2015

  economicActivity <- .loadAct(delta,year,rcol,acol,xcol,ycol,aNameCol,RAYfname,Afname,aColLow)
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
  year     <- 2010

  economicActivity <- .loadAct(delta,year,rcol,acol,xcol,ycol,aNameCol,RAYfname,Afname,aColLow)
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
  year     <- 2010

  economicActivity <- .loadAct(delta,year,rcol,acol,xcol,ycol,aNameCol,RAYfname,Afname,aColLow)
  return(economicActivity)
}


loadRegs <- function(useDec,year) {
  #useDec inidicates whether to use decades or years
  rcol     <- 'CBSA'
  rNameCol <- 'CBSA.Name'
  Rfname   <- 'US_Reg.csv'
  gdpCol   <- 'gdp.2015'
  empCol   <- 'emp.2015'

  if (useDec) {
    RYfname <- 'US_RegDec.csv'
    ycol    <- 'dec'
    popCol  <- 'newpop'
  } else {
    RYfname  <- 'US_RegYr.csv'
    if (year==2010) {
      popCol   <- paste0('pop.',year)
    } else {
      popCol   <- paste0('pop.est.',year)
    }
  }

  region <- read.csv(paste0("../1.Data/",RYfname))
  if (useDec) {
    region <- region[region[,ycol]==year,]
  }
  region$pop <- region[,popCol]
  region <- unique(region[,c(rcol,'pop',gdpCol,empCol)])
  region <- merge(region, read.csv(paste0("../1.Data/",Rfname))[,c(rcol,rNameCol,'Flag.CBSA.availability')], by=rcol)
  region <- region[region$Flag.CBSA.availability==1,c(rcol,rNameCol,'pop',gdpCol,empCol)] 
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


