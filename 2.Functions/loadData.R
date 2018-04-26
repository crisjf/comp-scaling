options(stringsAsFactors = FALSE)

.BRAEnv <- function(actType) {
  BRA.env <- new.env()
  assign("rcol"    , 'rcode', envir=BRA.env)
  assign("rNameCol", 'rname', envir=BRA.env)
  assign("useDec"  , FALSE  , envir=BRA.env)
  assign("year"    , 2010   , envir=BRA.env)
  if (actType=='none') {
    
  } else if (actType=='ind') {
    assign("acol"    , 'icode2', envir=BRA.env)
    assign("aNameCol", 'iname2', envir=BRA.env)
    assign("dirName" , 'BR_Ind', envir=BRA.env)
  } else if (actType=='occ') {
    assign("acol"    , 'ocode2', envir=BRA.env)
    assign("aNameCol", 'oname2', envir=BRA.env)
    assign("dirName" , 'BR_Occ', envir=BRA.env) 
  } else {
    stop(paste('Unrecognized Activity Type:',actType))
  }
  return(BRA.env)
}

.USEnv <- function(actType) {
  US.env <- new.env()
  assign("rcol"    , 'CBSA'     , envir=US.env)
  assign("rNameCol", 'CBSA.Name', envir=US.env)
  if (actType=='none') {

  } else if (actType=='field') {
    assign("acol"    , 'ASJC.2D'     , envir=US.env)
    assign("aNameCol", 'ASJC.2D.Name', envir=US.env)
    assign("dirName" , 'US_Field'    , envir=US.env)
    assign("year"    , 2010          , envir=US.env)
    assign("useDec"  , FALSE         , envir=US.env)
  } else if (actType=='techs') {
    assign("acol"    , 'NBER.Sub.Cat'     , envir=US.env)
    assign("aNameCol", 'NBER.Sub.Cat.Name', envir=US.env)
    assign("dirName" , 'US_Tech'          , envir=US.env)
    assign("year"    , 2000               , envir=US.env)
    assign("useDec"  , TRUE               , envir=US.env)
  } else if (actType=='ind') {
    assign("acol"    , 'NAICS.2D'     , envir=US.env)
    assign("aNameCol", 'NAICS.2D.Name', envir=US.env)
    assign("dirName" , 'US_Ind'       , envir=US.env)
    assign("year"    , 2015           , envir=US.env)
    assign("useDec"  , FALSE          , envir=US.env)
  } else if (actType=='occ') {
    assign("acol"    , 'Occ.2D'     , envir=US.env)
    assign("aNameCol", 'Occ.2D.Name', envir=US.env)
    assign("dirName" , 'US_Occ'     , envir=US.env)
    assign("year"    , 2015         , envir=US.env)
    assign("useDec"  , FALSE        , envir=US.env)
  } else {
    stop(paste('Unrecognized Activity Type:',actType))
  }
  return(US.env)
}


loadUSParams <- function(actType) {
  myenv <- .USEnv(actType)
  for(n in ls(myenv, all.names=TRUE)){assign(n, get(n, myenv), globalenv())}
}

loadBRAParams <- function(actType) {
  myenv <- .BRAEnv(actType)
  for(n in ls(myenv, all.names=TRUE)){assign(n, get(n, myenv), globalenv())}
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

loadUSActivity <- function(delta,actType) {
  myenv <- .USEnv(actType)
  for(n in ls(myenv, all.names=TRUE)){assign(n, get(n, myenv), environment())}

  if (actType=='field') {
    xcol     <- 'Nb.Papers.96.08'
    ycol     <- 'Year'
    RAYfname <- 'US_RegFieldYr.csv'
    Afname   <- 'US_Field.csv'
    aColLow  <- 'ASJC.4D'
  } else if (actType=='techs') {
    xcol     <- 'pat.count'
    ycol     <- 'dec' 
    RAYfname <- 'US_RegTechYr.csv'
    Afname   <- 'US_Tech.csv' 
    aColLow  <- 'Class'
  } else if (actType=='ind') {
    xcol     <- 'GDP'
    ycol     <- 'Year'
    aNameCol <- 'NAICS.2D.Name'
    RAYfname <- 'US_RegInd2DYr.csv'
    Afname   <- 'US_Ind.csv'
    aColLow  <- 'null'
  } else if (actType=='occ') {
    xcol     <- 'Nb.Emp'
    ycol     <- 'Year'
    aNameCol <- 'Occ.2D.Name'
    RAYfname <- 'US_RegOcc2DYr.csv'
    Afname   <- 'US_Occ.csv'
    aColLow  <- 'null'
  } else {
    stop(paste('Unrecognized Activity Type:',actType))
  }
  economicActivity <- .loadAct(delta,year,rcol,acol,xcol,ycol,aNameCol,RAYfname,Afname,aColLow)
  return(economicActivity)
}

loadBRActivity <- function(delta,actType) {
  myenv <- .BRAEnv(actType)
  for(n in ls(myenv, all.names=TRUE)){assign(n, get(n, myenv), environment())}

  if (actType=='occ') {
    xcol     <- 'no_people'
    ycol     <- 'year'
    RAYfname <- 'BRA_RegOccYr.csv'
    Afname   <- 'BRA_Occ.csv'
    aColLow  <- 'ocode3'
  } else if (actType=='ind') {
    xcol     <- 'no_people'
    ycol     <- 'year'
    RAYfname <- 'BRA_RegIndYr.csv'
    Afname   <- 'BRA_Ind.csv'
    aColLow  <- 'icode3'
  } else {
    stop(paste('Unrecognized Activity Type:',actType))
  }
  economicActivity <- .loadAct(delta,year,rcol,acol,xcol,ycol,aNameCol,RAYfname,Afname,aColLow)
  return(economicActivity)
}


loadUSRegs <- function(useDec,year) {
  #useDec inidicates whether to use decades or years
  myenv <- .USEnv('none')
  for(n in ls(myenv, all.names=TRUE)){assign(n, get(n, myenv), environment())}

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
  myenv <- .BRAEnv('none')
  for(n in ls(myenv, all.names=TRUE)){assign(n, get(n, myenv), environment())}
  ycol     <- 'year'
  popCol   <- 'pop'
  RYfname  <- 'BRA_RegYr.csv'
  Rfname   <- 'BRA_Reg.csv'
  popth    <- 60000
  
  region <- read.csv(paste0("../1.Data/",RYfname))[,c(ycol,rcol,popCol)]
  region <- region[region[,ycol]==2010,][,c(rcol,popCol)]
  region <- merge(region, read.csv(paste0("../1.Data/",Rfname))[,c(rcol,rNameCol)], by=rcol)
  region <- region[region[,popCol]>=popth,]
  region$pop <- region[,popCol]
  region <- region[,c(rcol,rNameCol,'pop')]
  return(region)
}


