options(stringsAsFactors = FALSE)

.USEnv <- function(actType,aggregate=TRUE) {
  US.env <- new.env()
  assign("rcol"    , 'CBSA'      , envir=US.env)
  assign("rNameCol", 'CBSA.Name' , envir=US.env)
  assign("Rfname"  , 'US_Reg.csv', envir=US.env)

  if (actType=='none') {

  } else if (actType=='field') {
    assign("dirName" , 'US_Field'         , envir=US.env)
    assign("year"    , 2010               , envir=US.env)
    assign("useDec"  , FALSE              , envir=US.env)
    assign("Afname"  , 'US_Field.csv'     , envir=US.env)
    assign("AYfname" , 'US_FieldYr.csv'   , envir=US.env)
    assign("RAYfname", 'US_RegFieldYr.csv', envir=US.env)
    if (aggregate) {
      assign("aggregate", TRUE          , envir=US.env)
      assign("acol"     , 'ASJC.2D'     , envir=US.env)
      assign("aNameCol" , 'ASJC.2D.Name', envir=US.env)
      assign("aColLow"  , 'ASJC.4D'     , envir=US.env)
    } else {
      assign("aggregate", FALSE         , envir=US.env)
      assign("acol"     , 'ASJC.4D'     , envir=US.env)
      assign("aNameCol" , 'ASJC.4D.Name', envir=US.env)
      assign("aColLow"  , 'null'        , envir=US.env)
    }
  } else if (actType=='techs') {
    assign("dirName" , 'US_Tech', envir=US.env)
    assign("year"    , 2000     , envir=US.env)
    assign("useDec"  , TRUE     , envir=US.env)
    assign("Afname"  , 'US_Tech.csv'     , envir=US.env)
    assign("AYfname" , 'US_TechDec.csv'   , envir=US.env)
    assign("RAYfname", 'US_RegTechYr.csv', envir=US.env)
    if (aggregate) {
      assign("aggregate", TRUE               , envir=US.env)
      assign("acol"     , 'NBER.Sub.Cat'     , envir=US.env)
      assign("aNameCol" , 'NBER.Sub.Cat.Name', envir=US.env)
      assign("aColLow"  , 'Class'            , envir=US.env)
    } else { 
      assign("aggregate", FALSE       , envir=US.env)
      assign("acol"     , 'Class'     , envir=US.env)
      assign("aNameCol" , 'Class.Name', envir=US.env)
      assign("aColLow"  , 'null'      , envir=US.env)
    }
    
  } else if (actType=='ind') {
    assign("dirName"  , 'US_Ind'       , envir=US.env)
    assign("year"     , 2015           , envir=US.env)
    assign("useDec"   , FALSE          , envir=US.env)
    assign("Afname"   , 'US_Ind.csv'   , envir=US.env)
    if (aggregate) {
      assign("aggregate", TRUE               , envir=US.env)
      assign("aColLow"  , 'null'             , envir=US.env)
      assign("acol"     , 'NAICS.2D'         , envir=US.env)
      assign("aNameCol" , 'NAICS.2D.Name'    , envir=US.env)
      assign("AYfname"  , 'US_IndYr.csv'   , envir=US.env)
      assign("RAYfname" , 'US_RegInd2DYr.csv', envir=US.env)
    } else {
      assign("aggregate", FALSE            , envir=US.env)
      assign("aColLow"  , 'null'           , envir=US.env)
      assign("acol"     , 'NAICS.4D'       , envir=US.env)
      assign("aNameCol" , 'NAICS.4D.Name'  , envir=US.env)
      assign("AYfname"  , 'US_IndYr.csv'   , envir=US.env)
      assign("RAYfname" , 'US_RegIndYr.csv', envir=US.env)
    }
  } else if (actType=='occ') {
    
    assign("dirName" , 'US_Occ'     , envir=US.env)
    assign("year"    , 2015         , envir=US.env)
    assign("useDec"  , FALSE        , envir=US.env)
    assign("Afname"  , 'US_Occ.csv' , envir=US.env)
    if (aggregate) {
      assign("aggregate", TRUE               , envir=US.env)
      assign("aColLow"  , 'null'             , envir=US.env)
      assign("acol"     , 'Occ.2D'           , envir=US.env)
      assign("aNameCol" , 'Occ.2D.Name'      , envir=US.env)
      assign("AYfname"  , 'US_OccYr.csv'   , envir=US.env)
      assign("RAYfname" , 'US_RegOcc2DYr.csv', envir=US.env)
    } else {
      assign("aggregate", FALSE            , envir=US.env)
      assign("aColLow"  , 'null'           , envir=US.env)
      assign("acol"     , 'Occ.3D'         , envir=US.env)
      assign("aNameCol" , 'Occ.3D.Name'    , envir=US.env)
      assign("AYfname"  , 'US_OccYr.csv'   , envir=US.env)
      assign("RAYfname" , 'US_RegOccYr.csv', envir=US.env)
    }
  } else {
    stop(paste('Unrecognized Activity Type:',actType,'\nChoose between: field, techs, ind, or occ.'))
  }
  return(US.env)
}


loadUSParams <- function(actType,aggregate=TRUE) {
  myenv <- .USEnv(actType,aggregate)
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
    df <- unique(read.csv(paste0("../1.Data/",Afname))[,c(acol,aNameCol)])
    df <- df[!is.na(df[,acol]),]
    economicActivity    <- merge(economicActivity,df,by=acol)
  }

  economicActivity$ID <- paste(economicActivity[,rcol],economicActivity[,acol])
  economicActivity$Ec.Output = ave(economicActivity[,xcol], economicActivity$ID, FUN = sum)
  economicActivity <- unique(economicActivity[,c(acol,rcol,aNameCol,'Ec.Output')])
  economicActivity$Ec.Output <- economicActivity$Ec.Output+delta
  economicActivity <- economicActivity[economicActivity$Ec.Output!=0,]
  return(economicActivity)
}


loadUSActivity <- function(delta,actType,aggregate=TRUE) {
  myenv <- .USEnv(actType,aggregate)
  for (n in ls(myenv, all.names=TRUE)){assign(n, get(n, myenv), environment())}
  if (actType=='field') {
    xcol     <- 'Nb.Papers.96.08'
    ycol     <- 'Year'
  } else if (actType=='techs') {
    xcol     <- 'pat.count'
    ycol     <- 'dec' 
  } else if (actType=='ind') {
    xcol     <- 'GDP'
    ycol     <- 'Year'
  } else if (actType=='occ') {
    xcol     <- 'Nb.Emp'
    ycol     <- 'Year'
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
    region <- merge(region,read.csv(paste0("../1.Data/US_RegYr.csv"))[,c(rcol,gdpCol,empCol)],by=rcol)
  }
  region$pop <- region[,popCol]
  region <- unique(region[,c(rcol,'pop',gdpCol,empCol)])
  region <- merge(region, read.csv(paste0("../1.Data/",Rfname))[,c(rcol,rNameCol,'Flag.CBSA.availability')], by=rcol)
  region <- region[region$Flag.CBSA.availability==1,c(rcol,rNameCol,'pop',gdpCol,empCol)] 
  return(region)
}


loadUSComplexity <- function(actType,aggregate=TRUE,sizeCol=NA,compCol=NA) {
  myenv <- .USEnv(actType,aggregate)
  for (n in ls(myenv, all.names=TRUE)){assign(n, get(n, myenv), environment())}

  if (actType=='field') {
    if (is.na(sizeCol)) {sizeCol <- 'Nb.Papers.96.08'}
    if (is.na(compCol)) {compCol <- 'Mean.Nb.Aut.96.08'}
    ycol    <- 'Year'
    if (aggregate) {
      exclude <- c("Agricultural and Biological Sciences","Environmental Science",
             "Earth and Planetary Sciences","Veterinary") 
    } else {
      exclude <- c()
    }
  } else if (actType=='techs') {
    if (is.na(sizeCol)) {sizeCol <- 'nb.pat.class.dec'}
    if (is.na(compCol)) {compCol <- 'age.class.dec'}
    ycol    <- 'dec'
    if (aggregate) {
      exclude <- c("Resins","Organic Compounds") 
    } else {
      exclude <- c()
    }
  } else if (actType=='ind') {
    if (is.na(sizeCol)) {sizeCol <- 'Employment'}
    if (is.na(compCol)) {compCol <- 'Share.STEM.occ'}
    ycol    <- 'Year'
    if (aggregate) {
      aColLow <- 'NAICS.4D'
      exclude <- c() 
    } else {
      exclude <- c()
    }
  } else if (actType=='occ') {
    if (is.na(sizeCol)) {sizeCol <- 'Nb.Emp'}
    if (is.na(compCol)) {compCol <- 'Mean.Y.Educ'}
    ycol <- 'Year'
    year <- 2016 #This overrides the default year
    if (aggregate) {
      aColLow <- 'Occ.3D'
      exclude <- c() 
    } else {
      exclude <- c()
    }
  } else {
    stop(paste('Unrecognized Activity Type:',actType,'\nChoose between: field, techs, ind, or occ.'))
  }
  comp <- read.csv(paste0('../1.Data/',AYfname))
  comp <- comp[comp[,ycol]==year,]

  if (aColLow != 'null') {
    comp <- unique(comp[,c(aColLow,sizeCol,compCol)])
    comp <- merge(comp,read.csv(paste0("../1.Data/",Afname))[,c(aColLow,acol,aNameCol)],by=aColLow)
    
    comp$totalSize <- ave(comp[,sizeCol], comp[,acol], FUN = sum)
    comp$comp      <- comp[,compCol]*comp[,sizeCol]/comp$totalSize
    comp$comp      <- ave(comp$comp, comp[,acol], FUN = sum)
  } else {
    comp <- unique(comp[,c(acol,sizeCol,compCol)])
    comp <- merge(comp,unique(read.csv(paste0("../1.Data/",Afname))[,c(acol,aNameCol)]),by=acol)
    comp$comp <- comp[,compCol]
  }
  comp <- unique(comp[,c(acol,aNameCol,'comp')])
  for (n in exclude) {
    comp <- comp[comp[,aNameCol]!=n,]
  }
  return(comp)   
}



##########################################################################################



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

loadBRAParams <- function(actType) {
  myenv <- .BRAEnv(actType)
  for(n in ls(myenv, all.names=TRUE)){assign(n, get(n, myenv), globalenv())}
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


