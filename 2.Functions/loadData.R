options(stringsAsFactors = FALSE)

.USEnv <- function(actType,aggregate=TRUE) {
  US.env <- new.env()
  assign("rcol"    , 'CBSA'      , envir=US.env)
  assign("rNameCol", 'CBSA.Name' , envir=US.env)
  assign("Rfname"  , 'US_Reg.csv', envir=US.env)

  if (actType=='none') {

  } else if (actType=='field') {
    assign('actName' ,'Scientific Fields' , envir=US.env)
    assign('compName','Average number of authors', envir=US.env)
    assign("dirName" , 'US_Field'         , envir=US.env)
    assign("year"    , 2010               , envir=US.env)
    assign("useDec"  , FALSE              , envir=US.env)
    assign("Afname"  , 'US_Field.csv'     , envir=US.env)
    assign("AYfname" , 'US_FieldYr.csv'   , envir=US.env)
    assign("RAYfname", 'US_RegFieldYr.csv', envir=US.env)
    if (aggregate) {
      assign("alternativeComp"     , c("age"), envir=US.env)
      assign("alternativeCompNames", c("Average age of references"), envir=US.env)

      assign("aggregate", TRUE          , envir=US.env)
      assign("acol"     , 'ASJC.2D'     , envir=US.env)
      assign("aNameCol" , 'ASJC.2D.Name', envir=US.env)
      assign("aColLow"  , 'ASJC.4D'     , envir=US.env)
    } else {
      assign("alternativeComp"     , c(), envir=US.env)
      assign("alternativeCompNames", c(), envir=US.env)

      assign("aggregate", FALSE         , envir=US.env)
      assign("acol"     , 'ASJC.4D'     , envir=US.env)
      assign("aNameCol" , 'ASJC.4D.Name', envir=US.env)
      assign("aColLow"  , 'null'        , envir=US.env)
    }
  } else if (actType=='techs') {
    assign('actName' ,'Technological Classes' , envir=US.env)
    assign('compName','Average year of subclass introduction', envir=US.env)
    assign("dirName" , 'US_Tech'    , envir=US.env)
    assign("year"    , 2000         , envir=US.env)
    assign("useDec"  , TRUE         , envir=US.env)
    assign("Afname"  , 'US_Tech.csv', envir=US.env)
    
    if (aggregate) {
      assign("alternativeComp"     , c('NK','date.est','m_invcount'), envir=US.env)
      assign("alternativeCompNames", c('NK-Complexity','Date Est.','Average number of inventors'), envir=US.env)

      assign("aggregate", TRUE                , envir=US.env)
      assign("aColLow"  , 'null'              , envir=US.env)
      assign("acol"     , 'NBER.Sub.Cat'      , envir=US.env)
      assign("aNameCol" , 'NBER.Sub.Cat.Name' , envir=US.env)
      assign("RAYfname" , 'US_RegTech2DYr.csv', envir=US.env)  
      assign("AYfname"  , 'US_Tech2DDec.csv'  , envir=US.env)
    } else { 
      assign("alternativeComp"     , c(), envir=US.env)
      assign("alternativeCompNames", c(), envir=US.env)

      assign("aggregate", FALSE       , envir=US.env)
      assign("acol"     , 'Class'     , envir=US.env)
      assign("aNameCol" , 'Class.Name', envir=US.env)
      assign("aColLow"  , 'null'      , envir=US.env)
      assign("RAYfname" , 'US_RegTechYr.csv', envir=US.env)  
      assign("AYfname"  , 'US_TechDec.csv'   , envir=US.env)
    }
    
  } else if (actType=='ind') {
    assign('actName' ,'Industries' , envir=US.env)
    assign('compName','Average year of education', envir=US.env)

    assign("dirName"  , 'US_Ind'       , envir=US.env)
    assign("year"     , 2015           , envir=US.env)
    assign("useDec"   , FALSE          , envir=US.env)
    assign("Afname"   , 'US_Ind.csv'   , envir=US.env)
    assign("AYfname"  , 'US_IndYr.csv'   , envir=US.env)
    if (aggregate) {
      assign("alternativeComp"     , c("Share.STEM.occ"), envir=US.env)
      assign("alternativeCompNames", c("Share of STEM workers"), envir=US.env)

      assign("aggregate", TRUE               , envir=US.env)
      assign("aColLow"  , 'null'             , envir=US.env)
      assign("acol"     , 'NAICS.2D'         , envir=US.env)
      assign("aNameCol" , 'NAICS.2D.Name'    , envir=US.env)
      assign("RAYfname" , 'US_RegInd2DYr.csv', envir=US.env)
    } else {
      assign("alternativeComp"     , c(), envir=US.env)
      assign("alternativeCompNames", c(), envir=US.env)

      assign("aggregate", FALSE              , envir=US.env)
      assign("aColLow"  , 'null'             , envir=US.env)
      assign("acol"     , 'NAICS.3D'         , envir=US.env)
      assign("aNameCol" , 'NAICS.3D.Name'    , envir=US.env)
      assign("RAYfname" , 'US_RegInd3DYr.csv', envir=US.env)
    }
  } else if (actType=='occ') {
    assign('actName' ,'Occupations' , envir=US.env)
    assign('compName','Average year of education', envir=US.env)

    assign("dirName" , 'US_Occ'     , envir=US.env)
    assign("year"    , 2015         , envir=US.env)
    assign("useDec"  , FALSE        , envir=US.env)
    assign("Afname"  , 'US_Occ.csv' , envir=US.env)
    if (aggregate) {
      assign("alternativeComp"     , c('H_MEAN','ProbAuto','Originality'), envir=US.env)
      assign("alternativeCompNames", c('Average wage','Probability of automation','Originality'), envir=US.env)

      assign("aggregate", TRUE               , envir=US.env)
      assign("aColLow"  , 'null'             , envir=US.env)
      assign("acol"     , 'Occ.2D'           , envir=US.env)
      assign("aNameCol" , 'Occ.2D.Name'      , envir=US.env)
      assign("AYfname"  , 'US_OccYr.csv'   , envir=US.env)
      assign("RAYfname" , 'US_RegOcc2DYr.csv', envir=US.env)
    } else {
      assign("alternativeComp"     , c(), envir=US.env)
      assign("alternativeCompNames", c(), envir=US.env)

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
  #If delta=0 it will drop all zeros
  economicActivity    <- read.csv(paste0("../1.Data/",RAYfname))
  economicActivity    <- economicActivity[economicActivity[ycol]==year,]
  if (aColLow != 'null') {
    economicActivity    <- merge(economicActivity,unique(read.csv(paste0("../1.Data/",Afname))[,c(aColLow,acol,aNameCol)]),by=aColLow)
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


loadUSActivity <- function(delta,actType,aggregate=TRUE,allCities=FALSE) {
  myenv <- .USEnv(actType,aggregate)
  for (n in ls(myenv, all.names=TRUE)){assign(n, get(n, myenv), environment())}
  if (actType=='field') {
    xcol     <- 'Nb.Papers.96.08'
    ycol     <- 'Year'
  } else if (actType=='techs') {
    xcol     <- 'pat.count' #claim.count
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
  economicActivity <- economicActivity[complete.cases(economicActivity),]
  return(economicActivity)
}


loadUSRegs <- function(useDec,year,allCities=FALSE) {
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
    if (!allCities) {
      region <- merge(region,read.csv(paste0("../1.Data/US_RegYr.csv"))[,c(rcol,gdpCol,empCol)],by=rcol)
    } else {      
      region <- merge(region,read.csv(paste0("../1.Data/US_RegYr.csv"))[,c(rcol,gdpCol,empCol)],by=rcol,all.x = TRUE)
    }
  }

  region$pop <- region[,popCol]
  region <- unique(region[,c(rcol,'pop',gdpCol,empCol)])
  if (!allCities) {
    region <- merge(region, read.csv(paste0("../1.Data/",Rfname))[,c(rcol,rNameCol,'Flag.CBSA.availability')], by=rcol)
  } else {
    region <- merge(region, read.csv(paste0("../1.Data/",Rfname))[,c(rcol,rNameCol,'Flag.CBSA.availability')], by=rcol,all.x = TRUE)
  }

  if (!allCities) {
    region <- region[region$Flag.CBSA.availability==1,c(rcol,rNameCol,'pop',gdpCol,empCol)] 
  } else {
    region <- region[,c(rcol,rNameCol,'pop',gdpCol,empCol,'Flag.CBSA.availability')] 
    region[["Flag.CBSA.availability"]][is.na(region[["Flag.CBSA.availability"]])] <- 0
  }
  return(region)
}


loadUSComplexity <- function(actType,aggregate=TRUE,sizeCol=NA,compCol=NA,exclude=TRUE) {
  myenv <- .USEnv(actType,aggregate)
  for (n in ls(myenv, all.names=TRUE)){assign(n, get(n, myenv), environment())}

  if (actType=='field') {
    if (is.na(sizeCol)) {sizeCol <- 'Nb.Papers.96.08'}
    if (is.na(compCol)) {compCol <- 'Mean.Nb.Aut.96.08'}
    ycol    <- 'Year'
    if (aggregate) {
      if (exclude){
        toexclude <- c("Agricultural and Biological Sciences","Environmental Science","Earth and Planetary Sciences","Veterinary") 
      } else {
        toexclude <- c()
      }
    } else {
      toexclude <- c()
    }
  } else if (actType=='techs') {
    if (is.na(sizeCol)) {sizeCol <- 'nb.pat.class.dec'}
    if (is.na(compCol)) {compCol <- 'date.1st.sub.cat.dec'}
    ycol    <- 'dec'
    if (aggregate) {
      sizeCol <- 'null'
      # acol <- 'NBER.Sub.Cat'
      # aNameCol <- 'NBER.Sub.Cat.Name'
      if (exclude){
        toexclude <- c("Agriculture, Husbandry, Food","Agriculture, Food, Textiles","Earth Working & Wells")
      } else {
        toexclude <- c()
      }
    } else {
      toexclude <- c()
    }
  } else if (actType=='ind') {
    if (is.na(sizeCol)) {sizeCol <- 'Employment'}
    if (is.na(compCol)) {compCol <- 'Mean.Y.Educ.NAICS'}
    ycol    <- 'Year'
    if (aggregate) {
      aColLow <- 'NAICS.4D'
      if (exclude){
        toexclude <- c("Agriculture, forestry, fishing, and hunting","  Utilities")
      } else {
        toexclude <- c()
      }
    } else {
      aColLow <- 'NAICS.4D'
      acol <- 'NAICS.3D'
      aNameCol <- 'NAICS.3D.Name'
      toexclude <- c()
    }
  } else if (actType=='occ') {
    if (is.na(sizeCol)) {sizeCol <- 'Nb.Emp'}
    if (is.na(compCol)) {compCol <- 'Mean.Y.Educ'}
    ycol <- 'Year'
    year <- 2016 #This overrides the default year
    if (aggregate) {
      aColLow <- 'Occ.3D'
      if (exclude){
        toexclude <- c("Farming, fishing, and forestry") 
      } else {
        toexclude <- c()
      }
    } else {
      toexclude <- c()
    }
  } else {
    stop(paste('Unrecognized Activity Type:',actType,'\nChoose between: field, techs, ind, or occ.'))
  }

  comp <- read.csv(paste0('../1.Data/',AYfname))
  comp <- comp[comp[,ycol]==year,]

  if (aColLow != 'null') {
    comp <- unique(comp[,c(aColLow,sizeCol,compCol)])
    comp <- comp[complete.cases(comp),]
    comp <- merge(comp,unique(read.csv(paste0("../1.Data/",Afname))[,c(aColLow,acol,aNameCol)]),by=aColLow)

    comp$totalSize <- ave(comp[,sizeCol], comp[,acol], FUN = sum)
    comp$comp      <- comp[,compCol]*comp[,sizeCol]/comp$totalSize
    comp$comp      <- ave(comp$comp, comp[,acol], FUN = sum)
  } else {
    if (sizeCol!='null') {
      comp <- unique(comp[,c(acol,sizeCol,compCol)])
    } else {
      comp <- unique(comp[,c(acol,compCol)])
    }
    comp <- comp[complete.cases(comp),]
    comp <- merge(comp,unique(read.csv(paste0("../1.Data/",Afname))[,c(acol,aNameCol)]),by=acol)
    comp$comp <- comp[,compCol]
  }
  comp <- unique(comp[,c(acol,aNameCol,'comp')])
  for (n in toexclude) {
    comp <- comp[comp[,aNameCol]!=n,]
  }
  return(comp)   
}


loadUSTechsDec <- function(delta,use,level,allCities=FALSE) {
  myenv <- .USEnv('techs',FALSE)
  for (n in ls(myenv, all.names=TRUE)){assign(n, get(n, myenv), environment())}
  catCol <- paste0('NBER.',level)
  catNameCol <- paste0(catCol,'.Name')
  RYfname <- "US_RegDec.csv"
  df = read.csv(paste0("../1.Data/",RAYfname))
  df = merge (df, unique(read.csv(paste0("../1.Data/",Afname))[, c("Class", catCol, catNameCol)]), by = "Class")
  df$ID = paste(df[,catCol], df$CBSA, df$dec, sep = "-")
  df[,paste0(use,'.s')] = ave(df[,paste0(use,'.count')], df$ID, FUN = sum)
  df = unique (df[, c("CBSA", catCol, catNameCol, "dec", paste0(use,".s"))])
  df = merge (df, read.csv(paste0("../1.Data/",RYfname))[, c("CBSA", "dec","newpop")], by = c("CBSA", "dec"))
  
  df$ID = paste(df$dec, df[,catNameCol], sep = "-")
  df = df[complete.cases(df),]
  df[,paste0(use,'.count')] = df[,paste0(use,'.s')] + delta
  cities <- read.csv(paste0('../1.Data/',Rfname))[,c("CBSA","Flag.CBSA.availability")]
  if (!allCities) {
    df <-merge(df,cities[cities$Flag.CBSA.availability==1,],by='CBSA')
  }

  df <- df[,c('CBSA','ID','dec',catCol,catNameCol,paste0(use,'.count'),'newpop')]

  return(df)
}


##########################################################################################



.BRAEnv <- function(actType,aggregate,level=2) {
  BRA.env <- new.env()
  assign("rcol"    , 'rcode', envir=BRA.env)
  assign("rNameCol", 'rname', envir=BRA.env)
  assign("useDec"  , FALSE  , envir=BRA.env)
  assign("year"    , 2010   , envir=BRA.env)

  assign("Rfname"  , 'BRA_Reg.csv'   , envir=BRA.env)
  assign("RYfname"  , 'BRA_RegYr.csv'   , envir=BRA.env)
  if (actType=='none') {
    
  } else if (actType=='ind') {
    assign('actName' ,'Industries' , envir=BRA.env)
    assign('compName','Average years of education', envir=BRA.env)
    assign("alternativeComp"     , c("avg_wage"), envir=BRA.env)
    assign("alternativeCompNames", c("Average wage"), envir=BRA.env)

    assign("dirName" , 'BR_Ind', envir=BRA.env)
    assign("Afname"  , 'BRA_Ind.csv'   , envir=BRA.env)
    assign("AYfname" , 'BRA_IndYr.csv'   , envir=BRA.env)
    assign("RAYfname" ,'BRA_RegIndYr.csv'   , envir=BRA.env)
    if (aggregate) {
      if (level==2) {
        assign("acol"    , 'icode2', envir=BRA.env)
        assign("aColLow" , 'icode3', envir=BRA.env)
        assign("aNameCol", 'iname2', envir=BRA.env)
      } else {
        assign("acol"    , 'icode1', envir=BRA.env)
        assign("aColLow" , 'icode3', envir=BRA.env)
        assign("aNameCol", 'iname1', envir=BRA.env)
      }
    } else {
      assign("acol"    , 'icode3', envir=BRA.env)
      assign("aColLow" , 'null', envir=BRA.env)
      assign("aNameCol", 'iname3', envir=BRA.env)
    }
  } else if (actType=='occ') {
    assign('actName' ,'Occupations' , envir=BRA.env)
    assign('compName','Average years of education', envir=BRA.env)
    assign("alternativeComp"     , c("avg_wage"), envir=BRA.env)
    assign("alternativeCompNames", c("Average wage"), envir=BRA.env)

    assign("dirName" , 'BR_Occ', envir=BRA.env) 
    assign("Afname"  , 'BRA_Occ.csv'   , envir=BRA.env)
    assign("AYfname" , 'BRA_OccYr.csv'   , envir=BRA.env)
    assign("RAYfname" ,'BRA_RegOccYr.csv'   , envir=BRA.env)
    if (aggregate) {
      if (level==2) {
        assign("acol"    , 'ocode2', envir=BRA.env)
        assign("aColLow" , 'ocode3', envir=BRA.env)
        assign("aNameCol", 'oname2', envir=BRA.env)  
      } else {
        assign("acol"    , 'ocode1', envir=BRA.env)
        assign("aColLow" , 'ocode3', envir=BRA.env)
        assign("aNameCol", 'oname1', envir=BRA.env)  
      }
      
    } else {
      assign("acol"    , 'ocode3', envir=BRA.env)
      assign("aColLow" , 'null', envir=BRA.env)
      assign("aNameCol", 'oname3', envir=BRA.env)
    }

  } else {
    stop(paste('Unrecognized Activity Type:',actType))
  }
  return(BRA.env)
}

loadBRAParams <- function(actType,aggregate,level=2) {
  myenv <- .BRAEnv(actType,aggregate,level=level)
  for(n in ls(myenv, all.names=TRUE)){assign(n, get(n, myenv), globalenv())}
}


loadBRActivity <- function(delta,actType,aggregate,level=2) {
  myenv <- .BRAEnv(actType,aggregate,level=level)
  for(n in ls(myenv, all.names=TRUE)){assign(n, get(n, myenv), environment())}
  xcol     <- 'no_people'
  ycol     <- 'year'
  economicActivity <- .loadAct(delta,year,rcol,acol,xcol,ycol,aNameCol,RAYfname,Afname,aColLow)
  return(economicActivity)
}



loadBRARegs <- function(year) {
  myenv <- .BRAEnv('none',TRUE,level=2)
  for(n in ls(myenv, all.names=TRUE)){assign(n, get(n, myenv), environment())}
  ycol     <- 'year'
  popCol   <- 'pop'
  RYfname  <- 'BRA_RegYr.csv'
  Rfname   <- 'BRA_Reg.csv'
  popth    <- 60000
  
  region <- read.csv(paste0("../1.Data/",RYfname))[,c(ycol,rcol,popCol)]
  region <- region[region[,ycol]==year,][,c(rcol,popCol)]
  region <- merge(region, read.csv(paste0("../1.Data/",Rfname))[,c(rcol,rNameCol)], by=rcol)
  region <- region[region[,popCol]>=popth,]
  region$pop <- region[,popCol]
  region <- region[,c(rcol,rNameCol,'pop')]
  return(region)
}



loadBRAComplexity <- function(actType,aggregate,compCol=NA,year=NA,level=2) {
  if (is.na(compCol)) {compCol <- 'edu'}
  if (is.na(year)) {year <- 2010}
  sizeCol <- 'no_people'
  myenv <- .BRAEnv(actType,aggregate,level=level)
  for (n in ls(myenv, all.names=TRUE)){assign(n, get(n, myenv), environment())}
  comp <- read.csv(paste0('../1.Data/',AYfname))
  comp <- comp[comp$year==year,]
  if (aColLow!='null') {
    comp <- unique(comp[,c(aColLow,sizeCol,compCol)])
    comp <- comp[complete.cases(comp),]
    comp <- merge(comp,unique(read.csv(paste0("../1.Data/",Afname))[,c(aColLow,acol,aNameCol)]),by=aColLow)
    
    comp$totalSize <- ave(comp[,sizeCol], comp[,acol], FUN = sum)
    comp$comp      <- comp[,compCol]*comp[,sizeCol]/comp$totalSize
    comp$comp      <- ave(comp$comp, comp[,acol], FUN = sum)
  } else {
    if (sizeCol!='null') {
      comp <- unique(comp[,c(acol,sizeCol,compCol)])
    } else {
      comp <- unique(comp[,c(acol,compCol)])
    }
    comp <- comp[complete.cases(comp),]
    comp <- merge(comp,unique(read.csv(paste0("../1.Data/",Afname))[,c(acol,aNameCol)]),by=acol)
    comp$comp <- comp[,compCol]
  }
  comp <- unique(comp[,c(acol,aNameCol,'comp')])
  return(comp)
}