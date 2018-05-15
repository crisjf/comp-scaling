remove(list = ls())
library(plotrix)
options(stringsAsFactors = FALSE)
source("../2.Functions/figScatter.R")
source("../2.Functions/loadData.R")
figure1Wrapper <- function(actType,aggregate,delta) {
  loadUSParams(actType,aggregate)
  economicActivity <- loadUSActivity(delta,actType,aggregate)
  region <- loadUSRegs(useDec,year)
  
  if ((actType!='ind')&(actType!='occ')) {
    figB <- merge(region,economicActivity,by=rcol)
    figB$Agg.Ec.Output = ave(figB$Ec.Output, figB[,rcol], FUN = sum)
    figB <- unique(figB[,c(rcol,'pop',rNameCol,'Agg.Ec.Output')])
    fig1Scatter(figB,'pop','Agg.Ec.Output',acol,acol,dirName)
  } else if (actType=='ind') {
    fig1Scatter(region,'pop','gdp.2015',acol,acol,dirName)
  } else if (actType=='occ') {
    fig1Scatter(region,'pop','emp.2015',acol,acol,dirName)
  }
  
  figC <- merge(region,economicActivity,by=rcol)
  for (i in unique(figC[,acol])){
    figC_activity <- figC[figC[,acol]==i,]
    actName <- unique(figC_activity[,aNameCol])
    actName <- gsub("/", "-", actName)
    fig1Scatter(figC_activity,'pop','Ec.Output',actName,acol,dirName)
  }
}
aggregate <- TRUE
delta <- 1

actType <- 'ind'
figure1Wrapper(actType,aggregate,delta)

actType <- 'occ'
figure1Wrapper(actType,aggregate,delta)

actType <- 'techs'
figure1Wrapper(actType,aggregate,delta)

actType <- 'field'
figure1Wrapper(actType,aggregate,delta)

print('END')