library(plotrix)
options(stringsAsFactors = FALSE)
source("../2.Functions/loadData.R")
source("../2.Functions/figScatter.R")
source("../2.Functions/scaling.R")
if (!('EconGeo' %in% installed.packages()[,"Package"])) {
  library('devtools')
  install_github("PABalland/EconGeo")
}
library(EconGeo)
figure1Wrapper <- function(actType,aggregate,delta,reverseX) {
  loadUSParams(actType,aggregate)
  
  comp   <- loadUSComplexity(actType,aggregate)
  comp <- comp[!is.na(comp$comp),]
  
  data   <- loadUSActivity(delta,actType,aggregate)
  data <- data[complete.cases(data),]
  region <- loadUSRegs(useDec,year)
  
  beta <- scaling(get.matrix(data[,c(rcol,acol,'Ec.Output')]), region[,c(rcol,'pop')])
  colnames(beta) <- c(acol,'Beta','r.sq','std.err')
  beta <- beta[!is.na(beta$Beta),]
  
  dirName <- 'SM'
  pub <- merge(beta, comp, by = acol) 
  fig2Scatter(pub,'comp','Beta',acol,paste0('Complexity measure for ',acol),dirName=dirName,reverseX=reverseX)
}

aggregate <- FALSE
delta <- 0.1

actType <- 'ind'
reverseX <- FALSE
figure1Wrapper(actType,aggregate,delta,reverseX)

actType <- 'occ'
reverseX <- FALSE
figure1Wrapper(actType,aggregate,delta,reverseX)

actType <- 'techs'
reverseX <- TRUE
figure1Wrapper(actType,aggregate,delta,reverseX)

actType <- 'field'
reverseX <- FALSE
figure1Wrapper(actType,aggregate,delta,reverseX)

