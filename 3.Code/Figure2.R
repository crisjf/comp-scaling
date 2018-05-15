remove(list = ls())
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

actType <- 'ind'
aggregate <- TRUE
delta <- 0.1
reverseX<- FALSE

loadUSParams(actType,aggregate)

comp   <- loadUSComplexity(actType,aggregate)
comp <- comp[!is.na(comp$comp),]

data   <- loadUSActivity(delta,actType,aggregate)
region <- loadUSRegs(useDec,year)

data <- data[complete.cases(data),]
beta <- scaling(get.matrix(data[,c(rcol,acol,'Ec.Output')]), region[,c(rcol,'pop')],th=150,delta=delta)
colnames(beta) <- c(acol,'Beta','r.sq','std.err')
beta <- beta[!is.na(beta$Beta),]


#==============#
# 3 - FIGURE 2 #
#==============#

pub <- merge(beta, comp, by = acol)
dirName <- ''
fig2Scatter(pub,'comp','Beta',acol,paste0('Complexity measure for ',acol),dirName='',reverseX=reverseX)

