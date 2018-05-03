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

actType <- 'techs'
aggregate <- TRUE
delta <- 0.1
loadUSParams(actType,aggregate)

comp   <- loadUSComplexity(actType,aggregate)
comp <- comp[!is.na(comp$comp),]

data   <- loadUSActivity(delta,actType,aggregate)
region <- loadUSRegs(useDec,year)

beta <- scaling(get.matrix(data[,c(rcol,acol,'Ec.Output')]), region[,c(rcol,'pop')])
colnames(beta) <- c(acol,'Beta','r.sq','std.err')
beta <- beta[!is.na(beta$Beta),]

#==============#
# 3 - FIGURE 1 #
#==============#

pub <- merge(beta, comp, by = acol) 
fig2Scatter(pub,'comp','Beta',acol,paste0('Complexity measure for ',acol),dirName=dirName)

