library(plotrix)
options(stringsAsFactors = FALSE)
source("../2.Functions/fig1Scatter.R")
source("../2.Functions/loadData.R")

#==========================#
# 0 - SELECT COLUMN NAMES  #
#==========================#

rcol     <- 'CBSA'
acol     <- 'ASJC.2D'
popCol   <- 'pop.2010'
rNameCol <- 'CBSA.Name'
aNameCol <- 'ASJC.2D.Name'

rcol     <- 'rcode'
acol     <- 'icode2'
popCol   <- 'pop'
rNameCol <- 'rname'
aNameCol <- 'iname2'

#=======================================#
# 1 - LOAD DATA ON ECONOMIC ACTIVITIES  #
#=======================================#

delta <- 0.1 #How much to offset economic output by (to deal with entries with zeros)
economicActivity <- loadFields(delta)
economicActivity <- loadBRAInds(delta)
economicActivity <- loadBRAOccs(delta)

#==========================#
# 2 - LOAD DATA ON CITIES  #
#==========================#

region <- loadRegs()
region <- loadBRARegs()

#=======================#
# 3 - FIGURE 1 - PART B #
#=======================#

figB <- merge(region,economicActivity,by=rcol)
figB$Agg.Ec.Output = ave(figB$Ec.Output, figB[,rcol], FUN = sum)
figB <- unique(figB[,c(rcol,popCol,rNameCol,'Agg.Ec.Output')])

fig1Scatter(figB,popCol,'Agg.Ec.Output',acol,acol)

#=======================#
# 3 - FIGURE 1 - PART C #
#=======================#

figC <- merge(region,economicActivity,by=rcol)

for (i in unique(figC[,acol])){
  figC_activity <- figC[figC[,acol]==i,]
  actName <- unique(figC_activity[,aNameCol])
  fig1Scatter(figC_activity,popCol,'Ec.Output',actName,acol)
 }

