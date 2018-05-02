library(plotrix)
options(stringsAsFactors = FALSE)
source("../2.Functions/figScatter.R")
source("../2.Functions/loadData.R")

#US  choose from: field, techs, ind, occ
#BRA choose from: ind, occ
actType <- 'field'


#==========================#
# 0 - SELECT COLUMN NAMES  #
#==========================#

loadUSParams(actType)

loadBRAParams(actType)

#=======================================#
# 1 - LOAD DATA ON ECONOMIC ACTIVITIES  #
#=======================================#

delta <- 0 #How much to offset economic output by (to deal with entries with zeros)

economicActivity <- loadUSActivity(delta,actType)

economicActivity <- loadBRActivity(delta,actType)

#==========================#
# 2 - LOAD DATA ON CITIES  #
#==========================#

region <- loadUSRegs(useDec,year)

region <- loadBRARegs()

#=======================#
# 3 - FIGURE 1 - PART B #
#=======================#

figB <- merge(region,economicActivity,by=rcol)
figB$Agg.Ec.Output = ave(figB$Ec.Output, figB[,rcol], FUN = sum)
figB <- unique(figB[,c(rcol,'pop',rNameCol,'Agg.Ec.Output')])
fig1Scatter(figB,'pop','Agg.Ec.Output',acol,acol,dirName)

# fig1Scatter(region,'pop','gdp.2015',acol,acol,'US_Ind')
# fig1Scatter(region,'pop','emp.2015',acol,acol,'US_Occ')

#=======================#
# 3 - FIGURE 1 - PART C #
#=======================#

figC <- merge(region,economicActivity,by=rcol)

for (i in unique(figC[,acol])){
  figC_activity <- figC[figC[,acol]==i,]
  actName <- unique(figC_activity[,aNameCol])
  fig1Scatter(figC_activity,'pop','Ec.Output',actName,acol,dirName)
 }

