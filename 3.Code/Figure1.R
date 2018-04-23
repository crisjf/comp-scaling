library(plotrix)
options(stringsAsFactors = FALSE)
source("../2.Functions/fig1Scatter.R")

#=======================================#
# 1 - LOAD DATA ON ECONOMIC ACTIVITIES  #
#=======================================#

NbPapersCol     <- 'Nb.Papers.96.08' #Column to use as ammount of output
delta           <- 0.1 #How much to add to entries with zeros
activityCol     <- 'ASJC.2D' #Column to aggregate the economic output by
activityNameCol <- 'ASJC.2D.Name' #Column to use as name in merged file

economicActivity    <- read.csv("../1.Data/US_RegFieldYr.csv")
economicActivity    <- economicActivity[economicActivity['Year']==2010,]
economicActivity    <- economicActivity[,c('CBSA','ASJC.4D',NbPapersCol)]
economicActivity    <- merge(economicActivity,read.csv("../1.Data/US_Field.csv")[,c('ASJC.4D',activityCol,activityNameCol)],by='ASJC.4D')

economicActivity$ID <- paste(economicActivity$CBSA,economicActivity[,activityCol])
economicActivity$Ec.Output = ave(economicActivity[,NbPapersCol], economicActivity$ID, FUN = sum)
economicActivity$Ec.Output <- economicActivity$Ec.Output+delta

economicActivity <- unique(economicActivity[,c(activityCol,'CBSA',activityNameCol,'Ec.Output')])

#==========================#
# 2 - LOAD DATA ON CITIES  #
#==========================#

popCol <- 'pop.2010' #Column to use as population data

region <- read.csv("../1.Data/US_RegYr.csv")[,c('CBSA',popCol)]
region <- merge(region, read.csv("../1.Data/US_Reg.csv")[,c('CBSA','CBSA.Name')], by='CBSA')

#=======================#
# 3 - FIGURE 1 - PART B #
#=======================#

figB <- merge(region,economicActivity,by='CBSA')
figB$Agg.Ec.Output = ave(figB$Ec.Output, figB$CBSA, FUN = sum)
figB <- unique(figB[,c('CBSA',popCol,'CBSA.Name','Agg.Ec.Output')])

fig1Scatter(figB,'pop.2010','Agg.Ec.Output',activityCol,activityCol)

#=======================#
# 3 - FIGURE 1 - PART C #
#=======================#

figC <- merge(region,economicActivity,by='CBSA')

for (i in unique(figC[,activityCol])){
  figC_activity <- figC[figC[,activityCol]==i,]
  actName <- unique(figC_activity[,activityNameCol])
  fig1Scatter(figC_activity,'pop.2010','Ec.Output',actName,activityCol)
 }

