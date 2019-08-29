remove(list = ls())
library(plotrix)
options(stringsAsFactors = FALSE)
source("../2.Functions/figScatter.R")
source("../2.Functions/loadData.R")

actType <- 'techs'
aggregate <- FALSE
delta <- 1

#==========================#
# 0 - SELECT COLUMN NAMES  #
#==========================#

loadUSParams(actType,aggregate)

#loadBRAParams(actType)

#=======================================#
# 1 - LOAD DATA ON ECONOMIC ACTIVITIES  #
#=======================================#

economicActivity <- loadUSActivity(delta,actType,aggregate)

#economicActivity <- loadBRActivity(delta,actType)

#==========================#
# 2 - LOAD DATA ON CITIES  #
#==========================#

region <- loadUSRegs(useDec,year)

#region <- loadBRARegs()

#=========================#
# 2.5 - COUNT BY CATEGORY #
#=========================#

df <- merge(region,economicActivity,by='CBSA')
df$total.Out <- ave(df[,'Ec.Output'], df[,acol], FUN = sum)
df$city.count <- ave(df[,'CBSA'], df[,acol], FUN = length)
df <- unique(df[,c(acol,aNameCol,'total.Out','city.count')])

#=======================#
# 3 - FIGURE 1 - PART B #
#=======================#

figB <- merge(region,economicActivity,by=rcol)
figB$Agg.Ec.Output = ave(figB$Ec.Output, figB[,rcol], FUN = sum)
figB <- unique(figB[,c(rcol,'pop',rNameCol,'Agg.Ec.Output')])
fig1Scatter(figB,'pop','Agg.Ec.Output',acol,acol,dirName)

fig1Scatter(region,'pop','gdp.2015',acol,acol,'US_Ind')
fig1Scatter(region,'pop','emp.2015',acol,acol,'US_Occ')

#=======================#
# 3 - FIGURE 1 - PART C #
#=======================#

figC <- merge(region,economicActivity,by=rcol)

for (i in unique(figC[,acol])){
  figC_activity <- figC[figC[,acol]==i,]
  actName <- unique(figC_activity[,aNameCol])
  actName <- gsub("/", "-", actName)
  print(c(i,actName))
  fig1Scatter(figC_activity,'pop','Ec.Output',actName,acol,dirName)
}



#=====================#
# 3 - FIGURE 1 - GRID #
#=====================#

library(ggplot2)

figC <- merge(region,economicActivity,by=rcol)

xcol = 'pop'
ycol = 'Ec.Output'

categories <-unique(figC[,acol]) 

newcats <- c()
for (i in categories) {
  df <- figC[figC[,acol]==i,]
  if ((nrow(df[df$Ec.Output>delta,]))>200) {
    newcats <- c(newcats,i)
  }
}
categories <- newcats

myplots <- list()  # new empty list
for (k in 1:length(categories)){
  i <- categories[k]
  figC_activity <- figC[figC[,acol]==i,]
  actName <- unique(figC_activity[,aNameCol])
  actName <- gsub("/", "-", actName)
  df <- figC_activity
  if (nrow(df)>=3) {
      df$logx = log10(df[,xcol])
      df$logy = log10(df[,ycol])
      m <- summary(lm(logy~logx,data=df))
      beta <- round(m$coefficients['logx',1],2)
      
      p <- ggplot(df,aes(x=logx, y=logy)) + geom_point() + labs(x = "",y="") + geom_smooth(method = "lm", se = FALSE) 
      p <- p+ggtitle(paste0(trimws(actName),' (','β','=',beta,')'))
    
      p <- p+theme(axis.text=element_text(size=14),axis.title=element_text(size=18,face="bold"))
      ycticks <- ggplot_build(p)$layout$panel_ranges[[1]]$y.major_source
      yticks <- c(expression('10'^0),expression('10'^1),expression('10'^2),expression('10'^3),expression('10'^4),expression('10'^5),expression('10'^6),expression('10'^7),expression('10'^8),expression('10'^9),expression('10'^10))
      ylabels <- c()
      ypos <- c()
      for (i in ycticks){
        if (i==as.integer(i)){
          ypos <- c(ypos,i)
          ylabels <-c(ylabels,yticks[i+1])
        }
      }
      p <- p + scale_x_continuous('', breaks=c(5,6,7), labels=c(expression('10'^5),expression('10'^6),expression('10'^7)))
      p <- p + scale_y_continuous('', breaks=ypos, labels=ylabels)
      p <- eval(substitute(p,list(i = k)))+theme_grey(base_size = 18) 
      myplots[[k]] <- p
      ggsave(paste0('../4.Results/Figure1/',dirName,'/SM/',actType,k,".pdf"), device=cairo_pdf,width = 8, height = 5)
  }
}


