remove(list = ls())
library(plotrix)
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(extrafont)
font_import("Avenir Next Condensed",prompt=FALSE)
options(stringsAsFactors = FALSE)
source("../2.Functions/loadData.R")
source("../2.Functions/figScatter.R")
source("../2.Functions/crunching.R")

if (!('EconGeo' %in% installed.packages()[,"Package"])) {
  library('devtools')
  install_github("PABalland/EconGeo")
}
library(EconGeo)

actType <- 'occ'
aggregate <- TRUE
delta <- 1
level <- 2

loadBRAParams(actType,aggregate,level=level)

comp <- loadBRAComplexity(actType,aggregate,level=level)
comp <- comp[!is.na(comp$comp),]

for (altComp in alternativeComp) {
  compAlt <- loadBRAComplexity(actType,aggregate,compCol = altComp,level=level)
  compAlt <- compAlt[!is.na(compAlt$comp),]
  colnames(compAlt) <- c(acol,aNameCol,altComp)
  comp <- merge(comp,compAlt)
}


data   <- loadBRActivity(delta,actType,aggregate,level=level)
region <- loadBRARegs(year)

data <- data[complete.cases(data),]
beta <- scaling(get.matrix(data[,c(rcol,acol,'Ec.Output')]), region[,c(rcol,'pop')],th=50,delta=delta)
colnames(beta) <- c(acol,'Beta','r.sq','std.err')
beta <- beta[!is.na(beta$Beta),]

pub <- merge(beta, comp, by = acol)

df <- merge(region,data,by=rcol)
df <- df[df$Ec.Output>delta,]
df$total.Out <- ave(df[,'Ec.Output'], df[,acol], FUN = sum)
df$city.count <- ave(df[,rcol], df[,acol], FUN = length)
df <- unique(df[,c(acol,'total.Out','city.count')])
df <- df[df$city.count>=100,]
pub <- merge(pub,df,by=acol)


ptitle <- actName

pxaxis <- compName
pyaxis <- 'Scaling exponent (β)'
fig2ScatterGG(pub,'comp','Beta',ptitle,pxaxis,pyaxis,actType)


for (i in 1:length(alternativeComp)) {
  pxaxis <- compName
  pyaxis <- alternativeCompNames[i]
  fig2ScatterGG(pub,'comp',alternativeComp[i],ptitle,pxaxis,pyaxis,actType,grayscale=TRUE)

  pxaxis <- alternativeCompNames[i]
  pyaxis <- 'Scaling exponent (β)'
  fig2ScatterGG(pub,alternativeComp[i],'Beta',ptitle,pxaxis,pyaxis,actType)
}





