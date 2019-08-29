remove(list = ls())
library(plotrix)
library(ivpack)
library(stargazer)
options(stringsAsFactors = FALSE)
source("../2.Functions/loadData.R")
source("../2.Functions/figScatter.R")
source("../2.Functions/scaling.R")
if (!('EconGeo' %in% installed.packages()[,"Package"])) {
  library('devtools')
  install_github("PABalland/EconGeo")
}
library(EconGeo)

loadTableWrapper <- function(actType,delta,aggregate,compCol=NA) {
  loadUSParams(actType,aggregate)
  comp <- loadUSComplexity(actType,aggregate,exclude=TRUE,compCol=compCol)
  comp <- comp[!is.na(comp$comp),]
  for (i in unique(comp[duplicated((comp[,acol])),acol])) {
    nnn <- comp[comp[,acol]==i,aNameCol][1]
    ccc <- comp[comp[,acol]==i,'comp'][1]
    comp <- comp[comp[,acol]!=i,]
    row <- c(i,nnn,ccc)
    comp <- rbind(comp, row)
  }
  comp$comp <- as.numeric(comp$comp)
  
  data   <- loadUSActivity(delta,actType,aggregate)
  data <- data[complete.cases(data),]
  data <- unique(data[,c(acol,'CBSA','Ec.Output')])
  region <- loadUSRegs(useDec,year)
  land <- read.csv('../1.Data/US_Reg.csv')[,c("CBSA",'Land.sqr.m')]
  region <- merge(region,land,by='CBSA')
  
  ivDec <- 1900
  df = read.csv("../1.Data/US_RegDec.csv")[,c('CBSA','dec','newpop')]
  df <- merge(df,read.csv(paste0("../1.Data/",Rfname))[,c('CBSA','Flag.CBSA.availability')],by='CBSA')
  df <- df[df$Flag.CBSA.availability==1,]
  df <- df[df$dec==ivDec,c('CBSA','newpop')]
  colnames(df) = c('CBSA','pop.iv')
  region <- merge(region,df,by='CBSA',all.x=TRUE)
  
  data <- data[complete.cases(data),]
  
  df <- merge(data,comp[,c(acol,'comp')],by=acol)
  df <- merge(df,region[,c('CBSA','pop','pop.iv','Land.sqr.m')],by='CBSA')
  cCount <- merge(region,data,by='CBSA')
  cCount <- cCount[cCount$Ec.Output>delta,]
  cCount$total.Out <- ave(cCount[,'Ec.Output'], cCount[,acol], FUN = sum)
  cCount$city.count <- ave(cCount[,'CBSA'], cCount[,acol], FUN = length)
  cCount <- unique(cCount[,c(acol,'total.Out','city.count')])
  cCount <- cCount[cCount$city.count>=200,]
  df <- merge(df,cCount,by=acol)
  df$fe <- as.factor(df[,acol])
  df$comp_norm <- (df$comp - mean(df$comp)) / sd(df$comp)
  df$comp_norm.pop <- df$comp_norm*log(df$pop)
  df$comp.pop <- df$comp_norm*log(df$pop)
  
  df$comp_norm.popiv <- df$comp_norm*log(df$pop.iv)
  return(df)
}

getBetaSD <- function(df){
  m <- lm(log(Ec.Output) ~ log(Land.sqr.m)+fe*log(pop), data=df)
  vals <- c()
  coeffs <- summary(m)$coefficients[,1]
  for (i in unique(df$fe)) {
    c <- coeffs[paste0('fe',i,':log(pop)')]
    if (is.na(c)){c=0}
    vals <- c(vals,c+coeffs['log(pop)'])
  }
  return(sd(vals))
}

getF <- function(m1,m2) {
  t = anova(m1,m2)
  out <- round(t[2,'F'],3)
  if (t[2,'Pr(>F)']<=0.01){
    out <- paste0(out,'^{***}')
  } else if (t[2,'Pr(>F)']<=0.05){
    out <- paste0(out,'^{**}')
  } else if (t[2,'Pr(>F)']<=0.1){
    out <- paste0(out,'^{*}')
  }
  return(out)  
}

getPearson <- function(actType,df) {
  betaSD <- getBetaSD(dfT)
  m <- lm(log(Ec.Output) ~ factor(fe)+comp_norm*log(pop), data=df)
  coef <- summary(m)$coefficients[,1]['comp_norm:log(pop)']
  r <- coef/betaSD
  return(r)
}

delta <- 1
dfT <- loadTableWrapper('techs',delta,TRUE)
dfF <- loadTableWrapper('field',delta,TRUE)
dfI <- loadTableWrapper('ind',delta,TRUE)
dfO <- loadTableWrapper('occ',delta,TRUE)

m0t <- lm(log(Ec.Output) ~ factor(fe)+log(Land.sqr.m)+log(pop), data=dfT)
m1t <- lm(log(Ec.Output) ~ factor(fe)+log(Land.sqr.m)+log(pop)+comp_norm.pop, data=dfT)
m2tb<- lm(log(Ec.Output) ~ factor(fe)+factor(CBSA), data=dfT)
m2t <- lm(log(Ec.Output) ~ factor(fe)+factor(CBSA)+comp_norm.pop, data=dfT)
ivt <- ivreg(log(Ec.Output) ~ factor(fe)+factor(CBSA)+comp_norm.pop|factor(fe)+factor(CBSA)+comp_norm.popiv,data=dfT)

m0f <- lm(log(Ec.Output) ~ factor(fe)+log(Land.sqr.m)+log(pop), data=dfF)
m1f <- lm(log(Ec.Output) ~ factor(fe)+log(Land.sqr.m)+log(pop)+comp_norm.pop, data=dfF)
m2fb<- lm(log(Ec.Output) ~ factor(fe)+factor(CBSA), data=dfF)
m2f <- lm(log(Ec.Output) ~ factor(fe)+factor(CBSA)+comp_norm.pop, data=dfF)
ivf <- ivreg(log(Ec.Output) ~ factor(fe)+factor(CBSA)+comp_norm.pop|factor(fe)+factor(CBSA)+comp_norm.popiv,data=dfF)

m0i <- lm(log(Ec.Output) ~ factor(fe)+log(Land.sqr.m)+log(pop), data=dfI)
m1i <- lm(log(Ec.Output) ~ factor(fe)+log(Land.sqr.m)+log(pop)+comp_norm.pop, data=dfI)
m2ib<- lm(log(Ec.Output) ~ factor(fe)+factor(CBSA), data=dfI)
m2i <- lm(log(Ec.Output) ~ factor(fe)+factor(CBSA)+comp_norm.pop, data=dfI)
ivi <- ivreg(log(Ec.Output) ~ factor(fe)+factor(CBSA)+comp_norm.pop|factor(fe)+factor(CBSA)+comp_norm.popiv,data=dfI)

m0o <- lm(log(Ec.Output) ~ factor(fe)+log(Land.sqr.m)+log(pop), data=dfO)
m1o <- lm(log(Ec.Output) ~ factor(fe)+log(Land.sqr.m)+log(pop)+comp_norm.pop, data=dfO)
m2ob<- lm(log(Ec.Output) ~ factor(fe)+factor(CBSA), data=dfO)
m2o <- lm(log(Ec.Output) ~ factor(fe)+factor(CBSA)+comp_norm.pop, data=dfO)
ivo <- ivreg(log(Ec.Output) ~ factor(fe)+factor(CBSA)+comp_norm.pop|factor(fe)+factor(CBSA)+comp_norm.popiv,data=dfO)

# r <- c('Pearson correlation',
#        '',round(getPearson('techs',dfT),3),'',
#        '',round(getPearson('field',dfF),3),'',
#        '',round(getPearson('ind',dfI),3),'',
#        '',round(getPearson('occ',dfO),3),'')

fe <- c('Activity f.e.','yes','yes','yes','yes','yes','yes','yes','yes','yes','yes','yes','yes','yes','yes','yes','yes')
rfe <- c('MSA f.e.','no','no','yes','yes','no','no','yes','yes','no','no','yes','yes','no','no','yes','yes')
Ft <- c('F for comp:pop',
        '',
        getF(m0t,m1t),
        getF(m2tb,m2t),'',
        '',
        getF(m0f,m1f),
        getF(m2fb,m2f),'',
        '',
        getF(m0i,m1i),
        getF(m2ib,m2i),'',
        '',
        getF(m0o,m1o),
        getF(m2ob,m2o),'')

stargazer(m0t,m1t,m2t,ivt,m0f,m1f,m2f,ivf,m0i,m1i,m2i,ivi,m0o,m1o,m2o,ivo,add.lines=list(fe,rfe,Ft),out='../4.Results/regression_IV',omit=c('fe','CBSA'),column.labels = c("pat","pat",'pat','pat',"pub","pub",'pub','pub',"ind","ind",'ind','ind',"occ","occ",'occ','occ'))



###########################################################################################

dfT <- loadTableWrapper('techs',delta,FALSE)
dfF <- loadTableWrapper('field',delta,FALSE)
dfO <- loadTableWrapper('occ',delta,FALSE)

m0t <- lm(log(Ec.Output) ~ factor(fe)+log(Land.sqr.m)+log(pop), data=dfT)
m1t <- lm(log(Ec.Output) ~ factor(fe)+log(Land.sqr.m)+log(pop)+comp_norm.pop, data=dfT)
m2tb<- lm(log(Ec.Output) ~ factor(fe)+factor(CBSA), data=dfT)
m2t <- lm(log(Ec.Output) ~ factor(fe)+factor(CBSA)+comp_norm.pop, data=dfT)

m0f <- lm(log(Ec.Output) ~ factor(fe)+log(Land.sqr.m)+log(pop), data=dfF)
m1f <- lm(log(Ec.Output) ~ factor(fe)+log(Land.sqr.m)+log(pop)+comp_norm.pop, data=dfF)
m2fb<- lm(log(Ec.Output) ~ factor(fe)+factor(CBSA), data=dfF)
m2f <- lm(log(Ec.Output) ~ factor(fe)+factor(CBSA)+comp_norm.pop, data=dfF)

m0o <- lm(log(Ec.Output) ~ factor(fe)+log(Land.sqr.m)+log(pop), data=dfO)
m1o <- lm(log(Ec.Output) ~ factor(fe)+log(Land.sqr.m)+log(pop)+comp_norm.pop, data=dfO)
m2ob<- lm(log(Ec.Output) ~ factor(fe)+factor(CBSA), data=dfO)
m2o <- lm(log(Ec.Output) ~ factor(fe)+factor(CBSA)+comp_norm.pop, data=dfO)

fe <- c('Activity f.e.','yes','yes','yes','yes','yes','yes','yes','yes','yes')
rfe <- c('MSA f.e.','no','no','yes','no','no','yes','no','no','yes')
Ft <- c('F for comp:pop',
        '',
        getF(m0t,m1t),
        getF(m2tb,m2t),
        '',
        getF(m0f,m1f),
        getF(m2fb,m2f),
        '',
        getF(m0o,m1o),
        getF(m2ob,m2o))

stargazer(m0t,m1t,m2t,m0f,m1f,m2f,m0o,m1o,m2o,add.lines=list(fe,rfe,Ft),out='../4.Results/regressionLower.html',omit=c('fe','CBSA'),column.labels = c("pat","pat",'pat',"pub","pub",'pub',"occ","occ",'occ'))

###########################################################################################



loadUSParams('techs',FALSE)
comp1 <- loadUSComplexity('techs',FALSE,exclude=TRUE,compCol='date.1st.sub.cat.dec')
comp2 <- loadUSComplexity('techs',FALSE,exclude=TRUE,compCol='NK')
comp2 <- comp2[,c(acol,'comp')]
colnames(comp2) <- c(acol,'NK')
comp <- merge(comp1,comp2)

cor(comp[,c('comp','NK')])
dev.off()
plot(comp$NK,comp$comp,xlab = 'NK',ylab = 'Age')
abline(lm(comp ~NK,data=comp),col="red")

delta <- 1
dfT1 <- loadTableWrapper('techs',delta,FALSE,compCol='date.1st.sub.cat.dec')
dfT2 <- loadTableWrapper('techs',delta,FALSE,compCol='NK')
dfT2 <- dfT2[,c(acol,'CBSA','comp_norm.pop')]
colnames(dfT2) <- c(acol,'CBSA','NK_norm.pop')
dfT <- merge(dfT1,dfT2)

m0t <- lm(log(Ec.Output) ~ factor(fe)+log(Land.sqr.m)+log(pop), data=dfT)
m1t <- lm(log(Ec.Output) ~ factor(fe)+log(Land.sqr.m)+log(pop)+comp_norm.pop, data=dfT)
m2t <- lm(log(Ec.Output) ~ factor(fe)+log(Land.sqr.m)+log(pop)+NK_norm.pop, data=dfT)
m3t <- lm(log(Ec.Output) ~ factor(fe)+log(Land.sqr.m)+log(pop)+comp_norm.pop+NK_norm.pop, data=dfT)
m4tb<- lm(log(Ec.Output) ~ factor(fe)+factor(CBSA), data=dfT)
m4t <- lm(log(Ec.Output) ~ factor(fe)+factor(CBSA)+comp_norm.pop+NK_norm.pop, data=dfT)

fe <- c('Activity f.e.','yes','yes','yes','yes','yes')
rfe <- c('MSA f.e.','no','no','no','no','yes')

stargazer(m0t,m1t,m2t,m3t,m4t,add.lines=list(fe,rfe),out='../4.Results/regression_July23.html',omit=c('fe','CBSA'))

