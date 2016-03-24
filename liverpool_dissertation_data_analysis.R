sink("output-file.txt")
# ==================================================================
# Name: Liverpool Dissertation Data Analysis 
# Author: Vitor Fabian Brock
# Date Created: 13/08/2014
# Updated: 10/10/2014
# Version: 1.2
#

# ==================================================================
# Setting Enviroment 
# 

# Installing and loading R package dependency

# Car package (Companion to Applied Regression)
# http://cran.r-project.org/web/packages/car/car.pdf
# install.packages("car")
library(car)

# Plyr package 
# http://cran.r-project.org/web/pakages/plyr/
# install.packages("plyr")
library(plyr)

# Sem package
# http://cran.r-project.org/web/packages/sem/
# install.packages("sem")
library(sem)

# Qgraph package 
# http://cran.r-project.org/web/packages/qgraph/
# install.packages("qgraph")
library(qgraph)

# QuantPsyc package 
# http://cran.r-project.org/web/packages/quantpsyc/
# install.packages("QuantPsyc")
library(QuantPsyc)

# Mediation package
# http://cran.r-project.org/web/packages/mediation/
# install.packages("mediation")
library(mediation)

# Setting working directory
setwd("z:/Dropbox/University of Liverpool/14.04 Dissertation/data analysis/")

# Loading statistical functions
source("data-analysis-functions.r")

# Loading dataset
survey = read.csv("dissertation-survey-dataset-20140930.csv")

# Attaching survey dataset and declaring variables
attach(survey)
df = data.frame(
  # Loading control variables
  contFte=Approximately..how.many.full.time.employees.does.your.company.employ.,
  contIte=How.many.full.time.employees.work.in.the.Information.Technology.and.or.Data.Analysis.department.,
  contAge=How.old.are.you.,
  contExp1=How.many.years.have.you.been.working.in.this.position.,
  contExp2=How.many.years.have.you.been.working.for.this.company.,
  contIntr=Approximately..what.is.the.percentage.of.international.clients.and.or.operations.,
  contInvs=Approximately..in.the.last.couple.years..what.percentage.of.the.company.s.revenue.has.been.allocated.to.new.hardware..software.and.or.IT.consulting.service.,
  contCour=Choice,
  contInds=Choice.1,
  
  # Variable for TAM Actual of Usage
  tamUse1=I.use.Big.Data.Analytics.in.my.company.very.intensively.to.support.my.work.,
  tamUse2=I.use.Big.Data.Analytics.in.my.company.very.frequently.to.support.my.work.,
  tamUse3=Big.Data.Analytics.significantly.impact.my.work.and.my.business.,
  tamUse4=I.would.not.be.able.to.perform.my.job.without.Big.Data.Analytics.,
  tamUse5=Overall..I.use.Big.Data.Analytics.a.lot.,
  
  # Variable for TAM Perceived Usefullness
  tamPu1=Using.Big.Data.Analytics.in.my.job.would.enable.me.to.accomplish.tasks.more.efficiently.,
  tamPu2=Using.Big.Data.Analytics.would.improve.my.job.performance.,
  tamPu3=Using.Big.Data.Analytics.in.my.job.would.increase.my.productivity.,
  tamPu4=Using.Big.Data.Analytics.in.my.job.would.enhance.my.effectiveness.,
  tamPu5=Using.Big.Data.Analytics.would.make.my.job.easier.to.do.,
  tamPu6=I.would.find.Dig.Data.Analytics.useful.in.my.job.,

  # Variable for TAM Perceived Ease of Use
  tamPeou1=Learning.to.use.Big.Data.Analytics.would.be.easy.for.me.,
  tamPeou2=I.would.find.easy.to.get.Big.Data.Analytics.to.do.what.I.want.to.do.,
  tamPeou3=My.interaction.with.Big.Data.Analytics.would.be.clear.and.understandable., 
  tamPeou4=I.would.find.Big.Data.Analytics.flexible.to.interact.with.,
  tamPeou5=It.would.be.easy.for.me.to.become.skilful.at.using.Big.Data.Analytics.,
  tamPeou6=I.would.find.Big.Data.Analytics.easy.to.use.,
  
  # Variable for OLC Managerial Commitment (olcMc)
  olcMc1=In.this.company..employee.learning.is.considered.a.key.factor.,
  olcMc2=The.company.s.management.looks.favourably.on.carrying.out.changes.in.any.area.to.adapt.to.new.conditions.,
  olcMc3=In.this.company..innovative.ideas.that.work.are.rewarded.,
  olcMc4=In.this.company..employee.learning.is.considered.an.investment.,
    
  # Variable for OLC System Perpective (olcSp)
  olcSp1=All.trained.employees.have.generalized.knowledge.regarding.this.company.s.goals.and.objectives.,
  olcSp2=All.subunits.that.make.up.this.company..departments..sections..divisions.work.teams.and.individuals..are.well.aware.of.how.they.contribute.to.achieve.the.overall.goals.and.objectives.,
  olcSp3=All.parts.that.make.up.my.company.are.interconnected.working.together.in.a.coordinated.manner.,
  olcSp4=Everyone.clearly.understands.the.chain.of.command.and.processes.within.this.company.,
  
  # Variable for OLC Openness and Experimentation (olcOe)
  olcOe1=My.company.promotes.experimentation.and.innovative.ideas.as.a.way.of.improving.business.processes.,
  olcOe2=My.company.follows.up.on.the.activities.of.other.companies.within.the.sector.and.is.willing.to.adopt.those.practices.and.techniques.that.it.believes.to.be.useful.and.interesting.,
  olcOe3=Experience.and.ideas.provided.by.external.sources..clients..consulting.firms..etc...are.considered.important.for.this.company.learning.,
  olcOe4=The.culture.of.this.company.encourages.expression.and.opinions.as.well.as.suggestions.regarding.the.procedures.and.methods.,

  # Variable for OLC Transfer and Integration (olcTi)
  olcTi1=Errors.and.failures.are.always.discussed.and.analysed.in.this.company.at.all.levels.,
  olcTi2=In.this.company..there.are.processes.and.structures.that.offer.employees.the.chance.to.talk.about.new.ideas..programs.and.activities.that.might.be.useful.to.the.company.,
  olcTi3=This.company.encourages.collaboration..team.work.and.information.dissemination.,
  olcTi4=The.company.has.a.mechanism.that.allows.what.has.been.learnt.in.past.situation.to.remain.valid.and.accessible.to.employees.,
  
  n=0
)
detach(survey)
 
# Recoding variables and converting to numeric
df <- colwise(recode)(df,recodes="'Completely Disagree'=1")
df <- colwise(recode)(df,recodes="'Disagree'=2")
df <- colwise(recode)(df,recodes="'Somewhat Disagree'=3")
df <- colwise(recode)(df,recodes="'Neither Agree nor Disagree'=4")
df <- colwise(recode)(df,recodes="'Somewhat Agree'=5")
df <- colwise(recode)(df,recodes="'Agree'=6")
df <- colwise(recode)(df,recodes="'Completely Agree'=7")

df$tamUse1 = as.numeric(df$tamUse1)
df$tamUse2 = as.numeric(df$tamUse2)
df$tamUse3 = as.numeric(df$tamUse3)
df$tamUse4 = as.numeric(df$tamUse4)
df$tamUse5 = as.numeric(df$tamUse5)

df$tamPu1 = as.numeric(df$tamPu1)
df$tamPu2 = as.numeric(df$tamPu2)
df$tamPu3 = as.numeric(df$tamPu3)
df$tamPu4 = as.numeric(df$tamPu4)
df$tamPu5 = as.numeric(df$tamPu5)
df$tamPu6 = as.numeric(df$tamPu6)

df$tamPeou1 = as.numeric(df$tamPeou1)
df$tamPeou2 = as.numeric(df$tamPeou2)
df$tamPeou3 = as.numeric(df$tamPeou3)
df$tamPeou4 = as.numeric(df$tamPeou4)
df$tamPeou5 = as.numeric(df$tamPeou5)
df$tamPeou6 = as.numeric(df$tamPeou6)

df$olcMc1 = as.numeric(df$olcMc1)
df$olcMc2 = as.numeric(df$olcMc2)
df$olcMc3 = as.numeric(df$olcMc3)
df$olcMc4 = as.numeric(df$olcMc4)

df$olcSp1 = as.numeric(df$olcSp1)
df$olcSp2 = as.numeric(df$olcSp2)
df$olcSp3 = as.numeric(df$olcSp3)
df$olcSp4 = as.numeric(df$olcSp4)

df$olcOe1 = as.numeric(df$olcOe1)
df$olcOe2 = as.numeric(df$olcOe2)
df$olcOe3 = as.numeric(df$olcOe3)
df$olcOe4 = as.numeric(df$olcOe4)

df$olcTi1 = as.numeric(df$olcTi1)
df$olcTi2 = as.numeric(df$olcTi2)
df$olcTi3 = as.numeric(df$olcTi3)
df$olcTi4 = as.numeric(df$olcTi4)

# ==================================================================
# Starting Data Analysis
# 

# Testing psychometric proprieties of constructs (check Chapter 5)
cat("Testing psychometric proprieties of constructs (check Chapter 5)\n")

# Confirmatory factor analysis (CFA) for TAM (check Chapter 3.3)
# Original model from the literature
cat("Confirmatory factor analysis (CFA) for TAM (check Chapter 3.3)\n")
cat("Testing original model from the literaute\n")

tamCfa=cfa(reference.indicators=FALSE)
  tamUse: tamUse1, tamUse2, tamUse3, tamUse4, tamUse5
  tamPeou: tamPeou1, tamPeou2, tamPeou3, tamPeou4, tamPeou5, tamPeou6
  tamPu: tamPu1, tamPu2, tamPu3, tamPu4, tamPu5, tamPu6
#

tamSem=sem(tamCfa, data=df)

summary(tamSem, fit.indices=c("GFI", "AGFI", "RMSEA", "NFI", "NNFI", "CFI", "RNI",
                               "IFI", "SRMR", "AIC", "BIC"))
crave(tamSem)
qgraph(tamSem,label.prop=3)

# Alternative CFA for better fit indices
# Dropping variables tamUse4, tamPeou1, tamPeou6, tamPu1, tamPu5 
cat("Alternative CFA for better fit indices\n")
cat("Dropping variables tamUse4, tamPeou1, tamPeou6, tamPu1, tamPu5\n")

tamCfa=cfa(reference.indicators=FALSE)
  tamUse: tamUse1, tamUse2, tamUse3, tamUse5
  tamPeou: tamPeou2, tamPeou3, tamPeou4, tamPeou5
  tamPu: tamPu2, tamPu3, tamPu4, tamPu6
#

tamSem=sem(tamCfa, data=df)
summary(tamSem, fit.indices=c("GFI", "AGFI", "RMSEA", "NFI", "NNFI", "CFI", "RNI",
                               "IFI", "SRMR", "AIC", "BIC"))

crave(tamSem)
qgraph(tamSem,label.prop=3)

# Confirmatory factor analysis (CFA) for OLC (check Chapter 3.3)
# Original Model from the literature
olcCfa=cfa(reference.indicators=FALSE)
  olcMc: olcMc1, olcMc2, olcMc3, olcMc4
  olcSP: olcSp1, olcSp2, olcSp3, olcSp4
  olcOe: olcOe1, olcOe2, olcOe3, olcOe4
  olcTi: olcTi1, olcTi2, olcTi3, olcTi4
#

olcSem=sem(olcCfa, data=df)

summary(olcSem, fit.indices=c("GFI", "AGFI", "RMSEA", "NFI", "NNFI", "CFI", "RNI",
                                   "IFI", "SRMR", "AIC", "BIC"))

crave(olcSem)
qgraph(olcSem,label.prop=3)

# Alternative CFA for better fit indices
# Dropping variables olcMc1, olcMc4, olcSP1, olcOe2, olcOe3
olcCfa=cfa(reference.indicators=FALSE)
  olcMc: olcMc2, olcMc3
  olcSp: olcSp2, olcSp3, olcSp4
  olcOe: olcOe1, olcOe4
  olcTi: olcTi1, olcTi2, olcTi3, olcTi4
#

olcSem=sem(olcCfa, data=df)

summary(olcSem, fit.indices=c("GFI", "AGFI", "RMSEA", "NFI", "NNFI", "CFI", "RNI",
                                 "IFI", "SRMR", "AIC", "BIC"))

crave(olcSem)
qgraph(olcSem,label.prop=3)

# Building new dataframe with row means and control variables 
attach(df)
df_means <- data.frame(
  tamUse = rowMeans(df[,c("tamUse1", "tamUse2", "tamUse3", "tamUse5")]),
  tamPeou = rowMeans(df[,c("tamPeou2", "tamPeou3", "tamPeou4", "tamPeou5")]),
  tamPu = rowMeans(df[,c("tamPu2", "tamPu3", "tamPu4","tamPu6")]),
  olcMc = rowMeans(df[,c("olcMc2", "olcMc3")]),
  olcSp = rowMeans(df[,c("olcSp2", "olcSp3", "olcSp4")]),
  olcOe = rowMeans(df[,c("olcOe1", "olcOe4")]),
  olcTi = rowMeans(df[,c("olcTi1", "olcTi2", "olcTi3", "olcTi4")]),
  contSize = log(contFte),
  contSizeIt = log(contIte),
  contExp1, # log?
  contExp2, # log?
  contAge,
  contIntr,
  contInvs,
  contInds = factor(df$contInds),
  contCour = factor(df$contCour),
  n
)
detach(df)

attach(df_means)
summary(df_means)

# Exporting table of descriptives and correlations
#write.csv(descr.cor(df_means),"table_descr_cor.csv")

# Attaching additional factors to data frame 
# Note that descr.cor function does not work with factor
#df_means$contInds = factor(df$contInds)
#df_means$contCour = factor(df$contCour)

# Converting continious variables to dichotomous variable (check Chapter 4.2)
# See Kenny and Judd (1984); Sauer and Dick (1993); Ping (1996).

# Seting cut point above the median
olcMcDic = cut(olcMc, br=c(0,5,7), labels = c("0","1"))
olcSpDic = cut(olcSp, br=c(0,5,7), labels = c("0","1"))
olcOeDic = cut(olcOe, br=c(0,5,7), labels = c("0","1"))
olcTiDic = cut(olcTi, br=c(0,5,7), labels = c("0","1"))

olcMcDic = as.numeric(olcMcDic)
olcSpDic = as.numeric(olcSpDic)
olcOeDic = as.numeric(olcOeDic)
olcTiDic = as.numeric(olcTiDic)

# Converting to binary variable
olcMcDic <- ifelse(olcMcDic == 2, 1, 0)
olcSpDic <- ifelse(olcSpDic == 2, 1, 0)
olcOeDic <- ifelse(olcOeDic == 2, 1, 0)
olcTiDic <- ifelse(olcTiDic == 2, 1, 0)

# Testing Moderation Effect using Baron & Kenny's Tests (1986)

# Modelling moderation equation (see Chapter 6.2)
olsMod1 = lm(tamUse ~ contSize + contSizeIt + contExp1 + contExp2 + contInds 
             + contCour + contIntr + contInvs + contAge, data=df_means)
olsMod2 = update(olsMod1, . ~ . + tamPu + tamPeou)
olsMod3 = update(olsMod2, . ~ . + olcMcDic + olcSpDic + olcOeDic + olcTiDic)
olsMod4 = update(olsMod3, . ~ . + (tamPu + tamPeou) 
                 * (olcMcDic + olcSpDic + olcOeDic + olcTiDic))


olsMod3 = update(olsMod1, . ~ . + olcMc + olcSp + olcOe + olcTi)

moderModel = consolidate.output(
  list(
    olsMod1, olsMod2, olsMod3, olsMod4
  )
)
#

summary(moderModel)
View(moderModel)

qqPlot(olsMod4, main="Regression Residuals: OLC Model 4")

# Testing the regression model for multicollinearity
# Cut off value suggested by Kutner et al. (2004). 
sqrt(vif(olsMod4)) > 2 # problem?
vif(olsMod4) > 10 # ideally 5

## Fixing multicolliearity problems

# Centering the variables
df_means$tamUseCent = scale(df_means$tamUse, center = TRUE, scale = FALSE)
df_means$tamPuCent = scale(df_means$tamPu, center = TRUE, scale = FALSE)
df_means$tamPeouCent = scale(df_means$tamPeou, center = TRUE, scale = FALSE)
df_means$olcMcCent = scale(df_means$olcMc, center = TRUE, scale = FALSE)
df_means$olcSpCent = scale(df_means$olcSp, center = TRUE, scale = FALSE)
df_means$olcOeCent = scale(df_means$olcOe, center = TRUE, scale = FALSE)
df_means$olcTiCent = scale(df_means$olcTi, center = TRUE, scale = FALSE)

# Runing model with centred variables
olsMod1 = lm(tamUseCent ~ contSize + contSizeIt + contExp1 + contExp2 + contInds 
             + contCour + contIntr + contInvs + contAge, data=df_means)
olsMod2 = update(olsMod1, . ~ . + tamPuCent + tamPeouCent)
olsMod3 = update(olsMod2, . ~ . + olcMcDic + olcSpDic + olcOeDic + olcTiDic)
olsMod4 = update(olsMod3, . ~ . + (tamPuCent + tamPeouCent) 
                 * (olcMcDic + olcSpDic + olcOeDic + olcTiDic))

moderModel = consolidate.output(
  list(
    olsMod1, olsMod2, olsMod3, olsMod4
  )
)

View(moderModel)

# Testing for Multicolliearity
sqrt(vif(olsMod4)) > 2 # problem?
vif(olsMod4) > 5

write.csv(moderModel,"table_moderation.csv")


# Testing for Mediation Effect using Indirect Effect Test
# See Fairchild and MacKinnon's Test (2009)

# Equation with temPu
# Modelling test for olcMc mediation on tamPu
fitM1 = lm(olcMc ~ tamUse + tamPu + contSize + contSizeIt + contExp1 
          + contExp2 + contInds + contCour + contIntr + contInvs + contAge)
fitY1 = lm(tamUse ~ tamPu + olcMc + contSize + contSizeIt + contExp1 
          + contExp2 + contInds + contCour + contIntr + contInvs + contAge)

# Modelling test for olcSp mediation on tamPu
fitM2 = lm(olcSp ~ tamUse + tamPu + contSize + contSizeIt + contExp1 
           + contExp2 + contInds + contCour + contIntr + contInvs + contAge)
fitY2 = lm(tamUse ~ tamPu + olcSp + contSize + contSizeIt + contExp1 
           + contExp2 + contInds + contCour + contIntr + contInvs + contAge)

# Modelling test for olcOe mediation on tamPu
fitM3 = lm(olcOe ~ tamUse + tamPu + contSize + contSizeIt + contExp1 
           + contExp2 + contInds + contCour + contIntr + contInvs + contAge)
fitY3 = lm(tamUse ~ tamPu + olcOe + contSize + contSizeIt + contExp1 
           + contExp2 + contInds + contCour + contIntr + contInvs + contAge)

# Modelling test for olcTi mediation on tamPu
fitM4 = lm(olcTi ~ tamUse + tamPu + contSize + contSizeIt + contExp1 
           + contExp2 + contInds + contCour + contIntr + contInvs + contAge)
fitY4 = lm(tamUse ~ tamPu + olcTi + contSize + contSizeIt + contExp1 
           + contExp2 + contInds + contCour + contIntr + contInvs + contAge)

# Equation with temPeou
# Modelling test for olcMc mediation on tamPeou
fitM5 = lm(olcMc ~ tamUse + tamPeou + contSize + contSizeIt + contExp1 
           + contExp2 + contInds + contCour + contIntr + contInvs + contAge)
fitY5 = lm(tamUse ~ tamPeou + olcMc + contSize + contSizeIt + contExp1 
           + contExp2 + contInds + contCour + contIntr + contInvs + contAge)

# Modelling test for olcSp mediation on tamPeou
fitM6 = lm(olcSp ~ tamUse + tamPeou + contSize + contSizeIt + contExp1 
           + contExp2 + contInds + contCour + contIntr + contInvs + contAge)
fitY6 = lm(tamUse ~ tamPeou + olcSp + contSize + contSizeIt + contExp1 
           + contExp2 + contInds + contCour + contIntr + contInvs + contAge)

# Modelling test for olcOe mediation on tamPeou
fitM7 = lm(olcOe ~ tamUse + tamPeou + contSize + contSizeIt + contExp1 
           + contExp2 + contInds + contCour + contIntr + contInvs + contAge)
fitY7 = lm(tamUse ~ tamPeou + olcOe + contSize + contSizeIt + contExp1 
           + contExp2 + contInds + contCour + contIntr + contInvs + contAge)

# Modelling test for olcTi mediation on tamPeou
fitM8 = lm(olcTi ~ tamUse + tamPeou + contSize + contSizeIt + contExp1 
           + contExp2 + contInds + contCour + contIntr + contInvs + contAge)
fitY8 = lm(tamUse ~ tamPeou + olcTi + contSize + contSizeIt + contExp1 
           + contExp2 + contInds + contCour + contIntr + contInvs + contAge)

# Running quasi-Bayesian Monte Carlo simulation 
# RobustSE for White's heteroskedasticity-consistent estimator (White, 1980)
# Size is exclusive controled variable due to its high correlation factors
# WARNING: Monte Carlo simulations take about 5 minutes each!

# Moderation test for olcMc on tamPu 
fitMed1 <- mediate(fitM1, fitY1, sims=10000, treat="tamPu", mediator="olcMc", 
                  control="contSize", robustSE = TRUE, data=df_means)
summary(fitMed1)

# Moderation test for olcSp on tamPu 
fitMed2 <- mediate(fitM2, fitY2, sims=10000, treat="tamPu", mediator="olcSp", 
                   control="contSize", robustSE = TRUE, data=df_means)
summary(fitMed2)

# Moderation test for olcOe on tamPu 
fitMed3 <- mediate(fitM3, fitY3, sims=10000, treat="tamPu", mediator="olcOe", 
                   control="contSize", robustSE = TRUE, data=df_means)
summary(fitMed3)

# Moderation test for olcTi on tamPu 
fitMed4 <- mediate(fitM4, fitY4, sims=10000, treat="tamPu", mediator="olcTi", 
                   control="contSize", robustSE = TRUE, data=df_means)
summary(fitMed4)

# Moderation test for olcMc on tamPu 
fitMed5 <- mediate(fitM5, fitY5, sims=10000, treat="tamPeou", mediator="olcMc", 
                   control="contSize", robustSE = TRUE, data=df_means)
summary(fitMed5)

# Moderation test for olcSp on tamPu 
fitMed6 <- mediate(fitM6, fitY6, sims=10000, treat="tamPeou", mediator="olcSp", 
                   control="contSize", robustSE = TRUE, data=df_means)
summary(fitMed6)

# Moderation test for olcOe on tamPu 
fitMed7 <- mediate(fitM7, fitY7, sims=10000, treat="tamPeou", mediator="olcOe", 
                   control="contSize", robustSE = TRUE, data=df_means)
summary(fitMed7)

# Moderation test for olcTi on tamPu 
fitMed8 <- mediate(fitM8, fitY8, sims=10000, treat="tamPeou", mediator="olcTi", 
                   control="contSize", robustSE = TRUE, data=df_means)
summary(fitMed8)

# ==================================================================
# Saving Data Analysis Output and Cleaning Environment 
# 

savehistory(file = "liverpool-dissertation-data-analysis-output.Rhistory")

closeAllConnections()
rm(list=ls())

## End of File

