##"Checks-Git.R"

## set working directory

#### packages needed
library(ggplot2)
library(survival)
library(dplyr)
library(mvna)
library(data.table)
library(etm)


########################################################################
##### CHECKS PERFORMED ON THE DATA DURING THE CLEANING####


#### load needed data - dt.TimeDep and dt.TimeIndep

# though should be noted some of the checks will produce diff
# results with "final" saved datasets as this is an interative process


#### checking length of stay + timelines for cases/controls (unadjusted)

## also checked for dt.casesclean before dt.TimeIndep created things like;
# length(which(dt.casesclean$disdate_lastfce<dt.casesclean$specdate_new))
# length(which(dt.casesclean$disdate_lastfce<dt.casesclean$admidate_new))
# x <- dt.casesclean[specdate_new<admidate_new]
# x[ ,check := admidate_new-specdate_new]
#  max(x$check)
# length(which(dt.controlsclean$disdate_lastfce<
#                 dt.controlsclean$admidate_new))

## load ggsurv function
source("ggsurv-Git.R")

summary(dt.TimeIndep$los)
min(dt.TimeIndep$los)
length(which(dt.TimeIndep$los==0))

#### checking admi2dis and admi2spec before censoring 
length(which(dt.TimeIndep$admi2spec>45))
length(which(dt.TimeIndep$admi2dis>45))
length(which(dt.TimeIndep$admi2dis>45 & dt.TimeIndep$inf==1))

#### checking that tstart<tstop once built time dep
length(which(dt.TimeDep$tstart<=dt.TimeDep$tstop))
length(which(dt.TimeDep$tstart>=dt.TimeDep$tstop))


#### checking groupings for elixhauser

# groups
# these groups were chosen based off of distribution plots
# check plots 

ggplot(dt.TimeIndep, aes(x=elix))+geom_density()
ggplot(dt.TimeIndep, aes(x=elix))+geom_density(adjust=3)

### checking whether can group non-tested with any other group

### load function for plotting survival curves easily

dt.TimeIndep.Inf <- dt.TimeIndep[inf==1]

table(dt.TimeIndep.Inf$ceph_res,dt.TimeIndep.Inf$dead, exclude = NULL)
table(dt.TimeIndep.Inf$cipro_res,dt.TimeIndep.Inf$dead, exclude = NULL)
# etc. for other resistances of interest

dt.TimeIndep.Inf[,list(median=median(admi2dis),
                       LQR=quantile(admi2dis, 0.25),
                       UQR=quantile(admi2dis, 0.75)),
                 by=ceph_res]

dt.TimeIndep.Inf[,list(median=median(admi2spec),
                       LQR=quantile(admi2dis, 0.25),
                       UQR=quantile(admi2dis, 0.75)),
                 by=cipro_res]
# etc. for other resistances of interest

km <- survfit(Surv(tstop,dead) ~ ceph_resN,
              data = dt.TimeIndep.Inf)

fun.ggsurv(km)

dt.TimeIndep.Inf[ , cipro_resN := cipro_res]
dt.TimeIndep.Inf[is.na(cipro_resN), cipro_resN:=2]

km <- survfit(Surv(tstop,dead) ~ cipro_resN,
              data = dt.TimeIndep.Inf)

fun.ggsurv(km)
# etc. for other resistances of interest


#### CHECKS PERFORMED ON THE DATA DURING ANALYSIS 
# 

#### load a subgroup for controls of 200,000 

dt.sub.Con <- dt.sub[case==0]
 
# ## compare descriptive stats between all controls & sample controls
table(dt.TimeDep.Con$sex_dummy)
table(dt.sub.Con$sex_dummy)
median(dt.TimeDep.Con$admiage_miss)
quantile(dt.TimeDep.Con$admiage_miss, 0.25)
quantile(dt.TimeDep.Con$admiage_miss, 0.75)
median(dt.sub.Con$admiage_miss)
quantile(dt.sub.Con$admiage_miss, 0.25)
quantile(dt.sub.Con$admiage_miss, 0.75)
table(dt.TimeDep.Con$elixgrp)
table(dt.sub.Con$elixgrp)
table(dt.TimeDep.Con$Organisation.Type)
table(dt.sub.Con$Organisation.Type)
median(dt.TimeDep.Con$los)
quantile(dt.TimeDep.Con$los, 0.25)
quantile(dt.TimeDep.Con$los, 0.75)
median(dt.sub.Con$los)
quantile(dt.sub.Con$los, 0.25)
quantile(dt.sub.Con$los, 0.75)
mean(dt.TimeDep.Con$los)
mean(dt.sub.Con$los)
length(which(dt.TimeDep.Con$dead==1))
length(which(dt.sub.Con$dead==1))

#### cox model fitting using this sub sample of the data

## number of ethnicity variables missing
X <- length(which(is.na(dt.sub$ethnicitygrp)))
Y <- length(which(is.na(dt.sub$ethnicitygrp)& dt.sub$inf==1))
z <- length(which(dt.sub$inf==1))

X/nrow(dt.sub)
Y/z

## VARIABLE SCALING AND INCLUSION FOR COX 

# FOR DEATH MODELS
# age
x1 <- (coxph(Surv(tstart,tstop,dead)~ cluster(ID) +
                       agegrp, data=dt.sub))
 summary(x1)
 BIC(x1)
x1 <- (coxph(Surv(tstart,tstop,dead)~ cluster(ID) +
                       center.age, data=dt.sub))
 summary(x1)
 BIC(x1)
 

# elix
x1 <- (coxph(Surv(tstart,tstop,dead)~ cluster(ID) +
                       elixgrp, data=dt.sub))
 summary(x1)
 BIC(x1)
x1 <- (coxph(Surv(tstart,tstop,dead)~ cluster(ID) +
                       center.elix, data=dt.sub))
 summary(x1)
 BIC(x1)
x1 <- (coxph(Surv(tstart,tstop,dead)~ cluster(ID) +
                       elix1, data=dt.sub))
 summary(x1)
 BIC(x1)
## etc. same done for other variables of interest
 ## and also performed for "deadordischarged" models

#### SCHOENFELD RESIDUALS FOR COX 
 
#with infection
 ## Death
 m.death <- (coxph(Surv(tstart,tstop,dead)~ cluster(ID) + inf +
                           center.age + center.elix +
                           center.age:center.elix + Orgtyp2+ sex_dummy, data=dt.sub))

  summary(m.death)
  BIC(m.death)
  
  zph.inf <- cox.zph(m.death)
  plot(zph.inf)
  
  #without dots
  plot(zph.inf, resid=FALSE, ylim=c(-2,2))
  

## Discharged
  m.DOD <- (coxph(Surv(tstart,tstop,deadordischarged)~ cluster(ID) + inf +
                    agegrp + elixgrp +
                    agegrp:elixgrp + Organisation.Type+ sex_dummy, data=dt.sub))
  
  summary(m.DOD)
  BIC(m.DOD)

  zph.inf <- cox.zph(m.DOD)
  plot(zph.inf, df=2)
  
  # problems with this model and plotting the coefficients: 
  # see - http://r.789695.n4.nabble.com/plot-cox-zph-td835443.html 
  
  m.DOD <- (coxph(Surv(tstart,tstop,deadordischarged)~ cluster(ID) + inf +
                    center.age + center.elix +
                    center.age:center.elix + Organisation.Type+ sex_dummy,data=dt.sub))
  
  summary(m.DOD)
  BIC(m.DOD)
  
  zph.inf <- cox.zph(m.DOD)
  plot(zph.inf, df=2)
  #without dots
  plot(zph.inf, df=2, resid=FALSE, ylim=c(-2,2))
  
## same checks performed for resistance types of interest
  
  
  
  