## "Length-of-Stay-Git.R"

## set working directory

## load packages

library(survival)
library(plyr)
library(mvna)
library(etm)
library(data.table)
library(geepack)

# load data - dt.TimeDep

# source functions needed
source("clos-Git.R")
source("NG-boot-Git.R")
source("boot-clos-Git.R")
source("los-equation-Git.R")
source("los-CO-Git.R")

### examples of analyses performed

# CLOS 
v.inf <- fun.inf.clos(dt.TimeDep)
v.ALF.RES <- fun.res.clos (dt.TimeDep, RESDR)
v.ALF.ceph <- fun.res.clos (dt.TimeDep, cephDR)
v.ALF.cipro <- fun.res.clos (dt.TimeDep, ciproDR)
v.ALF.gen <- fun.res.clos (ddt.TimeDep, genDR)
v.ALF.piptaz <- fun.res.clos (dt.TimeDep, piptazDR)

l.los <- list(v.inf, v.ALF.RES, v.ALF.ceph, 
                  v.ALF.cipro, v.ALF.gen , v.ALF.piptaz)
# save list

# BOOTSTRAPPING 
boot.Inf <- fun.boot.inf(dt.TimeDep)
#se
sqrt(var(boot.Inf))

boot.RES <- fun.boot.res(dt.TimeDep,RESDR)
sqrt(var(boot.RES))

# save bootstrap outputs
# done for other resistances

# comparing length of stay to non-infected controls using equations

# ALF
newlist <- fun.variable.creation(dt.TimeDep)
eq.res <- fun.eq.los.res(l.los,2)
eq.ceph <- fun.eq.los.res(l.los,3)
eq.cipro <- fun.eq.los.res(l.los,4)
eq.gen <- fun.eq.los.res(l.los,5)
eq.piptaz <- fun.eq.los.res(l.los,6)
l.eq <- list(eq.res, eq.ceph, eq.cipro,
              eq.gen, eq.piptaz)
l.eq

### calculating standard errors for this

#RES
fun.eq.los.boot(boot.Inf, boot.RES, dt.TimeDep, RESDR)
# repeated this process for other resistances interested in

# community onset needs slightly different functions for LOS as no transition from 
# uninfected to infected in the models

dt.CO <- dt.TimeDep[onset=="COHA"|onset=="COCA"|onset=="Not infected"]

los.CO <- inf.sub.CO(dt.CO)
los.CO

boot.CO <- boot.inf.sub.CO(dt.CO)
sqrt(var(boot.CO))

#### PSUEDO OBSERVATION ANALYSIS
# based on the etm package closPsuedo function

## load subample with  200,000 controls (and all cases) - dt.sub
# get jut infected cases- dt.TimeDep.Inf

tra <- matrix(FALSE, 3,3, dimnames = list(as.character(0:2),
                                          as.character(0:2)))
tra[1, 2:3]<- TRUE
tra[2,3] <- TRUE


msm.data <- data.table( id=dt.sub$ID,
                        entry=dt.sub$tstart,
                        exit=dt.sub$tstop,
                        from="0",
                        to="0",
                        inf=dt.sub$inf,
                        age=dt.sub$center.age,
                        sex=dt.sub$sex_dummy,
                        elix=dt.sub$center.elix,
                        DoD=dt.sub$deadordischarged,
                        case=dt.sub$case)
msm.data[inf==0, from := "0"]
msm.data[inf==0 & case==1, to:= "1"]

msm.data[inf==1, from := "1"]
msm.data[DoD==1, to := "2"]
msm.data[(DoD==0 & exit==45), to := "cens"]

msm.data[ , errorflag := 0]
msm.data[exit<entry, errorflag:=1]
msm.data <- msm.data[errorflag==0]
msm.data[ , errorflag := NULL]

msm.data[ , inf := NULL]
msm.data[ , case := NULL]

## computation of the pseudo-observations
ps.msm.data <- closPseudo(msm.data, c("0", "1", "2"), tra_ill(), "cens",
                          formula = ~ age + sex + elix)
## regression model using geepack
fit <- geeglm(ps.e.phi ~ age + sex + elix, id = id, data = ps.msm.data$pseudoData,
              family = gaussian)
summary(fit)

### for resistance

tra <- matrix(FALSE, 3,3, dimnames = list(as.character(0:2),
                                          as.character(0:2)))
tra[1,3]<- TRUE
tra[2,3] <- TRUE


msm.data <- data.table( id=dt.TimeDep.Inf$ID,
                        entry=dt.TimeDep.Inf$tstart,
                        exit=dt.TimeDep.Inf$tstop,
                        from="0",
                        to="0",
                        inf=dt.TimeDep.Inf$inf,
                        RES=dt.TimeDep.Inf$RESDR,
                        case=dt.TimeDep.Inf$case,
                        DoD=dt.TimeDep.Inf$deadordischarged,
                        age=dt.TimeDep.Inf$center.age,
                        elix=dt.TimeDep.Inf$center.elix,
                        sex=dt.TimeDep.Inf$sex_dummy)


## remove the admission --> infection state
msm.data[ ,flag := 0]
msm.data[inf==0 & case==1, flag := 1]
msm.data <- msm.data[flag==0]
msm.data[ , flag := NULL]


msm.data[RES==1, from := "1"]
msm.data[DoD==1, to := "2"]
msm.data[(DoD==0 & exit==45), to := "cens"]

msm.data[ , errorflag := 0]
msm.data[exit<entry, errorflag:=1]
msm.data <- msm.data[errorflag==0]
msm.data[ , errorflag := NULL]


## computation of the pseudo-observations
ps.msm.data <- closPseudo(msm.data, c("0", "1", "2"), tra_ill(), "cens",
                          formula = ~ age + sex + elix)
## regression model using geepack
fit <- geeglm(ps.e.phi ~ age + sex + elix, id = id, data = ps.msm.data$pseudoData,
              family = gaussian)
summary(fit)

### repeated process for other resistances of interest
