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
