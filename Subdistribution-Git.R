###### Subdristribution hazard model####

library(survival)
library(plyr)
library(mvna)
library(etm)
library(data.table)
library(kmi)
library(xtable)

# load dt.TimeDep data

####****functions****####

fun.extractCox <- function(cox){
  
  # adapted from code written by Nathan Green (HCAI burden code)
  # extract a subset of the Cox PH output values
  # infection status variable assumed to be the last covariate in list
  # cox: summary(coxph(Surv(start, stop, status)~x + y, data))
  
  dpl <- 2
  beta <- coef(cox)[ ,"coef"]      
  se   <- coef(cox)[ ,"se(coef)"]          
  low.CI   <- beta-(1.96*se)  
  high.CI   <- beta+(1.96*se)
  
  res <- cbind("exp(beta)"=round(exp(beta),dpl),
               "low.CI"=round(exp(low.CI),dpl),
               "high.CI"=round(exp(high.CI),dpl))
  return(res) 
}



####**** applied ****####

## code discharged in the risk set over the whole period
dt.TimeDep[ , tstop2 := tstop]
dt.TimeDep[discharged==1, tstop2 := 45]

## apply cox to new data set - unadjusted
inf.U <- fun.extractCox(summary(coxph(Surv(tstart,tstop2,dead)~ cluster(ID) + 
                                    inf, data=dt.TimeDep)))

inf.A <- fun.extractCox(summary(coxph(Surv(tstart,tstop2,dead)~ cluster(ID) + 
                                    inf + center.age + center.elix + 
                                    center.age:center.elix + sex_dummy +
                                    Orgtyp2, data=dt.TimeDep)))

# 2 = RES
RES.U <- fun.extractCox(summary(coxph(Surv(tstart,tstop2,dead)~ cluster(ID) + RESDR +
                                    RESDS, data=dt.TimeDep)))

RES.A <- fun.extractCox(summary(coxph(Surv(tstart,tstop2,dead)~ cluster(ID) + RESDR +
                                    RESDS + center.age + center.elix + center.age:center.elix + sex_dummy +
                                    Orgtyp2, data=dt.TimeDep)))

# 3 = ceph
ceph.U <- fun.extractCox(summary(coxph(Surv(tstart,tstop2,dead)~ cluster(ID) + cephDR +
                                     cephDS, data=dt.TimeDep)))

ceph.A <- fun.extractCox(summary(coxph(Surv(tstart,tstop2,dead)~ cluster(ID) + cephDR +
                                     cephDS + center.age + center.elix + center.age:center.elix + sex_dummy +
                                     Orgtyp2, data=dt.TimeDep)))


# 4 = cipro
cipro.U <- fun.extractCox(summary(coxph(Surv(tstart,tstop2,dead)~ cluster(ID) + ciproDR +
                                      ciproDS, data=dt.TimeDep)))

cipro.A <- fun.extractCox(summary(coxph(Surv(tstart,tstop2,dead)~ cluster(ID) + ciproDR +
                                      ciproDS + center.age + center.elix + center.age:center.elix + sex_dummy +
                                      Orgtyp2, data=dt.TimeDep)))


# 5. gen
gen.U <- fun.extractCox(summary(coxph(Surv(tstart,tstop2,dead)~ cluster(ID) + genDR +
                                    genDS, data=dt.TimeDep)))

gen.A <- fun.extractCox(summary(coxph(Surv(tstart,tstop2,dead)~ cluster(ID) + genDR +
                                    genDS + center.age + center.elix + center.age:center.elix + sex_dummy +
                                    Orgtyp2, data=dt.TimeDep)))


# 6. piptaz
piptaz.U <- fun.extractCox(summary(coxph(Surv(tstart,tstop2,dead)~ cluster(ID) + piptazDR +
                                       piptazDS, data=dt.TimeDep)))

piptaz.A <- fun.extractCox(summary(coxph(Surv(tstart,tstop2,dead)~ cluster(ID) + piptazDR +
                                       piptazDS + center.age + center.elix + center.age:center.elix + sex_dummy +
                                       Orgtyp2, data=dt.TimeDep)))


l.unadj <- list(inf.U, RES.U,ceph.U,cipro.U,gen.U,piptaz.U)
l.adj <- list(inf.A, RES.A,ceph.A,cipro.A,gen.A,piptaz.A)

# save lists
