## "Coxph-Git.R"

# set working directory

library(survival)
library(data.table)
library(dplyr)


### dead models

# load dataset that includes the stepfunction adaptation
# for the particular model of interest (e.g. 'dead')

# 1 = inf 
x <- (coxph(Surv(tstart,tstop,dead)~ cluster(ID) + inf:strata(tgroup) +
                  center.age + center.elix + center.age:center.elix + sex_dummy +
                  Orgtyp2, data=dt.TimeDep.F))
summary(x)

# 2 = RES
x <- coxph(Surv(tstart,tstop,dead)~ cluster(ID) + RESDR:strata(tgroup) +
             RESDS:strata(tgroup) + center.age + center.elix + center.age:center.elix + sex_dummy +
             Orgtyp2, data=dt.TimeDep.F)
summary(x)

## the same process was used for additional resistances of interest
# and for the "deadordischarged" outcome rather than "dead".