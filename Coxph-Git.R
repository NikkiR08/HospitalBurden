## "Coxph-Git.R"

# set working directory

library(survival)
library(data.table)
library(dplyr)


### deadordischarged 
# (as CIF models and subdistribution used for in-hospital outcomes)


# 1 = inf 
x <- (coxph(Surv(tstart,tstop,deadordischarged)~ cluster(ID) + inf:strata(tgroup) +
              center.age + center.elix + center.age:center.elix + sex_dummy +
              Orgtyp2, data=dt.TimeDep.S))
summary(x)

# 2 = RES
x <- coxph(Surv(tstart,tstop,deadordischarged)~ cluster(ID) + RESDR:strata(tgroup) +
             RESDS:strata(tgroup) + center.age + center.elix + center.age:center.elix + sex_dummy +
             Orgtyp2, data=dt.TimeDep.S)

## the same process was done for other exposure groups of interest