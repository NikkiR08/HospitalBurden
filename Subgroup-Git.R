############## Subgroup analyses.R #####################################

## see clos and etm_CIF r scripts for more detail on analysis code

## note that the functions do not all start with "fun." in this script

library(survival)
library(plyr)
library(mvna)
library(etm)
library(data.table)


## load time dependent data

### Length of stay subgroup analysis###############
####################################################


####### functions #######################################################

boot.clos.dtable <- function(data, state.names, tra, cens.name, s = 0, nboot) {
  # data.table version of boot_clos()
  # faster runtime
  # data: class(data.table)
  #
  res <- double(nboot)
  
  ## set id as the searchable key column
  setkey(data, "id")
  
  for (i in seq_len(nboot)) {
    
    index <- sample(unique(data[ ,id]), replace=TRUE)
    
    dboot <- data[J(index)]
    tr.prob <- etm(dboot, state.names, tra, cens.name, s, cova = FALSE)
    res[i] <- etm::clos(tr.prob)$e.phi
    gc()
  }
  res
}

### LOS
inf.sub <- function(x){
  
  tra <- matrix(FALSE, 4, 4, dimnames = list(as.character(0:3), as.character(0:3)))  # admission-infection-death-discharge
  tra[1, 2:4] <- TRUE
  tra[2, 3:4] <- TRUE
  
  msm.data <- data.table( id=x$ID,
                          entry=x$tstart,
                          exit=x$tstop,
                          from="0",
                          to="0",
                          inf=x$inf,
                          DoD=x$deadordischarged,
                          dead=x$dead,
                          case=x$case)
  msm.data[inf==0, from := "0"]
  msm.data[inf==1, from := "1"]
  msm.data[inf==0 & case==1, to := "1"]
  msm.data[DoD==1, to := "2"]
  msm.data[dead==1, to := "3"]
  msm.data[(DoD==0 & exit==45), to := "cens"]
  
  msm.data[ , errorflag := 0]
  msm.data[exit<entry, errorflag:=1]
  
  msm.data <- msm.data[errorflag==0]
  msm.data[ , errorflag := NULL]
  
  etm.data <- etm(msm.data, state.names=c("0","1","2","3"), tra=tra, cens.name="cens", 
                  s=0)
  
  z.inf <- clos(etm.data)
  
  return(z.inf)
  
}

# x is data and y is resistance type in the form of RES
res.sub <- function(x,y){
  
  tra <- matrix(FALSE, 4, 4, dimnames = list(as.character(0:3), as.character(0:3))) 
  tra[1, 3:4] <- TRUE
  tra[2, 3:4] <- TRUE
  
  y <- deparse(substitute(y))
  
  msm.data <- data.table( id=x$ID,
                          entry=x$tstart,
                          exit=x$tstop,
                          from="0",
                          to="0",
                          inf=x$inf,
                          RES=x[[y]],
                          case=x$case,
                          DoD=x$deadordischarged,
                          dead=x$dead)
  
  msm.data[ ,flag := 0]
  msm.data[inf==0 & case==1, flag := 1]
  msm.data <- msm.data[flag==0]
  msm.data[ , flag := NULL]
  
  
  msm.data[RES==1, from := "1"]
  msm.data[DoD==1, to := "2"]
  msm.data[dead==1, to := "3"]
  msm.data[(DoD==0 & exit==45), to := "cens"]
  
  msm.data[ , errorflag := 0]
  msm.data[exit<entry, errorflag:=1]
  msm.data <- msm.data[errorflag==0]
  msm.data[ , errorflag := NULL]
  
  etm.data <- etm(msm.data, state.names=c("0","1","2","3"), tra=tra, cens.name="cens", 
                  s=0)
  
  v.res <- clos(etm.data)
  
  return(v.res)
}



### cif subgroup analysis###

inf.CIF.sub <- function(x){
  
  msm.data <- data.table( id=x$ID,
                          entry=x$tstart,
                          exit=x$tstop,
                          from=0,
                          to=0,
                          inf=x$inf,
                          dead=x$dead,
                          discharged=x$discharged)
  
  msm.data[ , errorflag := 0]
  msm.data[exit<entry, errorflag:=1]
  
  msm.data <- msm.data[errorflag==0]
  msm.data[ , errorflag := NULL]
  
  msm.data[ , DoD := 0]
  msm.data[dead==1, DoD := 1]
  msm.data[discharged==1, DoD := 2]
  
  cif.inf <- etmCIF(Surv(entry, exit, DoD !=0)~
                      inf, msm.data, etype=DoD, failcode=1)
  
  x <- summary(cif.inf)
  
  return(x)
}



res.CIF.sub <- function(x,y){
  
  y <- deparse(substitute(y))
  
  msm.data <- data.table( id=x$ID,
                          entry=x$tstart,
                          exit=x$tstop,
                          from=0,
                          to=0,
                          inf=x$inf,
                          dead=x$dead,
                          discharged=x$discharged,
                          res=x[[y]])
  
  msm.data[ , errorflag := 0]
  msm.data[exit<entry, errorflag:=1]
  
  msm.data <- msm.data[errorflag==0]
  msm.data[ , errorflag := NULL]
  
  msm.data[ , DoD := 0]
  msm.data[dead==1, DoD := 1]
  msm.data[discharged==1, DoD := 2]
  
  cif.res <- etmCIF(Surv(entry, exit, DoD !=0)~
                      res, msm.data, etype=DoD, failcode=1)
  
  x <- summary(cif.res)
  
  return(x)
}

#### bootstrapping the LoS estimates

boot.inf.sub <- function(x){
  
  tra <- matrix(FALSE, 4, 4, dimnames = list(as.character(0:3), as.character(0:3)))  # admission-infection-death-discharge
  tra[1, 2:4] <- TRUE
  tra[2, 3:4] <- TRUE
  
  msm.data <- data.table( id=x$ID,
                          entry=x$tstart,
                          exit=x$tstop,
                          from="0",
                          to="0",
                          inf=x$inf,
                          DoD=x$deadordischarged,
                          dead=x$dead,
                          case=x$case)
  msm.data[inf==0, from := "0"]
  msm.data[inf==1, from := "1"]
  msm.data[inf==0 & case==1, to := "1"]
  msm.data[DoD==1, to := "2"]
  msm.data[dead==1, to := "3"]
  msm.data[(DoD==0 & exit==45), to := "cens"]
  
  msm.data[ , errorflag := 0]
  msm.data[exit<entry, errorflag:=1]
  
  msm.data <- msm.data[errorflag==0]
  msm.data[ , errorflag := NULL]
  
  msm.data <- as.data.table(msm.data)
  boot <- boot.clos.dtable(msm.data, c("0","1","2","3"), tra, "cens", 0,
                           nboot = 1000)
  return(boot)
  
}


boot.res.sub <- function(x,y){
  
  y <- deparse(substitute(y))
  
  tra <- matrix(FALSE, 4, 4, dimnames = list(as.character(0:3), as.character(0:3))) 
  tra[1, 3:4] <- TRUE
  tra[2, 3:4] <- TRUE
  
  
  msm.data <- data.table(id=x$ID,
                         entry=x$tstart,
                         exit=x$tstop,
                         from="0",
                         to="0",
                         inf=x$inf,
                         res=x[[y]],
                         case=x$case,
                         DoD=x$deadordischarged,
                         dead=x$dead)
  
  msm.data[ ,flag := 0]
  msm.data[inf==0 & case==1, flag := 1]
  msm.data <- msm.data[flag==0]
  msm.data[ , flag := NULL]
  msm.data[res==1, from := "1"]
  msm.data[DoD==1, to := "2"]
  msm.data[dead==1, to := "3"]
  msm.data[(DoD==0 & exit==45), to := "cens"]
  
  msm.data[ , errorflag := 0]
  msm.data[exit<entry, errorflag:=1]
  msm.data <- msm.data[errorflag==0]
  msm.data[ , errorflag := NULL]
  
  msm.data <- as.data.table(msm.data)
  boot <- boot.clos.dtable(msm.data, c("0","1","2","3"), tra, "cens", 0,
                           nboot = 1000)
  
  return(boot)
  
}

### EQUATION LOS

## have to make sure list number refers to the same as los lists created
# in the analysis section

#### average values
variable.creation <- function(x){
  
  v.n <- length(which(x$inf==1))
  
  p.RES <- ((length(which(x$RESDR==1)))/v.n)
  p.ceph_res <- ((length(which(x$cephDR==1)))/v.n)
  p.cipro_res <- ((length(which(x$ciproDR==1)))/v.n)
  p.gen_res <- ((length(which(x$genDR==1)))/v.n)
  p.piptaz_res <- ((length(which(x$piptazDR==1)))/v.n)
  
  newlist <- list(v.n,p.RES,p.ceph_res,p.cipro_res,p.gen_res,p.piptaz_res)
  
  return(newlist)
  
}


## have to put the number in the list in the function rather res. name
eq.los.res <- function(x,y){
  
  y <- as.numeric(y)
  
  v <- x[[1]]
  v.average <- as.numeric(v$e.phi[1,1])
  
  v <- x[[y]]
  v.res <- as.numeric(v$e.phi[1,1])
  
  p.res <- newlist[[y]]
  
  v.SUS <- (v.average - (p.res*v.res))
  v.RES <-  v.SUS + v.res
  
  l.eq <- list(v.RES, v.SUS)
  return(l.eq)
}

## standard errors
# x is infection boot, y is resistant boot, z is subgroup data frame
# w is resistant type in its dummy format
# in the returned list [[1]] is susceptible and [[2]] is resistant
eq.los.boot <- function(x,y,z,w) {
  
  w <- deparse(substitute(w))
  v.n <- length(which(z$inf==1))
  
  p.res <- ((length(which(z[[w]]==1)))/v.n)
  
  dt <- data.table(VI=x,VR=y)
  setnames(dt, c("VI","VR"))
  
  #VX - susceptible vs controls
  dt[ , VX := VI - (p.res*VR)]
  # VY - resistant vs controls
  dt[ , VY := VX + VR]
  
  # se.RESX <- sqrt(var(dt$VX))
  # se.RESY <- sqrt(var(dt$VY))
  # 
  # l.se <- list(se.RESX, se.RESY)
  # print(l.se)
  return(dt)
}


#######################################################################

### ANALYSIS
# splitting into age and sex groups wanted
mage <- 65

# cut into <= median and > median
dt.age.less <- dt.TimeDep[admiage_miss<mage]
dt.age.more <- dt.TimeDep[admiage_miss>=mage]

# cut into sex
dt.age.less.m <- dt.age.less[sex_dummy==1]
dt.age.less.f <- dt.age.less[sex_dummy==0]

dt.age.more.m <- dt.age.more[sex_dummy==1]
dt.age.more.f <- dt.age.more[sex_dummy==0]

# rm(dt.age.less)
# rm(dt.age.more)


## create right data.tables

dt.ALF.Inf <- dt.age.less.f[case==1]
dt.ALM.Inf <- dt.age.less.m[case==1]
dt.AMF.Inf <- dt.age.more.f[case==1]
dt.AMM.Inf <- dt.age.more.m[case==1]

##### the example below is for the female < 65 subgroup

##### LOS 
v.inf <- inf.sub(dt.age.less.f)
v.ALF.RES <- res.sub(dt.ALF.Inf, RESDR)
v.ALF.ceph <- res.sub(dt.ALF.Inf, cephDR)
v.ALF.cipro <- res.sub(dt.ALF.Inf, ciproDR)
v.ALF.gen <- res.sub(dt.ALF.Inf, genDR)
v.ALF.piptaz <- res.sub(dt.ALF.Inf, piptazDR)

l.ALF.los <- list(v.inf, v.ALF.RES, v.ALF.ceph, 
                  v.ALF.cipro, v.ALF.gen , v.ALF.piptaz)

##### CIF 
m.inf <- inf.CIF.sub(dt.age.less.f)
m.RES <- res.CIF.sub(dt.age.less.f,RES2)
m.ceph <- res.CIF.sub(dt.age.less.f,ceph_res2)
m.cipro <- res.CIF.sub(dt.age.less.f,cipro_res2)
m.gen <- res.CIF.sub(dt.age.less.f,gen_res2)
m.piptaz <- res.CIF.sub(dt.age.less.f,piptaz_res2)

l.ALF.CIF <- list(m.inf, m.RES, m.ceph, 
                  m.cipro, m.gen , m.piptaz)

##### BOOTSTRAPPING 

## ALF
boot.ALF.Inf <- boot.inf.sub(dt.age.less.f)
boot.ALF.RES <- boot.res.sub(dt.ALF.Inf,RESDR)
boot.ALF.ceph <- boot.res.sub(dt.ALF.Inf,cephDR)
boot.ALF.cipro <- boot.res.sub(dt.ALF.Inf.f,ciproDR)
boot.ALF.gen <- boot.res.sub(dt.ALF.Inf,genDR)
boot.ALF.piptaz <- boot.res.sub(dt.ALF.Inf,piptazDR)


## comparing length of stay to non-infected controls using equations

# ALF
newlist <- variable.creation(dt.age.less.f)
eq.ALF.res <- eq.los.res(l.ALF.los,2)
eq.ALF.ceph <- eq.los.res(l.ALF.los,3)
eq.ALF.cipro <- eq.los.res(l.ALF.los,4)
eq.ALF.gen <- eq.los.res(l.ALF.los,5)
eq.ALF.piptaz <- eq.los.res(l.ALF.los,6)
l.ALF <- list(eq.ALF.res, eq.ALF.ceph, eq.ALF.cipro,
              eq.ALF.gen, eq.ALF.piptaz)
l.ALF

### calculating SEs for this
## ALF
#RES
b.alf.res <- eq.los.boot(boot.ALF.Inf, boot.ALF.RES, dt.age.less.f, RESDR)
#Ceph
b.alf.ceph <- eq.los.boot(boot.ALF.Inf, boot.ALF.ceph, dt.age.less.f, cephDR)
#cipro
b.alf.cipro <- eq.los.boot(boot.ALF.Inf, boot.ALF.cipro, dt.age.less.f, ciproDR)
#gen
b.alf.gen <- eq.los.boot(boot.ALF.Inf, boot.ALF.gen, dt.age.less.f, genDR)
#piptaz
b.alf.pip <- eq.los.boot(boot.ALF.Inf, boot.ALF.piptaz, dt.age.less.f, piptazDR)

# can use percentile method or standard errors to calculate the 95% confidence intervals from above
## create tables/csv's with results