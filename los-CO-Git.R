## community onset functions for length of stay - clos & bootstrapping


library(survival)
library(plyr)
library(mvna)
library(etm)
library(data.table)
library(geepack)

fun.inf.CO <- function(x){
  
  tra <- matrix(FALSE, 4, 4, dimnames = list(as.character(0:3), as.character(0:3)))  # admission-infection-death-discharge
  tra[1, 3:4] <- TRUE
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


fun.boot.CO <- function(x){
  
  tra <- matrix(FALSE, 4, 4, dimnames = list(as.character(0:3), as.character(0:3)))  # admission-infection-death-discharge
  tra[1, 3:4] <- TRUE
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
