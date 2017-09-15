## a function which preps the data and performs functions
# within the etm package
# see : https://cran.r-project.org/web/packages/etm/etm.pdf 

library(survival)
library(plyr)
library(mvna)
library(etm)
library(data.table)


fun.inf.clos <- function(x){
  # x is data a data.table or data.frame object
  
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


fun.res.clos <- function(x,y){
  
  # x is data a data.table or data.frame object
  # and y is resistance type in the form of a time dependent resistance dummy variable
  
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


