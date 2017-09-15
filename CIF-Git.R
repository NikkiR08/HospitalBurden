## "CIF-Git.R"


# using the etmCIF function from the etm package

## set working directory

library(survival)
library(plyr)
library(mvna)
library(etm)
library(data.table)
library(geepack)

## load data - dt.TimeDep

## functions

fun.CIF.inf <- function(x){
  
  # x is a data.table of interest
  
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
  
  return(cif.inf)
}



fun.CIF.res <- function(x,y){
  
  # x is the data.table of interest
  # y is the resistance variable of interest - where by
  # 0 = non-infected, 1= suscpetible infection and 2 = resistant infection
  
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
  
  return(cif.res)
}

## examples of analysis

m.inf <- fun.CIF.inf(dt.TimeDep)
m.RES <- fun.CIF.res(dt.TimeDep,RES2)

# process done for other resistance variables of interest


