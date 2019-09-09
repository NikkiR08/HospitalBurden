## "StepFunctions-Git.R"

## set working directory


## loading packages that are needed
library(survival)
library(dplyr)
library(mvna)
library(data.table)
library(etm)

# load data - dt.TimeDep


####### split into Ho & (CO & cases)

dt.TimeDep[ , tgroup := 0]

# here CO also includes controls
CO <- dt.TimeDep[onset=="COCA"|onset=="COHA"|case==0]
HO <- dt.TimeDep[onset=="HO"]

## CO 

CO1 <- CO[flag.Step.Dis2=="disless2"|flag.Step.Dis2=="dis2"]

## admi --> dis --> 2
CO1[flag.Step.Dis2=="disless2", tgroup :=1]

## admi --> dis/2
CO1[flag.Step.Dis2=="dis2", tgroup := 1]

## admi --> 2 --> dis
CO.sub <- CO[flag.Step.Dis2=="disgreat2"]


v.co <- nrow(CO.sub)
CO.sub <- CO.sub[rep(1:v.co, each=2), ]

v.oddrows <- which(!duplicated(CO.sub$ID))
v.evenrows <- v.oddrows+1

CO.sub[v.oddrows, tgroup := 1]
CO.sub[v.oddrows, tstop := 2]
CO.sub[v.oddrows, dead := 0]
CO.sub[v.oddrows, deadordischarged := 0]
CO.sub[v.oddrows, discharged := 0]
CO.sub[v.evenrows, tgroup := 2]
CO.sub[v.evenrows, tstart := 2]

## merge to form CO 
l = list(CO1, CO.sub)

CO <- rbindlist(l, use.names=TRUE, fill=TRUE)
rm(CO.sub)
rm(CO1)

## HO

## need to separate out the types in order to avoid duplication when merging
# back at the end

HO[ , merge := 0]

## admi --> inf/2 --> dis
HO[flag.Step.Spec2=="spec2" & flagType=="HOA", tgroup :=1]
HO[flag.Step.Spec2=="spec2" & flagType=="HOB", tgroup :=2]

HO[flag.Step.Spec2=="spec2" & flagType=="HOA", merge :=1]
HO[flag.Step.Spec2=="spec2" & flagType=="HOB", merge :=1]

## admi --> inf --> dis/2
HO[flag.Step.Dis2=="dis2", tgroup := 1]
HO[flag.Step.Dis2=="dis2", merge := 1]

## admi --> inf --> dis --> 2
HO[flag.Step.Dis=="disless2", tgroup := 1]
HO[flag.Step.Dis=="disless2", merge := 1]

HO1 <- HO[merge==1]

## admi --> 2 --> inf --> dis
HO2 <- HO[flag.Step.Spec2=="specgreat2"]

HO.sub1A <- HO2[flagType=="HOA"]
HO.sub1B <- HO2[flagType=="HOB"]

v.nhoA <- nrow(HO.sub1A)
HO.sub1A <- HO.sub1A[rep(1:v.nhoA, each=2), ]

v.oddrows <- which(!duplicated(HO.sub1A$ID))
v.evenrows <- v.oddrows+1

HO.sub1A[v.oddrows, tgroup := 1]
HO.sub1A[v.oddrows, tstop := 2]
HO.sub1A[v.evenrows, tgroup := 2]
HO.sub1A[v.evenrows, tstart := 2]

HO.sub1B[ , tgroup:= 2]

l = list(HO.sub1A, HO.sub1B)

HO2 <- rbindlist(l, use.names=TRUE, fill=TRUE)
# then sort by id
setkey(HO2, ID)

## admi --> inf --> 2 --> dis
HO3 <- HO[flag.Step.Spec2=="specless2" & flag.Step.Dis2=="disgreat2"]

HO3A <- HO3[flagType=="HOA"]
HO3B <- HO3[flagType=="HOB"]


HO3A[ , tgroup:= 1]

v.nhoB <- nrow(HO3B)
HO3B <- HO3B[rep(1:v.nhoB, each=2), ]

v.oddrows <- which(!duplicated(HO3B$ID))
v.evenrows <- v.oddrows+1

HO3B[v.oddrows, tgroup := 1]
HO3B[v.oddrows, tstop := 2]
HO3B[v.evenrows, tgroup := 2]
HO3B[v.evenrows, tstart := 2]

l = list(HO3A, HO3B)

HO3 <- rbindlist(l, use.names=TRUE, fill=TRUE)


l = list(HO1, HO2, HO3, CO)

dt.TimeDep.S <- rbindlist(l, use.names=TRUE, fill=TRUE)

## remove unwanted var
dt.TimeDep.S[ , merge := NULL]

#save dataset

