## "StepFunctions-Git.R"

## set working directory


## loading packages that are needed
library(survival)
library(dplyr)
library(mvna)
library(data.table)
library(etm)

# load data - dt.TimeDep

#### FOR DEAD MODELS

#### split into Ho & (CO & cases)
dt.TimeDep[ , tgroup := 0]

# here CO also includes controls
CO <- dt.TimeDep[onset=="COCA"|onset=="COHA"|case==0]
HO <- dt.TimeDep[onset=="HO"]

## CO 

CO1 <- CO[flag.Step.Dis=="disless8"|flag.Step.Dis=="dis8"]

## admi --> dis --> 8
CO1[flag.Step.Dis=="disless8", tgroup :=1]

## admi --> dis/8
CO1[flag.Step.Dis=="dis8", tgroup := 1]

## admi --> 8 --> dis
CO2 <- CO[flag.Step.Dis=="disgreat8"]


v.co <- nrow(CO2)
CO2 <- CO2[rep(1:v.co, each=2), ]

v.oddrows <- which(!duplicated(CO2$ID))
v.evenrows <- v.oddrows+1

CO2[v.oddrows, tgroup := 1]
CO2[v.oddrows, tstop := 8]
CO2[v.oddrows, dead := 0]
CO2[v.oddrows, deadordischarged := 0]
CO2[v.oddrows, discharged := 0]
CO2[v.evenrows, tgroup := 2]
CO2[v.evenrows, tstart := 8]

## merge to form CO 
l = list(CO1, CO2)

CO <- rbindlist(l, use.names=TRUE, fill=TRUE)
rm(CO2)
rm(CO1)

## HO

## need to separate out the types in order to avoid duplication when merging
# back at the end
HO[ , merge := 0]

## admi --> inf/8 --> dis
HO[flag.Step.Spec=="spec8" & flagType=="HOA", tgroup :=1]
HO[flag.Step.Spec=="spec8" & flagType=="HOB", tgroup :=2]

HO[flag.Step.Spec=="spec8" & flagType=="HOA", merge :=1]
HO[flag.Step.Spec=="spec8" & flagType=="HOB", merge :=1]

## admi --> inf --> dis/8
HO[flag.Step.Dis=="dis8", tgroup := 1]
HO[flag.Step.Dis=="dis8", merge := 1]

## admi --> inf --> dis --> 8
HO[flag.Step.Dis=="disless8", tgroup := 1]
HO[flag.Step.Dis=="disless8", merge := 1]

HO1 <- HO[merge==1]

## admi --> 8 --> inf --> dis
HO2 <- HO[flag.Step.Spec=="specgreat8"]

HO.sub1A <- HO2[flagType=="HOA"] # admission --> inf
HO.sub1B <- HO2[flagType=="HOB"] # inf --> discharge

v.nhoA <- nrow(HO.sub1A)
HO.sub1A <- HO.sub1A[rep(1:v.nhoA, each=2), ]

v.oddrows <- which(!duplicated(HO.sub1A$ID))
v.evenrows <- v.oddrows+1

HO.sub1A[v.oddrows, tgroup := 1]
HO.sub1A[v.oddrows, tstop := 8]
HO.sub1A[v.evenrows, tgroup := 2]
HO.sub1A[v.evenrows, tstart := 8]

HO.sub1B[ , tgroup:= 2]

l = list(HO.sub1A, HO.sub1B)

HO2 <- rbindlist(l, use.names=TRUE, fill=TRUE)
# then sort by id
setkey(HO2, ID)

## admi --> inf --> 8 --> dis
HO3 <- HO[flag.Step.Spec=="specless8" & flag.Step.Dis=="disgreat8"]

HO3A <- HO3[flagType=="HOA"]
HO3B <- HO3[flagType=="HOB"]


HO3A[ , tgroup:= 1]

v.nhoB <- nrow(HO3B)
HO3B <- HO3B[rep(1:v.nhoB, each=2), ]

v.oddrows <- which(!duplicated(HO3B$ID))
v.evenrows <- v.oddrows+1

HO3B[v.oddrows, tgroup := 1]
HO3B[v.oddrows, tstop := 8]
HO3B[v.evenrows, tgroup := 2]
HO3B[v.evenrows, tstart := 8]

l = list(HO3A, HO3B)

HO3 <- rbindlist(l, use.names=TRUE, fill=TRUE)

l = list(HO1, HO2, HO3, CO)
dt.TimeDep.F <- rbindlist(l, use.names=TRUE, fill=TRUE)

## remove unwanted var
dt.TimeDep.F[ , merge := NULL]

#save dataset

#### performed the same process for dead or discharge models where
# the time cut off was 2 not 8
