##"Preprocessiong-Cases-Controls-Git.R"

## set working directory


## loading packages that are needed
library(plyr)
library(survival)
library(dplyr)
library(mvna)
library(data.table)
library(mvna)
library(etm)
library(ggplot2)

# load the cleaned cases and controls dataset

#### creating a base dataset

## merging the cases and controls to a dataset
dt.casesclean[ , inf := 1]
dt.controlsclean[ ,inf :=0]

#put in a flag to see cases and controls for when turn into time dependent
dt.casesclean[ , case :=1]
dt.controlsclean[ ,case :=0]


# create equivalent columns for needed vars in controls
dt.controlsclean[ , HO := NA]
dt.controlsclean[ , COCA := NA]
dt.controlsclean[ , COHA := NA]
dt.controlsclean[ , carb_res := NA]
dt.controlsclean[ , ceph_res := NA]
dt.controlsclean[ , cipro_res := NA]
dt.controlsclean[ , piptaz_res := NA]
dt.controlsclean[ , gen_res := NA]
dt.controlsclean[ , RES := NA]
dt.controlsclean[ , specdate_new := as.Date(NA)]

# need to have an NA as "difftime" class for admi2 spec etc
x <- as.Date(NA)
dt.controlsclean[ , admi2spec := (x-x)]
dt.controlsclean[ , dis2spec := (x-x)]
dt.controlsclean[ , spec2dis := (x-x)]

# make sure all variables are as dates that need to be
dt.controlsclean[ , specdate_new := as.Date(as.character(specdate_new),'%d%m%Y')]
dt.controlsclean[ , admidate_new:= as.Date(admidate_new)]
dt.controlsclean[ , disdate_lastfce := as.Date(disdate_lastfce)]

l = list(dt.casesclean, dt.controlsclean)
dt.TimeIndep <- rbindlist(l, use.names=TRUE, fill=TRUE)

nrow(dt.TimeIndep)
rm(dt.casesclean)
rm(dt.controlsclean)

####grouping variables
## creating dummy and factor variables - 2 examples below
# needs to be done for your variables of interest

# age
v.agebins <- c(18,45,85,500)
dt.TimeIndep[ , agegrp := findInterval(admiage_miss, v.agebins)]
dt.TimeIndep[ , agegrp := factor(agegrp, labels=c("18-44","45-84",">=85"),
                                 ordered=FALSE)]
dt.TimeIndep[ , agegrp := relevel(agegrp, "18-44")]

## also carried out groupings on e.g. Organisation Type in a similar manner

## recoding sex into a dummy variable where 1 is male, 0 is female
dt.TimeIndep[ , sex_dummy := NA_integer_]
dt.TimeIndep[sex_miss==2, sex_dummy := 0]
dt.TimeIndep[sex_miss==1, sex_dummy := 1]


## creating the time to discharge etc. variables
dt.TimeIndep[ , admi2dis := disdate_lastfce-admidate_new]

# changing same day events to 0.5
dt.TimeIndep[admi2dis==0, admi2dis := 0.5]
dt.TimeIndep[admi2spec==0, admi2spec := 0.5]
dt.TimeIndep[spec2dis==0, spec2dis := 0.5]
dt.TimeIndep[ , los := as.numeric(admi2dis)]

## articificially right censoring to reduce impact of outliers
dt.TimeIndep[admi2dis>45, deadordischarged := 0]
dt.TimeIndep[admi2dis>45, dead := 0]
dt.TimeIndep[admi2dis>45, discharged := 0]
dt.TimeIndep[admi2dis>45, admi2dis := 45]
dt.TimeIndep[admi2spec>45, inf := 0]
dt.TimeIndep[admi2spec>45 , HO := NA]
dt.TimeIndep[admi2spec>45 , COCA := NA]
dt.TimeIndep[admi2spec>45 , COHA := NA]
dt.TimeIndep[admi2spec>45 , carb_res := NA]
dt.TimeIndep[admi2spec>45 , ceph_res := NA]
dt.TimeIndep[admi2spec>45 , cipro_res := NA]
dt.TimeIndep[admi2spec>45 , piptaz_res := NA]
dt.TimeIndep[admi2spec>45 , gen_res := NA]
dt.TimeIndep[admi2spec>45 , RES := NA]
dt.TimeIndep[admi2spec>45, admi2spec := 45]

# censoring those with specimen data at 45
dt.TimeIndep[admi2spec==45, inf := 0]
dt.TimeIndep[admi2spec==45 , HO := NA]
dt.TimeIndep[admi2spec==45 , COCA := NA]
dt.TimeIndep[admi2spec==45 , COHA := NA]
dt.TimeIndep[admi2spec==45 , carb_res := NA]
dt.TimeIndep[admi2spec==45 , ceph_res := NA]
dt.TimeIndep[admi2spec==45 , cipro_res := NA]
dt.TimeIndep[admi2spec==45 , piptaz_res := NA]
dt.TimeIndep[admi2spec==45 , gen_res := NA]
dt.TimeIndep[admi2spec==45 , RES := NA]
dt.TimeIndep[admi2spec==45, admi2spec := 45]


dt.TimeIndep[ , los := as.numeric(admi2dis)]

## flags for if need to use step functions
# this is to be determined by your own analysis
## for death models
dt.TimeIndep[ , flag.Step.Dis := NA_character_]
dt.TimeIndep[admi2dis==8, flag.Step.Dis := "dis8"]
dt.TimeIndep[admi2dis<8, flag.Step.Dis := "disless8"]
dt.TimeIndep[admi2dis>8, flag.Step.Dis := "disgreat8"]

dt.TimeIndep[ , flag.Step.Spec := NA_character_]
dt.TimeIndep[admi2spec==8, flag.Step.Spec := "spec8"]
dt.TimeIndep[admi2spec<8, flag.Step.Spec := "specless8"]
dt.TimeIndep[admi2spec>8, flag.Step.Spec := "specgreat8"]

## for deadordischarged models
# a similar process is performed but relating to t=2 (not t=8)

### grouping non-tested and susceptible, and having non-inf controls = 0
dt.TimeIndep[ , ceph_res2 := 0]
dt.TimeIndep[inf==1, ceph_res2 := 1]
dt.TimeIndep[ceph_res==1, ceph_res2 := 2]
## done for the rest of resistance types of interest

# creating separate resistant and susceptible dummies for cox models
# where "susceptible" also includes non-tested
dt.TimeIndep[ , cephDS := 0]
dt.TimeIndep[ceph_res2==1, cephDS := 1]

dt.TimeIndep[ , cephDR := 0]
dt.TimeIndep[ceph_res2==2, cephDR := 1]
## done for the rest of resistance types of interest

# save dataset

#### CREATING TIME DEPENDENDENT DATASETS

# split out into HO vs CO & controls
dt.HO <- dt.TimeIndep[HO==1]
dt.Other <- dt.TimeIndep[HO==0|is.na(HO)]


# format the CO & controls
dt.Other[ , tstart := as.numeric(0)]
dt.Other[ , tstop := as.numeric(admi2dis)]

dt.Other[ , flagType := "CO"]


# create the time dependent rows for HO
v.nho <- nrow(dt.HO)
dt.HO[ , ID := 1:v.nho]

# create 2 rows for each patient - want the first to represent
# admi2spec and the second to represent spec2dis
dt.HO.dep <- dt.HO[rep(1:v.nho, each=2), ]

dt.HO.dep[ , tstart := 0]
dt.HO.dep[ , tstop := 0]

# new length of data.table
v.nho2 <- nrow(dt.HO.dep)

v.oddrows <- which(!duplicated(dt.HO.dep$ID))
v.evenrows <- v.oddrows+1

# odd rows are admi2spec
dt.HO.dep[v.oddrows, inf := 0]
dt.HO.dep[v.oddrows, dead := 0]
dt.HO.dep[v.oddrows, discharged := 0]
dt.HO.dep[v.oddrows, deadordischarged := 0]
dt.HO.dep[v.oddrows, flagType := "HOA"]

# need to convert admi2spec etc to numeric to allow
# input with rules etc. 
dt.HO.dep[v.oddrows, tstart := as.numeric(0)]
dt.HO.dep[v.oddrows, tstop := as.numeric(admi2spec)]
dt.HO.dep[v.evenrows, tstart := as.numeric(admi2spec)]
dt.HO.dep[v.evenrows, tstop := as.numeric(admi2dis)]
dt.HO.dep[v.evenrows, flagType := "HOB"]

# prep for merging HO and other
# need to create an ID variable for dt.Other
v.nother <- nrow(dt.Other)
dt.Other[ ,ID := (v.nho2+1):(v.nho2+v.nother)]


## because we are censoring at t=45, we previously set admi2spec
# and admi2dis at 45 if larger, for these patients we only use their
# data for t<=45
dt.HO.dep[ , errorflag := 0]
dt.HO.dep[tstart==45 & tstop==45 &
            deadordischarged==0, errorflag:=1]
dt.HO.dep <-  dt.HO.dep[errorflag==0]
dt.HO.dep[ , errorflag := NULL]

# merging to get time dependent dataset

## ensure there are the same covariates in each
# i.e. ensure the variables you want from both dt.Other & dt.HO.dep
# are the same before merging
# make sure columns are the same class

l = list(dt.HO.dep, dt.Other)

dt.TimeDep <- rbindlist(l, use.names=TRUE, fill=TRUE)

# put into numeric class for cox models
dt.TimeDep[ , tstart := as.numeric(tstart)]
dt.TimeDep[ , tstop := as.numeric(tstop)]

# need to adjust for same day tests and discharges
dt.TimeDep[tstart==tstop, tstop := (tstop)+0.5]

## making infection related variables time dependent
## making dummy variables for susceptible and resistant time dependent
dt.TimeDep[ , cephDS := 0]
dt.TimeDep[ceph_res2==1 & inf==1, cephDS := 1]

dt.TimeDep[ , cephDR := 0]
dt.TimeDep[ceph_res2==2 & inf==1, cephDR := 1]
## done for the rest of resistance types of interest

# save dataset 


