### "Preprocessing-Cases-Git.R"

#set working directory

#loading packages
library(foreign)
library(readstata13)
library(survival)
library(dplyr)
library(lattice)
library(mvna)
library(data.table)

#load data - df.casesraw
#data in the form of data.frame
#ensure have the variables needed/wanted
#remove wrongly coded unique patient identifiers

## recoding  admission and discharge dates
df.casesraw$admidate_new <- as.Date(as.character(df.casesraw$ADMIDATE),'%d%m%Y')
df.casesraw$disdate_new <- as.Date(as.character(df.casesraw$DISDATE), '%d%m%Y')
df.casesraw$epistart_new <- as.Date(as.character(df.casesraw$EPISTART), '%d%m%Y')
df.casesraw$epiend_new <- as.Date(as.character(df.casesraw$EPIEND), '%d%m%Y')
df.casesraw$dob_new <- as.Date(as.character(df.casesraw$DOB), '%d%m%Y')

## dropping 'old' versions of the variables
df.casesraw$ADMIDATE <- NULL
df.casesraw$DISDATE <- NULL
df.casesraw$EPISTART <- NULL
df.casesraw$EPIEND <- NULL
df.casesraw$DOB <- NULL

## dropping those with admiage < 18
df.casesraw$admiage_miss <- df.casesraw$ADMIAGE
df.casesraw$admiage_miss[df.casesraw$admiage_miss==999] <- NA
df.casesraw$ADMIAGE <- NULL
df.casesraw <- na.omit(df.casesraw, cols=c("admiage_miss"))
df.casesraw <- subset(df.casesraw, admiage_miss>=18)


## removing  cases coded as missing admission/discharge dates
invalid1 <- as.Date("01/01/1930", "%d/%m/%Y")
df.casesraw <- subset(df.casesraw, admidate_new!=invalid1)
df.casesraw <- subset(df.casesraw, disdate_new!=invalid1)

## removing those with codes "00" as the start 
df.casesraw$year <- substr(df.casesraw$admidate_new,1,2)
df.casesraw <- subset(df.casesraw, year!="00")

## missing values & recoding variables to write class
df.casesraw$sex_miss <- df.casesraw$SEX
df.casesraw$sex_miss[df.casesraw$sex_miss==0 | 
                       df.casesraw$sex_miss==9] <- NA
df.casesraw$SEX <- NULL

df.casesraw$dismeth <- df.casesraw$DISMETH

df.casesraw$epiorder_miss <- df.casesraw$EPIORDER
df.casesraw$epiorder_miss[df.casesraw$epiorder_miss==99 | 
                            df.casesraw$epiorder_miss==98] <- NA
df.casesraw$EPIORDER <- NULL

df.casesraw$admimeth_miss <- df.casesraw$ADMIMETH
df.casesraw$admimeth_miss[df.casesraw$admimeth_miss==99 |
                            df.casesraw$admimeth_miss==98 ] <- NA
df.casesraw$ADMIMETH <- NULL

df.casesraw$admisorc_miss <- df.casesraw$ADMISORC
df.casesraw$admisorc_miss[df.casesraw$admisorc_miss==99 | 
                            df.casesraw$admisorc_miss==98] <- NA
df.casesraw$ADMISORC <- NULL

df.casesraw$admistat_miss <- df.casesraw$ADMISTAT
df.casesraw$admistat_miss[df.casesraw$admistat_miss==99 | 
                            df.casesraw$admistat_miss==98] <- NA
df.casesraw$ADMISTAT <- NULL

df.casesraw$diagcode4list_miss <- df.casesraw$DiagCode4List
df.casesraw$diagcode4list_miss[df.casesraw$diagcode4list_miss=="null"] <- NA
df.casesraw$DiagCode4List <- NULL

df.casesraw$disdest_miss <- df.casesraw$DISDEST
df.casesraw$disdest_miss[df.casesraw$disdest_miss==99 | 
                           df.casesraw$disdest_miss==98] <- NA
df.casesraw$DISDEST <- NULL

df.casesraw$dismeth_miss <- df.casesraw$DISMETH
df.casesraw$dismeth_miss[df.casesraw$dismeth_miss==9]<- NA
df.casesraw$DISMETH <- NULL

df.casesraw$dismeth_miss <- as.character(df.casesraw$dismeth_miss)
df.casesraw$dismeth_miss <- as.numeric(df.casesraw$dismeth_miss)
df.casesraw$dismeth_miss[df.casesraw$dismeth_miss=="NA"] <- NA

invalid4 <- as.Date("01/01/1800","%d/%m/%Y")
invalid5 <- as.Date("01/01/1801","%d/%m/%Y")
invalid6 <- as.Date("01/01/1600","%d/%m/%Y")
invalid7 <- as.Date("15/10/1582","%d/%m/%Y")


df.casesraw$epidur_miss <- df.casesraw$EPIDUR
df.casesraw$epidur_miss[df.casesraw$epidur_miss=="null"] <- NA
df.casesraw$EPIDUR <- NULL

df.casesraw$epiend_miss <- df.casesraw$epiend_new
df.casesraw$epiend_miss[df.casesraw$epiend_miss==invalid4|
                          df.casesraw$epiend_miss==invalid5|
                          df.casesraw$epiend_miss==invalid6|
                          df.casesraw$epiend_miss==invalid7] <- NA
df.casesraw$epiend_new <- NULL

df.casesraw$epistart_miss <- df.casesraw$epistart_new
df.casesraw$epistart_miss[df.casesraw$epistart_miss==invalid4 |
                            df.casesraw$epistart_miss==invalid5 |
                            df.casesraw$epistart_miss==invalid6 |
                            df.casesraw$epistart_miss==invalid7] <- NA
df.casesraw$epistart_new <- NULL

## removing those with codes "00" as the start 
df.casesraw$year <- substr(df.casesraw$epistart_miss,1,2)
df.casesraw <- subset(df.casesraw, year!="00")
df.casesraw$year <- substr(df.casesraw$epiend_miss,1,2)
df.casesraw <- subset(df.casesraw, year!="00")
df.casesraw$year <- NULL

df.casesraw$ethnos_miss <- df.casesraw$ETHNOS
df.casesraw$ethnos_miss[df.casesraw$ethnos_miss=="99"|
                          df.casesraw$ethnos_miss=="Z"] <- NA
df.casesraw$ETHNOS <- NULL

df.casesraw$epistat <- df.casesraw$EPISTAT
df.casesraw$procodet <- df.casesraw$PROCODET
df.casesraw$protype <- df.casesraw$PROTYPE
df.casesraw$epitype <- df.casesraw$EPITYPE
df.casesraw$encrypted_hesid <- df.casesraw$ENCRYPTED_HESID
df.casesraw$epikey <- df.casesraw$EPIKEY

df.casesraw$ENCRYPTED_HESID <- NULL
df.casesraw$EPIKEY <- NULL
df.casesraw$DISMETH<- NULL
df.casesraw$EPISTAT<- NULL
df.casesraw$PROCODET<- NULL
df.casesraw$PROTYPE<- NULL
df.casesraw$EPITYPE<- NULL


#### creating spells

source("spellcreator-Git.R")

dt.casesraw <- as.data.table(df.casesraw)
rm(df.casesraw)

dt.input <- fun.spellcreator(dt.casesraw)

#### creadting variables of interest
dt.input[ , dead := 0L]
dt.input[dismeth_lastfce==4, dead := 1] 


dt.input[ , discharged := 0L]
dt.input[dismeth_lastfce==1|dismeth_lastfce==2|
              dismeth_lastfce==3, discharged := 1]


dt.input[ , deadordischarged := 0L]
dt.input[dead==1|discharged==1, deadordischarged := 1]


#### modified Elixhauser 

source("elixcreator-Git.R")

dt.output <- fun.elix(dt.input)

####case only coding
###coding HO, CO, COHA 

## reformat specimen date
dt.output[ , syear := substring(dt.output$datespecimen,1,4)]
dt.output[ , smonth := substring(dt.output$datespecimen,6,7)]
dt.output[ , sday := substring(dt.output$datespecimen,9,10)]
dt.output[ ,datespecimen2 :=  with(dt.output,paste0(sday,smonth,syear))]
dt.output[ ,specdate_new := as.Date(as.character(datespecimen2), '%d%m%Y')]

dt.output[ , admi2spec := specdate_new-admidate_new]
dt.output[ , disdate_lastfce := as.Date(disdate_lastfce)]
dt.output[ , dis2spec := specdate_new-disdate_lastfce]
dt.output[ , spec2dis := disdate_lastfce-specdate_new]

# remove spells where admission was >14 days from specimen
# as we are classifying these as non-related
dt.output <- dt.output[admi2spec>=(-14)]
# removing spells where discharge was > 14 days before specimen
dt.output <- dt.output[dis2spec<=(14)]

## discharge before specimen flag
dt.output[ , disb4spec := 0L]
dt.output[dis2spec>0, disb4spec :=1]

## HO flag
dt.output[ , HO := 0L]
dt.output[admi2spec>=2 , HO := 1]

## CO flag
dt.output[ ,CO := 0L]
dt.output[admi2spec<2,CO := 1]


## COHA + COCA flag
#HA
dt.output[ ,HA := 0L]
dt.output[(dis2spec<=14 & dis2spec>0),HA := 1]

#count HA by person/hesid - since in this dataset it is just 1 recorded spell
#per person
setkey(dt.output, encrypted_hesid, spell_che2)
dt.output[ , HAperson := sum(HA) , by=encrypted_hesid]

# now can drop ones where they were discharged prior to specimen
dt.output <- dt.output[disb4spec==0]

# COCA
dt.output[ , COCA := 0L]
dt.output[(CO==1 & HAperson==0), COCA := 1]

# COHA
dt.output[ , COHA := 0L]
dt.output[(CO==1 & HAperson>=1), COHA := 1]


### cooding resistance results #
# for example...
dt.output[ , ceph_res := 9]
dt.output[ , ceftazadime := as.character(ceftazadime)]
dt.output[ , cefotaxime := as.character(cefotaxime)]
dt.output[(cefotaxime=="SUSCEPTIBLE"|ceftazadime=="SUSCEPTIBLE"), ceph_res := 0]
dt.output[(cefotaxime=="RESISTANT"|ceftazadime=="RESISTANT"), ceph_res := 1]
dt.output[ ceph_res==9, ceph_res := NA]
# this was done for all resistance types


###sorting out hospital type

#link in data from ERIC 
#(not included in this version - needs to be added in dependent on your data)

# rename the organisation code to match dt.output (i.e to have same merge variable)
setnames(dt.hospitaltype,"Organisation.Code", "procode3")

# make sure both character class
dt.hospitaltype[ , procode3 := as.character(procode3)]
dt.output[ , procode3 := as.character(procode3)]
dt.output <- merge(dt.output, dt.hospitaltype, by="procode3", all.x=TRUE)

# if want to order by spell numbers;
setkey(dt.output, spell_che2)

## drop unwanted types - keeping acute for now
dt.output <- dt.output[Organisation.Type=="ACUTE - LARGE"|
                                 Organisation.Type=="ACUTE - MEDIUM"|
                                 Organisation.Type=="ACUTE - MULTI-SERVICE"|
                                 Organisation.Type=="ACUTE - SMALL"|
                                 Organisation.Type=="ACUTE - SPECIALIST"|
                                 Organisation.Type=="ACUTE - TEACHING"]

dt.casesclean <- dt.output

# save dataset
