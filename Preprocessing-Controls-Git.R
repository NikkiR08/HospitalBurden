### "Preprocessing-Controls-Git.R"

## set working directory

## loading packages that are needed
library(survival)
library(dplyr)
library(lattice)
library(mvna)
library(data.table)

## load data

## data cleaning 
dt.controls1 <- dt.controls1[,c("ACTIVAGE","ADMINCAT","CLASSPAT","FCE",
                                "PROVSPNO",
                                "WARDSTRT","OpertnList") := NULL ]


## dropping those with admiage < 18
dt.controls1[, admiage_miss := as.numeric(ADMIAGE)]
dt.controls1[,ADMIAGE := NULL ]
dt.controls1[admiage_miss==999 |admiage_miss=="" | admiage_miss=="NULL"] <- NA

dt.controls1 <- na.omit(dt.controls1, cols=c("admiage_miss"))
dt.controls1 <- dt.controls1[admiage_miss>=18]

nrow(dt.controls1)

## dates
dt.controls1[, admidate_new := as.Date(as.character(ADMIDATE),'%d%m%Y')]
dt.controls1[,c("ADMIDATE") := NULL ]

dt.controls1[, disdate_new := as.Date(as.character(DISDATE),'%d%m%Y')]
dt.controls1[,DISDATE := NULL ]

dt.controls1[, epistart_new := as.Date(as.character(EPISTART),'%d%m%Y')]
dt.controls1[,EPISTART := NULL ]

dt.controls1[, epiend_new := as.Date(as.character(EPIEND),'%d%m%Y')]
dt.controls1[,EPIEND := NULL ]

dt.controls1[, dob_new := as.Date(as.character(DOB),'%d%m%Y')]
dt.controls1[,DOB := NULL ]

## removing invalid dates
invalid1 <- as.Date("01/07/2011","%d/%m/%Y")
invalid2 <- as.Date("30/06/2012","%d/%m/%Y")
invalid3 <- as.Date("01/01/1930", "%d/%m/%Y")

dt.controls1 <- dt.controls1[admidate_new>=invalid1]
dt.controls1 <- dt.controls1[admidate_new<=invalid2]
dt.controls1 <- dt.controls1[admidate_new!=invalid3]
dt.controls1 <- dt.controls1[disdate_new!=invalid3]

nrow(dt.controls1)

## missing values

dt.controls1[, epiorder_miss := as.numeric(EPIORDER)]
dt.controls1[,EPIORDER := NULL ]
dt.controls1[epiorder_miss==99 | epiorder_miss==98 | epiorder_miss=="" | epiorder_miss=="NULL"] <- NA


dt.controls1[, admimeth_miss := as.numeric(ADMIMETH)]
dt.controls1[,ADMIMETH := NULL ]
dt.controls1[admimeth_miss==99 | admimeth_miss==98 | admimeth_miss=="" | admimeth_miss=="NULL"] <- NA

dt.controls1[, admincatst_miss := as.numeric(ADMINCATST)]
dt.controls1[,ADMINCATST := NULL ]
dt.controls1[admincatst_miss==99 | admincatst_miss==98 | admincatst_miss=="" | admincatst_miss=="NULL"] <- NA


dt.controls1[, admisorc_miss := as.numeric(ADMISORC)]
dt.controls1[,ADMISORC := NULL ]
dt.controls1[admisorc_miss==99 | admisorc_miss==98 | admisorc_miss=="" |admisorc_miss=="NULL"] <- NA

dt.controls1[, admistat_miss:= as.numeric(ADMISTAT)]
dt.controls1[,ADMISTAT := NULL ]
dt.controls1[admistat_miss==99 | admistat_miss==98 | admistat_miss=="" |admistat_miss=="NULL"] <- NA

dt.controls1[, diagcode4list_miss:= as.character(DiagCode4List)]
dt.controls1[,DiagCode4List := NULL ]
dt.controls1[diagcode4list_miss=="" |diagcode4list_miss=="NULL"] <- NA

dt.controls1[, disdest_miss:= as.numeric(as.character(DISDEST))]
dt.controls1[,DISDEST := NULL ]
dt.controls1[disdest_miss==99 | disdest_miss==98 | disdest_miss=="" |disdest_miss=="NULL"] <- NA

dt.controls1[, dismeth_miss:= as.numeric(as.character(DISMETH))]
dt.controls1[,DISMETH := NULL ]
dt.controls1[dismeth_miss==99 | dismeth_miss==98 | dismeth_miss=="" |dismeth_miss=="NULL"] <- NA

invalid4 <- as.Date("01/01/1800","%d/%m/%Y")
invalid5 <- as.Date("01/01/1801","%d/%m/%Y")
invalid6 <- as.Date("01/01/1600","%d/%m/%Y")
invalid7 <- as.Date("15/10/1582","%d/%m/%Y")

dt.controls1[, epidur_miss:= as.numeric(EPIDUR)]
dt.controls1[,EPIDUR := NULL ]
dt.controls1[epidur_miss=="" |epidur_miss=="NULL"] <- NA

dt.controls1[, epiend_miss:= epiend_new]
dt.controls1[,epiend_new := NULL ]
dt.controls1[epiend_miss==invalid4 | epiend_miss==invalid5 |
               epiend_miss==invalid6 | epiend_miss==invalid7] <- NA

dt.controls1[, epistart_miss:= epistart_new]
dt.controls1[,epistart_new := NULL ]
dt.controls1[epistart_miss==invalid4 | epistart_miss==invalid5 | 
               epistart_miss==invalid6 | epistart_miss==invalid7] <- NA

dt.controls1[, ethnos_miss:= ETHNOS]
dt.controls1[,ETHNOS := NULL ]
dt.controls1[ethnos_miss=="" |ethnos_miss=="NULL" |
               ethnos_miss=="99" | ethnos_miss=="Z"] <- NA

dt.controls1[, fyear_miss:= FYEAR]
dt.controls1[,FYEAR := NULL ]

dt.controls1[, mainspef_miss:= MAINSPEF]
dt.controls1[,MAINSPEF := NULL ]
dt.controls1[mainspef_miss=="" |mainspef_miss=="&" | mainspef_miss=="NULL"] <- NA

dt.controls1[, sex_miss:= as.numeric(SEX)]
dt.controls1[,SEX:= NULL ]
dt.controls1[sex_miss=="" |sex_miss==9 | sex_miss==0] <- NA

dt.controls1[, spelbgin_miss:= SPELBGIN]
dt.controls1[,SPELBGIN := NULL ]
dt.controls1[spelbgin_miss=="NULL"| spelbgin_miss == ""] <- NA

dt.controls1[, spelend_miss:= SPELEND]
dt.controls1[,SPELEND := NULL ]
dt.controls1[spelend_miss=="NULL" | spelend_miss==""] <- NA

dt.controls1[, speldur_miss:= SPELDUR]
dt.controls1[,SPELDUR := NULL ]
dt.controls1[speldur_miss=="NULL" | speldur_miss==""] <- NA

dt.controls1[, encrypted_hesid:= ENCRYPTED_HESID]
dt.controls1[,ENCRYPTED_HESID := NULL ]
dt.controls1[, epikey:= EPIKEY]
dt.controls1[,EPIKEY := NULL ]
dt.controls1[, epistat:= EPISTAT]
dt.controls1[,EPISTAT := NULL ]
dt.controls1[, procodet:= PROCODET]
dt.controls1[,PROCODET := NULL ]
dt.controls1[, protype:= PROTYPE]
dt.controls1[,PROTYPE := NULL ]
dt.controls1[, epitype:= EPITYPE]
dt.controls1[,EPITYPE := NULL ]


### remove cases from dt.controls1 

# load case data - dt.casesclean

v.cases <- dt.casesclean[["encrypted_hesid"]]

dt.controls1 <- dt.controls1[!(encrypted_hesid %in% v.cases)]

rm(v.cases)

#### SPELL CREATION

source("spellcreator-Git.R")

dt.input <- fun.spellcreator(dt.controls1)

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

#### sorting out hospital type


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

dt.controlsclean <- dt.output

# save dataset
