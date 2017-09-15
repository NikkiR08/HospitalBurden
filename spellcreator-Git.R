library(data.table)
library(dplyr)

fun.spellcreator <- function(x){
  
  # x is a data.table with episodes you want to change into spells
  dt.input <- x
  
  ## dropping NAs for columns needed for spell creation
  dt.input <- na.omit(dt.input, cols=c("procodet", 
                                       "encrypted_hesid",
                                       "epistart_miss","epiend_miss",
                                       "epiorder_miss", 
                                       "epikey",
                                       "dismeth_miss"))
  
  ## generate 3 digit provider code from procodet
  dt.input[ , procode3 := substring(procodet,1,3)]
  
  #use setkey to order in data.table
  setkey(dt.input,encrypted_hesid, epistart_miss,
         epiorder_miss, epiend_miss)
  
  ## create spell conditions used by CHE
  
  # create a transit variable to rank patients with same day events
  dt.input[,transit := 0L]
  
  dt.input[ , admisorc_miss := as.numeric(as.character(admisorc_miss))]
  dt.input[ , admimeth_miss := as.numeric(as.character(admimeth_miss))]
  dt.input[ , disdest_miss := as.numeric(as.character(disdest_miss))]
  
  dt.input[(((admisorc_miss<51|admisorc_miss>53) & admimeth_miss!=81) &
              (disdest_miss>=51 & disdest_miss<=53)), 
           transit := 1]
  
  dt.input[(((admisorc_miss>=51|admisorc_miss<=53) |admimeth_miss==81) &
              (disdest_miss<51|disdest_miss>53)),
           transit := 3]
  
  dt.input[(((admisorc_miss>=51 & admisorc_miss<=53) |admimeth_miss==81) &
              (disdest_miss>=51 & disdest_miss<=53)),
           transit := 2]
  
  
  setkey(dt.input,encrypted_hesid, epistart_miss, 
         epiorder_miss, epiend_miss, transit, admidate_new, 
         disdate_new, epikey)
  
  # removing duplicates in dt.input.table
  dt.input <- dt.input[!duplicated(rleidv(dt.input,
                                          cols=c('encrypted_hesid', 
                                                 'epistart_miss',
                                                 'epiorder_miss',
                                                 'epiend_miss',
                                                 'transit'))), ]
  
  
  # order on important variables
  setkey(dt.input,encrypted_hesid, epistart_miss, epiend_miss, epiorder_miss,
         transit, epikey)
  
  # create spell identifier variable
  dt.input[,spell_che := seq.int(nrow(dt.input)) ]
  
  # create a variable to keep this order for spell creation
  dt.input[ , v.order := 1:nrow(dt.input)]
  setkey(dt.input, v.order)
  
  # creating flags to notify if CHE spell conditions present
  dt.input[, Flag.Spell1 := 0] 
  dt.input[encrypted_hesid == shift(encrypted_hesid, 1L, type="lag"), 
           Flag.Spell1 := 1, by=v.order]
  
  dt.input[, Flag.Spell2 := 0]
  dt.input[procode3 == shift(procode3, 1L, type="lag"), 
           Flag.Spell2 := 1, by=v.order]
  
  dt.input[, Flag.Spell3 := 0]
  dt.input[admidate_new == shift(admidate_new, 1L, type="lag"), 
           Flag.Spell3 := 1, by=v.order]
  
  dt.input[, Flag.Spell4 := 0]
  dt.input[(shift(dismeth_miss, 1L, type="lag"))>5,
           Flag.Spell4 := 1, by=v.order]
  
  dt.input[, Flag.Spell5 := 0]
  dt.input[epistart_miss== shift(epiend_miss, 1L, type="lag"), 
           Flag.Spell5 := 1, by=v.order]
  
  dt.input[, Flag.Spell6 := 0]
  dt.input[epistart_miss== shift(epistart_miss, 1L, type="lag"), 
           Flag.Spell6 := 1, by=v.order];
  
  # flag those who have all the conditions
  dt.input[, spell.cond := 0]
  dt.input[Flag.Spell1==1 &                     
             Flag.Spell2==1 &
             (Flag.Spell3==1 |
                (Flag.Spell3==0 &
                   ((Flag.Spell4==1 & 
                       Flag.Spell5==1) |
                      Flag.Spell6==1))), 
           spell.cond :=1 , by=v.order]
  
  # recode the same spells as  the same spells
  dt.input[, spell_che2 := spell_che] 
  
  dt.input[ , spell_che2 := ifelse(spell.cond==1, (shift(spell_che2,1L,
                                                         type="lag")),
                                   spell_che2)]
  
  ## labeling first FCE
  dt.input[ ,firstFCE := 0]; 
  dt.input[spell_che2 != shift(spell_che2, 1L, type="lag"),
           firstFCE :=1,by=v.order]
  
  # note this makes the first observation coded as 0 - so need to add code
  # to rectify this
  dt.input[1, firstFCE:=1]
  
  ## labeling last FCE
  dt.input[ ,lastFCE := 0]; 
  dt.input[spell_che2 != shift(spell_che2, 1L, type="lead"),
           lastFCE :=1, by=v.order]
  
  # need to make last row last FCE
  nlast <- as.integer(nrow(dt.input))
  dt.input[nlast, lastFCE:=1]
  
  ## dropping middle episodes 
  # assumption is that taking first episode characteristics for the spell
  dt.input <- dt.input[firstFCE==1|lastFCE==1]
  
  ## puting the spell discharge date & discharge method in the first spell row
  dt.input[ , disdate_lastfce := as.character(disdate_new)]
  
  dt.input[ , dismeth_lastfce := as.character(dismeth_miss)]
  
  dt.input[ , disdate_lastfce := ifelse(firstFCE==1 &
                                          lastFCE==0,
                                        (shift(disdate_lastfce,1L,
                                               type="lead")),
                                        disdate_lastfce)]
  
  dt.input[ , dismeth_lastfce := ifelse(firstFCE==1 &
                                          lastFCE==0,
                                        (shift(dismeth_lastfce,1L,
                                               type="lead")),
                                        dismeth_lastfce)]
  
  ## drop lastfce to keep just first fce
  dt.input <- dt.input[firstFCE==1]
  
  ## delete flag columns
  dt.input[ , Flag.Spell1 := NULL]
  dt.input[ , Flag.Spell2 := NULL]
  dt.input[ , Flag.Spell3 := NULL]
  dt.input[ , Flag.Spell4 := NULL]
  dt.input[ , Flag.Spell5 := NULL]
  dt.input[ , Flag.Spell6 := NULL]
  
  
  # remove ones which have invalid admission or discharge dates
  dt.input[admidate_new==invalid4|
             admidate_new==invalid5|
             admidate_new==invalid6|
             admidate_new==invalid7, admidate_new := NA]
  
  dt.input[disdate_lastfce==invalid4|
             disdate_lastfce==invalid5|
             disdate_lastfce==invalid6|
             disdate_lastfce==invalid7, 
           disdate_lastfce := NA]
  
  ## remove those with no disdate or admidate
  dt.input <- na.omit(dt.input, cols=c("admidate_new","disdate_lastfce"))
  
  ## remove those who are coded as discharge method unknown or still in hosp
  dt.input <- dt.input[dismeth_lastfce!=9]
  dt.input <- dt.input[dismeth_lastfce!=8]
  dt.input <- dt.input[dismeth_lastfce!=5]
  
  return(dt.input)
  
}