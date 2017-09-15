## see codebook for more information

library(data.table)
library(dplyr)

fun.elix <- function(x){
  
  # x is a data.table
  
  x[ , mergeid := seq.int(nrow(x))]
  
  dt.elix <- dt.input[ ,.(mergeid,diagcode4list_miss)]
  
  ## Making sure that d$icd1 is in character format
  dt.elix[ , mergeid := as.character(mergeid)]
  dt.elix[ , icd1 := as.character(diagcode4list_miss)]
  dt.elix[ ,diagcode4list_miss := NULL]
  
  ## Congestive heart failure
  
  v.chf <- "I099|I110|I130|I132|I255|I420|I425|I426|
  I427|I428|I429|I43X|I50X|P290"
  dt.elix[ , chf:= 0L]
  dt.elix[grepl(v.chf,dt.elix$icd1), chf := 1]
  rm(v.chf)
  
  ## CARDIAC ARRHYTMIAS
  v.carr <- "I441|I442|I443|I456|I459|I47X|I48x|I49X|R000|
  R001|R008|T821|Z450|Z950"
  dt.elix[ , carr:= 0L]
  dt.elix[grepl(v.carr, dt.elix$icd1), carr := 1 ]
  rm(v.carr)
  
  ## VALVULAR DISEASE
  v.val <- "A520|I05X|I06X|I07X|I08X|I091|I098|I34X|I35X|I36X|I37X|I38X|
  I39X|Q230|Q231|Q232|Q233|Z952|Z953|Z954"
  dt.elix[ , val := 0L]
  dt.elix[grepl(v.val, dt.elix$icd1), val := 1 ]
  rm(v.val)
  
  ## PULMONARY CIRCULATION DISORDERS
  v.pul <- "I26X|I27X|I280|I288|I289"
  dt.elix[ , pul := 0L]
  dt.elix[grepl(v.pul, dt.elix$icd1), pul := 1 ]
  rm(v.pul)
  
  
  ## Peripheral Vascular Disease
  v.pvd <- "I70X|I71X|I731|I738|I739|I771|I790|I792|K551|K558|K559|
  Z958|Z959"
  dt.elix[ , pvd := 0L]
  dt.elix[grepl(v.pvd, dt.elix$icd1), pvd := 1 ]
  rm(v.pvd)
  
  ## HYPERTENSION, UNCOMPLICATED
  v.hyu <- "I10X"
  dt.elix[ , hyu := 0L]
  dt.elix[grepl(v.hyu, dt.elix$icd1), hyu := 1 ]
  rm(v.hyu)
  
  
  ## HYPERTENSION, COMPLICATED
  v.hyc <- "I11X|I12X|I13X|I15X"
  dt.elix[ , hyc := 0L]
  dt.elix[grepl(v.hyc, dt.elix$icd1), hyc := 1 ]
  rm(v.hyc)
  
  ## PARALYSIS
  v.par <- "G041|G114|G801|G802|G81X|G82X|G830|G831|G832|G833|G834|
  G839"
  dt.elix[ , par := 0L]
  dt.elix[grepl(v.par, dt.elix$icd1), par := 1 ]
  rm(v.par)
  
  ## OTHER NEUROLOGICAL DISORDERS
  v.neu <- "G10X|G11X|G12X|G13X|G20X|G21X|G22X|G254|G255|G312|G318|
  G319|G32X|G35X|G36X|G37X|G40X|G41X|G931|G934|R470|R56X"
  dt.elix[ , neu := 0L]
  dt.elix[grepl(v.neu, dt.elix$icd1), neu := 1 ]
  rm(v.neu)
  
  ## CHRONIC PULMONARY DISEASE
  v.cpd <- "I278|I279|J40X|J41X|J42X|J43X|J44X|J45X|J46X|J47X|
  J60X|J61X|J62X|J63X|J64X|J65X|J66X|J67X|J684|J701|J703"
  dt.elix[ , cpd := 0L]
  dt.elix[grepl(v.cpd, dt.elix$icd1), cpd := 1 ]
  rm(v.cpd)
  
  ## DIABETES UNCOMPLICATED
  v.diu <-"E100|E101|E109|E110|E111|E119|E120|E121|E129|
  E130|E131|E139|E140|E141|E149"
  dt.elix[ , diu := 0L]
  dt.elix[grepl(v.diu, dt.elix$icd1), diu := 1 ]
  rm(v.diu)
  
  ## DIABETES COMPLICATED
  v.dic <- "E102|E103|E104|E105|E106|E107|E108|
  E112|E113|E114|E115|E116|E117|E118|E122|E123|E124|E125|E126|E127|E128|E132|
  E133|E134|E135|E136|E137|E138|E142|E143|E144|E145|E146|E147|E148"
  dt.elix[ , dic := 0L]
  dt.elix[grepl(v.dic, dt.elix$icd1), dic := 1 ]
  rm(v.dic)
  
  ## HYPOTHYROIDISM
  v.hyp <- "E00X|E01X|E02X|E03X|E890"
  dt.elix[ , hyp := 0L]
  dt.elix[grepl(v.hyp, dt.elix$icd1), hyp := 1 ]
  rm(v.hyp)
  
  ## RENAL FAILURE
  v.ref <- "I120|I131|N18X|N19X|N250|Z490|Z491|Z492|
  Z940|Z992"
  dt.elix[ , ref := 0L]
  dt.elix[grepl(v.ref, dt.elix$icd1), ref := 1 ]
  rm(v.ref)
  
  ## LIVER DISEASE 
  v.lid <- "B18X|I85X|I864|I982|K70|K711|K713|
  K714|K715|K717X|K72X|K73X|K74X|K760|K762|
  K763|K764|K765|K766|K767|K768|K769|Z944"
  dt.elix[ , lid := 0L]
  dt.elix[grepl(v.lid, dt.elix$icd1), lid := 1 ]
  rm(v.lid)
  
  ## PEPTIC ULCER EXC BLEEDING
  v.pep <- "K257|K259|K267|K269|K277|K279|K287|K289"
  dt.elix[ , pep := 0L]
  dt.elix[grepl(v.pep, dt.elix$icd1), pep := 1 ]
  rm(v.pep)
  
  ## AIDS/HIV 
  v.hiv <- "B20X|B21X|B22X|B24X"
  dt.elix[ , hiv := 0L]
  dt.elix[grepl(v.hiv, dt.elix$icd1), hiv := 1 ]
  rm(v.hiv)
  
  ## LYMPHOMA
  v.lym <- "C81X|C82X|C83X|C84X|C85X|C88X|C96X|C900|C902"
  dt.elix[ , lym := 0L]
  dt.elix[grepl(v.lym, dt.elix$icd1), lym := 1 ]
  rm(v.lym)
  
  ## METS
  v.met <- "C77X|C78X|C79X|C80X"
  dt.elix[ , met := 0L]
  dt.elix[grepl(v.met, dt.elix$icd1), met := 1 ]
  rm(v.met)
  
  ## SOLID TUMOUR W/O METS
  v.tum <- "C00X|C01X|C02X|C03X|C04X|C05X|C06X|C07X|C08X|C09X|C10X|
  C11X|C12X|C13X|C14X|C15X|C16X|C17X|C18X|C19X|C20X|
  C21X|C22X|C23X|C24X|C25X|C26X|C30X|C31X|C32X|C33X|C34X|
  C37X|C38X|C39X|C40X|C41X|C43X|C45X|C46X|C47X|C48X|C49X|
  C50X|C51X|C52X|C53X|C54X|C55X|C56X|C57X|C58X|C60X|C61X|
  C62X|C63X|C64X|C65X|C66X|C67X|C68X|C69X|C70X|C71X|C72X|
  C73X|C74X|C75X|C76X|C97X"
  dt.elix[ , tum := 0L]
  dt.elix[grepl(v.tum, dt.elix$icd1), tum := 1 ]
  rm(v.tum)
  
  ## RHEMATOID ARTHRITIS/COLLAGEN VASCULAR DISEASES
  v.rhe <- "L940|L941|L943|M05X|M06X|M08X|M120|M123|
  M30X|M310|M311|M312|M313|M32X|M33X|M34X|M35X|M45X|
  M461|M468|M469"
  dt.elix[ , rhe := 0L]
  dt.elix[grepl(v.rhe, dt.elix$icd1), rhe := 1 ]
  rm(v.rhe)
  
  ## COAGULOPATHY
  v.coa <- "D65|D66|D67|D68X|D691|D693|D694|D695|D696"
  dt.elix[ , coa := 0L]
  dt.elix[grepl(v.coa, dt.elix$icd1), coa := 1 ]
  rm(v.coa)
  
  ## OBESITY
  v.obe <- "E66X"
  dt.elix[ , obe := 0L]
  dt.elix[grepl(v.obe, dt.elix$icd1), obe := 1 ]
  rm(v.obe)
  
  ## WEIGHT LOSS
  v.wl <- "E40X|E41X|E42X|E43X|E44X|E45X|E46X|R634|R64"
  dt.elix[ , wl := 0L]
  dt.elix[grepl(v.wl, dt.elix$icd1), wl := 1 ]
  rm(v.wl)
  
  ## FLUID AND ELECTROLYTE DISORDERS
  v.fle <- "E222|E86X|E87X"
  dt.elix[ , fle := 0L]
  dt.elix[grepl(v.fle, dt.elix$icd1), fle := 1 ]
  rm(v.fle)
  
  ## BLOOOD LOSS ANEMIA
  v.bla <- "D500"
  dt.elix[ , bla := 0L]
  dt.elix[grepl(v.bla, dt.elix$icd1), bla := 1 ]
  rm(v.bla)
  
  ## DEFICIENCY ANEMIA
  v.dan <- "D508|D509|D51X|D52X|D53X"
  dt.elix[ , dan := 0L]
  dt.elix[grepl(v.dan, dt.elix$icd1), dan := 1 ]
  rm(v.dan)
  
  ## ALCOHOL ABUSE
  v.alc <- "F10|E52|G621|I426|K292|K700|K703|K709|
  T51X|Z502|Z714|Z721"
  dt.elix[ , alc := 0L]
  dt.elix[grepl(v.alc, dt.elix$icd1), alc := 1 ]
  rm(v.alc)
  
  ## DRUG ABUSE
  v.dru <- "F11X|F12X|F13X|F14X|F15X|F16X|F18X|F19X|Z715|Z722"
  dt.elix[ , dru := 0L]
  dt.elix[grepl(v.dru, dt.elix$icd1), dru := 1 ]
  rm(v.dru)
  
  ## PSCHOSES
  v.psy <- "F20X|F22X|F23X|F24X|F25X|F28X|F29X|F302|F312|F315"
  dt.elix[ , psy := 0L]
  dt.elix[grepl(v.psy, dt.elix$icd1), psy := 1 ]
  rm(v.psy)
  
  ## DEPRESSION
  v.dep <- "F204|F313|F314|F315|F32X|F33X|F341|F412|F432"
  dt.elix[ , dep:= 0L]
  dt.elix[grepl(v.dep, dt.elix$icd1), dep := 1 ]
  rm(v.dep)
  
  ## ICD10 weights (from Bottle & Aylin, 2011, 2008-09 inpatient weights)
  
  dt.elix[ , chf_weight := 10L]
  dt.elix[ , carr_weight := 9L]
  dt.elix[ , val_weight := 1L]
  dt.elix[ , pul_weight := 8L]
  dt.elix[ , pvd_weight := 6L]
  dt.elix[ , hyu_weight := 4L]
  dt.elix[ , hyc_weight := -1L]
  dt.elix[ , par_weight := 7L]
  dt.elix[ , neu_weight := 8L]
  dt.elix[ , cpd_weight := 4L]
  dt.elix[ , diu_weight := 2L]
  dt.elix[ , dic_weight := 0L]
  dt.elix[ , hyp_weight := 2L]
  dt.elix[ , ref_weight := 10L]
  dt.elix[ , lid_weight := 10L]
  dt.elix[ , pep_weight := 5L]
  dt.elix[ , hiv_weight := 0L]
  dt.elix[ , lym_weight := 9L]
  dt.elix[ , met_weight := 15L]
  dt.elix[ , tum_weight := 6L]
  dt.elix[ , rhe_weight := 3L]
  dt.elix[ , coa_weight := 5L]
  dt.elix[ , obe_weight := -2L]
  dt.elix[ , bla_weight := 3L]
  dt.elix[ , wl_weight := 9L]
  dt.elix[ , fle_weight := 13L]
  dt.elix[ , dan_weight := 3L]
  dt.elix[ , alc_weight := 0L]
  dt.elix[ , dru_weight := -6L]
  dt.elix[ , psy_weight := 4L]
  dt.elix[ , dep_weight := 0L]
  
  
  ## summing indices with weights
  dt.elix[ ,chfw := chf * chf_weight]
  dt.elix[ , carrw := carr * carr_weight]
  dt.elix[ , valw := val * val_weight]
  dt.elix[ , pulw := pul * pul_weight]
  dt.elix[ , pvdw := pvd * pvd_weight]
  dt.elix[ , hyuw := hyu * hyu_weight]
  dt.elix[ , parw := par * par_weight]
  dt.elix[ , neuw := neu * neu_weight]
  dt.elix[ , cpdw := cpd * cpd_weight]
  dt.elix[ , diuw := diu * diu_weight]
  dt.elix[ , dicw := dic * dic_weight]
  dt.elix[ , refw := ref * ref_weight]
  dt.elix[ , lidw := lid * lid_weight]
  dt.elix[ , pepw := pep * pep_weight]
  dt.elix[ , lymw := lym * lym_weight]
  dt.elix[ , metw := met * met_weight]
  dt.elix[ , tumw := tum * tum_weight]
  dt.elix[ , rhew := rhe * rhe_weight]
  dt.elix[ , coaw := coa * coa_weight]
  dt.elix[ , obew := obe * obe_weight]
  dt.elix[ , wlw  := wl * wl_weight]
  dt.elix[ , flew := fle * fle_weight]
  dt.elix[ , blaw := bla * bla_weight]
  dt.elix[ , danw := dan * dan_weight]
  dt.elix[ , alcw := alc * alc_weight]
  dt.elix[ , druw := dru * dru_weight]
  dt.elix[ , psyw := psy * psy_weight]
  dt.elix[ , depw := dep * dep_weight]
  
  ## create final index column
  dt.elix[ , elix := rowSums(.SD), 
           .SDcols =c("chfw","carrw","valw","pulw",
                      "pvdw","hyuw", "parw", "neuw",
                      "cpdw", "diuw", "dicw" , "refw",
                      "lidw", "pepw", "lymw", "metw", 
                      "tumw", "rhew", "coaw", "obew", 
                      "wlw", "flew" , "blaw", "danw", 
                      "alcw", "druw", "psyw","depw")]
  
  ## remove the individual comorb bins
  dt.elix <- dt.elix[ ,.(mergeid,elix)]
  
  ## to ensure same class
  dt.input[ , mergeid := as.character(mergeid)]
  
  dt.output <- merge(dt.input, dt.elix, by="mergeid", all.x=TRUE)
  
  # if want to order by spell numbers;
  setkey(dt.output, spell_che2)
  
  return(dt.output)
}