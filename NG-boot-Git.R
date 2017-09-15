########### reference for this code:
#code written by Nathan Green - Imperial College London
# as part of the following study: 
# https://academic.oup.com/jpids/article/4/4/305/2579995/Quantifying-the-Burden-of-Hospital-Acquired

library(data.table)
library(survival)
library(plyr)
library(mvna)
library(etm)
library(data.table)

fun.boot.clos.dtable <- function(data, state.names, tra, cens.name, s = 0, nboot) {
  # data.table version of boot_clos()
  # faster runtime
  # data: class(data.table)
  #
  res <- double(nboot)
  
  ## set id as the searchable key column
  setkey(data, "id")
  
  for (i in seq_len(nboot)) {
    
    index <- sample(unique(data[ ,id]), replace=TRUE)
    
    dboot <- data[J(index)]
    tr.prob <- etm(dboot, state.names, tra, cens.name, s, cova = FALSE)
    res[i] <- etm::clos(tr.prob)$e.phi
    gc()
  }
  res
}
