## EQUATION LOS

## you have to make sure list number refers to the same as los lists created
# in the "analysis section" of "Length-of-Stay-Git.R"

## average values
fun.variable.creation <- function(x){
  
  # x is the data in data.table format (e.g. dt.TimeDep)
  
  v.n <- length(which(x$inf==1))
  
  p.RES <- ((length(which(x$RESDR==1)))/v.n)
  p.ceph_res <- ((length(which(x$cephDR==1)))/v.n)
  p.cipro_res <- ((length(which(x$ciproDR==1)))/v.n)
  p.gen_res <- ((length(which(x$genDR==1)))/v.n)
  p.piptaz_res <- ((length(which(x$piptazDR==1)))/v.n)
  
  newlist <- list(v.n,p.RES,p.ceph_res,p.cipro_res,p.gen_res,p.piptaz_res)
  
  return(newlist)
  
}



fun.eq.los.res <- function(x,y){
  
  # x is the list of length of stay estimates created after the functions
  # from "clos-git" have been run - see the analysis section of 
  # "Length-of-Stay-Git.R"
  # y is the number corresponsing to the list in the function that represents
  # the resistance variable of interest

  y <- as.numeric(y)
  
  v <- x[[1]]
  v.average <- as.numeric(v$e.phi[1,1])
  
  v <- x[[y]]
  v.res <- as.numeric(v$e.phi[1,1])
  
  p.res <- newlist[[y]]
  
  v.SUS <- (v.average - (p.res*v.res))
  v.RES <-  v.SUS + v.res
  
  l.eq <- list(v.RES, v.SUS)
  return(l.eq)
}

## getting standard errors for these estimates

fun.eq.los.boot <- function(x,y,z,w) {
  
  # x is infection boot (outcome of "boot-clos-Git.R"), 
  # y is resistant boot (outcome of "boot-clos-Git.R"), 
  # z is the data.frame or data.table of sample of interest
  # (e.g. dt.TimeDep)
  # w is resistant type in its dummy format
  # in the returned list [[1]] is susceptible and [[2]] is resistant
  
  w <- deparse(substitute(w))
  v.n <- length(which(z$inf==1))
  
  p.res <- ((length(which(z[[w]]==1)))/v.n)
  
  dt <- data.table(VI=x,VR=y)
  setnames(dt, c("VI","VR"))
  
  #VX - susceptible vs controls
  dt[ , VX := VI - (p.res*VR)]
  # VY - resistant vs controls
  dt[ , VY := VX + VR]
  
  se.RESX <- sqrt(var(dt$VX))
  se.RESY <- sqrt(var(dt$VY))
  
  l.se <- list(se.RESX, se.RESY) # se.RESX is the susceptible SE, se.RESY is resistant SE
  print(l.se)
}
