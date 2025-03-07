## ----echo = FALSE-------------------------------------------------------------
rm(list = ls())
library(EMC2)


## ----results = "hide"---------------------------------------------------------
rm(list = ls())
library(EMC2) 

## ----echo=TRUE, message=FALSE, warning=FALSE, results='hide'------------------
matchfun <- function(d) d$S == d$lR

design_LBA <- design(factors=list(subjects=1,S=c("left", "right")),
                     Rlevels = c("left", "right"),
                     matchfun = matchfun,
                     formula =list(v~lM,B~1, t0~1, sv~1, A~1),
                     constants=c(sv=log(1)),
                     model = LBA)

prior_LBA <- prior(design_LBA, type = "single",
                  pmean = c(1.3, .7, log(.8), log(.2), log(.3)),
                  psd = c(.2, .1, .1, .05, .05))

## ----fig.alt = "Prior for LBA non-hierarchical model", fig.height = 5---------
plot(prior_LBA)

## ----eval=FALSE---------------------------------------------------------------
#  SBC_LBA_single <- run_sbc(design_LBA, prior_LBA, replicates = 500, trials = 100, plot_data = FALSE,
#                    iter = 1000, n_post = 1000, fileName = "SBC_data/SBC_LBA_single.RData",
#                    cores_per_chain = 30)

## ----include=FALSE------------------------------------------------------------
load("SBC_data/SBC_LBA_single.RData")

## ----fig.height = 6, fig.alt = "Histogram SBC LBA single"---------------------
plot_sbc_hist(SBC_LBA_single, bins = 10)

## ----fig.height = 6, fig.alt = "ecdf LBA single"------------------------------
plot_sbc_ecdf(SBC_LBA_single)

## ----echo=TRUE, message=FALSE, warning=FALSE, results='hide', eval = FALSE----
#  #?DDM
#  design_DDM <- design(factors=list(subjects=1,S=c("left", "right")),
#                         Rlevels = c("left", "right"),
#                         formula =list(v~1,a~1, t0~1, s~1, Z~1, sv~1, SZ~1, st0 ~ 1),
#                         constants=c(s=log(1)),
#                         model = DDM)
#  
#  
#  prior_DDM <- prior(design_DDM, type = "single",
#                    pmean = c(1, log(.8), log(.3), qnorm(.5), log(.1), qnorm(.05), log(.05)),
#                    psd = c(.15, .15, .1, .05, .15, .15, .15))
#  
#  SBC_DDM_single <- run_sbc(design_DDM, prior_DDM, replicates = 500, trials = 100,
#                            fileName = "SBC_data/SBC_DDM_single.RData", cores_per_chain = 30)
#  

## ----include=FALSE------------------------------------------------------------
load("SBC_data/SBC_DDM_single.RData")

## ----fig.height = 6, fig.alt = "ecdf DDM single", fig.width= 10---------------
plot_sbc_ecdf(SBC_DDM_single)

## ----echo=TRUE, message=FALSE, warning=FALSE, results='hide'------------------
n_subjects <- 30
# 
matchfun <- function(d) d$S == d$lR

design_LBA <- design(factors=list(subjects=1:n_subjects,S=c("left", "right")),
                     Rlevels = c("left", "right"),
                     matchfun = matchfun,
                     formula =list(v~lM,B~1, t0~1, sv~1, A~1),
                     constants=c(sv=log(1)),
                     model = LBA)

prior_LBA <- prior(design_LBA, type = "diagonal-gamma",
                  mu_mean = c(1.3, .7, log(.8), log(.2), log(.3)),
                  mu_sd = c(.2, .1, .1, .05, .05),
                  shape = 10,
                  rate = c(.2, .2, .2, .1, .1))


## ----fig.alt = "Prior for LBA hierarchical model", fig.height = 5-------------
plot(prior_LBA, selection = "alpha")

## ----eval=FALSE---------------------------------------------------------------
#  SBC_LBA <- run_sbc(design_LBA, prior_LBA, replicates = 500, trials = 100,
#                          n_subjects = n_subjects, fileName = "SBC_data/SBC_LBA.RData", cores_per_chain = 30)

## ----include=FALSE------------------------------------------------------------
load("SBC_data/SBC_LBA.RData")

## ----fig.height = 6, fig.alt = "ECDF SBC LBA"---------------------------------
plot_sbc_ecdf(SBC_LBA)

## ----echo=TRUE, message=FALSE, warning=FALSE, results='hide'------------------
n_subjects <- 30

design_DDM <- design(factors=list(subjects=1:n_subjects,S=c("left", "right")),
                       Rlevels = c("left", "right"),
                       formula =list(v~1,a~1, t0~1, s~1, Z~1, sv~1, SZ~1, st0 ~ 1),
                       constants=c(s=log(1)),
                       model = DDM)


prior_DDM <- prior(design_DDM, type = "diagonal-gamma",
                  pmean = c(1.2, log(.8), log(.3), qnorm(.5), log(.1), qnorm(.05), log(.05)),
                  psd = c(.15, .15, .1, .05, .1, .1, .15),
                  shape = 10,
                  rate = c(.2, .2, .2, .1, .1, .1, .1))


## ----eval=FALSE---------------------------------------------------------------
#  SBC_DDM <- run_sbc(design_DDM, prior_DDM, replicates = 250, trials = 200,
#                          n_subjects = n_subjects, fileName = "SBC_data/SBC_DDM.RData", cores_per_chain = 30)

## ----include=FALSE------------------------------------------------------------
load("SBC_data/SBC_DDM.RData")

## ----fig.height = 7, fig.alt = "ECDF SBC DDM"---------------------------------
plot_sbc_ecdf(SBC_DDM)

## ----echo=TRUE, eval = FALSE--------------------------------------------------
#  n_subjects <- 30
#  
#  matchfun <- function(d) d$S == d$lR
#  
#  
#  design_RDM <- design(factors=list(subjects=1:n_subjects,S=c("left", "right")),
#                       Rlevels = c("left", "right"),
#                       matchfun = matchfun,
#                       formula =list(v~lM,B~1, t0~1, A~1, s ~ 1),
#                       constants=c(s=log(1)),
#                       model = RDM)
#  
#  prior_RDM <- prior(design_RDM, type = "diagonal-gamma",
#                    mu_mean = c(1.4, .3, log(1.5), log(.2), log(.3)),
#                    mu_sd = c(.05, .1, .1, .05, .05),
#                    shape = 10,
#                    rate = c(.1, .2, .2, .2, .1))
#  
#  SBC_RDM <- run_sbc(design_RDM, prior_RDM, replicates = 500, trials = 100,
#                          n_subjects = n_subjects, fileName = "SBC_RDM.RData", cores_per_chain = 30)
#  

## ----include=FALSE------------------------------------------------------------
load("SBC_data/SBC_RDM.RData")

## ----fig.height = 6, fig.alt = "ECDF SBC RDM"---------------------------------
plot_sbc_ecdf(SBC_RDM)

## ----echo=TRUE, eval = FALSE--------------------------------------------------
#  n_subjects <- 30
#  
#  matchfun <- function(d) d$S == d$lR
#  
#  design_LNR <- design(factors=list(subjects=1:n_subjects,S=c("left", "right")),
#                       Rlevels = c("left", "right"),
#                       matchfun = matchfun,
#                       formula =list(m~lM,s~1, t0~1),
#                       model = LNR)
#  
#  prior_LNR <- prior(design_LNR, type = "diagonal-gamma",
#                    mu_mean = c(-.7, -.5, log(1), log(.2)),
#                    mu_sd = c(.2, .1, .1, .05),
#                    shape = 10,
#                    rate = c(.2, .2, .2, .1))
#  
#  SBC_LNR <- run_sbc(design_LNR, prior_LNR, replicates = 500, trials = 100,
#                          n_subjects = n_subjects, fileName = "SBC_LNR.RData", cores_per_chain = 30)
#  

## ----include=FALSE------------------------------------------------------------
load("SBC_data/SBC_LNR.RData")

## ----fig.height = 6, fig.alt = "ECDF SBC RDM"---------------------------------
plot_sbc_ecdf(SBC_LNR, layout = c(2,2))

