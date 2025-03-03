## Authors: Jordi Cortes,  Marta Bofill, and Guadalupe Gomez
## CompAREdesign: Statistical Functions for the Design of Studies with 
## Composite Endpoints
## https://www.nejm.org/doi/full/10.1056/NEJMoa2030186
## =============================================================================

rm(list=ls())

################################################################################
#
# Install and load package
#
################################################################################
# install.packages("CompAREdesign")
library(devtools)  
install_github("CompARE-Composite/CompAREdesign", force = TRUE)
library(CompAREdesign)
library(copula)
library(rootSolve)
library(numDeriv)

path <- 'C:/Users/jcortes/Dropbox/RECERCA/Recerca LMJ/Compare/PACKAGES/Package CompAREdesign/Test_CompAREdesign/'
setwd(path)
source('modified_formulas_for_test.R') # Remove this line
# load('results_2025-02-25.RData')

################################################################################
#
# GRID
#
################################################################################
P0_E1 <- c(0.05,seq(0.1,0.9,0.4))
P0_E2 <- c(0.05,seq(0.1,0.9,0.4))
HR_E1 <- seq(0.7,0.9,0.2)
HR_E2 <- seq(0.7,0.9,0.2)
BETA_E1 <- c(0.5,1,2)
BETA_E2 <- c(0.5,1,2)
CASE  <- 1:4
COPULA <- c('Frank', 'Gumbel', 'Clayton')
RHO <- c(0.1, 0.3, 0.7)
RHO_TYPE <- c('Spearman','Kendall')
ALPHA <- 0.05
POWER <- 0.80 # c(0.80, 0.90)
SS_FORMULA <- c('schoenfeld','freedman')
FOLLOWUP <- 1

dd <- expand.grid(P0_E1 = P0_E1, P0_E2 = P0_E2, HR_E1 = HR_E1, HR_E2 = HR_E2, 
                  BETA_E1 = BETA_E1, BETA_E2 = BETA_E2, CASE = CASE,
                  COPULA = COPULA, RHO = RHO, RHO_TYPE = RHO_TYPE,
                  FOLLOWUP = FOLLOWUP,
                  ALPHA = ALPHA, POWER = POWER, SS_FORMULA = SS_FORMULA, stringsAsFactors = FALSE)
sel_rm <- dd$CASE==4 & (dd$P0_E1 + dd$P0_E2)>=0.95
dd <- dd[-which(sel_rm),]
n_dd <- nrow(dd)

################################################################################
#
# SIMULATION
#
################################################################################
SS_RES  <- matrix(ncol=3, nrow=n_dd)
EFF_RES <- matrix(ncol=4, nrow=n_dd)
ARE_RES <- matrix(ncol=1, nrow=n_dd)
TIM_RES <- matrix(ncol=1, nrow=n_dd)

set.seed(12345)
t0 <- Sys.time()
for(i in 1:n_dd){
  t00 <- Sys.time()
  ##-- Parameters
  p0_e1         = dd$P0_E1[i]
  p0_e2         = dd$P0_E2[i]
  HR_e1         = dd$HR_E1[i]
  HR_e2         = dd$HR_E2[i]
  beta_e1       = dd$BETA_E1[i]
  beta_e2       = dd$BETA_E2[i]
  case          = dd$CASE[i]
  copula        = dd$COPULA[i]
  rho           = dd$RHO[i]
  rho_type      = dd$RHO_TYPE[i]
  followup_time = dd$FOLLOWUP[i]
  alpha         = dd$ALPHA[i]
  power         = dd$POWER[i]
  ss_formula    = dd$SS_FORMULA[i]
  
  ##-- Main functions ----------------------------------------------------------
  ##-- Sample size
  ss_res <- try(samplesize_tte(p0_e1      = p0_e1    , p0_e2    = p0_e2, 
                 HR_e1      = HR_e1    , HR_e2    = HR_e2, 
                 beta_e1    = beta_e1  , beta_e2  = beta_e2,  
                 rho        = rho      , rho_type = rho_type, 
                 copula     = copula   , case     = case, 
                 alpha      = alpha    , power    = power, 
                 ss_formula = ss_formula),silent=TRUE)
  
  if(inherits(ss_res,'try-error')){
    SS_RES[i,] <- rep(NA,3)
  }else{
    SS_RES[i,] <- c(ss_res$ss_E1,ss_res$ss_E2,ss_res$ss_Ec)  
  }
  
  
  
  ##-- ARE
  are_res <- try(ARE_tte(p0_e1      = p0_e1    , p0_e2    = p0_e2, 
          HR_e1      = HR_e1    , HR_e2    = HR_e2, 
          beta_e1    = beta_e1  , beta_e2  = beta_e2,  
          rho        = rho      , rho_type = rho_type, 
          copula     = copula   , case     = case),silent=TRUE)
  
  if(inherits(are_res,'try-error')){
    ARE_RES[i,1] <- NA
  }else{
    ARE_RES[i,1] <- are_res$ARE  
  }
  
  

  
  ##-- Effect size
  eff_res <- try(effectsize_tte(p0_e1      = p0_e1    , p0_e2    = p0_e2, 
                            HR_e1      = HR_e1    , HR_e2    = HR_e2, 
                            beta_e1    = beta_e1  , beta_e2  = beta_e2,  
                            rho        = rho      , rho_type = rho_type, 
                            copula     = copula   , case     = case,
                            followup_time = 1,
                            subdivisions  = 1000, 
                            plot_print    = FALSE, plot_save = FALSE)$effect_size,silent=TRUE)
  if(inherits(eff_res,'try-error')){
    EFF_RES[i,] <- rep(NA,4)
  }else{
    EFF_RES[i,] <- c(eff_res$gAHR,eff_res$AHR,eff_res$RMST_ratio,eff_res$Median_Ratio)  
  }
  
  
  # if(any(is.na(c(SS_RES[i,],ARE_RES[i,1],EFF_RES[i,])))) stop('There are NAs')
  
  ##-- Followup
  t1 <- Sys.time()
  TIM_RES[i] <- round(as.numeric(difftime(t1,t00,units = 'min')),2)
  finish_time <- substr(as.character(Sys.time() + difftime(t1,t0,units = 'min') / i * n_dd),1,19)
  cat('Iteration:',i,'/',n_dd,'time:',TIM_RES[i],'finish:',finish_time,'\n')
  if(i%%1000==0) save.image(paste0(path,"results_",as.character(Sys.Date()),".RData"))
}

save.image(paste0(path,"validation_results.RData"))
