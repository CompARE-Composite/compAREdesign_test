##-- Remove objects in memory
rm(list=ls())

##-- Load libraries
library(ggplot2)

##-- Load data
load('../data/validation_results.RData')

##-- Cbind data
colnames(ARE_RES) <- 'ARE'
colnames(SS_RES)  <- c('ss_e1','ss_e2','ss_ce')
colnames(EFF_RES) <- c('gAHR','AHR','RMSTR','MR')
colnames(TIM_RES) <- 'minutes_execution'

d <- as.data.frame(cbind(dd,ARE_RES,SS_RES,EFF_RES,TIM_RES))
colnames(d)[15] <- 'ARE'
colnames(d)[23] <- 'minutes_execution'
for(i in 1:length(d)) if(class(d[,i])=='character') d[,i] <- as.factor(d[,i])
summary(d)
write.table(x = d,file = '../table/validation_results.csv',append = FALSE,sep = ',',row.names = FALSE,col.names = TRUE)

#-------------------------------------------------------------------------------
# Clean Dataset
#-------------------------------------------------------------------------------
summary(d$ARE)
summary(d$ss_ce)
summary(d$gAHR)

##-- Registers to remove (rm)
sel_rm_are  <- d$ARE==Inf   | is.na(d$ARE)               # NA or Inf in ARE
sel_rm_ss   <- d$ss_ce==Inf | is.na(d$ss_ce)             # NA or Inf in Sample size
sel_rm_gAHR <- d$gAHR==Inf  | is.na(d$gAHR) | d$gAHR>10  # NA or Inf or >10 in gAHR

sel_rm <- sel_rm_are | sel_rm_ss | sel_rm_gAHR           # Selected registers to remove

##-- Descriptive analysis of the unstable results
t_ARE  <- table(sel_rm)
t_ss   <- table(sel_rm_ss)
t_gAHR <- table(sel_rm_gAHR)
t_rm   <- table(sel_rm)

prop_t_ARE  <- prop.table(t_ARE)
prop_t_ss   <- prop.table(t_ss)
prop_t_gAHR <- prop.table(t_gAHR)
prop_t_rm   <- prop.table(t_rm)

d[sel_rm,]

##-- Clean dataset
d_com <- d[-which(sel_rm),]

##-- Summary
summary(d_com$ARE)
summary(d_com$ss_ce)
summary(d_com$gAHR)

##-- Labels
d_com$P0_E2_lab    <- paste0('p0_e2 = ',d_com$P0_E2)
d_com$HR_E2_lab    <- paste0('HR_e2 = ',d_com$HR_E2)
d_com$BETA_E2_lab  <- paste0('beta_e2 = ',d_com$BETA_E2)
d_com$COPULA_LAB   <- factor(paste0('copula = ',d_com$COPULA), 
                             levels = c('copula = Frank', 'copula = Gumbel', 'copula = Clayton'))
d_com$RHO_TYPE_LAB <- factor(paste0('rho_type = ',d_com$RHO_TYPE),
                             levels = c('rho_type = Spearman','rho_type = Kendall'))


#-------------------------------------------------------------------------------
# ARE
#-------------------------------------------------------------------------------
##-- ARE according to probabilities --------------------------------------------
ggplot(d_com, aes(x=as.factor(P0_E1),y=ARE)) + 
  facet_wrap(.~P0_E2_lab) +
  geom_boxplot() +
  xlab('p0_e1') +
  scale_y_log10()
ggsave(filename = '../figures/check_are_prob.png', width = 4, height = 4)

##-- ARE according to HRs ------------------------------------------------------
ggplot(d_com, aes(x=as.factor(HR_E1),y=ARE)) + 
  facet_wrap(.~HR_E2_lab) +
  geom_boxplot() +
  xlab('HR_e1') +
  scale_y_log10()
ggsave(filename = '../figures/check_are_HR.png', width = 4, height = 2)


##-- ARE according to betas (shape parameter of weibull) -----------------------
ggplot(d_com, aes(x=as.factor(BETA_E1),y=ARE)) + 
  facet_wrap(.~BETA_E2_lab) +
  geom_boxplot() +
  xlab('beta_e1') +
  scale_y_log10()
ggsave(filename = '../figures/check_are_beta.png', width = 6, height = 2)


##-- ARE according to rho and copula -------------------------------------------
ggplot(d_com, aes(x=as.factor(RHO), y=ARE)) + 
  facet_grid(RHO_TYPE_LAB~COPULA_LAB) +
  geom_boxplot() +
  xlab('rho/tau') +
  scale_y_log10()
ggsave(filename = '../figures/check_are_rho.png', width = 6, height = 4)

#-------------------------------------------------------------------------------
# Sample size
#-------------------------------------------------------------------------------

##-- SS according to probabilities ---------------------------------------------
ggplot(d_com, aes(x=as.factor(P0_E1),y=ss_ce)) + 
  facet_wrap(.~P0_E2_lab) +
  geom_boxplot() +
  xlab('p0_e1') +
  scale_y_log10()
ggsave(filename = '../figures/check_ss_prob.png', width = 4, height = 4)

##-- SS according to HRs -------------------------------------------------------
ggplot(d_com, aes(x=as.factor(HR_E1),y=ss_ce)) + 
  facet_wrap(.~HR_E2_lab) +
  geom_boxplot() +
  xlab('HR_e1') +
  scale_y_log10()
ggsave(filename = '../figures/check_ss_HR.png', width = 4, height = 2)


##-- SS according to betas (shape parameter of weibull) ------------------------
ggplot(d_com, aes(x=as.factor(BETA_E1),y=ss_ce)) + 
  facet_wrap(.~BETA_E2_lab) +
  geom_boxplot() +
  xlab('beta_e1') +
  scale_y_log10()
ggsave(filename = '../figures/check_ss_beta.png', width = 6, height = 2)


##-- SS according to rho and copula --------------------------------------------
ggplot(d_com, aes(x=as.factor(RHO), y=ss_ce)) + 
  facet_grid(RHO_TYPE_LAB~COPULA_LAB) +
  geom_boxplot() +
  xlab('rho/tau') +
  scale_y_log10()
ggsave(filename = '../figures/check_ss_rho.png', width = 6, height = 4)

#-------------------------------------------------------------------------------
# Effect size
#-------------------------------------------------------------------------------

##-- SS according to probabilities ---------------------------------------------
ggplot(d_com, aes(x=as.factor(P0_E1),y=gAHR)) + 
  facet_wrap(.~P0_E2_lab) +
  geom_boxplot() +
  xlab('p0_e1') +
  scale_y_log10()
ggsave(filename = '../figures/check_gAHR_prob.png', width = 4, height = 4)

##-- SS according to HRs -------------------------------------------------------
ggplot(d_com, aes(x=as.factor(HR_E1),y=gAHR)) + 
  facet_wrap(.~HR_E2_lab) +
  geom_boxplot() +
  xlab('HR_e1') +
  scale_y_log10()
ggsave(filename = '../figures/check_gAHR_HR.png', width = 4, height = 2)


##-- SS according to betas (shape parameter of weibull) ------------------------
ggplot(d_com, aes(x=as.factor(BETA_E1),y=gAHR)) + 
  facet_wrap(.~BETA_E2_lab) +
  geom_boxplot() +
  xlab('beta_e1') +
  scale_y_log10()
ggsave(filename = '../figures/check_gAHR_beta.png', width = 6, height = 2)


##-- SS according to rho and copula --------------------------------------------
ggplot(d_com, aes(x=as.factor(RHO), y=gAHR)) + 
  facet_grid(RHO_TYPE_LAB~COPULA_LAB) +
  geom_boxplot() +
  xlab('rho/tau') +
  scale_y_log10()
ggsave(filename = '../figures/check_gAHR_rho.png', width = 6, height = 4)

#-------------------------------------------------------------------------------
# Computation Time
#-------------------------------------------------------------------------------
summary(d_com$minutes_execution)
sum(d_com$minutes_execution)/60


save.image("../data/test_summary_results.RData")


