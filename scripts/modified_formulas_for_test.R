#-------------------------------------------------------------------------------
# ARE
#-------------------------------------------------------------------------------
ARE_tte <- function (p0_e1, p0_e2, HR_e1, HR_e2, beta_e1 = 1, beta_e2 = 1, 
          case, copula = "Frank", rho = 0.3, rho_type = "Spearman", 
          subdivisions = 50, plot_print = FALSE, plot_save = FALSE) {

  ARE_array <- c()

  copula0      <- CopulaSelection(copula = copula, rho = rho, rho_type = rho_type)
  which.copula <- copula0[[1]]
  theta        <- copula0[[2]]
  MarginSelec  <- MarginalsSelection(beta_e1, beta_e2, HR_e1, HR_e2, 
                                     p0_e1, p0_e2, case, rho, theta, copula)
  T1dist   <- MarginSelec[[1]]
  T2dist   <- MarginSelec[[2]]
  T1pdist  <- MarginSelec[[3]]
  T2pdist  <- MarginSelec[[4]]
  T10param <- MarginSelec[[5]]
  T20param <- MarginSelec[[6]]
  T11param <- MarginSelec[[7]]
  T21param <- MarginSelec[[8]]
  distribution0 <- mvdc(copula = which.copula, margins = c(T1dist,T2dist), paramMargins = list(T10param, T20param))
  distribution1 <- mvdc(copula = which.copula, margins = c(T1dist,T2dist), paramMargins = list(T11param, T21param))
  if (case == 1 | case == 3) {
    inside_integral <- function(t) {
      Sstar0 <- Sstar(x = t, dist1 = T1pdist, dist2 = T2pdist, 
                      param1 = T10param, param2 = T20param, dist_biv = distribution0)
      Sstar1 <- Sstar(x = t, dist1 = T1pdist, dist2 = T2pdist, 
                      param1 = T11param, param2 = T21param, dist_biv = distribution1)
      fstar0 <- (-grad(Sstar, x = t, dist1 = T1pdist, 
                       dist2 = T2pdist, param1 = T10param, param2 = T20param, 
                       dist_biv = distribution0))
      fstar1 <- (-grad(Sstar, x = t, dist1 = T1pdist, 
                       dist2 = T2pdist, param1 = T11param, param2 = T21param, 
                       dist_biv = distribution1))
      Lstar0 <- (fstar0/Sstar0)
      Lstar1 <- (fstar1/Sstar1)
      HRstar <- (Lstar1/Lstar0)
      logHRstar <- log(HRstar)
      fstar0[fstar0 < 0] <- 0
      logHRstar[is.na(logHRstar) | logHRstar == Inf | 
                  logHRstar == -Inf] <- 0
      return(logHRstar * fstar0)
    }
    integral <- integrate(inside_integral, lower = 0, 
                          upper = 1, subdivisions = 10000, stop.on.error = FALSE)
    numerator <- (integral$value)^2
    Sstar0_1 <- Sstar(x = 1, dist1 = T1pdist, dist2 = T2pdist, 
                      param1 = T10param, param2 = T20param, dist_biv = distribution0)
    ST10_1 <- 1 - do.call(T1pdist, c(q = 1, T10param))
    denominator <- ((log(HR_e1))^2) * (1 - Sstar0_1) * 
      (1 - ST10_1)
    AREstarT <- numerator/denominator
    if (integral$message != "OK") {
      AREstarT <- NA
    }
  }
  else if (case == 2 | case == 4) {
    b10 <- T10param$scale
    b20 <- T20param$scale
    b11 <- T11param$scale
    b21 <- T21param$scale
    fT0 <- function(t, beta, b) dweibull(x = t, beta, b)
    ST0 <- function(t, beta, b) 1 - pweibull(q = t, beta, b)
    Sstar0 <- function(t) Sstar(x = t, dist1 = T1pdist, 
                                dist2 = T2pdist, param1 = T10param, param2 = T20param, 
                                dist_biv = distribution0)
    fstar0 <- function(t) (-grad(Sstar, x = t, dist1 = T1pdist, 
                                 dist2 = T2pdist, param1 = T10param, param2 = T20param, 
                                 dist_biv = distribution0))
    aux21 <- function(t, y) theta * exp(-theta * (ST0(t, 
                                                      beta_e1, b10) + y)) * (1 - exp(-theta))/(exp(-theta) - 
                                                                                                 exp(-theta * ST0(t, beta_e1, b10)) - exp(-theta * 
                                                                                                                                            y) + exp(-theta * (ST0(t, beta_e1, b10) + y)))^2
    aux22 <- function(u) {
      integrate(aux21, 0, ST0(u, beta_e2, b20), t = u, 
                subdivisions = 10000)$value
    }
    lambdaC10 <- function(t) aux22(t) * fT0(t, beta_e1, b10)/(Sstar0(t) + 1e-6)
    lambdaC11 <- function(t) HR_e1 * lambdaC10(t)
    aux23 <- function(x, t) theta * exp(-theta * (x + 
                                                    ST0(t, beta_e2, b20))) * (1 - exp(-theta))/(exp(-theta) - 
                                                                                                  exp(-theta * x) - exp(-theta * ST0(t, beta_e2, 
                                                                                                                                     b20)) + exp(-theta * (x + ST0(t, beta_e2, b20))))^2
    aux24 <- Vectorize(function(u) {
      integrate(aux23, 0, ST0(u, beta_e1, b10), t = u, 
                subdivisions = 10000)$value
    })
    lambdaC20 <- function(t) aux24(t) * fT0(t, beta_e2, b20)/(Sstar0(t) + 1e-6)
    lambdaC21 <- function(t) HR_e2 * lambdaC20(t)
    LambdaC20_check <- tryCatch(LambdaC20 <- function(t) integrate(lambdaC20, 
                                                                   lower = 0, 
                                                                   upper = t, subdivisions = 10000)$value, 
                                error = function(e) e)
    lower_LambdaC20 <- 0
    while (inherits(LambdaC20_check, "error")) {
      lower_LambdaC20 <- lower_LambdaC20 + 0.001
      LambdaC20_check <- tryCatch(LambdaC20 <- function(t) integrate(lambdaC20, 
                                                                     lower = 0 + lower_LambdaC20, 
                                                                     upper = t, subdivisions = 10000)$value, 
                                  error = function(e) e)
    }
    LambdaC20 <- Vectorize(function(t) integrate(lambdaC20, 
                                                 lower = 0 + lower_LambdaC20, upper = t, subdivisions = 10000)$value)
    Lstar0 <- function(t) lambdaC10(t) + lambdaC20(t)
    Lstar1 <- function(t) lambdaC11(t) + lambdaC21(t)
    HRstar <- function(t) Lstar1(t)/Lstar0(t)
    logHRstar <- function(t) log(Lstar1(t)/Lstar0(t))
    temp3 <- Vectorize(function(t){res = logHRstar(t) * fstar0(t); res[is.na(res)] <- 0; res}) # ****************************** CAMBIO!!!!
    temp4_check <- tryCatch(temp4 <- integrate(temp3, 
                                               0, 1, subdivisions = 10000)$value, error = function(e) e)
    lower_temp4 <- 0
    while (inherits(temp4_check, "error") & lower_temp4 < 1) {
      lower_temp4 <- lower_temp4 + 0.001
      temp4_check <- tryCatch(temp4 <- integrate(temp3, lower_temp4, 1, subdivisions = 10000)$value, error = function(e) e)
    }
    temp4 <- integrate(temp3, 0 + lower_temp4, 1, subdivisions = 10000)$value
    numerator <- (temp4)^2
    PROBT1UNC_temp_num <- function(t) exp(-HR_e2 * LambdaC20(t)) * Sstar0(t) * lambdaC10(t)
    PROBT1UNC_temp_den <- function(t) 1/2 * (exp(-LambdaC20(t)) + exp(-HR_e2 * LambdaC20(t)))
    PROBT1UNC_temp <- Vectorize(function(t) {PROBT1UNC_temp_num(t)/PROBT1UNC_temp_den(t)})
    PROBT1UNC_int_check <- tryCatch(integrate(PROBT1UNC_temp, lower = 0, upper = 1, subdivisions = 10000)$value, 
                                    error = function(e) e)
    lower_LambdaC20 <- 0
    while (inherits(PROBT1UNC_int_check, "error") & lower_LambdaC20<=1) {
        lower_LambdaC20 <- lower_LambdaC20 + 0.001
        # PROBT1UNC_int_check <- tryCatch(integrate(PROBT1UNC_temp, 
        #                                           lower = lower_LambdaC20, upper = 1, theta = theta, 
        #                                           HR2 = HR_e2, subdivisions = 10000)$value, error = function(e) e)
        PROBT1UNC_int_check <- tryCatch(integrate(PROBT1UNC_temp, 
                                                  lower = lower_LambdaC20, upper = 1, subdivisions = 10000)$value, error = function(e) e)
    }  
    
    lower_PROBT1UNC_int <- lower_LambdaC20
    PROBT1UNC_int <- PROBT1UNC_int_check
    # PROBT1UNC_int <- integrate(PROBT1UNC_temp, lower = lower_PROBT1UNC_int, 
    #                             upper = 1, subdivisions = 10000)$value
    AREstarT <- numerator/((log(HR_e1)^2) * PROBT1UNC_int * (1 - Sstar0(1)))
    AREstarT
  }
  ARE_array <- c(ARE_array, AREstarT)
  
  
  
  return_object <- list(ARE = ARE_array[1], rho = rho)

  return(invisible(return_object))
}

#-------------------------------------------------------------------------------
# Sample size
#-------------------------------------------------------------------------------

samplesize_tte <- function (p0_e1, p0_e2, HR_e1, HR_e2, beta_e1 = 1, beta_e2 = 1, 
                            case, copula = "Frank", rho = 0.3, rho_type = "Spearman", 
                            alpha = 0.05, power = 0.8, ss_formula = "schoenfeld", subdivisions = 50, 
                            plot_print = FALSE, plot_save = FALSE){

  SS_array_1 <- SS_array_2 <- SS_array_c <- c()

  invisible(capture.output(eff_size <- effectsize_tte(p0_e1, 
                                                      p0_e2, HR_e1, HR_e2, beta_e1, beta_e2, case, copula, 
                                                      rho, rho_type, subdivisions = 1000, plot_print = FALSE)))
  gAHR <- eff_size$effect_size$gAHR
  events_1 <- ifelse(ss_formula == "schoenfeld", schoenfeld_formula(alpha, 
                                                                    power, HR_e1), freedman_formula(alpha, power, HR_e1))
  events_2 <- ifelse(ss_formula == "schoenfeld", schoenfeld_formula(alpha, 
                                                                    power, HR_e2), freedman_formula(alpha, power, HR_e2))
  events_c <- schoenfeld_formula(alpha, power, gAHR)
  p1_e1 <- eff_size$measures_by_group$p_e1[2]
  p1_e2 <- eff_size$measures_by_group$p_e2[2]
  p0_star <- eff_size$measures_by_group$pstar[1]
  p1_star <- eff_size$measures_by_group$pstar[2]
  ss_1 <- as.numeric(2 * ceiling(events_1/(p0_e1 + p1_e1)))
  ss_2 <- as.numeric(2 * ceiling(events_2/(p0_e2 + p1_e2)))
  ss_c <- as.numeric(2 * ceiling(events_c/(p0_star + p1_star)))
  SS_array_1 <- c(SS_array_1, ss_1)
  SS_array_2 <- c(SS_array_2, ss_2)
  SS_array_c <- c(SS_array_c, ss_c)
  
  return_object <- list(ss_E1 = SS_array_1[1], ss_E2 = SS_array_2[1], 
                        ss_Ec = SS_array_c[1], gg_object = NA)
  return(invisible(return_object))
}



#-------------------------------------------------------------------------------
# Effect size
#-------------------------------------------------------------------------------

effectsize_tte <- function (p0_e1, p0_e2, HR_e1, HR_e2, beta_e1 = 1, beta_e2 = 1, 
          case, copula = "Frank", rho = 0.3, rho_type = "Spearman", 
          followup_time = 1, subdivisions = 1000, plot_print = FALSE, 
          plot_save = FALSE){
 
  if (case == 4 && p0_e1 + p0_e2 > 1) {
    stop("The sum of the proportions of observed events in both endpoints in case 4 must be lower than 1")
  }
  
  t <- c(1e-04, seq(0.001, 1, length.out = subdivisions))
  copula0      <- CopulaSelection(copula, rho, rho_type)
  which.copula <- copula0[[1]]
  theta        <- copula0[[2]]
  MarginSelec  <- MarginalsSelection(beta_e1, beta_e2, HR_e1, 
                                    HR_e2, p0_e1, p0_e2, case, rho, theta, copula = copula)
  T1dist   <- MarginSelec[[1]]
  T2dist   <- MarginSelec[[2]]
  T1pdist  <- MarginSelec[[3]]
  T2pdist  <- MarginSelec[[4]]
  T10param <- MarginSelec[[5]]
  T20param <- MarginSelec[[6]]
  T11param <- MarginSelec[[7]]
  T21param <- MarginSelec[[8]]
  p1_e1    <- MarginSelec[[9]]
  p1_e2    <- MarginSelec[[10]]
  
  b10 <- T10param[[2]]
  b20 <- T20param[[2]]
  b11 <- T11param[[2]]
  b21 <- T21param[[2]]
  
  distribution0 <- mvdc(copula = which.copula, margins = c(T1dist, T2dist), paramMargins = list(T10param, T20param))
  distribution1 <- mvdc(copula = which.copula, margins = c(T1dist, T2dist), paramMargins = list(T11param, T21param))
  fT10 <- (beta_e1/b10) * ((t/b10)^(beta_e1 - 1)) * (exp(-(t/b10)^beta_e1))
  fT11 <- (beta_e1/b11) * ((t/b11)^(beta_e1 - 1)) * (exp(-(t/b11)^beta_e1))
  fT20 <- (beta_e2/b20) * ((t/b20)^(beta_e2 - 1)) * (exp(-(t/b20)^beta_e2))
  fT21 <- (beta_e2/b21) * ((t/b21)^(beta_e2 - 1)) * (exp(-(t/b21)^beta_e2))
  ST10 <- pmin(1 , exp(-(t/b10)^beta_e1) + 1e-6)
  ST11 <- pmin(1 , exp(-(t/b11)^beta_e1) + 1e-6)
  ST20 <- pmin(1 , exp(-(t/b20)^beta_e2) + 1e-6)
  ST21 <- pmin(1 , exp(-(t/b21)^beta_e2) + 1e-6)
  if (copula == "Frank") {
    Sstar0 <- pmin(1 , (-log(1 + (exp(-theta * ST10) - 1) * (exp(-theta * ST20) - 1)/(exp(-theta) - 1))/theta) + 1e-6)
    Sstar1 <- pmin(1 , (-log(1 + (exp(-theta * ST11) - 1) * (exp(-theta * ST21) - 1)/(exp(-theta) - 1))/theta) + 1e-6)
  } else if (copula == "Clayton") {
    Sstar0 <- pmin(1 , (ST10^(-theta) + ST20^(-theta) - 1)^{-1/theta} + 1e-6)
    Sstar1 <- pmin(1 , (ST11^(-theta) + ST21^(-theta) - 1)^{-1/theta} + 1e-6)
  } else if (copula == "Gumbel") {
    Sstar0 <- pmin(1 , exp(-((-log(ST10))^theta + (-log(ST20))^theta)^(1/theta)) + 1e-6)
    Sstar1 <- pmin(1 , exp(-((-log(ST11))^theta + (-log(ST21))^theta)^(1/theta)) + 1e-6)
  }
  if (copula == "Frank") {
    fstar0 <- pmax(0, (exp(-theta * ST10) * (exp(-theta * ST20) - 1) * fT10 + exp(-theta * ST20) * (exp(-theta * ST10) - 1) * fT20)/(exp(-theta * Sstar0) * (exp(-theta) - 1)))
    fstar1 <- pmax(0, (exp(-theta * ST11) * (exp(-theta * ST21) - 1) * fT11 + exp(-theta * ST21) * (exp(-theta * ST11) - 1) * fT21)/(exp(-theta * Sstar1) * (exp(-theta) - 1)))
  } else if (copula == "Clayton") {
    fstar0 <- pmax(0, (ST10^(theta + 1) * fT10 + ST20^(theta + 1) * fT20)/(Sstar0 * (ST10^(-theta) + ST20^(-theta) -1)))
    fstar1 <- pmax(0, (ST11^(theta + 1) * fT11 + ST20^(theta + 1) * fT21)/(Sstar1 * (ST11^(-theta) + ST21^(-theta) - 1)))
  } else if (copula == "Gumbel") {
    fstar0 <- pmax(0, Sstar0 * log(Sstar0) * ((-log(ST10))^(theta - 1) * fT10 * (-ST10)^(-1) + (-log(ST20))^(theta - 1) * fT20 * (-ST20)^(-1) + 1e-6)/((-log(ST10))^theta + (-log(ST20))^theta + 1e-6))
    fstar1 <- pmax(0, Sstar1 * log(Sstar1) * ((-log(ST11))^(theta - 1) * fT11 * (-ST11)^(-1) + (-log(ST21))^(theta - 1) * fT21 * (-ST21)^(-1) + 1e-6)/((-log(ST11))^theta + (-log(ST21))^theta + 1e-6))
  }
  Lstar0 <- fstar0/Sstar0
  Lstar1 <- fstar1/Sstar1
  HRstar <- (Lstar1 + 1e-6)/(Lstar0 + 1e-6)
  HRstar_int <- rowMeans(cbind(HRstar[-1], rev(rev(HRstar)[-1])))
  fstar0_int <- rowMeans(cbind(fstar0[-1], rev(rev(fstar0)[-1])))
  fstar1_int <- rowMeans(cbind(fstar1[-1], rev(rev(fstar1)[-1])))
  Lstar0_int <- rowMeans(cbind(Lstar0[-1], rev(rev(Lstar0)[-1])))
  Lstar1_int <- rowMeans(cbind(Lstar1[-1], rev(rev(Lstar1)[-1])))
  nHR <- (HR_e1 + HR_e2)/2
  mHR <- mean(HRstar)
  sAHR_0 <- sum(HRstar_int * fstar0_int)/sum(fstar0_int)
  gAHR_0 <- exp(sum(log(HRstar_int) * fstar0_int)/sum(fstar0_int))
  gAHR <- exp(sum(log(HRstar_int) * (fstar0_int + fstar1_int))/sum(fstar0_int + 
                                                                     fstar1_int))
  AHR_0_num <- sum((Lstar1_int + 1e-6)/(Lstar0_int + Lstar1_int + 1e-6) * fstar0_int)/sum(fstar0_int)
  AHR_0_den <- sum((Lstar0_int + 1e-6)/(Lstar0_int + Lstar1_int + 1e-6) * fstar0_int)/sum(fstar0_int)
  AHR_0 <- AHR_0_num/AHR_0_den
  AHR_num <- sum((Lstar1_int + 1e-6)/(Lstar0_int + Lstar1_int + 1e-6) * (fstar0_int + 
                                                           fstar1_int))/sum(fstar0_int + fstar1_int)
  AHR_den <- sum((Lstar0_int + 1e-6)/(Lstar0_int + Lstar1_int + 1e-6) * (fstar0_int + 
                                                           fstar1_int))/sum(fstar0_int + fstar1_int)
  AHR <- AHR_num/AHR_den
  RMST_0 <- followup_time * integrate(Sstar, dist1 = T1pdist, 
                                      dist2 = T2pdist, param1 = T10param, param2 = T20param, 
                                      dist_biv = distribution0, lower = 0, upper = 1, subdivisions = subdivisions)$value
  RMST_1 <- followup_time * integrate(Sstar, dist1 = T1pdist, 
                                      dist2 = T2pdist, param1 = T11param, param2 = T21param, 
                                      dist_biv = distribution1, lower = 0, upper = 1, subdivisions = subdivisions)$value
  pstar_0 <- 1 - Sstar(1, dist1 = T1pdist, dist2 = T2pdist, 
                       param1 = T10param, param2 = T20param, dist_biv = distribution0)
  pstar_1 <- 1 - Sstar(1, dist1 = T1pdist, dist2 = T2pdist, 
                       param1 = T11param, param2 = T21param, dist_biv = distribution1)
  limits <- c(0, 10)
  Med_0 <- followup_time * uniroot(Sstar_func_perc, interval = limits, 
                                   extendInt = "yes", perc = 0.5, dist1 = T1pdist, dist2 = T2pdist, 
                                   param1 = T10param, param2 = T20param, dist_biv = distribution0)$root
  Med_1 <- followup_time * uniroot(Sstar_func_perc, interval = limits, 
                                   extendInt = "yes", perc = 0.5, dist1 = T1pdist, dist2 = T2pdist, 
                                   param1 = T11param, param2 = T21param, dist_biv = distribution1)$root
  f_time <- as.numeric(followup_time)

  return_object <- list(effect_size       = list('gAHR'         = round(gAHR,4),
                                                 'AHR'          = round(AHR,4),
                                                 'RMST_ratio'   = round(RMST_1/RMST_0,4),
                                                 'Median_Ratio' = round(Med_1/Med_0,4)),
                        measures_by_group = list('pstar'  = c('Reference'=pstar_0,'Treated'=pstar_1),
                                                 'p_e1'   = c('Reference'=p0_e1,  'Treated'=p1_e1),
                                                 'p_e2'   = c('Reference'=p0_e2,  'Treated'=p1_e2),
                                                 'Median' = c('Reference'=Med_0,  'Treated'=Med_1)))                                                                                                                                                                                 
  return(invisible(return_object))
}
