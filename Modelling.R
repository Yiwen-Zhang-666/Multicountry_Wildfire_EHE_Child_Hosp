library(dlnm)
library(splines)
library(dplyr)
library(data.table)
library(mixmeta)
library(lubridate)
library(doParallel)
library(foreach)
library(zoo)
library(survival)
library(writexl)
library(ggplot2)

#### Variable definition ####
#####cb_fire: crossbasis function of exposure to HFPP days
#####cb_EHE: crossbasis function of exposure to EHE days
#####cb_compound: crossbasis function of exposure to compound-exposure days
#####outcome: hospitalization counts for different diseases
#####exposure: four-level categorical exposure variable: 1-exposure to low EHE & low HFPP; 2-exposure to low EHE & high HFPP; 3-exposure to high EHE & low HFPP; 4-exposure to high EHE & high HFPP
#####RH: relatuve humidity
#####dow: day of week
######RR01: RR for exposure to high EHE and low HFPP
######RR10: RR for exposure to low EHE and high HFPP
######RR11: RR for exposure to high EHE and high HFPP

# lag model
## first stage
lagday <- 7 ## length of lag, use 7 to explore the specific lag of effect
df_lag <- 3 ## degree of freedom used in lag-response dimension of the crossbasis function
data <- hosp_data ## hosp_data is the hospitalization data used in the time-series analysis
location.list ## community ID list

result_lag_RR_stage1 <- data.frame()
for (i in location.list) {
  
  dat_mid = data[data$location_ID == location.list[[i]],]
  dat_mid$time = 1:nrow(dat_mid)
  df_time = round(nrow(dat_mid)/365.25)*7
  dat_mid$rhum_mvavg = frollmean(dat_mid$RH,8)

  dat_mid$exposure <- factor(dat_mid$exposure, levels = 1:4,
                              labels = c("control", "HFPP_alone", "ehe_alone", "compound"))

          
          # High PM2.5 alone
          cb_fire <- crossbasis(
            x = as.numeric(dat_mid$exposure == "HFPP_alone"),
            argvar = list(fun = "lin"),  # Linear function for binary indicator
            arglag = list(fun = "ns", knots = logknots(lagdays,lag_df), intercept = TRUE),
            lag = lagdays
          )
          
          # EHE alone
          cb_ehe <- crossbasis(
            x = as.numeric(dat_mid$exposure == "ehe_alone"),
            argvar = list(fun = "lin"),  # Linear function for binary indicator
            arglag = list(fun = "ns", knots = logknots(lagdays,lag_df), intercept = TRUE),
            lag = lagdays
          )
          
          # Compound exposure
          cb_compound <- crossbasis(
            x = as.numeric(dat_mid$exposure == "compound"),
            argvar = list(fun = "lin"),  # Linear function for binary indicator
            arglag = list(fun = "ns", knots = logknots(lagdays,lag_df), intercept = TRUE),
            lag = lagdays
          )

          formula_mid = as.formula(paste0('outcome ~ cb_fire + cb_ehe + cb_compound + dow + ns(time,df=',df_time,') + ns(rhum_mvavg, df=3)'))

          model =glm(formula_mid,data = dat_mid,family=quasipoisson)
        
          ## Create prediction objects to interpret the complex DLNM parameters
          ## For each exposure category, we predict relative risks compared to no exposure
          ### for HFPP
          fire_mid = crossreduce(cb_fire,model,cen= 0,type = 'var',value = 1)

            ncoef = length(coef(fire_mid))
            par = c(coef(fire_mid), vechMat(vcov(fire_mid)))
            names(par) = c(paste0("coef", seq(ncoef)),
                           paste0("vcov", seq(ncoef*(ncoef+1)/2)))
            
            fire_mid_df <- data.frame(cityname = unique(dat_mid$LocIDLarge),
                                      air = air,outcome = outcome,exposure = "HFPP_alone",maxlag = lagdays,t(par))
          
          ### for EHE
          ehe_mid = crossreduce(cb_ehe,model,cen= 0,type = 'var',value = 1)

            ncoef = length(coef(ehe_mid))
            par = c(coef(ehe_mid), vechMat(vcov(ehe_mid)))
            names(par) = c(paste0("coef", seq(ncoef)),
                           paste0("vcov", seq(ncoef*(ncoef+1)/2)))
            
            ehe_mid_df <- data.frame(cityname = unique(dat_mid$LocIDLarge),
                                     air = air,outcome = outcome,exposure = "EHE_alone",maxlag = lagdays,t(par))
          
          ### for compound
          compound_mid <- crossreduce(cb_compound,model,cen= 0,type = 'var',value = 1)

          ncoef = length(coef(compound_mid))
          par = c(coef(compound_mid), vechMat(vcov(compound_mid)))
          names(par) = c(paste0("coef", seq(ncoef)),
                           paste0("vcov", seq(ncoef*(ncoef+1)/2)))
            
          compound_mid_df <- data.frame(cityname = unique(dat_mid$LocIDLarge),
                                        air = air,outcome = outcome,exposure = "compound",maxlag = lagdays,t(par))

          
          result_lag_RR_stage1 <- bind_rows(fire_mid_df,ehe_mid_df,compound_mid_df)
          
}

## second stage
exposures <- unique(result_lag_RR_stage1$exposure) ## i.e., HFPP, EHE and compound
result_lag_RR_stage2 <- data.frame()
for (exposure in exposures) {
      
      mid = result_lag_RR_stage1[result_lag_RR_stage1$exposure== exposure,]
      
      coef = as.matrix(mid[,grep("coef", names(mid))])
      vcov = as.matrix(mid[,grep("vcov", names(mid))])

      model_mid = mixmeta(coef_mid~1, vcov_mid, method="reml")
      
      xlag_mid = 0:lagdays
      
      blag_mid = onebasis(xlag_mid, fun="ns", knots=logknots(lagdays,lag_df),intercept = T)
      
      # PREDICT THE ASSOCIATION
      cp_mid = crosspred(blag_mid, coef=coef(model_mid), vcov=vcov(model_mid), model.link="log",
                         at=xlag_mid, cen= 0)
      
      
      result_lag_RR_stage2 = bind_rows(result_lag_RR_stage2, data.frame(exposure = exposure,
                                                  lagday = xlag_mid,
                                                  RR = unname(cp_mid$matRRfit[,1]),
                                                  RRlow = unname(cp_mid$matRRlow[,1]),
                                                  RRhigh = unname(cp_mid$matRRhigh[,1])))
}


# Calculate Relative risk (RR)
## first stage
lagdays <- 1
lag_df <- 1

result_RR_stage1 <- data.frame()
for (i in location.list) {
  
  dat_mid = data[data$location_ID == location.list[[i]],]
  dat_mid$time = 1:nrow(dat_mid)
  df_time = round(nrow(dat_mid)/365.25)*7
  dat_mid$rhum_mvavg = frollmean(dat_mid$RH,8)
  
  dat_mid$exposure <- factor(dat_mid$exposure, levels = 1:4,
                             labels = c("control", "HFPP_alone", "ehe_alone", "compound"))
  
      
  
  # High PM2.5 alone
  cb_fire <- crossbasis(
    x = as.numeric(dat_mid$exposure == "HFPP_alone"),
    argvar = list(fun = "lin"),  # Linear function for binary indicator
    arglag = list(fun = "ns", knots = logknots(lagdays,lag_df), intercept = TRUE),
    lag = lagdays
  )
  
  # EHE alone
  cb_ehe <- crossbasis(
    x = as.numeric(dat_mid$exposure == "ehe_alone"),
    argvar = list(fun = "lin"),  # Linear function for binary indicator
    arglag = list(fun = "ns", knots = logknots(lagdays,lag_df), intercept = TRUE),
    lag = lagdays
  )
  
  # Compound exposure
  cb_compound <- crossbasis(
    x = as.numeric(dat_mid$exposure == "compound"),
    argvar = list(fun = "lin"),  # Linear function for binary indicator
    arglag = list(fun = "ns", knots = logknots(lagdays,lag_df), intercept = TRUE),
    lag = lagdays
  )
  
  formula_mid = as.formula(paste0('outcome ~ cb_fire + cb_ehe + cb_compound + dow + ns(time,df=',df_time,') + ns(rhum_mvavg, df=3)'))
  
  model =glm(formula_mid,data = dat_mid,family=quasipoisson)

          
          # ---- PREDICTIONS FOR INTERPRETATION ----
          # Create prediction objects to interpret the complex DLNM parameters
          # For each exposure category, we predict relative risks compared to no exposure
          ### for HFPP
          fire_mid = crossreduce(cb_fire,model,cen= 0,value = c(0, lagdays))
          ncoef = length(coef(fire_mid))
          par = c(coef(fire_mid), vechMat(vcov(fire_mid)))
          names(par) = c(paste0("coef", seq(ncoef)),
                          paste0("vcov", seq(ncoef*(ncoef+1)/2)))
            
          fire_mid_df <- data.frame(cityname = unique(dat_mid$LocIDLarge),
                                    air = air,outcome = outcome,exposure = "HFPP_alone",maxlag = lagdays,t(par))
          
          ### for EHE
          ehe_mid = crossreduce(cb_ehe,model,cen= 0,value = c(0, lagdays))
          ncoef = length(coef(ehe_mid))
          par = c(coef(ehe_mid), vechMat(vcov(ehe_mid)))
          names(par) = c(paste0("coef", seq(ncoef)),
                          paste0("vcov", seq(ncoef*(ncoef+1)/2)))
            
            ehe_mid_df <- data.frame(cityname = unique(dat_mid$LocIDLarge),
                                     air = air,outcome = outcome,exposure = "EHE_alone",maxlag = lagdays,t(par))
          
          ### for compound
          compound_mid <- crossreduce(cb_compound,model,cen= 0,value = c(0, lagdays))
          ncoef = length(coef(compound_mid))
          par = c(coef(compound_mid), vechMat(vcov(compound_mid)))
          names(par) = c(paste0("coef", seq(ncoef)),
                           paste0("vcov", seq(ncoef*(ncoef+1)/2)))
            
          compound_mid_df <- data.frame(cityname = unique(dat_mid$LocIDLarge),
                                          air = air,outcome = outcome,exposure = "compound",maxlag = lagdays,t(par))
          
          result_RR_stage1 <- bind_rows(fire_mid_df,ehe_mid_df,compound_mid_df)

          
          
}

## second stage
exposures <- unique(result_RR_stage1$exposure)
result_lag_RR_stage2 <- data.frame()
for (exposure in exposures) {

  mid = result_lag_RR_stage1[result_lag_RR_stage1$exposure== exposure,]
  
  coef = as.matrix(mid[,grep("coef", names(mid))])
  vcov = as.matrix(mid[,grep("vcov", names(mid))])
  
  model_mid = mixmeta(coef_mid~1, vcov_mid, method="reml")
      
  sum_mid <- summary(model_mid)$coefficients
      
      
  result_RR = bind_rows(result_RR, data.frame(exposure = exposure,
                                              RR = exp(sum_mid[1]),
                                              RRlow = exp(sum_mid[5]),
                                              RRhigh = exp(sum_mid[6])))
      
  
}


# Calculate relative excess risk due to interaction (RERI)
## Function to calculate RERI with 95% CI using the delta method ##
calculate_RERI <- function(RR1, RR2, RR3, se_beta1, se_beta2, se_beta3) {
  # Calculate RERI
  RERI <- RR11 - RR01 - RR10 + 1
  
  # derivatives
  a1 <- -RR01     # dRERI/db1
  a2 <- -RR10     # dRERI/db2
  a3 <-  RR11     # dRERI/db3
  
  var_RERI <- 
    a1^2 * se_beta1^2 +
    a2^2 * se_beta2^2 +
    a3^2 * se_beta3^2
  
  # Variance estimation using the delta method
  se_RERI <- sqrt(var_RERI)                     # Standard error of RERI
  
  # Compute 95% CIs
  CI_RERI <- c(RERI - 1.96 * se_RERI, RERI + 1.96 * se_RERI)
  
  # Return results as a data frame
  return(data.frame(RERI, se_RERI, CI_RERI_low = CI_RERI[1], CI_RERI_high = CI_RERI[2]))
}

## Begin calculate the RERI
RR01 <- result_lag_RR_stage2[result_lag_RR_stage2$exposure == 'EHE_alone',]$RR  
RR10 <- result_lag_RR_stage2[result_lag_RR_stage2$exposure == 'HFPP_alone',]$RR 
RR11 <- result_lag_RR_stage2[result_lag_RR_stage2$exposure == 'compound',]$RR   
    
# 95% CI for RR01, RR10, and RR11 (lower and upper bounds)
se_beta01 <- (log(result_lag_RR_stage2[result_lag_RR_stage2$exposure == 'EHE_alone',]$RRhigh) - log(result_lag_RR_stage2[result_lag_RR_stage2$exposure == 'EHE_alone',]$RRlow)) / (2 * 1.96)
se_beta10 <- (log(result_lag_RR_stage2[result_lag_RR_stage2$exposure == 'HFPP_alone',]$RRhigh) - log(result_lag_RR_stage2[result_lag_RR_stage2$exposure == 'HFPP_alone',]$RRlow)) / (2 * 1.96)
se_beta11 <- (log(result_lag_RR_stage2[result_lag_RR_stage2$exposure == 'compound',]$RRhigh) - log(result_lag_RR_stage2[result_lag_RR_stage2$exposure == 'compound',]$RRlow)) / (2 * 1.96)
    
results <- calculate_interaction_metrics(RR1, RR2, RR3, se_beta1, se_beta2, se_beta3)
    
## Store results  
RERI.dat <- data.frame(
      estimate = c(results$RERI),
      CI_low = c(results$CI_RERI_low),
      CI_high = c(results$CI_RERI_high)
    )
