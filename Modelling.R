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
#####RH: relative humidity
#####dow: day of week
######RR1: RR for exposure to high EHE and low HFPP
######RR2: RR for exposure to low EHE and high HFPP
######RR3: RR for exposure to high EHE and high HFPP
######lagdays: The exposure window (i.e., how many days of the effect of exposure last)

# Calculate Relative risk (RR)
## first stage
lagdays <- 1
result_VCOV_stage1 <- data.frame()
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
    arglag = list(fun = "ns"),
    lag = lagdays
  )
  
  # EHE alone
  cb_ehe <- crossbasis(
    x = as.numeric(dat_mid$exposure == "ehe_alone"),
    argvar = list(fun = "lin"),  # Linear function for binary indicator
    arglag = list(fun = "ns"),
    lag = lagdays
  )
  
  # Compound exposure
  cb_compound <- crossbasis(
    x = as.numeric(dat_mid$exposure == "compound"),
    argvar = list(fun = "lin"),  # Linear function for binary indicator
    arglag = list(fun = "ns"),
    lag = lagdays
  )
  
  formula_mid = as.formula(paste0('outcome ~ cb_fire + cb_ehe + cb_compound + dow + ns(time,df=',df_time,') + ns(rhum_mvavg, df=3)'))
  
  model =glm(formula_mid,data = dat_mid,family=quasipoisson)

          
          # ---- PREDICTIONS FOR INTERPRETATION ----
          # Create prediction objects to interpret the complex DLNM parameters
          # For each exposure category, we predict relative risks compared to no exposure
          ### for HFPP
          fire_mid = crossreduce(cb_fire,model,cen= 0,value = c(0, lagdays))

          ### for EHE
          ehe_mid = crossreduce(cb_ehe,model,cen= 0,value = c(0, lagdays))

          ### for compound
          compound_mid <- crossreduce(cb_compound,model,cen= 0,value = c(0, lagdays))

          ### extract variance-covariance matrix ###
          # Step 1: Get model vcov and identify coefficient indices
          V_model    <- vcov(model)
          coef_names <- names(coef(model))
          idx_ehe   <- grep("^cb_ehev1\\.l",     coef_names)
          idx_fire  <- grep("^cb_firev1\\.l",    coef_names)
          idx_comp   <- grep("^cb_compoundv1\\.l", coef_names)
          
          # Step 2: Build contrast vectors for cumulative effects
          lags <- 0:lagdays
          lagbasis_ehe  <- dlnm::onebasis(lags, 
                                           fun = attr(cb_ehe, "arglag")$fun,
                                           df  = attr(cb_ehe, "arglag")$df,
                                           knots = attr(cb_ehe, "arglag")$knots,
                                           intercept = attr(cb_ehe, "arglag")$intercept)
          
          lagbasis_fire <- dlnm::onebasis(
            lags,
            fun       = attr(cb_fire, "arglag")$fun,
            df        = attr(cb_fire, "arglag")$df,
            knots     = attr(cb_fire, "arglag")$knots,
            intercept = attr(cb_fire, "arglag")$intercept
          )
          
          lagbasis_comp <- dlnm::onebasis(
            lags,
            fun       = attr(cb_compound, "arglag")$fun,
            df        = attr(cb_compound, "arglag")$df,
            knots     = attr(cb_compound, "arglag")$knots,
            intercept = attr(cb_compound, "arglag")$intercept
          )
          
          
          c_ehe  <- matrix(colSums(lagbasis_ehe),  nrow = 1)
          c_fire <- matrix(colSums(lagbasis_fire), nrow = 1)
          c_comp  <- matrix(colSums(lagbasis_comp),  nrow = 1)
          
          # Step 3: Extract variance-covariance matrix elements
          var1  <- as.numeric(c_ehe  %*% V_model[idx_ehe,  idx_ehe]  %*% t(c_ehe))
          var2  <- as.numeric(c_fire %*% V_model[idx_fire, idx_fire] %*% t(c_fire))
          var3  <- as.numeric(c_comp  %*% V_model[idx_comp,  idx_comp]  %*% t(c_comp))
          cov12 <- as.numeric(c_ehe  %*% V_model[idx_ehe,  idx_fire] %*% t(c_fire))
          cov13 <- as.numeric(c_ehe  %*% V_model[idx_ehe,  idx_comp]  %*% t(c_comp))
          cov23 <- as.numeric(c_fire %*% V_model[idx_fire, idx_comp]  %*% t(c_comp))
          
          # Build 3×3 matrix
          S <- matrix(c(var1, cov12, cov13,
                        cov12, var2, cov23,
                        cov13, cov23, var3), nrow = 3, byrow = TRUE)
          
          # ---- save results ----
          cross_cov_df <- data.frame(
            cityname       = unique(dat_mid$LocIDLarge),
            logRR_ehe      = ehe_mid$coefficients[[1]],
            logRR_fire     = fire_mid$coefficients[[1]],
            logRR_compound = compound_mid$coefficients[[1]],
            S11 = S[1, 1], S22 = S[2, 2], S33 = S[3, 3],
            S12 = S[1, 2], S13 = S[1, 3], S23 = S[2, 3]
          )
          
          result_VCOV_stage1 <- bind_rows(result_VCOV_stage1, cross_cov_df)
          
}

## second stage
### pool RR and calculate relative excess risk due to interaction (RERI) using the Delta method
#### prepare data
    logRR_mat <- as.matrix(
      result_VCOV_stage1[, c("logRR_ehe", "logRR_fire", "logRR_compound")]
    )
    rownames(logRR_mat) <- result_VCOV_stage1$cityname
    colnames(logRR_mat) <- c("EHE", "Fire", "Compound")
    
    S_list <- lapply(1:nrow(result_VCOV_stage1), function(i) {
      r <- result_VCOV_stage1[i, ]
      S <- matrix(c(r$S11, r$S12, r$S13,
                    r$S12, r$S22, r$S23,
                    r$S13, r$S23, r$S33),
                  nrow = 3, byrow = TRUE)
      rownames(S) <- colnames(S) <- c("EHE", "Fire", "Compound")
      return(S)
    })
    names(S_list) <- result_VCOV_stage1$cityname
    
    # Multivariate meta-analysis
      mv_fit <- mixmeta(logRR_mat ~ 1, S = S_list, method = "reml",
                        control = list(showiter = FALSE))
      
      # Extract pooled effec estimates
      pooled_coef <- coef(mv_fit)
      names(pooled_coef) <- c("EHE", "Fire", "Compound")
      V_pooled    <- vcov(mv_fit)
      
      # ---- Extract pooled RR and 95% CI ----
      
      # Function to calculate RR and CI from log-RR
      extract_RR_CI <- function(logRR, var_logRR) {
        RR <- exp(logRR)
        SE_logRR <- sqrt(var_logRR)
        
        # 95% CI on log scale
        logRR_lower <- logRR - 1.96 * SE_logRR
        logRR_upper <- logRR + 1.96 * SE_logRR
        
        # Transform to RR scale
        RR_lower <- exp(logRR_lower)
        RR_upper <- exp(logRR_upper)
        
        return(c(RR = RR, CI_lower = RR_lower, CI_upper = RR_upper))
      }
      
      # Extract for each exposure
      RR_ehe     <- extract_RR_CI(pooled_coef["EHE"],     V_pooled[1, 1])
      RR_fire    <- extract_RR_CI(pooled_coef["Fire"],    V_pooled[2, 2])
      RR_compound <- extract_RR_CI(pooled_coef["Compound"], V_pooled[3, 3])
      
      
      # RERI计算
      RR1 <- RR_ehe["RR.EHE"]
      RR2 <- RR_fire["RR.Fire"]
      RR3 <- RR_compound["RR.Compound"]
      
      RERI <- RR3 - RR2 - RR1 + 1
      
      # Delta method
      a1 <- -RR1
      a2 <- -RR2
      a3 <-  RR3
      
      var_RERI <- a1^2 * V_pooled[1,1] +
        a2^2 * V_pooled[2,2] +
        a3^2 * V_pooled[3,3] +
        2*a1*a2 * V_pooled[1,2] +
        2*a1*a3 * V_pooled[1,3] +
        2*a2*a3 * V_pooled[2,3]
      
      se_RERI  <- sqrt(var_RERI)
      CI_lower <- RERI - 1.96 * se_RERI
      CI_upper <- RERI + 1.96 * se_RERI
      p_value  <- 2 * (1 - pnorm(abs(RERI / se_RERI)))
      

      # Save results
      ## for pooled RR and 95% CI
      pooled_RR.dat <- data.frame(
        Exposure = c("EHE alone", "Wildfire alone", "Compound"),
        RR = c(RR_ehe["RR.EHE"], RR_fire["RR.Fire"], RR_compound["RR.Compound"]),
        CI_lower = c(RR_ehe["CI_lower.EHE"], RR_fire["CI_lower.Fire"], RR_compound["CI_lower.Compound"]),
        CI_upper = c(RR_ehe["CI_upper.EHE"], RR_fire["CI_upper.Fire"], RR_compound["CI_upper.Compound"])
      )
      
      ## for RERI and 95% CI
      RERI.dat <- data.frame(
        RR1_EHE        = RR1,
        RR2_Fire       = RR2,
        RR3_Compound   = RR3,
        RERI           = RERI,
        SE_RERI        = se_RERI,
        CI_lower       = CI_lower,
        CI_upper       = CI_upper,
        significant    = ifelse(CI_lower > 0, "Synergism",
                                ifelse(CI_upper < 0, "Antagonism", "No interaction"))
      )
      
      