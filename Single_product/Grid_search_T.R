##### Credentials #####

### Author:       R.M. (Robert) Verschuren
### Institution:  Amsterdam School of Economics, 
###               University of Amsterdam
### Date:         26/08/2020

##### Clear workspace #####
cat("\014")
rm(list = ls())
while (dev.cur()>1) dev.off()

#### Libraries #####
Packages <- c('dplyr', 'ggplot2','lme4',
              'reshape2', 'mgcv',
              'foreach', 'doParallel',
              'MASS', 'gamlss')
invisible(lapply(Packages, library, character.only = TRUE))

rm(Packages)

##### Load data #####
### Load confidential insurance data on travel (T) insurance
#   - T_Freq containing information on the claim counts;
#   - T_Sev containing information on the claim severities.

##### Evaluation sets #####
### Split date
dt_Split <- '2018-01-01'

### Split data into training and test set
T_Freq_Train <- T_Freq[which((as.Date(T_Freq$dt_End) <= as.Date(dt_Split)) & (T_Freq$History == 0)), ]
T_Freq_Test <- T_Freq[which((as.Date(T_Freq$dt_Start) >= as.Date(dt_Split)) & (T_Freq$History == 0)), ]
T_Sev_Train <- T_Sev[which(as.Date(T_Sev$dt_End) <= as.Date(dt_Split)), ]

##### GLM frequency #####
### T - Poisson model and forecasts
GLM_P_T_Freq <- glm(Count ~ 1 + Region + Age + FamilySituation + WinterCoverage +
                       MoneyCoverage + VehicleCoverage + MedicalCoverage +
                       AccidentCoverage + CancelCoverage,
                       offset = log(Exposure), data = T_Freq_Train, family = poisson(link = 'log'))

GLM_P_T_Freq_fc <- predict(GLM_P_T_Freq, T_Freq_Test, type = 'response')

### T - NB model and forecasts
GLM_NB_T_Freq <- glm.nb(Count ~ 1 + Region + Age + FamilySituation + WinterCoverage +
                           MoneyCoverage + VehicleCoverage + MedicalCoverage +
                           AccidentCoverage + CancelCoverage +
                           offset(log(Exposure)), data = T_Freq_Train, link = log)

GLM_NB_T_Freq_fc <- predict(GLM_NB_T_Freq, T_Freq_Test, type = 'response')

### GL - ZIP model and forecasts
GLM_ZIP_T_Freq <- gam(list(Count ~ 1 + Region + Age + FamilySituation + WinterCoverage +
                             MoneyCoverage + VehicleCoverage + MedicalCoverage +
                             AccidentCoverage + CancelCoverage + 
                              offset(log(Exposure)), ~ 1 + Region + Age + FamilySituation + WinterCoverage +
                             MoneyCoverage + VehicleCoverage + MedicalCoverage +
                             AccidentCoverage + CancelCoverage),
                       data = T_Freq_Train, family = ziplss())

GLM_ZIP_T_Freq_fc <- predict(GLM_ZIP_T_Freq, T_Freq_Test, type = 'response')

##### GLM severity #####
### T - Gamma model, forecasts and information criteria
GLM_G_T_Sev <- glm(Size ~ 1 + Region + Age + FamilySituation + WinterCoverage +
                      MoneyCoverage + VehicleCoverage + MedicalCoverage +
                      AccidentCoverage + CancelCoverage,
                      data = T_Sev_Train, family = Gamma(link = 'log'))

GLM_G_T_Sev_fc <- predict(GLM_G_T_Sev, T_Freq_Test, type = 'response')

IC_T <- c(AIC(GLM_P_T_Sev), BIC(GLM_P_T_Sev))

### T - IG model, forecasts and information criteria
GLM_IG_T_Sev <- glm(Size ~ 1 + Region + Age + FamilySituation + WinterCoverage +
                       MoneyCoverage + VehicleCoverage + MedicalCoverage +
                       AccidentCoverage + CancelCoverage,
                       data = T_Sev_Train, family = inverse.gaussian(link = 'log'), 
                       start = coef(GLM_G_T_Sev))

GLM_IG_T_Sev_fc <- predict(GLM_IG_T_Sev, T_Freq_Test, type = 'response')

IC_T <- rbind(IC_T, c(AIC(GLM_NB_T_Sev), BIC(GLM_NB_T_Sev)))

### GL - Pareto model, forecasts and information criteria
GLM_P_T_Sev <- gamlss(Size ~ 1 + Region + Age + FamilySituation + WinterCoverage +
                        MoneyCoverage + VehicleCoverage + MedicalCoverage +
                        AccidentCoverage + CancelCoverage, data = T_Sev_Train,
                       family = PARETO2o(mu.link = 'log', sigma.link = 'log'),
                       trace = FALSE)

GLM_P_T_Sev_fc <- predict(GLM_P_T_Sev, what = 'mu', newdata = 
                             T_Freq_Test[, c('Size', 'Region', 'Age','FamilySituation', 'WinterCoverage',
                                             'MoneyCoverage', 'VehicleCoverage', 'MedicalCoverage',
                                             'AccidentCoverage', 'CancelCoverage')], type = 'response')

IC_T <- rbind(IC_T, c(AIC(GLM_P_T_Sev), BIC(GLM_P_T_Sev)))

##### GLM predictions #####
### T
GLM_PG_T_fc <- GLM_P_T_Freq_fc * GLM_G_T_Sev_fc
GLM_PIG_T_fc <- GLM_P_T_Freq_fc * GLM_IG_T_Sev_fc
GLM_PP_T_fc <- GLM_P_T_Freq_fc * GLM_P_T_Sev_fc
GLM_NBG_T_fc <- GLM_NB_T_Freq_fc * GLM_G_T_Sev_fc
GLM_NBIG_T_fc <- GLM_NB_T_Freq_fc * GLM_IG_T_Sev_fc
GLM_NBP_T_fc <- GLM_NB_T_Freq_fc * GLM_P_T_Sev_fc
GLM_ZIPG_T_fc <- GLM_ZIP_T_Freq_fc * GLM_G_T_Sev_fc
GLM_ZIPIG_T_fc <- GLM_ZIP_T_Freq_fc * GLM_IG_T_Sev_fc
GLM_ZIPP_T_fc <- GLM_ZIP_T_Freq_fc * GLM_P_T_Sev_fc

##### Score settings #####
### Settings - Input
# Maximum score:
s <- c(3:25)
# Jump parameter:
Psi <- c(1:(max(s) - 1))
# Number of spline parameters:
k_cr <- 4

### Settings - Fixed
# Jump size after a claim-free period:
L_Bonus <- 1
# Lowest score:
L_Min <- 1
# Initial score for new policyholders:
ell_0 <- c((L_Min + 1):(max(s) - 1))

### Setup output
# ell_0 x s x Psi x (Ratio Gini, Variance) x (Benchmark, Alternative) x (Gamma, IG, Pareto) x (Poisson, NB, ZIP) x (GAM, Linear)
Score_Out_samp_T <- array(NA, dim = c(length(ell_0), length(s), length(Psi), 2, 2, 3, 3, 2))

### Setup custom R functions
# Preparatory function for (faster) claim score optimization
Input_Score <- dget('.../Custom_R_functions/Input_Score.R')
# Function for claim score calculation
Score <- dget('.../Custom_R_functions/Score.R')
# Function for ratio Gini coefficient calculation
Gini <- dget('.../Custom_R_functions/Gini.R')

### Setup claim score - T
T_Freq$Score <- 0
Input_Score_T <- Input_Score(Data = T_Freq)

##### Parallel setup #####
Cores <- detectCores() - 1
Clusters <- makeCluster(Cores) 
registerDoParallel(Clusters)

##### Score iterations - Parallel #####
### Loop over the maximum score levels (s)
for (maxi in s) {
  maxi_i <- which(s == maxi)
  # Only consider initial score levels where ell_0 < s:
  ell_0_maxi <- ell_0[which(ell_0 < maxi)]
  # Only consider jump parameters where Psi < s:
  Psi_maxi <- Psi[which(Psi < maxi)]
  
  ### Loop over the feasible jump parameters (Psi_maxi)
  for (mal in Psi_maxi) {
    mal_i <- which(Psi == mal)
  
    # Keep track of the progress:
    print(paste('Max ', maxi_i, '/', length(s),
                ', Malus ', mal_i, '/', length(Psi_maxi),
                ', s (', length(ell_0_maxi), ')',
                sep = ''))
    
    ### Loop over the feasible initial score levels (ell_0_maxi) - Parallel
    CS_Temp <- foreach(start_i = 1:length(ell_0_maxi), .packages = c('mgcv', 'MASS', 'gamlss')) %dopar% {
      start <- ell_0_maxi[start_i]
      
      Temp_T <- c()
      
      ##### Estimation - T #####
      ### Calculate the claim scores
      T_Freq$Score <- Score(Psi = mal, s = maxi, l_0 = start, Input_Score = Input_Score_T)
      T_Freq$Score_start <- T_Freq$Score - start

      ### Split data into training and test set
      T_Freq_Train <- T_Freq[which((as.Date(T_Freq$dt_End) <= as.Date(dt_Split)) & (T_Freq$History == 0)), ]
      T_Freq_Test <- T_Freq[which((as.Date(T_Freq$dt_Start) >= as.Date(dt_Split)) & (T_Freq$History == 0)), ]

      ### Output - GAM - Poisson
      # Predictions:
      GAM_P_T_fc <- predict(gam(Count ~ 1 + Region + Age + FamilySituation + WinterCoverage +
                                   MoneyCoverage + VehicleCoverage + MedicalCoverage +
                                   AccidentCoverage + CancelCoverage + 
                                   s(Score, bs = 'cr', k = k_cr, fx = TRUE, pc = start),
                                   offset = log(Exposure), data = T_Freq_Train, family = poisson(link = 'log')),
                               T_Freq_Test, type = 'response')
      # Ratio Gini coefficients with standard errors:
      Temp_T <- c(Temp_T, c(100, 1) * Gini(Candidate = GLM_PG_T_fc, Reference = GAM_P_T_fc * GLM_G_T_Sev_fc,
                                               Actual = T_Freq_Test$Size)[[1]],
                    c(100, 1) * Gini(Candidate = GAM_P_T_fc * GLM_G_T_Sev_fc, Reference = GLM_PG_T_fc,
                                     Actual = T_Freq_Test$Size)[[1]],
                    c(100, 1) * Gini(Candidate = GLM_PIG_T_fc, Reference = GAM_P_T_fc * GLM_IG_T_Sev_fc,
                                     Actual = T_Freq_Test$Size)[[1]],
                    c(100, 1) * Gini(Candidate = GAM_P_T_fc * GLM_IG_T_Sev_fc, Reference = GLM_PIG_T_fc,
                                     Actual = T_Freq_Test$Size)[[1]],
                  c(100, 1) * Gini(Candidate = GLM_PP_T_fc, Reference = GAM_P_T_fc * GLM_P_T_Sev_fc, 
                                   Actual = T_Freq_Test$Size)[[1]],
                  c(100, 1) * Gini(Candidate = GAM_P_T_fc * GLM_P_T_Sev_fc, Reference = GLM_PP_T_fc, 
                                   Actual = T_Freq_Test$Size)[[1]])

      ### Output - GAM - NB
      # Predictions:
      GAM_NB_T_fc <- predict(gam(Count ~ 1 + Region + Age + FamilySituation + WinterCoverage +
                                    MoneyCoverage + VehicleCoverage + MedicalCoverage +
                                    AccidentCoverage + CancelCoverage + 
                                    s(Score, bs = 'cr', k = k_cr, fx = TRUE, pc = start),
                                    offset = log(Exposure), data = T_Freq_Train, family = nb(link = 'log')),
                                T_Freq_Test, type = 'response')
      # Ratio Gini coefficients with standard errors:
      Temp_T <- c(Temp_T, c(100, 1) * Gini(Candidate = GLM_NBG_T_fc, Reference = GAM_NB_T_fc * GLM_G_T_Sev_fc,
                                               Actual = T_Freq_Test$Size)[[1]],
                    c(100, 1) * Gini(Candidate = GAM_NB_T_fc * GLM_G_T_Sev_fc, Reference = GLM_NBG_T_fc,
                                     Actual = T_Freq_Test$Size)[[1]],
                    c(100, 1) * Gini(Candidate = GLM_NBIG_T_fc, Reference = GAM_NB_T_fc * GLM_IG_T_Sev_fc,
                                     Actual = T_Freq_Test$Size)[[1]],
                    c(100, 1) * Gini(Candidate = GAM_NB_T_fc * GLM_IG_T_Sev_fc, Reference = GLM_NBIG_T_fc,
                                     Actual = T_Freq_Test$Size)[[1]],
                  c(100, 1) * Gini(Candidate = GLM_NBP_T_fc, Reference = GAM_NB_T_fc * GLM_P_T_Sev_fc, 
                                   Actual = T_Freq_Test$Size)[[1]],
                  c(100, 1) * Gini(Candidate = GAM_NB_T_fc * GLM_P_T_Sev_fc, Reference = GLM_NBP_T_fc, 
                                   Actual = T_Freq_Test$Size)[[1]])
      
      ### Output - GAM - ZIP
      # Predictions:
      GAM_ZIP_T_fc <- predict(gam(list(Count ~ 1 + Region + Age + FamilySituation + WinterCoverage +
                                         MoneyCoverage + VehicleCoverage + MedicalCoverage +
                                         AccidentCoverage + CancelCoverage +
                                         s(Score, bs = 'cr', k = k_cr, fx = TRUE, pc = start) + 
                                         offset(log(Exposure)), ~ 1 + Region + Age + FamilySituation + WinterCoverage +
                                         MoneyCoverage + VehicleCoverage + MedicalCoverage +
                                         AccidentCoverage + CancelCoverage),
                                  data = T_Freq_Train, family = ziplss()),
                              T_Freq_Test, type = 'response')
      # Ratio Gini coefficients with standard errors:
      Temp_T <- c(Temp_T, c(100, 1) * Gini(Candidate = GLM_ZIPG_T_fc, Reference = GAM_ZIP_T_fc * GLM_G_T_Sev_fc, 
                                             Actual = T_Freq_Test$Size)[[1]],
                   c(100, 1) * Gini(Candidate = GAM_ZIP_T_fc * GLM_G_T_Sev_fc, Reference = GLM_ZIPG_T_fc, 
                                    Actual = T_Freq_Test$Size)[[1]],
                   c(100, 1) * Gini(Candidate = GLM_ZIPIG_T_fc, Reference = GAM_ZIP_T_fc * GLM_IG_T_Sev_fc, 
                                    Actual = T_Freq_Test$Size)[[1]],
                   c(100, 1) * Gini(Candidate = GAM_ZIP_T_fc * GLM_IG_T_Sev_fc, Reference = GLM_ZIPIG_T_fc, 
                                    Actual = T_Freq_Test$Size)[[1]],
                   c(100, 1) * Gini(Candidate = GLM_ZIPP_T_fc, Reference = GAM_ZIP_T_fc * GLM_P_T_Sev_fc, 
                                    Actual = T_Freq_Test$Size)[[1]],
                   c(100, 1) * Gini(Candidate = GAM_ZIP_T_fc * GLM_P_T_Sev_fc, Reference = GLM_ZIPP_T_fc, 
                                    Actual = T_Freq_Test$Size)[[1]])

      ### Output - Linear - Poisson
      # Predictions:
      Lin_P_T_fc <- predict(glm(Count ~ 1 + Region + Age + FamilySituation + WinterCoverage +
                                   MoneyCoverage + VehicleCoverage + MedicalCoverage +
                                   AccidentCoverage + CancelCoverage + 
                                   I(Score - start),
                                   offset = log(Exposure), data = T_Freq_Train, family = poisson(link = 'log')),
                               T_Freq_Test, type = 'response')
      # Ratio Gini coefficients with standard errors:
      Temp_T <- c(Temp_T, c(100, 1) * Gini(Candidate = GLM_PG_T_fc, Reference = Lin_P_T_fc * GLM_G_T_Sev_fc,
                                               Actual = T_Freq_Test$Size)[[1]],
                    c(100, 1) * Gini(Candidate = Lin_P_T_fc * GLM_G_T_Sev_fc, Reference = GLM_PG_T_fc,
                                     Actual = T_Freq_Test$Size)[[1]],
                    c(100, 1) * Gini(Candidate = GLM_PIG_T_fc, Reference = Lin_P_T_fc * GLM_IG_T_Sev_fc,
                                     Actual = T_Freq_Test$Size)[[1]],
                    c(100, 1) * Gini(Candidate = Lin_P_T_fc * GLM_IG_T_Sev_fc, Reference = GLM_PIG_T_fc,
                                     Actual = T_Freq_Test$Size)[[1]],
                  c(100, 1) * Gini(Candidate = GLM_PP_T_fc, Reference = Lin_P_T_fc * GLM_P_T_Sev_fc, 
                                   Actual = T_Freq_Test$Size)[[1]],
                  c(100, 1) * Gini(Candidate = Lin_P_T_fc * GLM_P_T_Sev_fc, Reference = GLM_PP_T_fc, 
                                   Actual = T_Freq_Test$Size)[[1]])

      ### Output - Linear - NB
      # Predictions:
      Lin_NB_T_fc <- predict(glm.nb(Count ~ 1 + Region + Age + FamilySituation + WinterCoverage +
                                       MoneyCoverage + VehicleCoverage + MedicalCoverage +
                                       AccidentCoverage + CancelCoverage + 
                                       I(Score - start) +
                                       offset(log(Exposure)), data = T_Freq_Train, link = log),
                                T_Freq_Test, type = 'response')
      # Ratio Gini coefficients with standard errors:
      Temp_T <- c(Temp_T, c(100, 1) * Gini(Candidate = GLM_NBG_T_fc, Reference = Lin_NB_T_fc * GLM_G_T_Sev_fc,
                                               Actual = T_Freq_Test$Size)[[1]],
                    c(100, 1) * Gini(Candidate = Lin_NB_T_fc * GLM_G_T_Sev_fc, Reference = GLM_NBG_T_fc,
                                     Actual = T_Freq_Test$Size)[[1]],
                    c(100, 1) * Gini(Candidate = GLM_NBIG_T_fc, Reference = Lin_NB_T_fc * GLM_IG_T_Sev_fc,
                                     Actual = T_Freq_Test$Size)[[1]],
                    c(100, 1) * Gini(Candidate = Lin_NB_T_fc * GLM_IG_T_Sev_fc, Reference = GLM_NBIG_T_fc,
                                     Actual = T_Freq_Test$Size)[[1]],
                  c(100, 1) * Gini(Candidate = GLM_NBP_T_fc, Reference = Lin_NB_T_fc * GLM_P_T_Sev_fc, 
                                   Actual = T_Freq_Test$Size)[[1]],
                  c(100, 1) * Gini(Candidate = Lin_NB_T_fc * GLM_P_T_Sev_fc, Reference = GLM_NBP_T_fc, 
                                   Actual = T_Freq_Test$Size)[[1]])
      
      ### Output - Linear - ZIP
      # Predictions:
      Lin_ZIP_T_fc <- predict(gam(list(Count ~ 1 + Region + Age + FamilySituation + WinterCoverage +
                                         MoneyCoverage + VehicleCoverage + MedicalCoverage +
                                         AccidentCoverage + CancelCoverage +
                                         Score_start +
                                         offset(log(Exposure)), ~ 1 + Region + Age + FamilySituation + WinterCoverage +
                                         MoneyCoverage + VehicleCoverage + MedicalCoverage +
                                         AccidentCoverage + CancelCoverage),
                                  data = T_Freq_Train, family = ziplss()),
                              T_Freq_Test, type = 'response')
      # Ratio Gini coefficients with standard errors:
      Temp_T <- c(Temp_T, c(100, 1) * Gini(Candidate = GLM_ZIPG_T_fc, Reference = Lin_ZIP_T_fc * GLM_G_T_Sev_fc, 
                                             Actual = T_Freq_Test$Size)[[1]],
                   c(100, 1) * Gini(Candidate = Lin_ZIP_T_fc * GLM_G_T_Sev_fc, Reference = GLM_ZIPG_T_fc, 
                                    Actual = T_Freq_Test$Size)[[1]],
                   c(100, 1) * Gini(Candidate = GLM_ZIPIG_T_fc, Reference = Lin_ZIP_T_fc * GLM_IG_T_Sev_fc, 
                                    Actual = T_Freq_Test$Size)[[1]],
                   c(100, 1) * Gini(Candidate = Lin_ZIP_T_fc * GLM_IG_T_Sev_fc, Reference = GLM_ZIPIG_T_fc, 
                                    Actual = T_Freq_Test$Size)[[1]],
                   c(100, 1) * Gini(Candidate = GLM_ZIPP_T_fc, Reference = Lin_ZIP_T_fc * GLM_P_T_Sev_fc, 
                                    Actual = T_Freq_Test$Size)[[1]],
                   c(100, 1) * Gini(Candidate = Lin_ZIP_T_fc * GLM_P_T_Sev_fc, Reference = GLM_ZIPP_T_fc, 
                                    Actual = T_Freq_Test$Size)[[1]])

      ### Collect all output
      Temp_T
    }
    
    ### Store parallel output
    CS_Temp <- t(sapply(CS_Temp, rbind))
    dim(CS_Temp) <- c(length(ell_0_maxi), dim(Score_Out_samp_T)[-c(1:3)])
    Score_Out_samp_T[1:length(ell_0_maxi), maxi_i, mal_i, , , , , ] <- CS_Temp
  }
}
stopCluster(Clusters)

### Store the final results
