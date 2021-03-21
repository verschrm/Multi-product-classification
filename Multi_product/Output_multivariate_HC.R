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

##### Optimal score data #####
### Other products first
# Load results from Output_univariate_GL.R
# Load results from Output_univariate_H.R
# Load results from Output_univariate_T.R

### Main product - HC
# Load results from Output_univariate_HC.R

### Initialize claim score functions
# Preparatory function for (faster) claim score optimization
Input_Score <- dget('.../Custom_R_functions/Input_Score.R')
# Function for claim score calculation
Score <- dget('.../Custom_R_functions/Score.R')
# Function for claim score calculation on other products
Multi_Score <- dget('.../Custom_R_functions/Multi_Score.R')
# Split date:
dt_Split <- '2018-01-01'
# Number of spline parameters:
k_cr <- 4

##### Multi-product models #####
### GAM - PG - HC
# Retrieve optimal claim score parameters:
fm <- 1
mal <- c(Opt_Scores_GL[fm, 3], Opt_Scores_HC[fm, 3], Opt_Scores_H[fm, 3], Opt_Scores_T[fm, 3])
maxi <- c(Opt_Scores_GL[fm, 2], Opt_Scores_HC[fm, 2], Opt_Scores_H[fm, 2], Opt_Scores_T[fm, 2])
start <- c(Opt_Scores_GL[fm, 1], Opt_Scores_HC[fm, 1], Opt_Scores_H[fm, 1], Opt_Scores_T[fm, 1])
# Calculate the (multi) claim scores:
GL_Freq$Score <- Score(Psi = mal[1], s = maxi[1], l_0 = start[1], Input_Score = Input_Score_GL)
HC_Freq$Score_HC <- Score(Psi = mal[2], s = maxi[2], l_0 = start[2], Input_Score = Input_Score_HC)
H_Freq$Score <- Score(Psi = mal[3], s = maxi[3], l_0 = start[3], Input_Score = Input_Score_H)
T_Freq$Score <- Score(Psi = mal[4], s = maxi[4], l_0 = start[4], Input_Score = Input_Score_T)

HC_Freq$Score_GL <- Multi_Score(Input_Score_A = Input_Score_HC, Data_A = HC_Freq, Data_B = GL_Freq, l_0_B = start[1])
HC_Freq$Score_H <- Multi_Score(Input_Score_A = Input_Score_HC, Data_A = HC_Freq, Data_B = H_Freq, l_0_B = start[3])
HC_Freq$Score_T <- Multi_Score(Input_Score_A = Input_Score_HC, Data_A = HC_Freq, Data_B = T_Freq, l_0_B = start[4])

# Split data into training and test set:
HC_Freq_Train <- HC_Freq[which((as.Date(HC_Freq$dt_End) <= as.Date(dt_Split)) & (HC_Freq$History == 0)), ]
HC_Freq_Test <- HC_Freq[which((as.Date(HC_Freq$dt_Start) >= as.Date(dt_Split)) & (HC_Freq$History == 0)), ]
# Estimate the model:
GAM_Freq_HC <- gam(Count ~ 1 + ProductType + Year + Age + 
                 BuildingType + RoofType + FloorSpace + HomeOwner + Residence + 
                 Urban + GlassCoverage + 
                 s(Score_GL, bs = 'cr', k = k_cr, fx = TRUE, pc = start[1]) +
                 s(Score_HC, bs = 'cr', k = k_cr, fx = TRUE, pc = start[2]) +
                 s(Score_H, bs = 'cr', k = k_cr, fx = TRUE, pc = start[3]) +
                 s(Score_T, bs = 'cr', k = k_cr, fx = TRUE, pc = start[4]), 
                 offset = log(Exposure), data = HC_Freq_Train, family = poisson(link = 'log'))
# Plot the splines:
plot(GAM_Freq_HC, col = 'blue', xlab = 'Claim score level', ylab = 'Estimated effect', pages = 1)
# Summarize the model output:
summary(GAM_Freq_HC)
# Retrieve the model predictions:
GAM_PG_fc <- predict(GAM_Freq_HC, HC_Freq_Test, type = 'response')
# Determine AIC and BIC:
IC_HC <- rbind(IC_HC, c(AIC(GAM_Freq_HC), BIC(GAM_Freq_HC)))
# Retrieve log-likelihood and number of parameters:
logL_Multi_HC <- logLik(GAM_Freq_HC)
K_Multi_HC <- length(GAM_Freq_HC$coefficients)

### GAM - PIG - HC
# Retrieve optimal claim score parameters:
fm <- 2
mal <- c(Opt_Scores_GL[fm, 3], Opt_Scores_HC[fm, 3], Opt_Scores_H[fm, 3], Opt_Scores_T[fm, 3])
maxi <- c(Opt_Scores_GL[fm, 2], Opt_Scores_HC[fm, 2], Opt_Scores_H[fm, 2], Opt_Scores_T[fm, 2])
start <- c(Opt_Scores_GL[fm, 1], Opt_Scores_HC[fm, 1], Opt_Scores_H[fm, 1], Opt_Scores_T[fm, 1])
# Calculate the (multi) claim scores:
GL_Freq$Score <- Score(Psi = mal[1], s = maxi[1], l_0 = start[1], Input_Score = Input_Score_GL)
HC_Freq$Score_HC <- Score(Psi = mal[2], s = maxi[2], l_0 = start[2], Input_Score = Input_Score_HC)
H_Freq$Score <- Score(Psi = mal[3], s = maxi[3], l_0 = start[3], Input_Score = Input_Score_H)
T_Freq$Score <- Score(Psi = mal[4], s = maxi[4], l_0 = start[4], Input_Score = Input_Score_T)

HC_Freq$Score_GL <- Multi_Score(Input_Score_A = Input_Score_HC, Data_A = HC_Freq, Data_B = GL_Freq, l_0_B = start[1])
HC_Freq$Score_H <- Multi_Score(Input_Score_A = Input_Score_HC, Data_A = HC_Freq, Data_B = H_Freq, l_0_B = start[3])
HC_Freq$Score_T <- Multi_Score(Input_Score_A = Input_Score_HC, Data_A = HC_Freq, Data_B = T_Freq, l_0_B = start[4])

# Split data into training and test set:
HC_Freq_Train <- HC_Freq[which((as.Date(HC_Freq$dt_End) <= as.Date(dt_Split)) & (HC_Freq$History == 0)), ]
HC_Freq_Test <- HC_Freq[which((as.Date(HC_Freq$dt_Start) >= as.Date(dt_Split)) & (HC_Freq$History == 0)), ]
# Estimate the model:
GAM_Freq_HC <- gam(Count ~ 1 + ProductType + Year + Age + 
                 BuildingType + RoofType + FloorSpace + HomeOwner + Residence + 
                 Urban + GlassCoverage + 
                 s(Score_GL, bs = 'cr', k = k_cr, fx = TRUE, pc = start[1]) +
                 s(Score_HC, bs = 'cr', k = k_cr, fx = TRUE, pc = start[2]) +
                 s(Score_H, bs = 'cr', k = k_cr, fx = TRUE, pc = start[3]) +
                 s(Score_T, bs = 'cr', k = k_cr, fx = TRUE, pc = start[4]), 
                 offset = log(Exposure), data = HC_Freq_Train, family = poisson(link = 'log'))
# Plot the splines:
plot(GAM_Freq_HC, col = 'blue', xlab = 'Claim score level', ylab = 'Estimated effect', pages = 1)
# Summarize the model output:
summary(GAM_Freq_HC)
# Retrieve the model predictions:
GAM_PIG_fc <- predict(GAM_Freq_HC, HC_Freq_Test, type = 'response')
# Determine AIC and BIC:
IC_HC <- rbind(IC_HC, c(AIC(GAM_Freq_HC), BIC(GAM_Freq_HC)))
# Retrieve log-likelihood and number of parameters:
logL_Multi_HC <- rbind(logL_Multi_HC, logLik(GAM_Freq_HC))
K_Multi_HC <- rbind(K_Multi_HC, length(GAM_Freq_HC$coefficients))

### GAM - PP - HC
# Retrieve optimal claim score parameters:
fm <- 3
mal <- c(Opt_Scores_GL[fm, 3], Opt_Scores_HC[fm, 3], Opt_Scores_H[fm, 3], Opt_Scores_T[fm, 3])
maxi <- c(Opt_Scores_GL[fm, 2], Opt_Scores_HC[fm, 2], Opt_Scores_H[fm, 2], Opt_Scores_T[fm, 2])
start <- c(Opt_Scores_GL[fm, 1], Opt_Scores_HC[fm, 1], Opt_Scores_H[fm, 1], Opt_Scores_T[fm, 1])
# Calculate the (multi) claim scores:
GL_Freq$Score <- Score(Psi = mal[1], s = maxi[1], l_0 = start[1], Input_Score = Input_Score_GL)
HC_Freq$Score_HC <- Score(Psi = mal[2], s = maxi[2], l_0 = start[2], Input_Score = Input_Score_HC)
H_Freq$Score <- Score(Psi = mal[3], s = maxi[3], l_0 = start[3], Input_Score = Input_Score_H)
T_Freq$Score <- Score(Psi = mal[4], s = maxi[4], l_0 = start[4], Input_Score = Input_Score_T)

HC_Freq$Score_GL <- Multi_Score(Input_Score_A = Input_Score_HC, Data_A = HC_Freq, Data_B = GL_Freq, l_0_B = start[1])
HC_Freq$Score_H <- Multi_Score(Input_Score_A = Input_Score_HC, Data_A = HC_Freq, Data_B = H_Freq, l_0_B = start[3])
HC_Freq$Score_T <- Multi_Score(Input_Score_A = Input_Score_HC, Data_A = HC_Freq, Data_B = T_Freq, l_0_B = start[4])

# Split data into training and test set:
HC_Freq_Train <- HC_Freq[which((as.Date(HC_Freq$dt_End) <= as.Date(dt_Split)) & (HC_Freq$History == 0)), ]
HC_Freq_Test <- HC_Freq[which((as.Date(HC_Freq$dt_Start) >= as.Date(dt_Split)) & (HC_Freq$History == 0)), ]
# Estimate the model:
GAM_Freq_HC <- gam(Count ~ 1 + ProductType + Year + Age + 
                     BuildingType + RoofType + FloorSpace + HomeOwner + Residence + 
                     Urban + GlassCoverage + 
                     s(Score_GL, bs = 'cr', k = k_cr, fx = TRUE, pc = start[1]) +
                     s(Score_HC, bs = 'cr', k = k_cr, fx = TRUE, pc = start[2]) +
                     s(Score_H, bs = 'cr', k = k_cr, fx = TRUE, pc = start[3]) +
                     s(Score_T, bs = 'cr', k = k_cr, fx = TRUE, pc = start[4]), 
                   offset = log(Exposure), data = HC_Freq_Train, family = poisson(link = 'log'))
# Plot the splines:
plot(GAM_Freq_HC, col = 'blue', xlab = 'Claim score level', ylab = 'Estimated effect', pages = 1)
# Summarize the model output:
summary(GAM_Freq_HC)
# Retrieve the model predictions:
GAM_PP_fc <- predict(GAM_Freq_HC, HC_Freq_Test, type = 'response')
# Determine AIC and BIC:
IC_HC <- rbind(IC_HC, c(AIC(GAM_Freq_HC), BIC(GAM_Freq_HC)))
# Retrieve log-likelihood and number of parameters:
logL_Multi_HC <- rbind(logL_Multi_HC, logLik(GAM_Freq_HC))
K_Multi_HC <- rbind(K_Multi_HC, length(GAM_Freq_HC$coefficients))

### GAM - NBG - HC
# Retrieve optimal claim score parameters:
fm <- 4
mal <- c(Opt_Scores_GL[fm, 3], Opt_Scores_HC[fm, 3], Opt_Scores_H[fm, 3], Opt_Scores_T[fm, 3])
maxi <- c(Opt_Scores_GL[fm, 2], Opt_Scores_HC[fm, 2], Opt_Scores_H[fm, 2], Opt_Scores_T[fm, 2])
start <- c(Opt_Scores_GL[fm, 1], Opt_Scores_HC[fm, 1], Opt_Scores_H[fm, 1], Opt_Scores_T[fm, 1])
# Calculate the (multi) claim scores:
GL_Freq$Score <- Score(Psi = mal[1], s = maxi[1], l_0 = start[1], Input_Score = Input_Score_GL)
HC_Freq$Score_HC <- Score(Psi = mal[2], s = maxi[2], l_0 = start[2], Input_Score = Input_Score_HC)
H_Freq$Score <- Score(Psi = mal[3], s = maxi[3], l_0 = start[3], Input_Score = Input_Score_H)
T_Freq$Score <- Score(Psi = mal[4], s = maxi[4], l_0 = start[4], Input_Score = Input_Score_T)

HC_Freq$Score_GL <- Multi_Score(Input_Score_A = Input_Score_HC, Data_A = HC_Freq, Data_B = GL_Freq, l_0_B = start[1])
HC_Freq$Score_H <- Multi_Score(Input_Score_A = Input_Score_HC, Data_A = HC_Freq, Data_B = H_Freq, l_0_B = start[3])
HC_Freq$Score_T <- Multi_Score(Input_Score_A = Input_Score_HC, Data_A = HC_Freq, Data_B = T_Freq, l_0_B = start[4])

# Split data into training and test set:
HC_Freq_Train <- HC_Freq[which((as.Date(HC_Freq$dt_End) <= as.Date(dt_Split)) & (HC_Freq$History == 0)), ]
HC_Freq_Test <- HC_Freq[which((as.Date(HC_Freq$dt_Start) >= as.Date(dt_Split)) & (HC_Freq$History == 0)), ]
# Estimate the model:
GAM_Freq_HC <- gam(Count ~ 1 + ProductType + Year + Age + 
                 BuildingType + RoofType + FloorSpace + HomeOwner + Residence + 
                 Urban + GlassCoverage +
                 s(Score_GL, bs = 'cr', k = k_cr, fx = TRUE, pc = start[1]) +
                 s(Score_HC, bs = 'cr', k = k_cr, fx = TRUE, pc = start[2]) +
                 s(Score_H, bs = 'cr', k = k_cr, fx = TRUE, pc = start[3]) +
                 s(Score_T, bs = 'cr', k = k_cr, fx = TRUE, pc = start[4]),
                 offset = log(Exposure), data = HC_Freq_Train, family = nb(link = 'log'))
# Plot the splines:
plot(GAM_Freq_HC, col = 'blue', xlab = 'Claim score level', ylab = 'Estimated effect', pages = 1)
# Summarize the model output:
summary(GAM_Freq_HC)
# Retrieve the model predictions:
GAM_NBG_fc <- predict(GAM_Freq_HC, HC_Freq_Test, type = 'response')
# Determine AIC and BIC:
IC_HC <- rbind(IC_HC, c(AIC(GAM_Freq_HC), BIC(GAM_Freq_HC)))
# Retrieve log-likelihood and number of parameters:
logL_Multi_HC <- rbind(logL_Multi_HC, logLik(GAM_Freq_HC))
K_Multi_HC <- rbind(K_Multi_HC, length(GAM_Freq_HC$coefficients))

### GAM - NBIG - HC
# Retrieve optimal claim score parameters:
fm <- 5
mal <- c(Opt_Scores_GL[fm, 3], Opt_Scores_HC[fm, 3], Opt_Scores_H[fm, 3], Opt_Scores_T[fm, 3])
maxi <- c(Opt_Scores_GL[fm, 2], Opt_Scores_HC[fm, 2], Opt_Scores_H[fm, 2], Opt_Scores_T[fm, 2])
start <- c(Opt_Scores_GL[fm, 1], Opt_Scores_HC[fm, 1], Opt_Scores_H[fm, 1], Opt_Scores_T[fm, 1])
# Calculate the (multi) claim scores:
GL_Freq$Score <- Score(Psi = mal[1], s = maxi[1], l_0 = start[1], Input_Score = Input_Score_GL)
HC_Freq$Score_HC <- Score(Psi = mal[2], s = maxi[2], l_0 = start[2], Input_Score = Input_Score_HC)
H_Freq$Score <- Score(Psi = mal[3], s = maxi[3], l_0 = start[3], Input_Score = Input_Score_H)
T_Freq$Score <- Score(Psi = mal[4], s = maxi[4], l_0 = start[4], Input_Score = Input_Score_T)

HC_Freq$Score_GL <- Multi_Score(Input_Score_A = Input_Score_HC, Data_A = HC_Freq, Data_B = GL_Freq, l_0_B = start[1])
HC_Freq$Score_H <- Multi_Score(Input_Score_A = Input_Score_HC, Data_A = HC_Freq, Data_B = H_Freq, l_0_B = start[3])
HC_Freq$Score_T <- Multi_Score(Input_Score_A = Input_Score_HC, Data_A = HC_Freq, Data_B = T_Freq, l_0_B = start[4])

# Split data into training and test set:
HC_Freq_Train <- HC_Freq[which((as.Date(HC_Freq$dt_End) <= as.Date(dt_Split)) & (HC_Freq$History == 0)), ]
HC_Freq_Test <- HC_Freq[which((as.Date(HC_Freq$dt_Start) >= as.Date(dt_Split)) & (HC_Freq$History == 0)), ]
# Estimate the model:
GAM_Freq_HC <- gam(Count ~ 1 + ProductType + Year + Age + 
                 BuildingType + RoofType + FloorSpace + HomeOwner + Residence + 
                 Urban + GlassCoverage +
                 s(Score_GL, bs = 'cr', k = k_cr, fx = TRUE, pc = start[1]) +
                 s(Score_HC, bs = 'cr', k = k_cr, fx = TRUE, pc = start[2]) +
                 s(Score_H, bs = 'cr', k = k_cr, fx = TRUE, pc = start[3]) +
                 s(Score_T, bs = 'cr', k = k_cr, fx = TRUE, pc = start[4]),
                 offset = log(Exposure), data = HC_Freq_Train, family = nb(link = 'log'))
# Plot the splines:
plot(GAM_Freq_HC, col = 'blue', xlab = 'Claim score level', ylab = 'Estimated effect', pages = 1)
# Summarize the model output:
summary(GAM_Freq_HC)
# Retrieve the model predictions:
GAM_NBIG_fc <- predict(GAM_Freq_HC, HC_Freq_Test, type = 'response')
# Determine AIC and BIC:
IC_HC <- rbind(IC_HC, c(AIC(GAM_Freq_HC), BIC(GAM_Freq_HC)))
# Retrieve log-likelihood and number of parameters:
logL_Multi_HC <- rbind(logL_Multi_HC, logLik(GAM_Freq_HC))
K_Multi_HC <- rbind(K_Multi_HC, length(GAM_Freq_HC$coefficients))

### GAM - NBP - HC
# Retrieve optimal claim score parameters:
fm <- 6
mal <- c(Opt_Scores_GL[fm, 3], Opt_Scores_HC[fm, 3], Opt_Scores_H[fm, 3], Opt_Scores_T[fm, 3])
maxi <- c(Opt_Scores_GL[fm, 2], Opt_Scores_HC[fm, 2], Opt_Scores_H[fm, 2], Opt_Scores_T[fm, 2])
start <- c(Opt_Scores_GL[fm, 1], Opt_Scores_HC[fm, 1], Opt_Scores_H[fm, 1], Opt_Scores_T[fm, 1])
# Calculate the (multi) claim scores:
GL_Freq$Score <- Score(Psi = mal[1], s = maxi[1], l_0 = start[1], Input_Score = Input_Score_GL)
HC_Freq$Score_HC <- Score(Psi = mal[2], s = maxi[2], l_0 = start[2], Input_Score = Input_Score_HC)
H_Freq$Score <- Score(Psi = mal[3], s = maxi[3], l_0 = start[3], Input_Score = Input_Score_H)
T_Freq$Score <- Score(Psi = mal[4], s = maxi[4], l_0 = start[4], Input_Score = Input_Score_T)

HC_Freq$Score_GL <- Multi_Score(Input_Score_A = Input_Score_HC, Data_A = HC_Freq, Data_B = GL_Freq, l_0_B = start[1])
HC_Freq$Score_H <- Multi_Score(Input_Score_A = Input_Score_HC, Data_A = HC_Freq, Data_B = H_Freq, l_0_B = start[3])
HC_Freq$Score_T <- Multi_Score(Input_Score_A = Input_Score_HC, Data_A = HC_Freq, Data_B = T_Freq, l_0_B = start[4])

# Split data into training and test set:
HC_Freq_Train <- HC_Freq[which((as.Date(HC_Freq$dt_End) <= as.Date(dt_Split)) & (HC_Freq$History == 0)), ]
HC_Freq_Test <- HC_Freq[which((as.Date(HC_Freq$dt_Start) >= as.Date(dt_Split)) & (HC_Freq$History == 0)), ]
# Estimate the model:
GAM_Freq_HC <- gam(Count ~ 1 + ProductType + Year + Age + 
                     BuildingType + RoofType + FloorSpace + HomeOwner + Residence + 
                     Urban + GlassCoverage +
                     s(Score_GL, bs = 'cr', k = k_cr, fx = TRUE, pc = start[1]) +
                     s(Score_HC, bs = 'cr', k = k_cr, fx = TRUE, pc = start[2]) +
                     s(Score_H, bs = 'cr', k = k_cr, fx = TRUE, pc = start[3]) +
                     s(Score_T, bs = 'cr', k = k_cr, fx = TRUE, pc = start[4]),
                   offset = log(Exposure), data = HC_Freq_Train, family = nb(link = 'log'))
# Plot the splines:
plot(GAM_Freq_HC, col = 'blue', xlab = 'Claim score level', ylab = 'Estimated effect', pages = 1)
# Summarize the model output:
summary(GAM_Freq_HC)
# Retrieve the model predictions:
GAM_NBP_fc <- predict(GAM_Freq_HC, HC_Freq_Test, type = 'response')
# Determine AIC and BIC:
IC_HC <- rbind(IC_HC, c(AIC(GAM_Freq_HC), BIC(GAM_Freq_HC)))
# Retrieve log-likelihood and number of parameters:
logL_Multi_HC <- rbind(logL_Multi_HC, logLik(GAM_Freq_HC))
K_Multi_HC <- rbind(K_Multi_HC, length(GAM_Freq_HC$coefficients))

### GAM - ZIPG - HC
# Retrieve optimal claim score parameters:
fm <- 7
mal <- c(Opt_Scores_GL[fm, 3], Opt_Scores_HC[fm, 3], Opt_Scores_H[fm, 3], Opt_Scores_T[fm, 3])
maxi <- c(Opt_Scores_GL[fm, 2], Opt_Scores_HC[fm, 2], Opt_Scores_H[fm, 2], Opt_Scores_T[fm, 2])
start <- c(Opt_Scores_GL[fm, 1], Opt_Scores_HC[fm, 1], Opt_Scores_H[fm, 1], Opt_Scores_T[fm, 1])
# Calculate the (multi) claim scores:
GL_Freq$Score <- Score(Psi = mal[1], s = maxi[1], l_0 = start[1], Input_Score = Input_Score_GL)
HC_Freq$Score_HC <- Score(Psi = mal[2], s = maxi[2], l_0 = start[2], Input_Score = Input_Score_HC)
H_Freq$Score <- Score(Psi = mal[3], s = maxi[3], l_0 = start[3], Input_Score = Input_Score_H)
T_Freq$Score <- Score(Psi = mal[4], s = maxi[4], l_0 = start[4], Input_Score = Input_Score_T)

HC_Freq$Score_GL <- Multi_Score(Input_Score_A = Input_Score_HC, Data_A = HC_Freq, Data_B = GL_Freq, l_0_B = start[1])
HC_Freq$Score_H <- Multi_Score(Input_Score_A = Input_Score_HC, Data_A = HC_Freq, Data_B = H_Freq, l_0_B = start[3])
HC_Freq$Score_T <- Multi_Score(Input_Score_A = Input_Score_HC, Data_A = HC_Freq, Data_B = T_Freq, l_0_B = start[4])

# Split data into training and test set:
HC_Freq_Train <- HC_Freq[which((as.Date(HC_Freq$dt_End) <= as.Date(dt_Split)) & (HC_Freq$History == 0)), ]
HC_Freq_Test <- HC_Freq[which((as.Date(HC_Freq$dt_Start) >= as.Date(dt_Split)) & (HC_Freq$History == 0)), ]
# Estimate the model:
GAM_Freq_HC <- gam(list(Count ~ 1 + ProductType + Year + Age + 
                          BuildingType + RoofType + FloorSpace + HomeOwner + Residence + 
                          Urban + GlassCoverage +
                          s(Score_GL, bs = 'cr', k = k_cr, fx = TRUE, pc = start[1]) +
                          s(Score_HC, bs = 'cr', k = k_cr, fx = TRUE, pc = start[2]) +
                          s(Score_H, bs = 'cr', k = k_cr, fx = TRUE, pc = start[3]) +
                          s(Score_T, bs = 'cr', k = k_cr, fx = TRUE, pc = start[4]) +
                          offset(log(Exposure)), ~ 1 + ProductType + Year + Age + 
                          BuildingType + RoofType + FloorSpace + HomeOwner + Residence + 
                          Urban + GlassCoverage), 
                   data = HC_Freq_Train, family = ziplss())
# Plot the splines:
plot(GAM_Freq_HC, col = 'blue', xlab = 'Claim score level', ylab = 'Estimated effect', pages = 1)
# Summarize the model output:
summary(GAM_Freq_HC)
# Retrieve the model predictions:
GAM_ZIPG_fc <- predict(GAM_Freq_HC, HC_Freq_Test, type = 'response')
# Determine AIC and BIC:
IC_HC <- rbind(IC_HC, c(AIC(GAM_Freq_HC), BIC(GAM_Freq_HC)))
# Retrieve log-likelihood and number of parameters:
logL_Multi_HC <- rbind(logL_Multi_HC, logLik(GAM_Freq_HC))
K_Multi_HC <- rbind(K_Multi_HC, length(GAM_Freq_HC$coefficients))

### GAM - ZIPIG - HC
# Retrieve optimal claim score parameters:
fm <- 8
mal <- c(Opt_Scores_GL[fm, 3], Opt_Scores_HC[fm, 3], Opt_Scores_H[fm, 3], Opt_Scores_T[fm, 3])
maxi <- c(Opt_Scores_GL[fm, 2], Opt_Scores_HC[fm, 2], Opt_Scores_H[fm, 2], Opt_Scores_T[fm, 2])
start <- c(Opt_Scores_GL[fm, 1], Opt_Scores_HC[fm, 1], Opt_Scores_H[fm, 1], Opt_Scores_T[fm, 1])
# Calculate the (multi) claim scores:
GL_Freq$Score <- Score(Psi = mal[1], s = maxi[1], l_0 = start[1], Input_Score = Input_Score_GL)
HC_Freq$Score_HC <- Score(Psi = mal[2], s = maxi[2], l_0 = start[2], Input_Score = Input_Score_HC)
H_Freq$Score <- Score(Psi = mal[3], s = maxi[3], l_0 = start[3], Input_Score = Input_Score_H)
T_Freq$Score <- Score(Psi = mal[4], s = maxi[4], l_0 = start[4], Input_Score = Input_Score_T)

HC_Freq$Score_GL <- Multi_Score(Input_Score_A = Input_Score_HC, Data_A = HC_Freq, Data_B = GL_Freq, l_0_B = start[1])
HC_Freq$Score_H <- Multi_Score(Input_Score_A = Input_Score_HC, Data_A = HC_Freq, Data_B = H_Freq, l_0_B = start[3])
HC_Freq$Score_T <- Multi_Score(Input_Score_A = Input_Score_HC, Data_A = HC_Freq, Data_B = T_Freq, l_0_B = start[4])

# Split data into training and test set:
HC_Freq_Train <- HC_Freq[which((as.Date(HC_Freq$dt_End) <= as.Date(dt_Split)) & (HC_Freq$History == 0)), ]
HC_Freq_Test <- HC_Freq[which((as.Date(HC_Freq$dt_Start) >= as.Date(dt_Split)) & (HC_Freq$History == 0)), ]
# Estimate the model:
GAM_Freq_HC <- gam(list(Count ~ 1 + ProductType + Year + Age + 
                          BuildingType + RoofType + FloorSpace + HomeOwner + Residence + 
                          Urban + GlassCoverage +
                          s(Score_GL, bs = 'cr', k = k_cr, fx = TRUE, pc = start[1]) +
                          s(Score_HC, bs = 'cr', k = k_cr, fx = TRUE, pc = start[2]) +
                          s(Score_H, bs = 'cr', k = k_cr, fx = TRUE, pc = start[3]) +
                          s(Score_T, bs = 'cr', k = k_cr, fx = TRUE, pc = start[4]) +
                          offset(log(Exposure)), ~ 1 + ProductType + Year + Age + 
                          BuildingType + RoofType + FloorSpace + HomeOwner + Residence + 
                          Urban + GlassCoverage), 
                   data = HC_Freq_Train, family = ziplss())
# Plot the splines:
plot(GAM_Freq_HC, col = 'blue', xlab = 'Claim score level', ylab = 'Estimated effect', pages = 1)
# Summarize the model output:
summary(GAM_Freq_HC)
# Retrieve the model predictions:
GAM_ZIPIG_fc <- predict(GAM_Freq_HC, HC_Freq_Test, type = 'response')
# Determine AIC and BIC:
IC_HC <- rbind(IC_HC, c(AIC(GAM_Freq_HC), BIC(GAM_Freq_HC)))
# Retrieve log-likelihood and number of parameters:
logL_Multi_HC <- rbind(logL_Multi_HC, logLik(GAM_Freq_HC))
K_Multi_HC <- rbind(K_Multi_HC, length(GAM_Freq_HC$coefficients))

### GAM - ZIPP - HC
# Retrieve optimal claim score parameters:
fm <- 9
mal <- c(Opt_Scores_GL[fm, 3], Opt_Scores_HC[fm, 3], Opt_Scores_H[fm, 3], Opt_Scores_T[fm, 3])
maxi <- c(Opt_Scores_GL[fm, 2], Opt_Scores_HC[fm, 2], Opt_Scores_H[fm, 2], Opt_Scores_T[fm, 2])
start <- c(Opt_Scores_GL[fm, 1], Opt_Scores_HC[fm, 1], Opt_Scores_H[fm, 1], Opt_Scores_T[fm, 1])
# Calculate the (multi) claim scores:
GL_Freq$Score <- Score(Psi = mal[1], s = maxi[1], l_0 = start[1], Input_Score = Input_Score_GL)
HC_Freq$Score_HC <- Score(Psi = mal[2], s = maxi[2], l_0 = start[2], Input_Score = Input_Score_HC)
H_Freq$Score <- Score(Psi = mal[3], s = maxi[3], l_0 = start[3], Input_Score = Input_Score_H)
T_Freq$Score <- Score(Psi = mal[4], s = maxi[4], l_0 = start[4], Input_Score = Input_Score_T)

HC_Freq$Score_GL <- Multi_Score(Input_Score_A = Input_Score_HC, Data_A = HC_Freq, Data_B = GL_Freq, l_0_B = start[1])
HC_Freq$Score_H <- Multi_Score(Input_Score_A = Input_Score_HC, Data_A = HC_Freq, Data_B = H_Freq, l_0_B = start[3])
HC_Freq$Score_T <- Multi_Score(Input_Score_A = Input_Score_HC, Data_A = HC_Freq, Data_B = T_Freq, l_0_B = start[4])

# Split data into training and test set:
HC_Freq_Train <- HC_Freq[which((as.Date(HC_Freq$dt_End) <= as.Date(dt_Split)) & (HC_Freq$History == 0)), ]
HC_Freq_Test <- HC_Freq[which((as.Date(HC_Freq$dt_Start) >= as.Date(dt_Split)) & (HC_Freq$History == 0)), ]
# Estimate the model:
GAM_Freq_HC <- gam(list(Count ~ 1 + ProductType + Year + Age + 
                          BuildingType + RoofType + FloorSpace + HomeOwner + Residence + 
                          Urban + GlassCoverage +
                          s(Score_GL, bs = 'cr', k = k_cr, fx = TRUE, pc = start[1]) +
                          s(Score_HC, bs = 'cr', k = k_cr, fx = TRUE, pc = start[2]) +
                          s(Score_H, bs = 'cr', k = k_cr, fx = TRUE, pc = start[3]) +
                          s(Score_T, bs = 'cr', k = k_cr, fx = TRUE, pc = start[4]) +
                          offset(log(Exposure)), ~ 1 + ProductType + Year + Age + 
                          BuildingType + RoofType + FloorSpace + HomeOwner + Residence + 
                          Urban + GlassCoverage), 
                   data = HC_Freq_Train, family = ziplss())
# Plot the splines:
plot(GAM_Freq_HC, col = 'blue', xlab = 'Claim score level', ylab = 'Estimated effect', pages = 1)
# Summarize the model output:
summary(GAM_Freq_HC)
# Retrieve the model predictions:
GAM_ZIPP_fc <- predict(GAM_Freq_HC, HC_Freq_Test, type = 'response')
# Determine AIC and BIC:
IC_HC <- rbind(IC_HC, c(AIC(GAM_Freq_HC), BIC(GAM_Freq_HC)))
# Retrieve log-likelihood and number of parameters:
logL_Multi_HC <- rbind(logL_Multi_HC, logLik(GAM_Freq_HC))
K_Multi_HC <- rbind(K_Multi_HC, length(GAM_Freq_HC$coefficients))

### Linear - PG - HC
# Retrieve optimal claim score parameters:
fm <- 10
mal <- c(Opt_Scores_GL[fm, 3], Opt_Scores_HC[fm, 3], Opt_Scores_H[fm, 3], Opt_Scores_T[fm, 3])
maxi <- c(Opt_Scores_GL[fm, 2], Opt_Scores_HC[fm, 2], Opt_Scores_H[fm, 2], Opt_Scores_T[fm, 2])
start <- c(Opt_Scores_GL[fm, 1], Opt_Scores_HC[fm, 1], Opt_Scores_H[fm, 1], Opt_Scores_T[fm, 1])
# Calculate the (multi) claim scores:
GL_Freq$Score <- Score(Psi = mal[1], s = maxi[1], l_0 = start[1], Input_Score = Input_Score_GL)
HC_Freq$Score_HC <- Score(Psi = mal[2], s = maxi[2], l_0 = start[2], Input_Score = Input_Score_HC)
H_Freq$Score <- Score(Psi = mal[3], s = maxi[3], l_0 = start[3], Input_Score = Input_Score_H)
T_Freq$Score <- Score(Psi = mal[4], s = maxi[4], l_0 = start[4], Input_Score = Input_Score_T)

HC_Freq$Score_GL <- Multi_Score(Input_Score_A = Input_Score_HC, Data_A = HC_Freq, Data_B = GL_Freq, l_0_B = start[1])
HC_Freq$Score_H <- Multi_Score(Input_Score_A = Input_Score_HC, Data_A = HC_Freq, Data_B = H_Freq, l_0_B = start[3])
HC_Freq$Score_T <- Multi_Score(Input_Score_A = Input_Score_HC, Data_A = HC_Freq, Data_B = T_Freq, l_0_B = start[4])

# Split data into training and test set:
HC_Freq_Train <- HC_Freq[which((as.Date(HC_Freq$dt_End) <= as.Date(dt_Split)) & (HC_Freq$History == 0)), ]
HC_Freq_Test <- HC_Freq[which((as.Date(HC_Freq$dt_Start) >= as.Date(dt_Split)) & (HC_Freq$History == 0)), ]
# Estimate the model:
Lin_Freq_HC <- glm(Count ~ 1 + ProductType + Year + Age + 
                 BuildingType + RoofType + FloorSpace + HomeOwner + Residence + 
                 Urban + GlassCoverage +
                 I(Score_GL - start[1]) +
                 I(Score_HC - start[2]) +
                 I(Score_H - start[3]) +
                 I(Score_T - start[4]),
                 offset = log(Exposure), data = HC_Freq_Train, family = poisson(link = 'log'))
# Summarize the model output:
summary(Lin_Freq_HC)
# Retrieve the model predictions:
Lin_PG_fc <- predict(Lin_Freq_HC, HC_Freq_Test, type = 'response')
# Determine AIC and BIC:
IC_HC <- rbind(IC_HC, c(AIC(Lin_L_HC), BIC(Lin_L_HC)))
# Retrieve log-likelihood and number of parameters:
logL_Multi_HC <- rbind(logL_Multi_HC, logLik(Lin_L_HC))
K_Multi_HC <- rbind(K_Multi_HC, length(Lin_L_HC$coefficients))

### Linear - PIG - HC
# Retrieve optimal claim score parameters:
fm <- 11
mal <- c(Opt_Scores_GL[fm, 3], Opt_Scores_HC[fm, 3], Opt_Scores_H[fm, 3], Opt_Scores_T[fm, 3])
maxi <- c(Opt_Scores_GL[fm, 2], Opt_Scores_HC[fm, 2], Opt_Scores_H[fm, 2], Opt_Scores_T[fm, 2])
start <- c(Opt_Scores_GL[fm, 1], Opt_Scores_HC[fm, 1], Opt_Scores_H[fm, 1], Opt_Scores_T[fm, 1])
# Calculate the (multi) claim scores:
GL_Freq$Score <- Score(Psi = mal[1], s = maxi[1], l_0 = start[1], Input_Score = Input_Score_GL)
HC_Freq$Score_HC <- Score(Psi = mal[2], s = maxi[2], l_0 = start[2], Input_Score = Input_Score_HC)
H_Freq$Score <- Score(Psi = mal[3], s = maxi[3], l_0 = start[3], Input_Score = Input_Score_H)
T_Freq$Score <- Score(Psi = mal[4], s = maxi[4], l_0 = start[4], Input_Score = Input_Score_T)

HC_Freq$Score_GL <- Multi_Score(Input_Score_A = Input_Score_HC, Data_A = HC_Freq, Data_B = GL_Freq, l_0_B = start[1])
HC_Freq$Score_H <- Multi_Score(Input_Score_A = Input_Score_HC, Data_A = HC_Freq, Data_B = H_Freq, l_0_B = start[3])
HC_Freq$Score_T <- Multi_Score(Input_Score_A = Input_Score_HC, Data_A = HC_Freq, Data_B = T_Freq, l_0_B = start[4])

# Split data into training and test set:
HC_Freq_Train <- HC_Freq[which((as.Date(HC_Freq$dt_End) <= as.Date(dt_Split)) & (HC_Freq$History == 0)), ]
HC_Freq_Test <- HC_Freq[which((as.Date(HC_Freq$dt_Start) >= as.Date(dt_Split)) & (HC_Freq$History == 0)), ]
# Estimate the model:
Lin_Freq_HC <- glm(Count ~ 1 + ProductType + Year + Age + 
                 BuildingType + RoofType + FloorSpace + HomeOwner + Residence + 
                 Urban + GlassCoverage +
                 I(Score_GL - start[1]) +
                 I(Score_HC - start[2]) +
                 I(Score_H - start[3]) +
                 I(Score_T - start[4]),
                 offset = log(Exposure), data = HC_Freq_Train, family = poisson(link = 'log'))
# Summarize the model output:
summary(Lin_Freq_HC)
# Retrieve the model predictions:
Lin_PIG_fc <- predict(Lin_Freq_HC, HC_Freq_Test, type = 'response')
# Determine AIC and BIC:
IC_HC <- rbind(IC_HC, c(AIC(Lin_L_HC), BIC(Lin_L_HC)))
# Retrieve log-likelihood and number of parameters:
logL_Multi_HC <- rbind(logL_Multi_HC, logLik(Lin_L_HC))
K_Multi_HC <- rbind(K_Multi_HC, length(Lin_L_HC$coefficients))

### Linear - PP - HC
# Retrieve optimal claim score parameters:
fm <- 12
mal <- c(Opt_Scores_GL[fm, 3], Opt_Scores_HC[fm, 3], Opt_Scores_H[fm, 3], Opt_Scores_T[fm, 3])
maxi <- c(Opt_Scores_GL[fm, 2], Opt_Scores_HC[fm, 2], Opt_Scores_H[fm, 2], Opt_Scores_T[fm, 2])
start <- c(Opt_Scores_GL[fm, 1], Opt_Scores_HC[fm, 1], Opt_Scores_H[fm, 1], Opt_Scores_T[fm, 1])
# Calculate the (multi) claim scores:
GL_Freq$Score <- Score(Psi = mal[1], s = maxi[1], l_0 = start[1], Input_Score = Input_Score_GL)
HC_Freq$Score_HC <- Score(Psi = mal[2], s = maxi[2], l_0 = start[2], Input_Score = Input_Score_HC)
H_Freq$Score <- Score(Psi = mal[3], s = maxi[3], l_0 = start[3], Input_Score = Input_Score_H)
T_Freq$Score <- Score(Psi = mal[4], s = maxi[4], l_0 = start[4], Input_Score = Input_Score_T)

HC_Freq$Score_GL <- Multi_Score(Input_Score_A = Input_Score_HC, Data_A = HC_Freq, Data_B = GL_Freq, l_0_B = start[1])
HC_Freq$Score_H <- Multi_Score(Input_Score_A = Input_Score_HC, Data_A = HC_Freq, Data_B = H_Freq, l_0_B = start[3])
HC_Freq$Score_T <- Multi_Score(Input_Score_A = Input_Score_HC, Data_A = HC_Freq, Data_B = T_Freq, l_0_B = start[4])

# Split data into training and test set:
HC_Freq_Train <- HC_Freq[which((as.Date(HC_Freq$dt_End) <= as.Date(dt_Split)) & (HC_Freq$History == 0)), ]
HC_Freq_Test <- HC_Freq[which((as.Date(HC_Freq$dt_Start) >= as.Date(dt_Split)) & (HC_Freq$History == 0)), ]
# Estimate the model:
Lin_Freq_HC <- glm(Count ~ 1 + ProductType + Year + Age + 
                     BuildingType + RoofType + FloorSpace + HomeOwner + Residence + 
                     Urban + GlassCoverage +
                     I(Score_GL - start[1]) +
                     I(Score_HC - start[2]) +
                     I(Score_H - start[3]) +
                     I(Score_T - start[4]),
                   offset = log(Exposure), data = HC_Freq_Train, family = poisson(link = 'log'))
# Summarize the model output:
summary(Lin_Freq_HC)
# Retrieve the model predictions:
Lin_PP_fc <- predict(Lin_Freq_HC, HC_Freq_Test, type = 'response')
# Determine AIC and BIC:
IC_HC <- rbind(IC_HC, c(AIC(Lin_L_HC), BIC(Lin_L_HC)))
# Retrieve log-likelihood and number of parameters:
logL_Multi_HC <- rbind(logL_Multi_HC, logLik(Lin_L_HC))
K_Multi_HC <- rbind(K_Multi_HC, length(Lin_L_HC$coefficients))

### Linear - NBG - HC
# Retrieve optimal claim score parameters:
fm <- 13
mal <- c(Opt_Scores_GL[fm, 3], Opt_Scores_HC[fm, 3], Opt_Scores_H[fm, 3], Opt_Scores_T[fm, 3])
maxi <- c(Opt_Scores_GL[fm, 2], Opt_Scores_HC[fm, 2], Opt_Scores_H[fm, 2], Opt_Scores_T[fm, 2])
start <- c(Opt_Scores_GL[fm, 1], Opt_Scores_HC[fm, 1], Opt_Scores_H[fm, 1], Opt_Scores_T[fm, 1])
# Calculate the (multi) claim scores:
GL_Freq$Score <- Score(Psi = mal[1], s = maxi[1], l_0 = start[1], Input_Score = Input_Score_GL)
HC_Freq$Score_HC <- Score(Psi = mal[2], s = maxi[2], l_0 = start[2], Input_Score = Input_Score_HC)
H_Freq$Score <- Score(Psi = mal[3], s = maxi[3], l_0 = start[3], Input_Score = Input_Score_H)
T_Freq$Score <- Score(Psi = mal[4], s = maxi[4], l_0 = start[4], Input_Score = Input_Score_T)

HC_Freq$Score_GL <- Multi_Score(Input_Score_A = Input_Score_HC, Data_A = HC_Freq, Data_B = GL_Freq, l_0_B = start[1])
HC_Freq$Score_H <- Multi_Score(Input_Score_A = Input_Score_HC, Data_A = HC_Freq, Data_B = H_Freq, l_0_B = start[3])
HC_Freq$Score_T <- Multi_Score(Input_Score_A = Input_Score_HC, Data_A = HC_Freq, Data_B = T_Freq, l_0_B = start[4])

# Split data into training and test set:
HC_Freq_Train <- HC_Freq[which((as.Date(HC_Freq$dt_End) <= as.Date(dt_Split)) & (HC_Freq$History == 0)), ]
HC_Freq_Test <- HC_Freq[which((as.Date(HC_Freq$dt_Start) >= as.Date(dt_Split)) & (HC_Freq$History == 0)), ]
# Estimate the model:
Lin_Freq_HC <- glm.nb(Count ~ 1 + ProductType + Year + Age + 
                    BuildingType + RoofType + FloorSpace + HomeOwner + Residence + 
                    Urban + GlassCoverage +
                    I(Score_GL - start[1]) +
                    I(Score_HC - start[2]) +
                    I(Score_H - start[3]) +
                    I(Score_T - start[4]) +
                    offset(log(Exposure)), data = HC_Freq_Train, link = log)
# Summarize the model output:
summary(Lin_Freq_HC)
# Retrieve the model predictions:
Lin_NBG_fc <- predict(Lin_Freq_HC, HC_Freq_Test, type = 'response')
# Determine AIC and BIC:
IC_HC <- rbind(IC_HC, c(AIC(Lin_L_HC), BIC(Lin_L_HC)))
# Retrieve log-likelihood and number of parameters:
logL_Multi_HC <- rbind(logL_Multi_HC, logLik(Lin_L_HC))
K_Multi_HC <- rbind(K_Multi_HC, length(Lin_L_HC$coefficients))

### Linear - NBIG - HC
# Retrieve optimal claim score parameters:
fm <- 14
mal <- c(Opt_Scores_GL[fm, 3], Opt_Scores_HC[fm, 3], Opt_Scores_H[fm, 3], Opt_Scores_T[fm, 3])
maxi <- c(Opt_Scores_GL[fm, 2], Opt_Scores_HC[fm, 2], Opt_Scores_H[fm, 2], Opt_Scores_T[fm, 2])
start <- c(Opt_Scores_GL[fm, 1], Opt_Scores_HC[fm, 1], Opt_Scores_H[fm, 1], Opt_Scores_T[fm, 1])
# Calculate the (multi) claim scores:
GL_Freq$Score <- Score(Psi = mal[1], s = maxi[1], l_0 = start[1], Input_Score = Input_Score_GL)
HC_Freq$Score_HC <- Score(Psi = mal[2], s = maxi[2], l_0 = start[2], Input_Score = Input_Score_HC)
H_Freq$Score <- Score(Psi = mal[3], s = maxi[3], l_0 = start[3], Input_Score = Input_Score_H)
T_Freq$Score <- Score(Psi = mal[4], s = maxi[4], l_0 = start[4], Input_Score = Input_Score_T)

HC_Freq$Score_GL <- Multi_Score(Input_Score_A = Input_Score_HC, Data_A = HC_Freq, Data_B = GL_Freq, l_0_B = start[1])
HC_Freq$Score_H <- Multi_Score(Input_Score_A = Input_Score_HC, Data_A = HC_Freq, Data_B = H_Freq, l_0_B = start[3])
HC_Freq$Score_T <- Multi_Score(Input_Score_A = Input_Score_HC, Data_A = HC_Freq, Data_B = T_Freq, l_0_B = start[4])

# Split data into training and test set:
HC_Freq_Train <- HC_Freq[which((as.Date(HC_Freq$dt_End) <= as.Date(dt_Split)) & (HC_Freq$History == 0)), ]
HC_Freq_Test <- HC_Freq[which((as.Date(HC_Freq$dt_Start) >= as.Date(dt_Split)) & (HC_Freq$History == 0)), ]
# Estimate the model:
Lin_Freq_HC <- glm.nb(Count ~ 1 + ProductType + Year + Age + 
                    BuildingType + RoofType + FloorSpace + HomeOwner + Residence + 
                    Urban + GlassCoverage +
                    I(Score_GL - start[1]) +
                    I(Score_HC - start[2]) +
                    I(Score_H - start[3]) +
                    I(Score_T - start[4]) +
                    offset(log(Exposure)), data = HC_Freq_Train, link = log)
# Summarize the model output:
summary(Lin_Freq_HC)
# Retrieve the model predictions:
Lin_NBIG_fc <- predict(Lin_Freq_HC, HC_Freq_Test, type = 'response')
# Determine AIC and BIC:
IC_HC <- rbind(IC_HC, c(AIC(Lin_L_HC), BIC(Lin_L_HC)))
# Retrieve log-likelihood and number of parameters:
logL_Multi_HC <- rbind(logL_Multi_HC, logLik(Lin_L_HC))
K_Multi_HC <- rbind(K_Multi_HC, length(Lin_L_HC$coefficients))

### Linear - NBP - HC
# Retrieve optimal claim score parameters:
fm <- 15
mal <- c(Opt_Scores_GL[fm, 3], Opt_Scores_HC[fm, 3], Opt_Scores_H[fm, 3], Opt_Scores_T[fm, 3])
maxi <- c(Opt_Scores_GL[fm, 2], Opt_Scores_HC[fm, 2], Opt_Scores_H[fm, 2], Opt_Scores_T[fm, 2])
start <- c(Opt_Scores_GL[fm, 1], Opt_Scores_HC[fm, 1], Opt_Scores_H[fm, 1], Opt_Scores_T[fm, 1])
# Calculate the (multi) claim scores:
GL_Freq$Score <- Score(Psi = mal[1], s = maxi[1], l_0 = start[1], Input_Score = Input_Score_GL)
HC_Freq$Score_HC <- Score(Psi = mal[2], s = maxi[2], l_0 = start[2], Input_Score = Input_Score_HC)
H_Freq$Score <- Score(Psi = mal[3], s = maxi[3], l_0 = start[3], Input_Score = Input_Score_H)
T_Freq$Score <- Score(Psi = mal[4], s = maxi[4], l_0 = start[4], Input_Score = Input_Score_T)

HC_Freq$Score_GL <- Multi_Score(Input_Score_A = Input_Score_HC, Data_A = HC_Freq, Data_B = GL_Freq, l_0_B = start[1])
HC_Freq$Score_H <- Multi_Score(Input_Score_A = Input_Score_HC, Data_A = HC_Freq, Data_B = H_Freq, l_0_B = start[3])
HC_Freq$Score_T <- Multi_Score(Input_Score_A = Input_Score_HC, Data_A = HC_Freq, Data_B = T_Freq, l_0_B = start[4])

# Split data into training and test set:
HC_Freq_Train <- HC_Freq[which((as.Date(HC_Freq$dt_End) <= as.Date(dt_Split)) & (HC_Freq$History == 0)), ]
HC_Freq_Test <- HC_Freq[which((as.Date(HC_Freq$dt_Start) >= as.Date(dt_Split)) & (HC_Freq$History == 0)), ]
# Estimate the model:
Lin_Freq_HC <- glm.nb(Count ~ 1 + ProductType + Year + Age + 
                        BuildingType + RoofType + FloorSpace + HomeOwner + Residence + 
                        Urban + GlassCoverage +
                        I(Score_GL - start[1]) +
                        I(Score_HC - start[2]) +
                        I(Score_H - start[3]) +
                        I(Score_T - start[4]) +
                        offset(log(Exposure)), data = HC_Freq_Train, link = log)
# Summarize the model output:
summary(Lin_Freq_HC)
# Retrieve the model predictions:
Lin_NBP_fc <- predict(Lin_Freq_HC, HC_Freq_Test, type = 'response')
# Determine AIC and BIC:
IC_HC <- rbind(IC_HC, c(AIC(Lin_L_HC), BIC(Lin_L_HC)))
# Retrieve log-likelihood and number of parameters:
logL_Multi_HC <- rbind(logL_Multi_HC, logLik(Lin_L_HC))
K_Multi_HC <- rbind(K_Multi_HC, length(Lin_L_HC$coefficients))

### Linear - ZIPG - HC
# Retrieve optimal claim score parameters:
fm <- 16
mal <- c(Opt_Scores_GL[fm, 3], Opt_Scores_HC[fm, 3], Opt_Scores_H[fm, 3], Opt_Scores_T[fm, 3])
maxi <- c(Opt_Scores_GL[fm, 2], Opt_Scores_HC[fm, 2], Opt_Scores_H[fm, 2], Opt_Scores_T[fm, 2])
start <- c(Opt_Scores_GL[fm, 1], Opt_Scores_HC[fm, 1], Opt_Scores_H[fm, 1], Opt_Scores_T[fm, 1])
# Calculate the (multi) claim scores:
GL_Freq$Score <- Score(Psi = mal[1], s = maxi[1], l_0 = start[1], Input_Score = Input_Score_GL)
HC_Freq$Score_HC <- Score(Psi = mal[2], s = maxi[2], l_0 = start[2], Input_Score = Input_Score_HC)
H_Freq$Score <- Score(Psi = mal[3], s = maxi[3], l_0 = start[3], Input_Score = Input_Score_H)
T_Freq$Score <- Score(Psi = mal[4], s = maxi[4], l_0 = start[4], Input_Score = Input_Score_T)

HC_Freq$Score_GL <- Multi_Score(Input_Score_A = Input_Score_HC, Data_A = HC_Freq, Data_B = GL_Freq, l_0_B = start[1])
HC_Freq$Score_H <- Multi_Score(Input_Score_A = Input_Score_HC, Data_A = HC_Freq, Data_B = H_Freq, l_0_B = start[3])
HC_Freq$Score_T <- Multi_Score(Input_Score_A = Input_Score_HC, Data_A = HC_Freq, Data_B = T_Freq, l_0_B = start[4])

HC_Freq$Score_GL_start <- HC_Freq$Score_GL - start[1]
HC_Freq$Score_HC_start <- HC_Freq$Score_HC - start[2]
HC_Freq$Score_H_start <- HC_Freq$Score_H - start[3]
HC_Freq$Score_T_start <- HC_Freq$Score_T - start[4]

# Split data into training and test set:
HC_Freq_Train <- HC_Freq[which((as.Date(HC_Freq$dt_End) <= as.Date(dt_Split)) & (HC_Freq$History == 0)), ]
HC_Freq_Test <- HC_Freq[which((as.Date(HC_Freq$dt_Start) >= as.Date(dt_Split)) & (HC_Freq$History == 0)), ]
# Estimate the model:
Lin_Freq_HC <- gam(list(Count ~ 1 + ProductType + Year + Age + 
                          BuildingType + RoofType + FloorSpace + HomeOwner + Residence + 
                          Urban + GlassCoverage +
                          Score_GL_start +
                          Score_HC_start +
                          Score_H_start +
                          Score_T_start +
                          offset(log(Exposure)), ~ 1 + ProductType + Year + Age + 
                          BuildingType + RoofType + FloorSpace + HomeOwner + Residence + 
                          Urban + GlassCoverage), 
                   data = HC_Freq_Train, family = ziplss())
# Summarize the model output:
summary(Lin_Freq_HC)
# Retrieve the model predictions:
Lin_ZIPG_fc <- predict(Lin_Freq_HC, HC_Freq_Test, type = 'response')
# Determine AIC and BIC:
IC_HC <- rbind(IC_HC, c(AIC(Lin_L_HC), BIC(Lin_L_HC)))
# Retrieve log-likelihood and number of parameters:
logL_Multi_HC <- rbind(logL_Multi_HC, logLik(Lin_L_HC))
K_Multi_HC <- rbind(K_Multi_HC, length(Lin_L_HC$coefficients))

### Linear - ZIPIG - HC
# Retrieve optimal claim score parameters:
fm <- 17
mal <- c(Opt_Scores_GL[fm, 3], Opt_Scores_HC[fm, 3], Opt_Scores_H[fm, 3], Opt_Scores_T[fm, 3])
maxi <- c(Opt_Scores_GL[fm, 2], Opt_Scores_HC[fm, 2], Opt_Scores_H[fm, 2], Opt_Scores_T[fm, 2])
start <- c(Opt_Scores_GL[fm, 1], Opt_Scores_HC[fm, 1], Opt_Scores_H[fm, 1], Opt_Scores_T[fm, 1])
# Calculate the (multi) claim scores:
GL_Freq$Score <- Score(Psi = mal[1], s = maxi[1], l_0 = start[1], Input_Score = Input_Score_GL)
HC_Freq$Score_HC <- Score(Psi = mal[2], s = maxi[2], l_0 = start[2], Input_Score = Input_Score_HC)
H_Freq$Score <- Score(Psi = mal[3], s = maxi[3], l_0 = start[3], Input_Score = Input_Score_H)
T_Freq$Score <- Score(Psi = mal[4], s = maxi[4], l_0 = start[4], Input_Score = Input_Score_T)

HC_Freq$Score_GL <- Multi_Score(Input_Score_A = Input_Score_HC, Data_A = HC_Freq, Data_B = GL_Freq, l_0_B = start[1])
HC_Freq$Score_H <- Multi_Score(Input_Score_A = Input_Score_HC, Data_A = HC_Freq, Data_B = H_Freq, l_0_B = start[3])
HC_Freq$Score_T <- Multi_Score(Input_Score_A = Input_Score_HC, Data_A = HC_Freq, Data_B = T_Freq, l_0_B = start[4])

HC_Freq$Score_GL_start <- HC_Freq$Score_GL - start[1]
HC_Freq$Score_HC_start <- HC_Freq$Score_HC - start[2]
HC_Freq$Score_H_start <- HC_Freq$Score_H - start[3]
HC_Freq$Score_T_start <- HC_Freq$Score_T - start[4]

# Split data into training and test set:
HC_Freq_Train <- HC_Freq[which((as.Date(HC_Freq$dt_End) <= as.Date(dt_Split)) & (HC_Freq$History == 0)), ]
HC_Freq_Test <- HC_Freq[which((as.Date(HC_Freq$dt_Start) >= as.Date(dt_Split)) & (HC_Freq$History == 0)), ]
# Estimate the model:
Lin_Freq_HC <- gam(list(Count ~ 1 + ProductType + Year + Age + 
                          BuildingType + RoofType + FloorSpace + HomeOwner + Residence + 
                          Urban + GlassCoverage +
                          Score_GL_start +
                          Score_HC_start +
                          Score_H_start +
                          Score_T_start +
                          offset(log(Exposure)), ~ 1 + ProductType + Year + Age + 
                          BuildingType + RoofType + FloorSpace + HomeOwner + Residence + 
                          Urban + GlassCoverage), 
                   data = HC_Freq_Train, family = ziplss())
# Summarize the model output:
summary(Lin_Freq_HC)
# Retrieve the model predictions:
Lin_ZIPIG_fc <- predict(Lin_Freq_HC, HC_Freq_Test, type = 'response')
# Determine AIC and BIC:
IC_HC <- rbind(IC_HC, c(AIC(Lin_L_HC), BIC(Lin_L_HC)))
# Retrieve log-likelihood and number of parameters:
logL_Multi_HC <- rbind(logL_Multi_HC, logLik(Lin_L_HC))
K_Multi_HC <- rbind(K_Multi_HC, length(Lin_L_HC$coefficients))

### Linear - ZIPP - HC
# Retrieve optimal claim score parameters:
fm <- 18
mal <- c(Opt_Scores_GL[fm, 3], Opt_Scores_HC[fm, 3], Opt_Scores_H[fm, 3], Opt_Scores_T[fm, 3])
maxi <- c(Opt_Scores_GL[fm, 2], Opt_Scores_HC[fm, 2], Opt_Scores_H[fm, 2], Opt_Scores_T[fm, 2])
start <- c(Opt_Scores_GL[fm, 1], Opt_Scores_HC[fm, 1], Opt_Scores_H[fm, 1], Opt_Scores_T[fm, 1])
# Calculate the (multi) claim scores:
GL_Freq$Score <- Score(Psi = mal[1], s = maxi[1], l_0 = start[1], Input_Score = Input_Score_GL)
HC_Freq$Score_HC <- Score(Psi = mal[2], s = maxi[2], l_0 = start[2], Input_Score = Input_Score_HC)
H_Freq$Score <- Score(Psi = mal[3], s = maxi[3], l_0 = start[3], Input_Score = Input_Score_H)
T_Freq$Score <- Score(Psi = mal[4], s = maxi[4], l_0 = start[4], Input_Score = Input_Score_T)

HC_Freq$Score_GL <- Multi_Score(Input_Score_A = Input_Score_HC, Data_A = HC_Freq, Data_B = GL_Freq, l_0_B = start[1])
HC_Freq$Score_H <- Multi_Score(Input_Score_A = Input_Score_HC, Data_A = HC_Freq, Data_B = H_Freq, l_0_B = start[3])
HC_Freq$Score_T <- Multi_Score(Input_Score_A = Input_Score_HC, Data_A = HC_Freq, Data_B = T_Freq, l_0_B = start[4])

HC_Freq$Score_GL_start <- HC_Freq$Score_GL - start[1]
HC_Freq$Score_HC_start <- HC_Freq$Score_HC - start[2]
HC_Freq$Score_H_start <- HC_Freq$Score_H - start[3]
HC_Freq$Score_T_start <- HC_Freq$Score_T - start[4]

# Split data into training and test set:
HC_Freq_Train <- HC_Freq[which((as.Date(HC_Freq$dt_End) <= as.Date(dt_Split)) & (HC_Freq$History == 0)), ]
HC_Freq_Test <- HC_Freq[which((as.Date(HC_Freq$dt_Start) >= as.Date(dt_Split)) & (HC_Freq$History == 0)), ]
# Estimate the model:
Lin_Freq_HC <- gam(list(Count ~ 1 + ProductType + Year + Age + 
                          BuildingType + RoofType + FloorSpace + HomeOwner + Residence + 
                          Urban + GlassCoverage +
                          Score_GL_start +
                          Score_HC_start +
                          Score_H_start +
                          Score_T_start +
                          offset(log(Exposure)), ~ 1 + ProductType + Year + Age + 
                          BuildingType + RoofType + FloorSpace + HomeOwner + Residence + 
                          Urban + GlassCoverage), 
                   data = HC_Freq_Train, family = ziplss())
# Summarize the model output:
summary(Lin_Freq_HC)
# Retrieve the model predictions:
Lin_ZIPP_fc <- predict(Lin_Freq_HC, HC_Freq_Test, type = 'response')
# Determine AIC and BIC:
IC_HC <- rbind(IC_HC, c(AIC(Lin_L_HC), BIC(Lin_L_HC)))
# Retrieve log-likelihood and number of parameters:
logL_Multi_HC <- rbind(logL_Multi_HC, logLik(Lin_L_HC))
K_Multi_HC <- rbind(K_Multi_HC, length(Lin_L_HC$coefficients))

##### Table with ratio Gini coefficients and SE's #####
### Setup output
# (Ratio Gini, Variance) x (Benchmark [45]) x (Alternative [45])
Dynamic_Out_samp <- array(NA, dim = c(2, 45, 45))

### Setup custom R functions
# Function for ratio Gini coefficient calculation
Gini <- dget('.../Custom_R_functions/Gini.R')

### Further construction of model forecasts
Mod_fc <- cbind(Mod_fc, 
                GAM_PG_fc * GLM_G_HC_Sev_fc, GAM_PIG_fc * GLM_IG_HC_Sev_fc, GAM_PP_fc * GLM_P_HC_Sev_fc,
                GAM_NBG_fc * GLM_G_HC_Sev_fc, GAM_NBIG_fc * GLM_IG_HC_Sev_fc, GAM_NBP_fc * GLM_P_HC_Sev_fc,
                GAM_ZIPG_fc * GLM_G_HC_Sev_fc, GAM_ZIPIG_fc * GLM_IG_HC_Sev_fc, GAM_ZIPP_fc * GLM_P_HC_Sev_fc,
                Lin_PG_fc * GLM_G_HC_Sev_fc, Lin_PIG_fc * GLM_IG_HC_Sev_fc, Lin_PP_fc * GLM_P_HC_Sev_fc,
                Lin_NBG_fc * GLM_G_HC_Sev_fc, Lin_NBIG_fc * GLM_IG_HC_Sev_fc, Lin_NBP_fc * GLM_P_HC_Sev_fc,
                Lin_ZIPG_fc * GLM_G_HC_Sev_fc, Lin_ZIPIG_fc * GLM_IG_HC_Sev_fc, Lin_ZIPP_fc * GLM_P_HC_Sev_fc)

### Parallel setup
Cores <- detectCores() - 1
Clusters <- makeCluster(Cores) 
registerDoParallel(Clusters)

### Calculation of ratio Gini coefficients and variances
# Loop over all benchmark models:
for (b in 1:ncol(Mod_fc)) {
  # Keep track of the progress:
  print(paste('Benchmark: ', b, '/', ncol(Mod_fc), sep = ''))
  # Loop over all alternative models and excluding the benchmark model - Parallel:
  Temp_Gini <- foreach(a = c(1:ncol(Mod_fc))[-b]) %dopar% {
    # Determine the ratio Gini coefficient and its variance:
    c(100, 1) * Gini(Candidate = Mod_fc[, a], Reference = Mod_fc[, b], Actual = HC_Freq_Test$Size)[[1]]
  }
  # Store output:
  Dynamic_Out_samp[, b, c(1:ncol(Mod_fc))[-b]] <- sapply(Temp_Gini, rbind)
}

### Stop clusters
stopCluster(Clusters)

### Transform and scale variances
Dynamic_Out_samp[2, , ] <- 100 * sqrt(Dynamic_Out_samp[2, , ])

### Print to LaTeX table
library('xtable')
xtable(Dynamic_Out_samp[1, , ], type = 'latex')
xtable(Dynamic_Out_samp[2, , ], type = 'latex')

### Determine maximal ratio Gini coefficients
Max_rGini <- which(Dynamic_Out_samp[1, , ] == apply(Dynamic_Out_samp[1, , ], 1, max, na.rm = TRUE), arr.ind = TRUE)
Max_rGini <- Max_rGini[order(Max_rGini[, 1]), ]
cbind(Max_rGini, Dynamic_Out_samp[2, , ][Max_rGini])
apply(Dynamic_Out_samp[1, , ], 1, max, na.rm = TRUE)

##### LR - Chi-squared test #####
### Test setup
# (Test statistic) x (p-value)
LR_Test_Multi_HC <- matrix(cbind(rep(NA, 9 * 2), rep(NA, 9 * 2)), 9 * 2, 2)
logL_null_Multi_HC <- c(logL_One_HC)
logL_alt_Multi_HC <- c(logL_Multi_HC)
K_null_Multi_HC <- c(K_One_HC)
K_alt_Multi_HC <- c(K_Multi_HC)

### Calculation of test statistics and p-values
for (assump in 1:2) {
  for (mod in 1:9) {
    ind <- (assump - 1) * 9 + mod
    LR_Test_Multi_HC[ind, 1] <- (-2) * (logL_null_Multi_HC[ind] - logL_alt_Multi_HC[ind])
    LR_Test_Multi_HC[ind, 2] <- 1 - pchisq(LR_Test_Multi_HC[ind, 1], df = (K_alt_Multi_HC[ind] - K_null_Multi_HC[ind]))
  }
}

### Store output
