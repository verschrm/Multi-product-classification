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
# Load results from Output_univariate_HC.R
# Load results from Output_univariate_H.R
# Load results from Output_univariate_T.R

### Main product - GL
# Load results from Output_univariate_GL.R

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

##### Piecewise linear models #####
### PL - PG - GL
# Retrieve optimal claim score parameters:
fm <- 1
mal <- c(Opt_Scores_GL[fm, 3], Opt_Scores_HC[fm, 3], Opt_Scores_H[fm, 3], Opt_Scores_T[fm, 3])
maxi <- c(Opt_Scores_GL[fm, 2], Opt_Scores_HC[fm, 2], Opt_Scores_H[fm, 2], Opt_Scores_T[fm, 2])
start <- c(Opt_Scores_GL[fm, 1], Opt_Scores_HC[fm, 1], Opt_Scores_H[fm, 1], Opt_Scores_T[fm, 1])
# Calculate the (multi) claim scores:
GL_Freq$Score_GL <- Score(Psi = mal[1], s = maxi[1], l_0 = start[1], Input_Score = Input_Score_GL)
HC_Freq$Score <- Score(Psi = mal[2], s = maxi[2], l_0 = start[2], Input_Score = Input_Score_HC)
H_Freq$Score <- Score(Psi = mal[3], s = maxi[3], l_0 = start[3], Input_Score = Input_Score_H)
T_Freq$Score <- Score(Psi = mal[4], s = maxi[4], l_0 = start[4], Input_Score = Input_Score_T)

GL_Freq$Score_HC <- Multi_Score(Input_Score_A = Input_Score_GL, Data_A = GL_Freq, Data_B = HC_Freq, l_0_B = start[2])
GL_Freq$Score_H <- Multi_Score(Input_Score_A = Input_Score_GL, Data_A = GL_Freq, Data_B = H_Freq, l_0_B = start[3])
GL_Freq$Score_T <- Multi_Score(Input_Score_A = Input_Score_GL, Data_A = GL_Freq, Data_B = T_Freq, l_0_B = start[4])

# Split data into training and test set:
GL_Freq_Train <- GL_Freq[which((as.Date(GL_Freq$dt_End) <= as.Date(dt_Split)) & (GL_Freq$History == 0)), ]
GL_Freq_Test <- GL_Freq[which((as.Date(GL_Freq$dt_Start) >= as.Date(dt_Split)) & (GL_Freq$History == 0)), ]
# Estimate the model:
PL_Freq_GL <- gam(Count ~ 1 + FamilySituation + 
                  s(Score_GL, bs = 'ps', k = k_cr, m = 0, fx = TRUE, pc = start[1]) +
                  s(Score_HC, bs = 'ps', k = k_cr, m = 0, fx = TRUE, pc = start[2]) +
                  s(Score_H, bs = 'ps', k = k_cr, m = 0, fx = TRUE, pc = start[3]) +
                  s(Score_T, bs = 'ps', k = k_cr, m = 0, fx = TRUE, pc = start[4]), 
                offset = log(Exposure), data = GL_Freq_Train, family = poisson(link = 'log'))
# Plot the splines:
plot(PL_Freq_GL, col = 'blue', xlab = 'Claim score level', ylab = 'Estimated effect', pages = 1)
# Summarize the model output:
summary(PL_Freq_GL)
# Retrieve the model predictions:
PL_PG_fc <- predict(PL_Freq_GL, GL_Freq_Test, type = 'response')
# Determine AIC and BIC:
IC_GL <- rbind(IC_GL, c(AIC(PL_Freq_GL), BIC(PL_Freq_GL)))

### PL - PIG - GL
# Retrieve optimal claim score parameters:
fm <- 2
mal <- c(Opt_Scores_GL[fm, 3], Opt_Scores_HC[fm, 3], Opt_Scores_H[fm, 3], Opt_Scores_T[fm, 3])
maxi <- c(Opt_Scores_GL[fm, 2], Opt_Scores_HC[fm, 2], Opt_Scores_H[fm, 2], Opt_Scores_T[fm, 2])
start <- c(Opt_Scores_GL[fm, 1], Opt_Scores_HC[fm, 1], Opt_Scores_H[fm, 1], Opt_Scores_T[fm, 1])
# Calculate the (multi) claim scores:
GL_Freq$Score_GL <- Score(Psi = mal[1], s = maxi[1], l_0 = start[1], Input_Score = Input_Score_GL)
HC_Freq$Score <- Score(Psi = mal[2], s = maxi[2], l_0 = start[2], Input_Score = Input_Score_HC)
H_Freq$Score <- Score(Psi = mal[3], s = maxi[3], l_0 = start[3], Input_Score = Input_Score_H)
T_Freq$Score <- Score(Psi = mal[4], s = maxi[4], l_0 = start[4], Input_Score = Input_Score_T)

GL_Freq$Score_HC <- Multi_Score(Input_Score_A = Input_Score_GL, Data_A = GL_Freq, Data_B = HC_Freq, l_0_B = start[2])
GL_Freq$Score_H <- Multi_Score(Input_Score_A = Input_Score_GL, Data_A = GL_Freq, Data_B = H_Freq, l_0_B = start[3])
GL_Freq$Score_T <- Multi_Score(Input_Score_A = Input_Score_GL, Data_A = GL_Freq, Data_B = T_Freq, l_0_B = start[4])

# Split data into training and test set:
GL_Freq_Train <- GL_Freq[which((as.Date(GL_Freq$dt_End) <= as.Date(dt_Split)) & (GL_Freq$History == 0)), ]
GL_Freq_Test <- GL_Freq[which((as.Date(GL_Freq$dt_Start) >= as.Date(dt_Split)) & (GL_Freq$History == 0)), ]
# Estimate the model:
PL_Freq_GL <- gam(Count ~ 1 + FamilySituation + 
                  s(Score_GL, bs = 'ps', k = k_cr, m = 0, fx = TRUE, pc = start[1]) +
                  s(Score_HC, bs = 'ps', k = k_cr, m = 0, fx = TRUE, pc = start[2]) +
                  s(Score_H, bs = 'ps', k = k_cr, m = 0, fx = TRUE, pc = start[3]) +
                  s(Score_T, bs = 'ps', k = k_cr, m = 0, fx = TRUE, pc = start[4]), 
                offset = log(Exposure), data = GL_Freq_Train, family = poisson(link = 'log'))
# Plot the splines:
plot(PL_Freq_GL, col = 'blue', xlab = 'Claim score level', ylab = 'Estimated effect', pages = 1)
# Summarize the model output:
summary(PL_Freq_GL)
# Retrieve the model predictions:
PL_PIG_fc <- predict(PL_Freq_GL, GL_Freq_Test, type = 'response')
# Determine AIC and BIC:
IC_GL <- rbind(IC_GL, c(AIC(PL_Freq_GL), BIC(PL_Freq_GL)))

### PL - PP - GL
# Retrieve optimal claim score parameters:
fm <- 3
mal <- c(Opt_Scores_GL[fm, 3], Opt_Scores_HC[fm, 3], Opt_Scores_H[fm, 3], Opt_Scores_T[fm, 3])
maxi <- c(Opt_Scores_GL[fm, 2], Opt_Scores_HC[fm, 2], Opt_Scores_H[fm, 2], Opt_Scores_T[fm, 2])
start <- c(Opt_Scores_GL[fm, 1], Opt_Scores_HC[fm, 1], Opt_Scores_H[fm, 1], Opt_Scores_T[fm, 1])
# Calculate the (multi) claim scores:
GL_Freq$Score_GL <- Score(Psi = mal[1], s = maxi[1], l_0 = start[1], Input_Score = Input_Score_GL)
HC_Freq$Score <- Score(Psi = mal[2], s = maxi[2], l_0 = start[2], Input_Score = Input_Score_HC)
H_Freq$Score <- Score(Psi = mal[3], s = maxi[3], l_0 = start[3], Input_Score = Input_Score_H)
T_Freq$Score <- Score(Psi = mal[4], s = maxi[4], l_0 = start[4], Input_Score = Input_Score_T)

GL_Freq$Score_HC <- Multi_Score(Input_Score_A = Input_Score_GL, Data_A = GL_Freq, Data_B = HC_Freq, l_0_B = start[2])
GL_Freq$Score_H <- Multi_Score(Input_Score_A = Input_Score_GL, Data_A = GL_Freq, Data_B = H_Freq, l_0_B = start[3])
GL_Freq$Score_T <- Multi_Score(Input_Score_A = Input_Score_GL, Data_A = GL_Freq, Data_B = T_Freq, l_0_B = start[4])

# Split data into training and test set:
GL_Freq_Train <- GL_Freq[which((as.Date(GL_Freq$dt_End) <= as.Date(dt_Split)) & (GL_Freq$History == 0)), ]
GL_Freq_Test <- GL_Freq[which((as.Date(GL_Freq$dt_Start) >= as.Date(dt_Split)) & (GL_Freq$History == 0)), ]
# Estimate the model:
PL_Freq_GL <- gam(Count ~ 1 + FamilySituation + 
                    s(Score_GL, bs = 'ps', k = k_cr, m = 0, fx = TRUE, pc = start[1]) +
                    s(Score_HC, bs = 'ps', k = k_cr, m = 0, fx = TRUE, pc = start[2]) +
                    s(Score_H, bs = 'ps', k = k_cr, m = 0, fx = TRUE, pc = start[3]) +
                    s(Score_T, bs = 'ps', k = k_cr, m = 0, fx = TRUE, pc = start[4]), 
                  offset = log(Exposure), data = GL_Freq_Train, family = poisson(link = 'log'))
# Plot the splines:
plot(PL_Freq_GL, col = 'blue', xlab = 'Claim score level', ylab = 'Estimated effect', pages = 1)
# Summarize the model output:
summary(PL_Freq_GL)
# Retrieve the model predictions:
PL_PP_fc <- predict(PL_Freq_GL, GL_Freq_Test, type = 'response')
# Determine AIC and BIC:
IC_GL <- rbind(IC_GL, c(AIC(PL_Freq_GL), BIC(PL_Freq_GL)))

### PL - NBG - GL
# Retrieve optimal claim score parameters:
fm <- 4
mal <- c(Opt_Scores_GL[fm, 3], Opt_Scores_HC[fm, 3], Opt_Scores_H[fm, 3], Opt_Scores_T[fm, 3])
maxi <- c(Opt_Scores_GL[fm, 2], Opt_Scores_HC[fm, 2], Opt_Scores_H[fm, 2], Opt_Scores_T[fm, 2])
start <- c(Opt_Scores_GL[fm, 1], Opt_Scores_HC[fm, 1], Opt_Scores_H[fm, 1], Opt_Scores_T[fm, 1])
# Calculate the (multi) claim scores:
GL_Freq$Score_GL <- Score(Psi = mal[1], s = maxi[1], l_0 = start[1], Input_Score = Input_Score_GL)
HC_Freq$Score <- Score(Psi = mal[2], s = maxi[2], l_0 = start[2], Input_Score = Input_Score_HC)
H_Freq$Score <- Score(Psi = mal[3], s = maxi[3], l_0 = start[3], Input_Score = Input_Score_H)
T_Freq$Score <- Score(Psi = mal[4], s = maxi[4], l_0 = start[4], Input_Score = Input_Score_T)

GL_Freq$Score_HC <- Multi_Score(Input_Score_A = Input_Score_GL, Data_A = GL_Freq, Data_B = HC_Freq, l_0_B = start[2])
GL_Freq$Score_H <- Multi_Score(Input_Score_A = Input_Score_GL, Data_A = GL_Freq, Data_B = H_Freq, l_0_B = start[3])
GL_Freq$Score_T <- Multi_Score(Input_Score_A = Input_Score_GL, Data_A = GL_Freq, Data_B = T_Freq, l_0_B = start[4])

# Split data into training and test set:
GL_Freq_Train <- GL_Freq[which((as.Date(GL_Freq$dt_End) <= as.Date(dt_Split)) & (GL_Freq$History == 0)), ]
GL_Freq_Test <- GL_Freq[which((as.Date(GL_Freq$dt_Start) >= as.Date(dt_Split)) & (GL_Freq$History == 0)), ]
# Estimate the model:
PL_Freq_GL <- gam(Count ~ 1 + FamilySituation +
                  s(Score_GL, bs = 'ps', k = k_cr, m = 0, fx = TRUE, pc = start[1]) +
                  s(Score_HC, bs = 'ps', k = k_cr, m = 0, fx = TRUE, pc = start[2]) +
                  s(Score_H, bs = 'ps', k = k_cr, m = 0, fx = TRUE, pc = start[3]) +
                  s(Score_T, bs = 'ps', k = k_cr, m = 0, fx = TRUE, pc = start[4]),
                offset = log(Exposure), data = GL_Freq_Train, family = nb(link = 'log'))
# Plot the splines:
plot(PL_Freq_GL, col = 'blue', xlab = 'Claim score level', ylab = 'Estimated effect', pages = 1)
# Summarize the model output:
summary(PL_Freq_GL)
# Retrieve the model predictions:
PL_NBG_fc <- predict(PL_Freq_GL, GL_Freq_Test, type = 'response')
# Determine AIC and BIC:
IC_GL <- rbind(IC_GL, c(AIC(PL_Freq_GL), BIC(PL_Freq_GL)))

### PL - NBIG - GL
# Retrieve optimal claim score parameters:
fm <- 5
mal <- c(Opt_Scores_GL[fm, 3], Opt_Scores_HC[fm, 3], Opt_Scores_H[fm, 3], Opt_Scores_T[fm, 3])
maxi <- c(Opt_Scores_GL[fm, 2], Opt_Scores_HC[fm, 2], Opt_Scores_H[fm, 2], Opt_Scores_T[fm, 2])
start <- c(Opt_Scores_GL[fm, 1], Opt_Scores_HC[fm, 1], Opt_Scores_H[fm, 1], Opt_Scores_T[fm, 1])
# Calculate the (multi) claim scores:
GL_Freq$Score_GL <- Score(Psi = mal[1], s = maxi[1], l_0 = start[1], Input_Score = Input_Score_GL)
HC_Freq$Score <- Score(Psi = mal[2], s = maxi[2], l_0 = start[2], Input_Score = Input_Score_HC)
H_Freq$Score <- Score(Psi = mal[3], s = maxi[3], l_0 = start[3], Input_Score = Input_Score_H)
T_Freq$Score <- Score(Psi = mal[4], s = maxi[4], l_0 = start[4], Input_Score = Input_Score_T)

GL_Freq$Score_HC <- Multi_Score(Input_Score_A = Input_Score_GL, Data_A = GL_Freq, Data_B = HC_Freq, l_0_B = start[2])
GL_Freq$Score_H <- Multi_Score(Input_Score_A = Input_Score_GL, Data_A = GL_Freq, Data_B = H_Freq, l_0_B = start[3])
GL_Freq$Score_T <- Multi_Score(Input_Score_A = Input_Score_GL, Data_A = GL_Freq, Data_B = T_Freq, l_0_B = start[4])

# Split data into training and test set:
GL_Freq_Train <- GL_Freq[which((as.Date(GL_Freq$dt_End) <= as.Date(dt_Split)) & (GL_Freq$History == 0)), ]
GL_Freq_Test <- GL_Freq[which((as.Date(GL_Freq$dt_Start) >= as.Date(dt_Split)) & (GL_Freq$History == 0)), ]
# Estimate the model:
PL_Freq_GL <- gam(Count ~ 1 + FamilySituation +
                  s(Score_GL, bs = 'ps', k = k_cr, m = 0, fx = TRUE, pc = start[1]) +
                  s(Score_HC, bs = 'ps', k = k_cr, m = 0, fx = TRUE, pc = start[2]) +
                  s(Score_H, bs = 'ps', k = k_cr, m = 0, fx = TRUE, pc = start[3]) +
                  s(Score_T, bs = 'ps', k = k_cr, m = 0, fx = TRUE, pc = start[4]),
                offset = log(Exposure), data = GL_Freq_Train, family = nb(link = 'log'))
# Plot the splines:
plot(PL_Freq_GL, col = 'blue', xlab = 'Claim score level', ylab = 'Estimated effect', pages = 1)
# Summarize the model output:
summary(PL_Freq_GL)
# Retrieve the model predictions:
PL_NBIG_fc <- predict(PL_Freq_GL, GL_Freq_Test, type = 'response')
# Determine AIC and BIC:
IC_GL <- rbind(IC_GL, c(AIC(PL_Freq_GL), BIC(PL_Freq_GL)))

### PL - NBP - GL
# Retrieve optimal claim score parameters:
fm <- 6
mal <- c(Opt_Scores_GL[fm, 3], Opt_Scores_HC[fm, 3], Opt_Scores_H[fm, 3], Opt_Scores_T[fm, 3])
maxi <- c(Opt_Scores_GL[fm, 2], Opt_Scores_HC[fm, 2], Opt_Scores_H[fm, 2], Opt_Scores_T[fm, 2])
start <- c(Opt_Scores_GL[fm, 1], Opt_Scores_HC[fm, 1], Opt_Scores_H[fm, 1], Opt_Scores_T[fm, 1])
# Calculate the (multi) claim scores:
GL_Freq$Score_GL <- Score(Psi = mal[1], s = maxi[1], l_0 = start[1], Input_Score = Input_Score_GL)
HC_Freq$Score <- Score(Psi = mal[2], s = maxi[2], l_0 = start[2], Input_Score = Input_Score_HC)
H_Freq$Score <- Score(Psi = mal[3], s = maxi[3], l_0 = start[3], Input_Score = Input_Score_H)
T_Freq$Score <- Score(Psi = mal[4], s = maxi[4], l_0 = start[4], Input_Score = Input_Score_T)

GL_Freq$Score_HC <- Multi_Score(Input_Score_A = Input_Score_GL, Data_A = GL_Freq, Data_B = HC_Freq, l_0_B = start[2])
GL_Freq$Score_H <- Multi_Score(Input_Score_A = Input_Score_GL, Data_A = GL_Freq, Data_B = H_Freq, l_0_B = start[3])
GL_Freq$Score_T <- Multi_Score(Input_Score_A = Input_Score_GL, Data_A = GL_Freq, Data_B = T_Freq, l_0_B = start[4])

# Split data into training and test set:
GL_Freq_Train <- GL_Freq[which((as.Date(GL_Freq$dt_End) <= as.Date(dt_Split)) & (GL_Freq$History == 0)), ]
GL_Freq_Test <- GL_Freq[which((as.Date(GL_Freq$dt_Start) >= as.Date(dt_Split)) & (GL_Freq$History == 0)), ]
# Estimate the model:
PL_Freq_GL <- gam(Count ~ 1 + FamilySituation +
                    s(Score_GL, bs = 'ps', k = k_cr, m = 0, fx = TRUE, pc = start[1]) +
                    s(Score_HC, bs = 'ps', k = k_cr, m = 0, fx = TRUE, pc = start[2]) +
                    s(Score_H, bs = 'ps', k = k_cr, m = 0, fx = TRUE, pc = start[3]) +
                    s(Score_T, bs = 'ps', k = k_cr, m = 0, fx = TRUE, pc = start[4]),
                  offset = log(Exposure), data = GL_Freq_Train, family = nb(link = 'log'))
# Plot the splines:
plot(PL_Freq_GL, col = 'blue', xlab = 'Claim score level', ylab = 'Estimated effect', pages = 1)
# Summarize the model output:
summary(PL_Freq_GL)
# Retrieve the model predictions:
PL_NBP_fc <- predict(PL_Freq_GL, GL_Freq_Test, type = 'response')
# Determine AIC and BIC:
IC_GL <- rbind(IC_GL, c(AIC(PL_Freq_GL), BIC(PL_Freq_GL)))

### PL - ZIPG - GL
# Retrieve optimal claim score parameters:
fm <- 7
mal <- c(Opt_Scores_GL[fm, 3], Opt_Scores_HC[fm, 3], Opt_Scores_H[fm, 3], Opt_Scores_T[fm, 3])
maxi <- c(Opt_Scores_GL[fm, 2], Opt_Scores_HC[fm, 2], Opt_Scores_H[fm, 2], Opt_Scores_T[fm, 2])
start <- c(Opt_Scores_GL[fm, 1], Opt_Scores_HC[fm, 1], Opt_Scores_H[fm, 1], Opt_Scores_T[fm, 1])
# Calculate the (multi) claim scores:
GL_Freq$Score_GL <- Score(Psi = mal[1], s = maxi[1], l_0 = start[1], Input_Score = Input_Score_GL)
HC_Freq$Score <- Score(Psi = mal[2], s = maxi[2], l_0 = start[2], Input_Score = Input_Score_HC)
H_Freq$Score <- Score(Psi = mal[3], s = maxi[3], l_0 = start[3], Input_Score = Input_Score_H)
T_Freq$Score <- Score(Psi = mal[4], s = maxi[4], l_0 = start[4], Input_Score = Input_Score_T)

GL_Freq$Score_HC <- Multi_Score(Input_Score_A = Input_Score_GL, Data_A = GL_Freq, Data_B = HC_Freq, l_0_B = start[2])
GL_Freq$Score_H <- Multi_Score(Input_Score_A = Input_Score_GL, Data_A = GL_Freq, Data_B = H_Freq, l_0_B = start[3])
GL_Freq$Score_T <- Multi_Score(Input_Score_A = Input_Score_GL, Data_A = GL_Freq, Data_B = T_Freq, l_0_B = start[4])

# Split data into training and test set:
GL_Freq_Train <- GL_Freq[which((as.Date(GL_Freq$dt_End) <= as.Date(dt_Split)) & (GL_Freq$History == 0)), ]
GL_Freq_Test <- GL_Freq[which((as.Date(GL_Freq$dt_Start) >= as.Date(dt_Split)) & (GL_Freq$History == 0)), ]
# Estimate the model:
PL_Freq_GL <- gam(list(Count ~ 1 + FamilySituation +
                         s(Score_GL, bs = 'ps', k = k_cr, m = 0, fx = TRUE, pc = start[1]) +
                         s(Score_HC, bs = 'ps', k = k_cr, m = 0, fx = TRUE, pc = start[2]) +
                         s(Score_H, bs = 'ps', k = k_cr, m = 0, fx = TRUE, pc = start[3]) +
                         s(Score_T, bs = 'ps', k = k_cr, m = 0, fx = TRUE, pc = start[4]) +
                         offset(log(Exposure)), ~ 1 + FamilySituation), 
                  data = GL_Freq_Train, family = ziplss())
# Plot the splines:
plot(PL_Freq_GL, col = 'blue', xlab = 'Claim score level', ylab = 'Estimated effect', pages = 1)
# Summarize the model output:
summary(PL_Freq_GL)
# Retrieve the model predictions:
PL_ZIPG_fc <- predict(PL_Freq_GL, GL_Freq_Test, type = 'response')
# Determine AIC and BIC:
IC_GL <- rbind(IC_GL, c(AIC(PL_Freq_GL), BIC(PL_Freq_GL)))

### PL - ZIPIG - GL
# Retrieve optimal claim score parameters:
fm <- 8
mal <- c(Opt_Scores_GL[fm, 3], Opt_Scores_HC[fm, 3], Opt_Scores_H[fm, 3], Opt_Scores_T[fm, 3])
maxi <- c(Opt_Scores_GL[fm, 2], Opt_Scores_HC[fm, 2], Opt_Scores_H[fm, 2], Opt_Scores_T[fm, 2])
start <- c(Opt_Scores_GL[fm, 1], Opt_Scores_HC[fm, 1], Opt_Scores_H[fm, 1], Opt_Scores_T[fm, 1])
# Calculate the (multi) claim scores:
GL_Freq$Score_GL <- Score(Psi = mal[1], s = maxi[1], l_0 = start[1], Input_Score = Input_Score_GL)
HC_Freq$Score <- Score(Psi = mal[2], s = maxi[2], l_0 = start[2], Input_Score = Input_Score_HC)
H_Freq$Score <- Score(Psi = mal[3], s = maxi[3], l_0 = start[3], Input_Score = Input_Score_H)
T_Freq$Score <- Score(Psi = mal[4], s = maxi[4], l_0 = start[4], Input_Score = Input_Score_T)

GL_Freq$Score_HC <- Multi_Score(Input_Score_A = Input_Score_GL, Data_A = GL_Freq, Data_B = HC_Freq, l_0_B = start[2])
GL_Freq$Score_H <- Multi_Score(Input_Score_A = Input_Score_GL, Data_A = GL_Freq, Data_B = H_Freq, l_0_B = start[3])
GL_Freq$Score_T <- Multi_Score(Input_Score_A = Input_Score_GL, Data_A = GL_Freq, Data_B = T_Freq, l_0_B = start[4])

# Split data into training and test set:
GL_Freq_Train <- GL_Freq[which((as.Date(GL_Freq$dt_End) <= as.Date(dt_Split)) & (GL_Freq$History == 0)), ]
GL_Freq_Test <- GL_Freq[which((as.Date(GL_Freq$dt_Start) >= as.Date(dt_Split)) & (GL_Freq$History == 0)), ]
# Estimate the model:
PL_Freq_GL <- gam(list(Count ~ 1 + FamilySituation +
                         s(Score_GL, bs = 'ps', k = k_cr, m = 0, fx = TRUE, pc = start[1]) +
                         s(Score_HC, bs = 'ps', k = k_cr, m = 0, fx = TRUE, pc = start[2]) +
                         s(Score_H, bs = 'ps', k = k_cr, m = 0, fx = TRUE, pc = start[3]) +
                         s(Score_T, bs = 'ps', k = k_cr, m = 0, fx = TRUE, pc = start[4]) +
                         offset(log(Exposure)), ~ 1 + FamilySituation), 
                  data = GL_Freq_Train, family = ziplss())
# Plot the splines:
plot(PL_Freq_GL, col = 'blue', xlab = 'Claim score level', ylab = 'Estimated effect', pages = 1)
# Summarize the model output:
summary(PL_Freq_GL)
# Retrieve the model predictions:
PL_ZIPIG_fc <- predict(PL_Freq_GL, GL_Freq_Test, type = 'response')
# Determine AIC and BIC:
IC_GL <- rbind(IC_GL, c(AIC(PL_Freq_GL), BIC(PL_Freq_GL)))

### PL - ZIPP - GL
# Retrieve optimal claim score parameters:
fm <- 9
mal <- c(Opt_Scores_GL[fm, 3], Opt_Scores_HC[fm, 3], Opt_Scores_H[fm, 3], Opt_Scores_T[fm, 3])
maxi <- c(Opt_Scores_GL[fm, 2], Opt_Scores_HC[fm, 2], Opt_Scores_H[fm, 2], Opt_Scores_T[fm, 2])
start <- c(Opt_Scores_GL[fm, 1], Opt_Scores_HC[fm, 1], Opt_Scores_H[fm, 1], Opt_Scores_T[fm, 1])
# Calculate the (multi) claim scores:
GL_Freq$Score_GL <- Score(Psi = mal[1], s = maxi[1], l_0 = start[1], Input_Score = Input_Score_GL)
HC_Freq$Score <- Score(Psi = mal[2], s = maxi[2], l_0 = start[2], Input_Score = Input_Score_HC)
H_Freq$Score <- Score(Psi = mal[3], s = maxi[3], l_0 = start[3], Input_Score = Input_Score_H)
T_Freq$Score <- Score(Psi = mal[4], s = maxi[4], l_0 = start[4], Input_Score = Input_Score_T)

GL_Freq$Score_HC <- Multi_Score(Input_Score_A = Input_Score_GL, Data_A = GL_Freq, Data_B = HC_Freq, l_0_B = start[2])
GL_Freq$Score_H <- Multi_Score(Input_Score_A = Input_Score_GL, Data_A = GL_Freq, Data_B = H_Freq, l_0_B = start[3])
GL_Freq$Score_T <- Multi_Score(Input_Score_A = Input_Score_GL, Data_A = GL_Freq, Data_B = T_Freq, l_0_B = start[4])

# Split data into training and test set:
GL_Freq_Train <- GL_Freq[which((as.Date(GL_Freq$dt_End) <= as.Date(dt_Split)) & (GL_Freq$History == 0)), ]
GL_Freq_Test <- GL_Freq[which((as.Date(GL_Freq$dt_Start) >= as.Date(dt_Split)) & (GL_Freq$History == 0)), ]
# Estimate the model:
PL_Freq_GL <- gam(list(Count ~ 1 + FamilySituation +
                         s(Score_GL, bs = 'ps', k = k_cr, m = 0, fx = TRUE, pc = start[1]) +
                         s(Score_HC, bs = 'ps', k = k_cr, m = 0, fx = TRUE, pc = start[2]) +
                         s(Score_H, bs = 'ps', k = k_cr, m = 0, fx = TRUE, pc = start[3]) +
                         s(Score_T, bs = 'ps', k = k_cr, m = 0, fx = TRUE, pc = start[4]) +
                         offset(log(Exposure)), ~ 1 + FamilySituation), 
                  data = GL_Freq_Train, family = ziplss())
# Plot the splines:
plot(PL_Freq_GL, col = 'blue', xlab = 'Claim score level', ylab = 'Estimated effect', pages = 1)
# Summarize the model output:
summary(PL_Freq_GL)
# Retrieve the model predictions:
PL_ZIPP_fc <- predict(PL_Freq_GL, GL_Freq_Test, type = 'response')
# Determine AIC and BIC:
IC_GL <- rbind(IC_GL, c(AIC(PL_Freq_GL), BIC(PL_Freq_GL)))

##### Table with ratio Gini coefficients and SE's #####
### Setup output
# (Ratio Gini, Variance) x (Benchmark [54]) x (Alternative [54])
Dynamic_Out_samp <- array(NA, dim = c(2, 54, 54))

### Setup Gini function
# Function for ratio Gini coefficient calculation
Gini <- dget('.../Custom_R_functions/Gini.R')

### Further construction of model forecasts
Mod_fc <- cbind(Mod_fc, 
                PL_PG_fc * GLM_G_GL_Sev_fc, PL_PIG_fc * GLM_IG_GL_Sev_fc, PL_PP_fc * GLM_P_GL_Sev_fc,
                PL_NBG_fc * GLM_G_GL_Sev_fc, PL_NBIG_fc * GLM_IG_GL_Sev_fc, PL_NBP_fc * GLM_P_GL_Sev_fc,
                PL_ZIPG_fc * GLM_G_GL_Sev_fc, PL_ZIPIG_fc * GLM_IG_GL_Sev_fc, PL_ZIPP_fc * GLM_P_GL_Sev_fc)

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
    c(100, 1) * Gini(Candidate = Mod_fc[, a], Reference = Mod_fc[, b], Actual = GL_Freq_Test$Size)[[1]]
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

### Print information criteria of the three severity models and all frequency models to LaTeX table
xtable(IC_GL, type = 'latex')

### Store output
