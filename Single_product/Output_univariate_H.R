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

##### Score iterations data #####
### Load results from Optimal_claim_scores.R

### Prepare input claim score
# Preparatory function for (faster) claim score optimization
Input_Score <- dget('.../Custom_R_functions/Input_Score.R')
# Function for claim score calculation
Score <- dget('.../Custom_R_functions/Score.R')
dt_Split <- '2018-01-01'
# Split date:
dt_Split <- '2018-01-01'
# Number of spline parameters:
k_cr <- 4

##### Standard GLMs #####
### P
# Estimate the model:
GLM_Freq <- glm(Count ~ 1 + ProductType + Year + Age + FamilySituation +
                  BuildingType + RoofType + Capacity + ConstructionYear + Residence + Urban + GlassCoverage, 
                offset = log(Exposure), data = H_Freq_Train, family = poisson(link = 'log'))
# Determine AIC and BIC:
IC_H <- rbind(IC_H, c(AIC(GLM_Freq), BIC(GLM_Freq)))
# Retrieve log-likelihood and number of parameters:
logL_GLM_H <- logLik(GLM_Freq)
K_GLM_H <- length(GLM_Freq$coefficients)

### NB
# Estimate the model:
GLM_Freq <- glm.nb(Count ~ 1 + ProductType + Year + Age + FamilySituation +
                     BuildingType + RoofType + Capacity + ConstructionYear + Residence + Urban + GlassCoverage + 
                     offset(log(Exposure)), data = H_Freq_Train, link = log)
# Determine AIC and BIC:
IC_H <- rbind(IC_H, c(AIC(GLM_Freq), BIC(GLM_Freq)))
# Retrieve log-likelihood and number of parameters:
logL_GLM_H <- rbind(logL_GLM_H, logLik(GLM_Freq))
K_GLM_H <- rbind(K_GLM_H, length(GLM_Freq$coefficients))

### ZIP
# Estimate the model:
GLM_Freq <- gam(list(Count ~ 1 + ProductType + Year + Age + FamilySituation +
                       BuildingType + RoofType + Capacity + ConstructionYear + Residence + Urban + GlassCoverage + 
                       offset(log(Exposure)), ~ 1 + ProductType + Year + Age + FamilySituation +
                       BuildingType + RoofType + Capacity + ConstructionYear + Residence + Urban + GlassCoverage), 
                data = H_Freq_Train, family = ziplss())
# Determine AIC and BIC:
IC_H <- rbind(IC_H, c(AIC(GLM_Freq), BIC(GLM_Freq)))
# Retrieve log-likelihood and number of parameters:
logL_GLM_H <- rbind(logL_GLM_H, logLik(GLM_Freq))
K_GLM_H <- rbind(K_GLM_H, length(GLM_Freq$coefficients))

##### Single-product models #####
### GAM - PG - H
# Retrieve optimal claim score parameters:
fm <- 1
mal <- Opt_Scores_H[fm, 3]
maxi <- Opt_Scores_H[fm, 2]
start <- Opt_Scores_H[fm, 1]
# Calculate the claim scores:
H_Freq$Score <- Score(Psi = mal, s = maxi, l_0 = start, Input_Score = Input_Score_H)
# Split data into training and test set:
H_Freq_Train <- H_Freq[which((as.Date(H_Freq$dt_End) <= as.Date(dt_Split)) & (H_Freq$History == 0)), ]
H_Freq_Test <- H_Freq[which((as.Date(H_Freq$dt_Start) >= as.Date(dt_Split)) & (H_Freq$History == 0)), ]
# Estimate the model:
GAM_Freq_H <- gam(Count ~ 1 + ProductType + Year + Age +
                 FamilySituation + BuildingType + RoofType + Capacity + ConstructionYear + Residence +
                 Urban + GlassCoverage + 
                 s(Score, bs = 'cr', k = k_cr, fx = TRUE, pc = start), 
                 offset = log(Exposure), data = H_Freq_Train, family = poisson(link = 'log'))
# Plot the spline:
plot(GAM_Freq_H, col = 'blue', xlab = 'Claim score level', 
     ylab = 'Estimated effect')[[1]][c('x', 'se', 'fit')]
# Summarize the model output:
summary(GAM_Freq_H)
# Retrieve the model predictions:
GAM_PG_H_fc <- predict(GAM_Freq_H, H_Freq_Test, type = 'response')
# Determine AIC and BIC:
IC_H <- rbind(IC_H, c(AIC(GAM_Freq_H), BIC(GAM_Freq_H)))
# Retrieve log-likelihood and number of parameters:
logL_One_H <- logLik(GAM_Freq_H)
K_One_H <- length(GAM_Freq_H$coefficients)

### GAM - PIG - H
# Retrieve optimal claim score parameters:
fm <- 2
mal <- Opt_Scores_H[fm, 3]
maxi <- Opt_Scores_H[fm, 2]
start <- Opt_Scores_H[fm, 1]
# Calculate the claim scores:
H_Freq$Score <- Score(Psi = mal, s = maxi, l_0 = start, Input_Score = Input_Score_H)
# Split data into training and test set:
H_Freq_Train <- H_Freq[which((as.Date(H_Freq$dt_End) <= as.Date(dt_Split)) & (H_Freq$History == 0)), ]
H_Freq_Test <- H_Freq[which((as.Date(H_Freq$dt_Start) >= as.Date(dt_Split)) & (H_Freq$History == 0)), ]
# Estimate the model:
GAM_Freq_H <- gam(Count ~ 1 + ProductType + Year + Age +
                 FamilySituation + BuildingType + RoofType + Capacity + ConstructionYear + Residence +
                 Urban + GlassCoverage + 
                 s(Score, bs = 'cr', k = k_cr, fx = TRUE, pc = start), 
                 offset = log(Exposure), data = H_Freq_Train, family = poisson(link = 'log'))
# Plot the spline:
plot(GAM_Freq_H, col = 'blue', xlab = 'Claim score level', 
     ylab = 'Estimated effect')[[1]][c('x', 'se', 'fit')]
# Summarize the model output:
summary(GAM_Freq_H)
# Retrieve the model predictions:
GAM_PIG_H_fc <- predict(GAM_Freq_H, H_Freq_Test, type = 'response')
# Determine AIC and BIC:
IC_H <- rbind(IC_H, c(AIC(GAM_Freq_H), BIC(GAM_Freq_H)))
# Retrieve log-likelihood and number of parameters:
logL_One_H <- rbind(logL_One_H, logLik(GAM_Freq_H))
K_One_H <- rbind(K_One_H, length(GAM_Freq_H$coefficients))

### GAM - PP - H
# Retrieve optimal claim score parameters:
fm <- 3
mal <- Opt_Scores_H[fm, 3]
maxi <- Opt_Scores_H[fm, 2]
start <- Opt_Scores_H[fm, 1]
# Calculate the claim scores:
H_Freq$Score <- Score(Psi = mal, s = maxi, l_0 = start, Input_Score = Input_Score_H)
# Split data into training and test set:
H_Freq_Train <- H_Freq[which((as.Date(H_Freq$dt_End) <= as.Date(dt_Split)) & (H_Freq$History == 0)), ]
H_Freq_Test <- H_Freq[which((as.Date(H_Freq$dt_Start) >= as.Date(dt_Split)) & (H_Freq$History == 0)), ]
# Estimate the model:
GAM_Freq_H <- gam(Count ~ 1 + ProductType + Year + Age +
                    FamilySituation + BuildingType + RoofType + Capacity + ConstructionYear + Residence +
                    Urban + GlassCoverage + 
                    s(Score, bs = 'cr', k = k_cr, fx = TRUE, pc = start), 
                  offset = log(Exposure), data = H_Freq_Train, family = poisson(link = 'log'))
# Plot the spline:
plot(GAM_Freq_H, col = 'blue', xlab = 'Claim score level', 
     ylab = 'Estimated effect')[[1]][c('x', 'se', 'fit')]
# Summarize the model output:
summary(GAM_Freq_H)
# Retrieve the model predictions:
GAM_PP_H_fc <- predict(GAM_Freq_H, H_Freq_Test, type = 'response')
# Determine AIC and BIC:
IC_H <- rbind(IC_H, c(AIC(GAM_Freq_H), BIC(GAM_Freq_H)))
# Retrieve log-likelihood and number of parameters:
logL_One_H <- rbind(logL_One_H, logLik(GAM_Freq_H))
K_One_H <- rbind(K_One_H, length(GAM_Freq_H$coefficients))

### GAM - NBG - H
# Retrieve optimal claim score parameters:
fm <- 4
mal <- Opt_Scores_H[fm, 3]
maxi <- Opt_Scores_H[fm, 2]
start <- Opt_Scores_H[fm, 1]
# Calculate the claim scores:
H_Freq$Score <- Score(Psi = mal, s = maxi, l_0 = start, Input_Score = Input_Score_H)
# Split data into training and test set:
H_Freq_Train <- H_Freq[which((as.Date(H_Freq$dt_End) <= as.Date(dt_Split)) & (H_Freq$History == 0)), ]
H_Freq_Test <- H_Freq[which((as.Date(H_Freq$dt_Start) >= as.Date(dt_Split)) & (H_Freq$History == 0)), ]
# Estimate the model:
GAM_Freq_H <- gam(Count ~ 1 + ProductType + Year + Age +
                 FamilySituation + BuildingType + RoofType + Capacity + ConstructionYear + Residence +
                 Urban + GlassCoverage +
                 s(Score, bs = 'cr', k = k_cr, fx = TRUE, pc = start),
                 offset = log(Exposure), data = H_Freq_Train, family = nb(link = 'log'))
# Plot the spline:
plot(GAM_Freq_H, col = 'blue', xlab = 'Claim score level', 
     ylab = 'Estimated effect')[[1]][c('x', 'se', 'fit')]
# Summarize the model output:
summary(GAM_Freq_H)
# Retrieve the model predictions:
GAM_NBG_H_fc <- predict(GAM_Freq_H, H_Freq_Test, type = 'response')
# Determine AIC and BIC:
IC_H <- rbind(IC_H, c(AIC(GAM_Freq_H), BIC(GAM_Freq_H)))
# Retrieve log-likelihood and number of parameters:
logL_One_H <- rbind(logL_One_H, logLik(GAM_Freq_H))
K_One_H <- rbind(K_One_H, length(GAM_Freq_H$coefficients))


### GAM - NBIG - H
# Retrieve optimal claim score parameters:
fm <- 4
mal <- Opt_Scores_H[fm, 3]
maxi <- Opt_Scores_H[fm, 2]
start <- Opt_Scores_H[fm, 1]
# Calculate the claim scores:
H_Freq$Score <- Score(Psi = mal, s = maxi, l_0 = start, Input_Score = Input_Score_H)
# Split data into training and test set:
H_Freq_Train <- H_Freq[which((as.Date(H_Freq$dt_End) <= as.Date(dt_Split)) & (H_Freq$History == 0)), ]
H_Freq_Test <- H_Freq[which((as.Date(H_Freq$dt_Start) >= as.Date(dt_Split)) & (H_Freq$History == 0)), ]
# Estimate the model:
GAM_Freq_H <- gam(Count ~ 1 + ProductType + Year + Age +
                 FamilySituation + BuildingType + RoofType + Capacity + ConstructionYear + Residence +
                 Urban + GlassCoverage +
                 s(Score, bs = 'cr', k = k_cr, fx = TRUE, pc = start),
                 offset = log(Exposure), data = H_Freq_Train, family = nb(link = 'log'))
# Plot the spline:
plot(GAM_Freq_H, col = 'blue', xlab = 'Claim score level', 
     ylab = 'Estimated effect')[[1]][c('x', 'se', 'fit')]
# Summarize the model output:
summary(GAM_Freq_H)
# Retrieve the model predictions:
GAM_NBIG_H_fc <- predict(GAM_Freq_H, H_Freq_Test, type = 'response')
# Determine AIC and BIC:
IC_H <- rbind(IC_H, c(AIC(GAM_Freq_H), BIC(GAM_Freq_H)))
# Retrieve log-likelihood and number of parameters:
logL_One_H <- rbind(logL_One_H, logLik(GAM_Freq_H))
K_One_H <- rbind(K_One_H, length(GAM_Freq_H$coefficients))

### GAM - NBP - H
# Retrieve optimal claim score parameters:
fm <- 5
mal <- Opt_Scores_H[fm, 3]
maxi <- Opt_Scores_H[fm, 2]
start <- Opt_Scores_H[fm, 1]
# Calculate the claim scores:
H_Freq$Score <- Score(Psi = mal, s = maxi, l_0 = start, Input_Score = Input_Score_H)
# Split data into training and test set:
H_Freq_Train <- H_Freq[which((as.Date(H_Freq$dt_End) <= as.Date(dt_Split)) & (H_Freq$History == 0)), ]
H_Freq_Test <- H_Freq[which((as.Date(H_Freq$dt_Start) >= as.Date(dt_Split)) & (H_Freq$History == 0)), ]
# Estimate the model:
GAM_Freq_H <- gam(Count ~ 1 + ProductType + Year + Age +
                    FamilySituation + BuildingType + RoofType + Capacity + ConstructionYear + Residence +
                    Urban + GlassCoverage +
                    s(Score, bs = 'cr', k = k_cr, fx = TRUE, pc = start),
                  offset = log(Exposure), data = H_Freq_Train, family = nb(link = 'log'))
# Plot the spline:
plot(GAM_Freq_H, col = 'blue', xlab = 'Claim score level', 
     ylab = 'Estimated effect')[[1]][c('x', 'se', 'fit')]
# Summarize the model output:
summary(GAM_Freq_H)
# Retrieve the model predictions:
GAM_NBP_H_fc <- predict(GAM_Freq_H, H_Freq_Test, type = 'response')
# Determine AIC and BIC:
IC_H <- rbind(IC_H, c(AIC(GAM_Freq_H), BIC(GAM_Freq_H)))
# Retrieve log-likelihood and number of parameters:
logL_One_H <- rbind(logL_One_H, logLik(GAM_Freq_H))
K_One_H <- rbind(K_One_H, length(GAM_Freq_H$coefficients))

### GAM - ZIPG - H
# Retrieve optimal claim score parameters:
fm <- 7
mal <- Opt_Scores_H[fm, 3]
maxi <- Opt_Scores_H[fm, 2]
start <- Opt_Scores_H[fm, 1]
# Calculate the claim scores:
H_Freq$Score <- Score(Psi = mal, s = maxi, l_0 = start, Input_Score = Input_Score_H)
# Split data into training and test set:
H_Freq_Train <- H_Freq[which((as.Date(H_Freq$dt_Eind_risico) <= as.Date(dt_Split)) & (H_Freq$History == 0)), ]
H_Freq_Test <- H_Freq[which((as.Date(H_Freq$dt_Start_risico) >= as.Date(dt_Split)) & (H_Freq$History == 0)), ]
# Estimate the model:
GAM_Freq_H <- gam(list(Count ~ 1 + ProductType + Year + Age +
                         FamilySituation + BuildingType + RoofType + Capacity + ConstructionYear + Residence +
                         Urban + GlassCoverage +
                         s(Score, bs = 'cr', k = k_cr, fx = TRUE, pc = start) + 
                         offset(log(Exposure)), ~ 1 + ProductType + Year + Age +
                         FamilySituation + BuildingType + RoofType + Capacity + ConstructionYear + Residence +
                         Urban + GlassCoverage),
                  data = H_Freq_Train, family = ziplss())
# Plot the spline:
plot(GAM_Freq_H, col = 'blue', xlab = 'Claim score level', 
     ylab = 'Estimated effect')[[1]][c('x', 'se', 'fit')]
# Summarize the model output:
summary(GAM_Freq_H)
# Retrieve the model predictions:
GAM_ZIPG_H_fc <- predict(GAM_Freq_H, H_Freq_Test, type = 'response')
# Determine AIC and BIC:
IC_H <- rbind(IC_H, c(AIC(GAM_Freq_H), BIC(GAM_Freq_H)))
# Retrieve log-likelihood and number of parameters:
logL_One_H <- rbind(logL_One_H, logLik(GAM_Freq_H))
K_One_H <- rbind(K_One_H, length(GAM_Freq_H$coefficients))

### GAM - ZIPIG - H
# Retrieve optimal claim score parameters:
fm <- 8
mal <- Opt_Scores_H[fm, 3]
maxi <- Opt_Scores_H[fm, 2]
start <- Opt_Scores_H[fm, 1]
# Calculate the claim scores:
H_Freq$Score <- Score(Psi = mal, s = maxi, l_0 = start, Input_Score = Input_Score_H)
# Split data into training and test set:
H_Freq_Train <- H_Freq[which((as.Date(H_Freq$dt_Eind_risico) <= as.Date(dt_Split)) & (H_Freq$History == 0)), ]
H_Freq_Test <- H_Freq[which((as.Date(H_Freq$dt_Start_risico) >= as.Date(dt_Split)) & (H_Freq$History == 0)), ]
# Estimate the model:
GAM_Freq_H <- gam(list(Count ~ 1 + ProductType + Year + Age +
                         FamilySituation + BuildingType + RoofType + Capacity + ConstructionYear + Residence +
                         Urban + GlassCoverage +
                         s(Score, bs = 'cr', k = k_cr, fx = TRUE, pc = start) + 
                         offset(log(Exposure)), ~ 1 + ProductType + Year + Age +
                         FamilySituation + BuildingType + RoofType + Capacity + ConstructionYear + Residence +
                         Urban + GlassCoverage),
                  data = H_Freq_Train, family = ziplss())
# Plot the spline:
plot(GAM_Freq_H, col = 'blue', xlab = 'Claim score level', 
     ylab = 'Estimated effect')[[1]][c('x', 'se', 'fit')]
# Summarize the model output:
summary(GAM_Freq_H)
# Retrieve the model predictions:
GAM_ZIPIG_H_fc <- predict(GAM_Freq_H, H_Freq_Test, type = 'response')
# Determine AIC and BIC:
IC_H <- rbind(IC_H, c(AIC(GAM_Freq_H), BIC(GAM_Freq_H)))
# Retrieve log-likelihood and number of parameters:
logL_One_H <- rbind(logL_One_H, logLik(GAM_Freq_H))
K_One_H <- rbind(K_One_H, length(GAM_Freq_H$coefficients))

### GAM - ZIPP - H
# Retrieve optimal claim score parameters:
fm <- 9
mal <- Opt_Scores_H[fm, 3]
maxi <- Opt_Scores_H[fm, 2]
start <- Opt_Scores_H[fm, 1]
# Calculate the claim scores:
H_Freq$Score <- Score(Psi = mal, s = maxi, l_0 = start, Input_Score = Input_Score_H)
# Split data into training and test set:
H_Freq_Train <- H_Freq[which((as.Date(H_Freq$dt_Eind_risico) <= as.Date(dt_Split)) & (H_Freq$History == 0)), ]
H_Freq_Test <- H_Freq[which((as.Date(H_Freq$dt_Start_risico) >= as.Date(dt_Split)) & (H_Freq$History == 0)), ]
# Estimate the model:
GAM_Freq_H <- gam(list(Count ~ 1 + ProductType + Year + Age +
                         FamilySituation + BuildingType + RoofType + Capacity + ConstructionYear + Residence +
                         Urban + GlassCoverage +
                         s(Score, bs = 'cr', k = k_cr, fx = TRUE, pc = start) + 
                         offset(log(Exposure)), ~ 1 + ProductType + Year + Age +
                         FamilySituation + BuildingType + RoofType + Capacity + ConstructionYear + Residence +
                         Urban + GlassCoverage),
                  data = H_Freq_Train, family = ziplss())
# Plot the spline:
plot(GAM_Freq_H, col = 'blue', xlab = 'Claim score level', 
     ylab = 'Estimated effect')[[1]][c('x', 'se', 'fit')]
# Summarize the model output:
summary(GAM_Freq_H)
# Retrieve the model predictions:
GAM_ZIPP_H_fc <- predict(GAM_Freq_H, H_Freq_Test, type = 'response')
# Determine AIC and BIC:
IC_H <- rbind(IC_H, c(AIC(GAM_Freq_H), BIC(GAM_Freq_H)))
# Retrieve log-likelihood and number of parameters:
logL_One_H <- rbind(logL_One_H, logLik(GAM_Freq_H))
K_One_H <- rbind(K_One_H, length(GAM_Freq_H$coefficients))

### Linear - PG - H
# Retrieve optimal claim score parameters:
fm <- 10
mal <- Opt_Scores_H[fm, 3]
maxi <- Opt_Scores_H[fm, 2]
start <- Opt_Scores_H[fm, 1]
# Calculate the claim scores:
H_Freq$Score <- Score(Psi = mal, s = maxi, l_0 = start, Input_Score = Input_Score_H)
# Split data into training and test set:
H_Freq_Train <- H_Freq[which((as.Date(H_Freq$dt_End) <= as.Date(dt_Split)) & (H_Freq$History == 0)), ]
H_Freq_Test <- H_Freq[which((as.Date(H_Freq$dt_Start) >= as.Date(dt_Split)) & (H_Freq$History == 0)), ]
# Estimate the model:
Lin_Freq_H <- glm(Count ~ 1 + ProductType + Year + Age +
                 FamilySituation + BuildingType + RoofType + Capacity + ConstructionYear + Residence +
                 Urban + GlassCoverage +
                 I(Score - start),
                 offset = log(Exposure), data = H_Freq_Train, family = poisson(link = 'log'))
# Summarize the model output:
summary(Lin_Freq_H)
# Retrieve the model predictions:
Lin_PG_H_fc <- predict(Lin_Freq_H, H_Freq_Test, type = 'response')
# Determine AIC and BIC:
IC_H <- rbind(IC_H, c(AIC(Lin_Freq_H), BIC(Lin_Freq_H)))
# Retrieve log-likelihood and number of parameters:
logL_One_H <- rbind(logL_One_H, logLik(Lin_Freq_H))
K_One_H <- rbind(K_One_H, length(Lin_Freq_H$coefficients))

### Linear - PIG - H
# Retrieve optimal claim score parameters:
fm <- 11
mal <- Opt_Scores_H[fm, 3]
maxi <- Opt_Scores_H[fm, 2]
start <- Opt_Scores_H[fm, 1]
# Calculate the claim scores:
H_Freq$Score <- Score(Psi = mal, s = maxi, l_0 = start, Input_Score = Input_Score_H)
# Split data into training and test set:
H_Freq_Train <- H_Freq[which((as.Date(H_Freq$dt_End) <= as.Date(dt_Split)) & (H_Freq$History == 0)), ]
H_Freq_Test <- H_Freq[which((as.Date(H_Freq$dt_Start) >= as.Date(dt_Split)) & (H_Freq$History == 0)), ]
# Estimate the model:
Lin_Freq_H <- glm(Count ~ 1 + ProductType + Year + Age +
                 FamilySituation + BuildingType + RoofType + Capacity + ConstructionYear + Residence +
                 Urban + GlassCoverage +
                 I(Score - start),
                 offset = log(Exposure), data = H_Freq_Train, family = poisson(link = 'log'))
# Summarize the model output:
summary(Lin_Freq_H)
# Retrieve the model predictions:
Lin_PIG_H_fc <- predict(Lin_Freq_H, H_Freq_Test, type = 'response')
# Determine AIC and BIC:
IC_H <- rbind(IC_H, c(AIC(Lin_Freq_H), BIC(Lin_Freq_H)))
# Retrieve log-likelihood and number of parameters:
logL_One_H <- rbind(logL_One_H, logLik(Lin_Freq_H))
K_One_H <- rbind(K_One_H, length(Lin_Freq_H$coefficients))

### Linear - PP - H
# Retrieve optimal claim score parameters:
fm <- 12
mal <- Opt_Scores_H[fm, 3]
maxi <- Opt_Scores_H[fm, 2]
start <- Opt_Scores_H[fm, 1]
# Calculate the claim scores:
H_Freq$Score <- Score(Psi = mal, s = maxi, l_0 = start, Input_Score = Input_Score_H)
# Split data into training and test set:
H_Freq_Train <- H_Freq[which((as.Date(H_Freq$dt_End) <= as.Date(dt_Split)) & (H_Freq$History == 0)), ]
H_Freq_Test <- H_Freq[which((as.Date(H_Freq$dt_Start) >= as.Date(dt_Split)) & (H_Freq$History == 0)), ]
# Estimate the model:
Lin_Freq_H <- glm(Count ~ 1 + ProductType + Year + Age +
                    FamilySituation + BuildingType + RoofType + Capacity + ConstructionYear + Residence +
                    Urban + GlassCoverage +
                    I(Score - start),
                  offset = log(Exposure), data = H_Freq_Train, family = poisson(link = 'log'))
# Summarize the model output:
summary(Lin_Freq_H)
# Retrieve the model predictions:
Lin_PP_H_fc <- predict(Lin_Freq_H, H_Freq_Test, type = 'response')
# Determine AIC and BIC:
IC_H <- rbind(IC_H, c(AIC(Lin_Freq_H), BIC(Lin_Freq_H)))
# Retrieve log-likelihood and number of parameters:
logL_One_H <- rbind(logL_One_H, logLik(Lin_Freq_H))
K_One_H <- rbind(K_One_H, length(Lin_Freq_H$coefficients))

### Linear - NBG - H
# Retrieve optimal claim score parameters:
fm <- 13
mal <- Opt_Scores_H[fm, 3]
maxi <- Opt_Scores_H[fm, 2]
start <- Opt_Scores_H[fm, 1]
# Calculate the claim scores:
H_Freq$Score <- Score(Psi = mal, s = maxi, l_0 = start, Input_Score = Input_Score_H)
# Split data into training and test set:
H_Freq_Train <- H_Freq[which((as.Date(H_Freq$dt_End) <= as.Date(dt_Split)) & (H_Freq$History == 0)), ]
H_Freq_Test <- H_Freq[which((as.Date(H_Freq$dt_Start) >= as.Date(dt_Split)) & (H_Freq$History == 0)), ]
# Estimate the model:
Lin_Freq_H <- glm.nb(Count ~ 1 + ProductType + Year + Age +
                    FamilySituation + BuildingType + RoofType + Capacity + ConstructionYear + Residence +
                    Urban + GlassCoverage +
                    I(Score - start) +
                    offset(log(Exposure)), data = H_Freq_Train, link = log)
# Summarize the model output:
summary(Lin_Freq_H)
# Retrieve the model predictions:
Lin_NBG_H_fc <- predict(Lin_Freq_H, H_Freq_Test, type = 'response')
# Determine AIC and BIC:
IC_H <- rbind(IC_H, c(AIC(Lin_Freq_H), BIC(Lin_Freq_H)))
# Retrieve log-likelihood and number of parameters:
logL_One_H <- rbind(logL_One_H, logLik(Lin_Freq_H))
K_One_H <- rbind(K_One_H, length(Lin_Freq_H$coefficients))

### Linear - NBIG - H
# Retrieve optimal claim score parameters:
fm <- 14
mal <- Opt_Scores_H[fm, 3]
maxi <- Opt_Scores_H[fm, 2]
start <- Opt_Scores_H[fm, 1]
# Calculate the claim scores:
H_Freq$Score <- Score(Psi = mal, s = maxi, l_0 = start, Input_Score = Input_Score_H)
# Split data into training and test set:
H_Freq_Train <- H_Freq[which((as.Date(H_Freq$dt_End) <= as.Date(dt_Split)) & (H_Freq$History == 0)), ]
H_Freq_Test <- H_Freq[which((as.Date(H_Freq$dt_Start) >= as.Date(dt_Split)) & (H_Freq$History == 0)), ]
# Estimate the model:
Lin_Freq_H <- glm.nb(Count ~ 1 + ProductType + Year + Age +
                    FamilySituation + BuildingType + RoofType + Capacity + ConstructionYear + Residence +
                    Urban + GlassCoverage +
                    I(Score - start) +
                    offset(log(Exposure)), data = H_Freq_Train, link = log)
# Summarize the model output:
summary(Lin_Freq_H)
# Retrieve the model predictions:
Lin_NBIG_H_fc <- predict(Lin_Freq_H, H_Freq_Test, type = 'response')
# Determine AIC and BIC:
IC_H <- rbind(IC_H, c(AIC(Lin_Freq_H), BIC(Lin_Freq_H)))
# Retrieve log-likelihood and number of parameters:
logL_One_H <- rbind(logL_One_H, logLik(Lin_Freq_H))
K_One_H <- rbind(K_One_H, length(Lin_Freq_H$coefficients))

### Linear - NBP - H
# Retrieve optimal claim score parameters:
fm <- 15
mal <- Opt_Scores_H[fm, 3]
maxi <- Opt_Scores_H[fm, 2]
start <- Opt_Scores_H[fm, 1]
# Calculate the claim scores:
H_Freq$Score <- Score(Psi = mal, s = maxi, l_0 = start, Input_Score = Input_Score_H)
# Split data into training and test set:
H_Freq_Train <- H_Freq[which((as.Date(H_Freq$dt_End) <= as.Date(dt_Split)) & (H_Freq$History == 0)), ]
H_Freq_Test <- H_Freq[which((as.Date(H_Freq$dt_Start) >= as.Date(dt_Split)) & (H_Freq$History == 0)), ]
# Estimate the model:
Lin_Freq_H <- glm.nb(Count ~ 1 + ProductType + Year + Age +
                       FamilySituation + BuildingType + RoofType + Capacity + ConstructionYear + Residence +
                       Urban + GlassCoverage +
                       I(Score - start) +
                       offset(log(Exposure)), data = H_Freq_Train, link = log)
# Summarize the model output:
summary(Lin_Freq_H)
# Retrieve the model predictions:
Lin_NBP_H_fc <- predict(Lin_Freq_H, H_Freq_Test, type = 'response')
# Determine AIC and BIC:
IC_H <- rbind(IC_H, c(AIC(Lin_Freq_H), BIC(Lin_Freq_H)))
# Retrieve log-likelihood and number of parameters:
logL_One_H <- rbind(logL_One_H, logLik(Lin_Freq_H))
K_One_H <- rbind(K_One_H, length(Lin_Freq_H$coefficients))

### Linear - ZIPG - H
# Retrieve optimal claim score parameters:
fm <- 16
mal <- Opt_Scores_H[fm, 3]
maxi <- Opt_Scores_H[fm, 2]
start <- Opt_Scores_H[fm, 1]
# Calculate the claim scores:
H_Freq$Score <- Score(Psi = mal, s = maxi, l_0 = start, Input_Score = Input_Score_H)
H_Freq$Score_start <- H_Freq$Score - start
# Split data into training and test set:
H_Freq_Train <- H_Freq[which((as.Date(H_Freq$dt_Eind_risico) <= as.Date(dt_Split)) & (H_Freq$History == 0)), ]
H_Freq_Test <- H_Freq[which((as.Date(H_Freq$dt_Start_risico) >= as.Date(dt_Split)) & (H_Freq$History == 0)), ]
# Estimate the model:
Lin_Freq_H <- gam(list(Count ~ 1 + ProductType + Year + Age +
                         FamilySituation + BuildingType + RoofType + Capacity + ConstructionYear + Residence +
                         Urban + GlassCoverage +
                         Score_start +
                         offset(log(Exposure)), ~ 1 + ProductType + Year + Age +
                         FamilySituation + BuildingType + RoofType + Capacity + ConstructionYear + Residence +
                         Urban + GlassCoverage),
                  data = H_Freq_Train, family = ziplss())
# Summarize the model output:
summary(Lin_Freq_H)
# Retrieve the model predictions:
Lin_ZIPG_H_fc <- predict(Lin_Freq_H, H_Freq_Test, type = 'response')
# Determine AIC and BIC:
IC_H <- rbind(IC_H, c(AIC(Lin_Freq_H), BIC(Lin_Freq_H)))
# Retrieve log-likelihood and number of parameters:
logL_One_H <- rbind(logL_One_H, logLik(Lin_Freq_H))
K_One_H <- rbind(K_One_H, length(Lin_Freq_H$coefficients))

### Linear - ZIPIG - H
# Retrieve optimal claim score parameters:
fm <- 17
mal <- Opt_Scores_H[fm, 3]
maxi <- Opt_Scores_H[fm, 2]
start <- Opt_Scores_H[fm, 1]
# Calculate the claim scores:
H_Freq$Score <- Score(Psi = mal, s = maxi, l_0 = start, Input_Score = Input_Score_H)
H_Freq$Score_start <- H_Freq$Score - start
# Split data into training and test set:
H_Freq_Train <- H_Freq[which((as.Date(H_Freq$dt_Eind_risico) <= as.Date(dt_Split)) & (H_Freq$History == 0)), ]
H_Freq_Test <- H_Freq[which((as.Date(H_Freq$dt_Start_risico) >= as.Date(dt_Split)) & (H_Freq$History == 0)), ]
# Estimate the model:
Lin_Freq_H <- gam(list(Count ~ 1 + ProductType + Year + Age +
                         FamilySituation + BuildingType + RoofType + Capacity + ConstructionYear + Residence +
                         Urban + GlassCoverage +
                         Score_start +
                         offset(log(Exposure)), ~ 1 + ProductType + Year + Age +
                         FamilySituation + BuildingType + RoofType + Capacity + ConstructionYear + Residence +
                         Urban + GlassCoverage),
                  data = H_Freq_Train, family = ziplss())
# Summarize the model output:
summary(Lin_Freq_H)
# Retrieve the model predictions:
Lin_ZIPIG_H_fc <- predict(Lin_Freq_H, H_Freq_Test, type = 'response')
# Determine AIC and BIC:
IC_H <- rbind(IC_H, c(AIC(Lin_Freq_H), BIC(Lin_Freq_H)))
# Retrieve log-likelihood and number of parameters:
logL_One_H <- rbind(logL_One_H, logLik(Lin_Freq_H))
K_One_H <- rbind(K_One_H, length(Lin_Freq_H$coefficients))

### Linear - ZIPP - H
# Retrieve optimal claim score parameters:
fm <- 18
mal <- Opt_Scores_H[fm, 3]
maxi <- Opt_Scores_H[fm, 2]
start <- Opt_Scores_H[fm, 1]
# Calculate the claim scores:
H_Freq$Score <- Score(Psi = mal, s = maxi, l_0 = start, Input_Score = Input_Score_H)
H_Freq$Score_start <- H_Freq$Score - start
# Split data into training and test set:
H_Freq_Train <- H_Freq[which((as.Date(H_Freq$dt_Eind_risico) <= as.Date(dt_Split)) & (H_Freq$History == 0)), ]
H_Freq_Test <- H_Freq[which((as.Date(H_Freq$dt_Start_risico) >= as.Date(dt_Split)) & (H_Freq$History == 0)), ]
# Estimate the model:
Lin_Freq_H <- gam(list(Count ~ 1 + ProductType + Year + Age +
                         FamilySituation + BuildingType + RoofType + Capacity + ConstructionYear + Residence +
                         Urban + GlassCoverage +
                         Score_start +
                         offset(log(Exposure)), ~ 1 + ProductType + Year + Age +
                         FamilySituation + BuildingType + RoofType + Capacity + ConstructionYear + Residence +
                         Urban + GlassCoverage),
                  data = H_Freq_Train, family = ziplss())
# Summarize the model output:
summary(Lin_Freq_H)
# Retrieve the model predictions:
Lin_ZIPP_H_fc <- predict(Lin_Freq_H, H_Freq_Test, type = 'response')
# Determine AIC and BIC:
IC_H <- rbind(IC_H, c(AIC(Lin_Freq_H), BIC(Lin_Freq_H)))
# Retrieve log-likelihood and number of parameters:
logL_One_H <- rbind(logL_One_H, logLik(Lin_Freq_H))
K_One_H <- rbind(K_One_H, length(Lin_Freq_H$coefficients))

##### Table with ratio Gini coefficients and SE's #####
### Setup output
# (Ratio Gini, Variance) x 
# ([Benchmark] GLM_PG, GLM_PIG, GLM_PP, GLM_NBG, GLM_NBIG, GLM_NBP, GLM_ZIPG, GLM_ZIPIG, GLM_ZIPP,
# GAM_PG, GAM_PIG, GAM_PP, GAM_NBG, GAM_NBIG, GAM_NBP, GAM_ZIPG, GAM_ZIPIG, GAM_ZIPP,
# Lin_PG, Lin_PIG, Lin_PP, Lin_NBG, Lin_NBIG, Lin_NBP, Lin_ZIPG, Lin_ZIPIG, Lin_ZIPP) x 
# ([Alternative] GLM_PG, GLM_PIG, GLM_PP, GLM_NBG, GLM_NBIG, GLM_NBP, GLM_ZIPG, GLM_ZIPIG, GLM_ZIPP,
# GAM_PG, GAM_PIG, GAM_PP, GAM_NBG, GAM_NBIG, GAM_NBP, GAM_ZIPG, GAM_ZIPIG, GAM_ZIPP,
# Lin_PG, Lin_PIG, Lin_PP, Lin_NBG, Lin_NBIG, Lin_NBP, Lin_ZIPG, Lin_ZIPIG, Lin_ZIPP)
Dynamic_Out_samp <- array(NA, dim = c(2, 27, 27))

### Setup custom R functions
# Function for ratio Gini coefficient calculation
Gini <- dget('.../Custom_R_functions/Gini.R')

### Construction of model forecasts
Mod_fc <- cbind(GLM_PG_H_fc, GLM_PIG_H_fc, GLM_PP_H_fc, 
                GLM_NBG_H_fc, GLM_NBIG_H_fc, GLM_NBP_H_fc,
                GLM_ZIPG_H_fc, GLM_ZIPIG_H_fc, GLM_ZIPP_H_fc,
                GAM_PG_H_fc * GLM_G_H_Sev_fc, GAM_PIG_H_fc * GLM_IG_H_Sev_fc, GAM_PP_H_fc * GLM_P_H_Sev_fc,
                GAM_NBG_H_fc * GLM_G_H_Sev_fc, GAM_NBIG_H_fc * GLM_IG_H_Sev_fc, GAM_NBP_H_fc * GLM_P_H_Sev_fc,
                GAM_ZIPG_H_fc * GLM_G_H_Sev_fc, GAM_ZIPIG_H_fc * GLM_IG_H_Sev_fc, GAM_ZIPP_H_fc * GLM_P_H_Sev_fc,
                Lin_PG_H_fc * GLM_G_H_Sev_fc, Lin_PIG_H_fc * GLM_IG_H_Sev_fc, Lin_PP_H_fc * GLM_P_H_Sev_fc,
                Lin_NBG_H_fc * GLM_G_H_Sev_fc, Lin_NBIG_H_fc * GLM_IG_H_Sev_fc, Lin_NBP_H_fc * GLM_P_H_Sev_fc,
                Lin_ZIPG_H_fc * GLM_G_H_Sev_fc, Lin_ZIPIG_H_fc * GLM_IG_H_Sev_fc, Lin_ZIPP_H_fc * GLM_P_H_Sev_fc)

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
    c(100, 1) * Gini(Candidate = Mod_fc[, a], Reference = Mod_fc[, b], Actual = H_Freq_Test$Size)[[1]]
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
LR_Test_Single_H <- matrix(cbind(rep(NA, 9 * 2), rep(NA, 9 * 2)), 9 * 2, 2)
logL_null_Single_H <- c(rep(rep(logL_GLM_H, each = 3), 2))
logL_alt_Single_H <- c(logL_One_H)
K_null_Single_H <- c(rep(rep(K_GLM_H, each = 3), 2))
K_alt_Single_H <- c(K_One_H)

### Calculation of test statistics and p-values
for (assump in 1:2) {
  for (mod in 1:9) {
    ind <- (assump - 1) * 9 + mod
    LR_Test_Single_H[ind, 1] <- (-2) * (logL_null_Single_H[ind] - logL_alt_Single_H[ind])
    LR_Test_Single_H[ind, 2] <- 1 - pchisq(LR_Test_Single_H[ind, 1], df = (K_alt_Single_H[ind] - K_null_Single_H[ind]))
  }
}

### Store output
