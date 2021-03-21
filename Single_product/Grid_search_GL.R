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
### Load confidential insurance data on general liability (GL) insurance
#   - GL_Freq containing information on the claim counts;
#   - GL_Sev containing information on the claim severities.

##### Evaluation sets #####
### Split date
dt_Split <- '2018-01-01'

### Split data into training and test set
GL_Freq_Train <- GL_Freq[which((as.Date(GL_Freq$dt_End) <= as.Date(dt_Split)) & (GL_Freq$History == 0)), ]
GL_Freq_Test <- GL_Freq[which((as.Date(GL_Freq$dt_Start) >= as.Date(dt_Split)) & (GL_Freq$History == 0)), ]
GL_Sev_Train <- GL_Sev[which(as.Date(GL_Sev$dt_End) <= as.Date(dt_Split)), ]

##### GLM frequency #####
### GL - Poisson model and forecasts
GLM_P_GL_Freq <- glm(Count ~ 1 + FamilySituation, 
                     offset = log(Exposure), data = GL_Freq_Train, family = poisson(link = 'log'))

GLM_P_GL_Freq_fc <- predict(GLM_P_GL_Freq, GL_Freq_Test, type = 'response')

### GL - NB model and forecasts
GLM_NB_GL_Freq <- glm.nb(Count ~ 1 + FamilySituation + 
                           offset(log(Exposure)), data = GL_Freq_Train, link = log)

GLM_NB_GL_Freq_fc <- predict(GLM_NB_GL_Freq, GL_Freq_Test, type = 'response')

### GL - ZIP model and forecasts
GLM_ZIP_GL_Freq <- gam(list(Count ~ 1 + FamilySituation + 
                              offset(log(Exposure)), ~ 1 + FamilySituation),
                       data = GL_Freq_Train, family = ziplss())

GLM_ZIP_GL_Freq_fc <- predict(GLM_ZIP_GL_Freq, GL_Freq_Test, type = 'response')

##### GLM severity #####
### GL - Gamma model, forecasts and information criteria
GLM_G_GL_Sev <- glm(Size ~ 1 + FamilySituation, data = GL_Sev_Train,
                    family = Gamma(link = 'log'))

GLM_G_GL_Sev_fc <- predict(GLM_G_GL_Sev, GL_Freq_Test, type = 'response')

IC_GL <- c(AIC(GLM_P_GL_Sev), BIC(GLM_P_GL_Sev))

### GL - IG model, forecasts and information criteria
GLM_IG_GL_Sev <- glm(Size ~ 1 + FamilySituation, data = GL_Sev_Train,
                     family = inverse.gaussian(link = 'log'))

GLM_IG_GL_Sev_fc <- predict(GLM_IG_GL_Sev, GL_Freq_Test, type = 'response')

IC_GL <- rbind(IC_GL, c(AIC(GLM_NB_GL_Sev), BIC(GLM_NB_GL_Sev)))

### GL - Pareto model, forecasts and information criteria
GLM_P_GL_Sev <- gamlss(Size ~ 1 + FamilySituation, data = GL_Sev_Train,
                       family = PARETO2o(mu.link = 'log', sigma.link = 'log'),
                       trace = FALSE)

GLM_P_GL_Sev_fc <- predict(GLM_P_GL_Sev, what = 'mu', newdata = 
                              GL_Freq_Test[, c('Size', 'FamilySituation')], type = 'response')

IC_GL <- rbind(IC_GL, c(AIC(GLM_P_GL_Sev), BIC(GLM_P_GL_Sev)))

##### GLM predictions #####
### GL
GLM_PG_GL_fc <- GLM_P_GL_Freq_fc * GLM_G_GL_Sev_fc
GLM_PIG_GL_fc <- GLM_P_GL_Freq_fc * GLM_IG_GL_Sev_fc
GLM_PP_GL_fc <- GLM_P_GL_Freq_fc * GLM_P_GL_Sev_fc
GLM_NBG_GL_fc <- GLM_NB_GL_Freq_fc * GLM_G_GL_Sev_fc
GLM_NBIG_GL_fc <- GLM_NB_GL_Freq_fc * GLM_IG_GL_Sev_fc
GLM_NBP_GL_fc <- GLM_NB_GL_Freq_fc * GLM_P_GL_Sev_fc
GLM_ZIPG_GL_fc <- GLM_ZIP_GL_Freq_fc * GLM_G_GL_Sev_fc
GLM_ZIPIG_GL_fc <- GLM_ZIP_GL_Freq_fc * GLM_IG_GL_Sev_fc
GLM_ZIPP_GL_fc <- GLM_ZIP_GL_Freq_fc * GLM_P_GL_Sev_fc

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
Score_Out_samp_GL <- array(NA, dim = c(length(ell_0), length(s), length(Psi), 2, 2, 3, 3, 2))

### Setup custom R functions
# Preparatory function for (faster) claim score optimization
Input_Score <- dget('.../Custom_R_functions/Input_Score.R')
# Function for claim score calculation
Score <- dget('.../Custom_R_functions/Score.R')
# Function for ratio Gini coefficient calculation
Gini <- dget('.../Custom_R_functions/Gini.R')

### Setup claim score - GL
GL_Freq$Score <- 0
Input_Score_GL <- Input_Score(Data = GL_Freq)

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
      
      Temp_GL <- c()
      
      ##### Estimation - GL ####
      ### Calculate the claim scores
      GL_Freq$Score <- Score(Psi = mal, s = maxi, l_0 = start, Input_Score = Input_Score_GL)
      GL_Freq$Score_start <- GL_Freq$Score - start
      
      ### Split data into training and test set
      GL_Freq_Train <- GL_Freq[which((as.Date(GL_Freq$dt_End) <= as.Date(dt_Split)) & (GL_Freq$History == 0)), ]
      GL_Freq_Test <- GL_Freq[which((as.Date(GL_Freq$dt_Start) >= as.Date(dt_Split)) & (GL_Freq$History == 0)), ]
      
      ### Output - GAM - Poisson
      # Predictions:
      GAM_P_GL_fc <- predict(gam(Count ~ 1 + FamilySituation +
                                   s(Score, bs = 'cr', k = k_cr, fx = TRUE, pc = start),
                                 offset = log(Exposure), data = GL_Freq_Train, family = poisson(link = 'log')), 
                             GL_Freq_Test, type = 'response')
      # Ratio Gini coefficients with standard errors:
      Temp_GL <- c(Temp_GL, c(100, 1) * Gini(Candidate = GLM_PG_GL_fc, Reference = GAM_P_GL_fc * GLM_G_GL_Sev_fc, 
                                               Actual = GL_Freq_Test$Size)[[1]],
                    c(100, 1) * Gini(Candidate = GAM_P_GL_fc * GLM_G_GL_Sev_fc, Reference = GLM_PG_GL_fc, 
                                           Actual = GL_Freq_Test$Size)[[1]],
                    c(100, 1) * Gini(Candidate = GLM_PIG_GL_fc, Reference = GAM_P_GL_fc * GLM_IG_GL_Sev_fc, 
                                     Actual = GL_Freq_Test$Size)[[1]],
                    c(100, 1) * Gini(Candidate = GAM_P_GL_fc * GLM_IG_GL_Sev_fc, Reference = GLM_PIG_GL_fc, 
                                     Actual = GL_Freq_Test$Size)[[1]],
                   c(100, 1) * Gini(Candidate = GLM_PP_GL_fc, Reference = GAM_P_GL_fc * GLM_P_GL_Sev_fc, 
                                    Actual = GL_Freq_Test$Size)[[1]],
                   c(100, 1) * Gini(Candidate = GAM_P_GL_fc * GLM_P_GL_Sev_fc, Reference = GLM_PP_GL_fc, 
                                    Actual = GL_Freq_Test$Size)[[1]])
      
      ### Output - GAM - NB
      # Predictions:
      GAM_NB_GL_fc <- predict(gam(Count ~ 1 + FamilySituation +
                                   s(Score, bs = 'cr', k = k_cr, fx = TRUE, pc = start),
                                   offset = log(Exposure), data = GL_Freq_Train, family = nb(link = 'log')), 
                               GL_Freq_Test, type = 'response')
      # Ratio Gini coefficients with standard errors:
      Temp_GL <- c(Temp_GL, c(100, 1) * Gini(Candidate = GLM_NBG_GL_fc, Reference = GAM_NB_GL_fc * GLM_G_GL_Sev_fc, 
                                               Actual = GL_Freq_Test$Size)[[1]],
                    c(100, 1) * Gini(Candidate = GAM_NB_GL_fc * GLM_G_GL_Sev_fc, Reference = GLM_NBG_GL_fc, 
                                     Actual = GL_Freq_Test$Size)[[1]],
                    c(100, 1) * Gini(Candidate = GLM_NBIG_GL_fc, Reference = GAM_NB_GL_fc * GLM_IG_GL_Sev_fc, 
                                     Actual = GL_Freq_Test$Size)[[1]],
                    c(100, 1) * Gini(Candidate = GAM_NB_GL_fc * GLM_IG_GL_Sev_fc, Reference = GLM_NBIG_GL_fc, 
                                     Actual = GL_Freq_Test$Size)[[1]],
                   c(100, 1) * Gini(Candidate = GLM_NBP_GL_fc, Reference = GAM_NB_GL_fc * GLM_P_GL_Sev_fc, 
                                    Actual = GL_Freq_Test$Size)[[1]],
                   c(100, 1) * Gini(Candidate = GAM_NB_GL_fc * GLM_P_GL_Sev_fc, Reference = GLM_NBP_GL_fc, 
                                    Actual = GL_Freq_Test$Size)[[1]])
      
      ### Output - GAM - ZIP
      # Predictions:
      GAM_ZIP_GL_fc <- predict(gam(list(Count ~ 1 + FamilySituation +
                                          s(Score, bs = 'cr', k = k_cr, fx = TRUE, pc = start) + 
                                          offset(log(Exposure)), ~ 1 + FamilySituation),
                                   data = GL_Freq_Train, family = ziplss()),
                               GL_Freq_Test, type = 'response')
      # Ratio Gini coefficients with standard errors:
      Temp_GL <- c(Temp_GL, c(100, 1) * Gini(Candidate = GLM_ZIPG_GL_fc, Reference = GAM_ZIP_GL_fc * GLM_G_GL_Sev_fc, 
                                               Actual = GL_Freq_Test$Size)[[1]],
                    c(100, 1) * Gini(Candidate = GAM_ZIP_GL_fc * GLM_G_GL_Sev_fc, Reference = GLM_ZIPG_GL_fc, 
                                     Actual = GL_Freq_Test$Size)[[1]],
                    c(100, 1) * Gini(Candidate = GLM_ZIPIG_GL_fc, Reference = GAM_ZIP_GL_fc * GLM_IG_GL_Sev_fc, 
                                     Actual = GL_Freq_Test$Size)[[1]],
                    c(100, 1) * Gini(Candidate = GAM_ZIP_GL_fc * GLM_IG_GL_Sev_fc, Reference = GLM_ZIPIG_GL_fc, 
                                     Actual = GL_Freq_Test$Size)[[1]],
                    c(100, 1) * Gini(Candidate = GLM_ZIPP_GL_fc, Reference = GAM_ZIP_GL_fc * GLM_P_GL_Sev_fc, 
                                     Actual = GL_Freq_Test$Size)[[1]],
                    c(100, 1) * Gini(Candidate = GAM_ZIP_GL_fc * GLM_P_GL_Sev_fc, Reference = GLM_ZIPP_GL_fc, 
                                     Actual = GL_Freq_Test$Size)[[1]])
      
      ### Output - Linear - Poisson
      # Predictions:
      Lin_P_GL_fc <- predict(glm(Count ~ 1 + FamilySituation +
                                  I(Score - start),
                                  offset = log(Exposure), data = GL_Freq_Train, family = poisson(link = 'log')), 
                              GL_Freq_Test, type = 'response')
      # Ratio Gini coefficients with standard errors:
      Temp_GL <- c(Temp_GL, c(100, 1) * Gini(Candidate = GLM_PG_GL_fc, Reference = Lin_P_GL_fc * GLM_G_GL_Sev_fc, 
                                               Actual = GL_Freq_Test$Size)[[1]],
                    c(100, 1) * Gini(Candidate = Lin_P_GL_fc * GLM_G_GL_Sev_fc, Reference = GLM_PG_GL_fc, 
                                     Actual = GL_Freq_Test$Size)[[1]],
                    c(100, 1) * Gini(Candidate = GLM_PIG_GL_fc, Reference = Lin_P_GL_fc * GLM_IG_GL_Sev_fc, 
                                     Actual = GL_Freq_Test$Size)[[1]],
                    c(100, 1) * Gini(Candidate = Lin_P_GL_fc * GLM_IG_GL_Sev_fc, Reference = GLM_PIG_GL_fc, 
                                     Actual = GL_Freq_Test$Size)[[1]],
                   c(100, 1) * Gini(Candidate = GLM_PP_GL_fc, Reference = Lin_P_GL_fc * GLM_P_GL_Sev_fc, 
                                    Actual = GL_Freq_Test$Size)[[1]],
                   c(100, 1) * Gini(Candidate = Lin_P_GL_fc * GLM_P_GL_Sev_fc, Reference = GLM_PP_GL_fc, 
                                    Actual = GL_Freq_Test$Size)[[1]])
      
      ### Output - Linear - NB
      # Predictions:
      Lin_NB_GL_fc <- predict(glm.nb(Count ~ 1 + FamilySituation +
                                      I(Score - start) +
                                      offset(log(Exposure)), data = GL_Freq_Train, link = log), 
                               GL_Freq_Test, type = 'response')
      # Ratio Gini coefficients with standard errors:
      Temp_GL <- c(Temp_GL, c(100, 1) * Gini(Candidate = GLM_NBG_GL_fc, Reference = Lin_NB_GL_fc * GLM_G_GL_Sev_fc, 
                                               Actual = GL_Freq_Test$Size)[[1]],
                    c(100, 1) * Gini(Candidate = Lin_NB_GL_fc * GLM_G_GL_Sev_fc, Reference = GLM_NBG_GL_fc, 
                                     Actual = GL_Freq_Test$Size)[[1]],
                    c(100, 1) * Gini(Candidate = GLM_NBIG_GL_fc, Reference = Lin_NB_GL_fc * GLM_IG_GL_Sev_fc, 
                                     Actual = GL_Freq_Test$Size)[[1]],
                    c(100, 1) * Gini(Candidate = Lin_NB_GL_fc * GLM_IG_GL_Sev_fc, Reference = GLM_NBIG_GL_fc, 
                                     Actual = GL_Freq_Test$Size)[[1]],
                   c(100, 1) * Gini(Candidate = GLM_NBP_GL_fc, Reference = Lin_NB_GL_fc * GLM_P_GL_Sev_fc, 
                                    Actual = GL_Freq_Test$Size)[[1]],
                   c(100, 1) * Gini(Candidate = Lin_NB_GL_fc * GLM_P_GL_Sev_fc, Reference = GLM_NBP_GL_fc, 
                                    Actual = GL_Freq_Test$Size)[[1]])
      
      ### Output - Linear - ZIP
      # Predictions:
      Lin_ZIP_GL_fc <- predict(gam(list(Count ~ 1 + FamilySituation +
                                           Score_start +
                                           offset(log(Exposure)), ~ 1 + FamilySituation),
                                    data = GL_Freq_Train, family = ziplss()),
                                GL_Freq_Test, type = 'response')
      # Ratio Gini coefficients with standard errors:
      Temp_GL <- c(Temp_GL, c(100, 1) * Gini(Candidate = GLM_ZIPG_GL_fc, Reference = Lin_ZIP_GL_fc * GLM_G_GL_Sev_fc, 
                                               Actual = GL_Freq_Test$Size)[[1]],
                    c(100, 1) * Gini(Candidate = Lin_ZIP_GL_fc * GLM_G_GL_Sev_fc, Reference = GLM_ZIPG_GL_fc, 
                                     Actual = GL_Freq_Test$Size)[[1]],
                    c(100, 1) * Gini(Candidate = GLM_ZIPIG_GL_fc, Reference = Lin_ZIP_GL_fc * GLM_IG_GL_Sev_fc, 
                                     Actual = GL_Freq_Test$Size)[[1]],
                    c(100, 1) * Gini(Candidate = Lin_ZIP_GL_fc * GLM_IG_GL_Sev_fc, Reference = GLM_ZIPIG_GL_fc, 
                                     Actual = GL_Freq_Test$Size)[[1]],
                    c(100, 1) * Gini(Candidate = GLM_ZIPP_GL_fc, Reference = Lin_ZIP_GL_fc * GLM_P_GL_Sev_fc, 
                                     Actual = GL_Freq_Test$Size)[[1]],
                    c(100, 1) * Gini(Candidate = Lin_ZIP_GL_fc * GLM_P_GL_Sev_fc, Reference = GLM_ZIPP_GL_fc, 
                                     Actual = GL_Freq_Test$Size)[[1]])
      
      ### Collect all output
      Temp_GL
    }
    
    ### Store parallel output
    CS_Temp <- t(sapply(CS_Temp, rbind))
    dim(CS_Temp) <- c(length(ell_0_maxi), dim(Score_Out_samp_GL)[-c(1:3)])
    Score_Out_samp_GL[1:length(ell_0_maxi), maxi_i, mal_i, , , , , ] <- CS_Temp
  }
}
stopCluster(Clusters)

### Store the final results
