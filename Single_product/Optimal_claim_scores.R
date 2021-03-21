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

##### Settings - Input #####
### Setup custom R functions
# Function for claim score calculation
Score <- dget('.../Custom_R_functions/Score.R')

### Score parameters - Input
# Maximum score:
s <- c(3:25)
# Jump parameter:
Psi <- c(1:(max(s) - 1))

### Score parameters - Fixed
# Lowest score:
L_Min <- 1
# Initial score for new policyholders:
ell_0 <- c((L_Min + 1):(max(s) - 1))

### Constraint (number of observations in each bucket >= 0.01%)
Exp_Min <- 0.01 / 100

##### Score iterations data #####
### Load results from rGini_Optimization_GL.R

### Load results from rGini_Optimization_HC.R

### Load results from rGini_Optimization_H.R

### Load results from rGini_Optimization_T.R

##### Checking of all iterations #####
### Output preparation
# ell_0 x s x Psi x (GL, HC, H, T)
Exp_Buckets_rGini <- array(NA, dim = c(length(ell_0), length(s), length(Psi), 4))

### Parallel setup
Cores <- detectCores() - 1
Clusters <- makeCluster(Cores) 
registerDoParallel(Clusters)

### GL
# Extract exposures from training set:
GL_Train <- which((as.Date(GL_Freq$dt_End) <= as.Date(dt_Split)) & (GL_Freq$History == 0))
Exp_GL <- GL_Freq$Exposure[GL_Train]
# Loop over the maximum score levels (s):
for (j in 1:length(s)){
  # Only consider jump parameters where Psi < s:
  Psi_h <- Psi[which(Psi < s[j])]
  
  # Loop over the feasible jump parameters (Psi_h):
  for (k in 1:length(Psi_h)){
    # Only consider initial score levels where ell_0 < s:
    ell_0_h <- ell_0[which(ell_0 < s[j])]
    
    # Keep track of the progress:
    print(paste('GL - Max ', j, '/', length(s),
                ', Malus ', k, '/', length(Psi_h),
                ', s (', length(ell_0_h), ')',
                sep = ''))
    
    # Loop over the feasible initial score levels (ell_0_h) - Parallel:
    Check_Temp <- foreach(i = 1:length(ell_0_h)) %dopar% {
      # Calculate the claim scores in training set:
      Score_Check <- Score(Psi = Psi_h[k], s = s[j], l_0 = ell_0_h[i], Input_Score = Input_Score_GL)
      Score_Check <- Score_Check[GL_Train]
      # Determine the exposure in each claim score level after truncation towards ell_0:
      Exp_Check <- t(apply(as.matrix(1:s[j]), 1, function(z) sum(Exp_GL[which((trunc(Score_Check - ell_0_h[i]) 
                           + ell_0_h[i]) == z)]))) / sum(Exp_GL)
      # Check whether the exposure constraint is satisfied in all claim score levels:
      sum(length(which(Exp_Check >= Exp_Min))) == length(Exp_Check)
    }
    
    # Store parallel output:
    Exp_Buckets_rGini[1:length(ell_0_h), j, k, 1] <- unlist(Check_Temp)
  }
}

### HC
# Extract exposures from training set:
HC_Train <- which((as.Date(HC_Freq$dt_End) <= as.Date(dt_Split)) & (HC_Freq$History == 0))
Exp_HC <- HC_Freq$Exposure[HC_Train]
# Loop over the maximum score levels (s):
for (j in 1:length(s)){
  # Only consider jump parameters where Psi < s:
  Psi_h <- Psi[which(Psi < s[j])]
  
  # Loop over the feasible jump parameters (Psi_h):
  for (k in 1:length(Psi_h)){
    # Only consider initial score levels where ell_0 < s:
    ell_0_h <- ell_0[which(ell_0 < s[j])]
    
    # Keep track of the progress:
    print(paste('HC - Max ', j, '/', length(s),
                ', Malus ', k, '/', length(Psi_h),
                ', s (', length(ell_0_h), ')',
                sep = ''))
    
    # Loop over the feasible initial score levels (ell_0_h) - Parallel:
    Check_Temp <- foreach(i = 1:length(ell_0_h)) %dopar% {
      # Calculate the claim scores in training set:
      Score_Check <- Score(Psi = Psi_h[k], s = s[j], l_0 = ell_0_h[i], Input_Score = Input_Score_HC)
      Score_Check <- Score_Check[HC_Train]
      # Determine the exposure in each claim score level after truncation towards ell_0:
      Exp_Check <- t(apply(as.matrix(1:s[j]), 1, function(z) sum(Exp_HC[which((trunc(Score_Check - ell_0_h[i]) 
                           + ell_0_h[i]) == z)]))) / sum(Exp_HC)
      # Check whether the exposure constraint is satisfied in all claim score levels:
      sum(length(which(Exp_Check >= Exp_Min))) == length(Exp_Check)
    }
    
    # Store parallel output:
    Exp_Buckets_rGini[1:length(ell_0_h), j, k, 2] <- unlist(Check_Temp)
  }
}

### H
# Extract exposures from training set:
H_Train <- which((as.Date(H_Freq$dt_End) <= as.Date(dt_Split)) & (H_Freq$History == 0))
Exp_H <- H_Freq$Exposure[H_Train]
# Loop over the maximum score levels (s):
for (j in 1:length(s)){
  # Only consider jump parameters where Psi < s:
  Psi_h <- Psi[which(Psi < s[j])]
  
  # Loop over the feasible jump parameters (Psi_h):
  for (k in 1:length(Psi_h)){
    # Only consider initial score levels where ell_0 < s:
    ell_0_h <- ell_0[which(ell_0 < s[j])]
    
    # Keep track of the progress:
    print(paste('H - Max ', j, '/', length(s),
                ', Malus ', k, '/', length(Psi_h),
                ', s (', length(ell_0_h), ')',
                sep = ''))
    
    # Loop over the feasible initial score levels (ell_0_h) - Parallel:
    Check_Temp <- foreach(i = 1:length(ell_0_h)) %dopar% {
      # Calculate the claim scores in training set:
      Score_Check <- Score(Psi = Psi_h[k], s = s[j], l_0 = ell_0_h[i], Input_Score = Input_Score_H)
      Score_Check <- Score_Check[H_Train]
      # Determine the exposure in each claim score level after truncation towards ell_0:
      Exp_Check <- t(apply(as.matrix(1:s[j]), 1, function(z) sum(Exp_H[which((trunc(Score_Check - ell_0_h[i]) 
                           + ell_0_h[i]) == z)]))) / sum(Exp_H)
      # Check whether the exposure constraint is satisfied in all claim score levels:
      sum(length(which(Exp_Check >= Exp_Min))) == length(Exp_Check)
    }
    
    # Store parallel output:
    Exp_Buckets_rGini[1:length(ell_0_h), j, k, 3] <- unlist(Check_Temp)
  }
}

### T
# Extract exposures from training set:
T_Train <- which((as.Date(T_Freq$dt_End) <= as.Date(dt_Split)) & (T_Freq$History == 0))
Exp_T <- T_Freq$Exposure[T_Train]
# Loop over the maximum score levels (s):
for (j in 1:length(s)){
  # Only consider jump parameters where Psi < s:
  Psi_h <- Psi[which(Psi < s[j])]
  
  # Loop over the feasible jump parameters (Psi_h):
  for (k in 1:length(Psi_h)){
    # Only consider initial score levels where ell_0 < s:
    ell_0_h <- ell_0[which(ell_0 < s[j])]
    
    # Keep track of the progress:
    print(paste('T - Max ', j, '/', length(s),
                ', Malus ', k, '/', length(Psi_h),
                ', s (', length(ell_0_h), ')',
                sep = ''))
    
    # Loop over the feasible initial score levels (ell_0_h) - Parallel:
    Check_Temp <- foreach(i = 1:length(ell_0_h)) %dopar% {
      # Calculate the claim scores in training set:
      Score_Check <- Score(Psi = Psi_h[k], s = s[j], l_0 = ell_0_h[i], Input_Score = Input_Score_T)
      Score_Check <- Score_Check[T_Train]
      # Determine the exposure in each claim score level after truncation towards ell_0:
      Exp_Check <- t(apply(as.matrix(1:s[j]), 1, function(z) sum(Exp_T[which((trunc(Score_Check - ell_0_h[i]) 
                           + ell_0_h[i]) == z)]))) / sum(Exp_T)
      # Check whether the exposure constraint is satisfied in all claim score levels:
      sum(length(which(Exp_Check >= Exp_Min))) == length(Exp_Check)
    }
    
    # Store parallel output:
    Exp_Buckets_rGini[1:length(ell_0_h), j, k, 4] <- unlist(Check_Temp)
  }
}

### Stop clusters
stopCluster(Clusters)

##### Adjusted score selection #####
### Function to determine the optimal claim score parameters
Diagnostic_Adj <- function(Opt_rGini, Exp_rGini) {
  # Select the exposures which satisfy the exposure constraint:
  Select_Exp <- which(Exp_rGini == TRUE, arr.ind = TRUE)
  # Select the claim score parameter combinations with the lowest rank that satisfy the exposure constraint:
  Opt_1 <- which(Opt_rGini[, , , 1] == min(Opt_rGini[, , , 1][Select_Exp], na.rm = TRUE), arr.ind = TRUE)
  Opt_1 <- as.matrix(inner_join(as.data.frame(Opt_1), as.data.frame(Select_Exp), by = c('dim1', 'dim2', 'dim3')))
  # Of these claim score parameter combinations, select the combinations that lead to lowest ratio Gini coefficient:
  Opt_2 <- which(Opt_rGini[, , , 2] == min(Opt_rGini[, , , 2][Opt_1]), arr.ind = TRUE)
  Opt_2 <- as.matrix(inner_join(as.data.frame(Opt_2), as.data.frame(Opt_1), by = c('dim1', 'dim2', 'dim3')))
  # In case of a draw, now select the claim score parameter combinations with the lowest variance for the ratio Gini coefficient:
  Opt_3 <- which(Opt_rGini[, , , 3] == min(Opt_rGini[, , , 3][Opt_2]), arr.ind = TRUE)
  Opt_3 <- as.matrix(inner_join(as.data.frame(Opt_3), as.data.frame(Opt_2), by = c('dim1', 'dim2', 'dim3')))
  # Return output:
  return(Opt_3)
}

### Data
# GL
# Load results from Grid_search_GL.R
Score_Out_samp_Multi <- list(Score_Out_samp_GL)
# HC
# Load results from Grid_search_HC.R
Score_Out_samp_Multi[[2]] <- Score_Out_samp_HC
# H
# Load results from Grid_search_H.R
Score_Out_samp_Multi[[3]] <- Score_Out_samp_H
# T
# Load results from Grid_search_T.R
Score_Out_samp_Multi[[4]] <- Score_Out_samp_T

### Output preparation
# ell_0 x s x Psi x (GAM, Linear) x (Poisson, NB, ZIP) x (Gamma, IG, Pareto) x (Rank, Ratio Gini, Variance) x (GL, HC, H, T)
rGini_Out_samp_Multi <- array(NA, dim = c(length(ell_0), length(s), length(Psi), 2, 3, 3, 3, 4))

### Parallel setup
Cores <- detectCores() - 1
Clusters <- makeCluster(Cores) 
registerDoParallel(Clusters)

##### Ratio Gini iterations #####
### GL
# Loop over the maximum score levels (s):
for (j in 1:length(s)){
  # Only consider jump parameters where Psi < s:
  Psi_h <- Psi[which(Psi < s[j])]
  
  # Loop over the feasible jump parameters (Psi_h):
  for (k in 1:length(Psi_h)){
    # Only consider initial score levels where ell_0 < s:
    ell_0_h <- ell_0[which(ell_0 < s[j])]
    
    # Keep track of the progress:
    print(paste('GL - Max ', j, '/', length(s),
                ', Malus ', k, '/', length(Psi_h),
                ', s (', length(ell_0_h), ')',
                sep = ''))
    
    # Loop over the feasible initial score levels (ell_0_h) - Parallel:
    Temp_rGini <- foreach(i = 1:length(ell_0_h)) %dopar% {
      Temp_i <- array(NA, dim = c(2, 3, 3, 3))
      # Loop over the model specifications (GAM/Linear):
      for (m in 1:2){
        # Loop over the frequency models (Poisson/NB/ZIP):
        for (df in 1:3){
          # Loop over the severity models (Gamma/IG/Pareto):
          for (ds in 1:3){
            # Determine the rank, ratio Gini coefficient and its variance:
            Temp_i[m, df, ds, ] <- rbind(apply(as.matrix(Score_Out_samp_Multi[[1]][i, j, k, 1, , df, ds, m]), 2, rank, 
                                               ties.method = 'first')[1, ], 
                                               Score_Out_samp_Multi[[1]][i, j, k, 1, , df, ds, m][1], 
                                               Score_Out_samp_Multi[[1]][i, j, k, 2, , df, ds, m][1])
          }
        }
      }
      # Collect all output:
      Temp_i
    }
    # Store parallel output:
    Temp_rGini <- t(sapply(Temp_rGini, rbind))
    dim(Temp_rGini) <- c(length(ell_0_h), dim(rGini_Out_samp_Multi)[4:7])
    rGini_Out_samp_Multi[1:length(ell_0_h), j, k, , , , , 1] <- Temp_rGini
  }
}

### HC
# Loop over the maximum score levels (s):
for (j in 1:length(s)){
  # Only consider jump parameters where Psi < s:
  Psi_h <- Psi[which(Psi < s[j])]
  
  # Loop over the feasible jump parameters (Psi_h):
  for (k in 1:length(Psi_h)){
    # Only consider initial score levels where ell_0 < s:
    ell_0_h <- ell_0[which(ell_0 < s[j])]
    
    # Keep track of the progress:
    print(paste('HC - Max ', j, '/', length(s),
                ', Malus ', k, '/', length(Psi_h),
                ', s (', length(ell_0_h), ')',
                sep = ''))
    
    # Loop over the feasible initial score levels (ell_0_h) - Parallel:
    Temp_rGini <- foreach(i = 1:length(ell_0_h)) %dopar% {
      Temp_i <- array(NA, dim = c(2, 3, 3, 3))
      # Loop over the model specifications (GAM/Linear):
      for (m in 1:2){
        # Loop over the frequency models (Poisson/NB/ZIP):
        for (df in 1:3){
          # Loop over the severity models (Gamma/IG/Pareto):
          for (ds in 1:3){
            # Determine the rank, ratio Gini coefficient and its variance:
            Temp_i[m, df, ds, ] <- rbind(apply(as.matrix(Score_Out_samp_Multi[[2]][i, j, k, 1, , df, ds, m]), 2, rank, 
                                               ties.method = 'first')[1, ], 
                                               Score_Out_samp_Multi[[2]][i, j, k, 1, , df, ds, m][1], 
                                               Score_Out_samp_Multi[[2]][i, j, k, 2, , df, ds, m][1])
          }
        }
      }
      # Collect all output:
      Temp_i
    }
    # Store parallel output:
    Temp_rGini <- t(sapply(Temp_rGini, rbind))
    dim(Temp_rGini) <- c(length(ell_0_h), dim(rGini_Out_samp_Multi)[4:7])
    rGini_Out_samp_Multi[1:length(ell_0_h), j, k, , , , , 2] <- Temp_rGini
  }
}

### H
# Loop over the maximum score levels (s):
for (j in 1:length(s)){
  # Only consider jump parameters where Psi < s:
  Psi_h <- Psi[which(Psi < s[j])]
  
  # Loop over the feasible jump parameters (Psi_h):
  for (k in 1:length(Psi_h)){
    # Only consider initial score levels where ell_0 < s:
    ell_0_h <- ell_0[which(ell_0 < s[j])]
    
    # Keep track of the progress:
    print(paste('H - Max ', j, '/', length(s),
                ', Malus ', k, '/', length(Psi_h),
                ', s (', length(ell_0_h), ')',
                sep = ''))
    
    # Loop over the feasible initial score levels (ell_0_h) - Parallel:
    Temp_rGini <- foreach(i = 1:length(ell_0_h)) %dopar% {
      Temp_i <- array(NA, dim = c(2, 3, 3, 3))
      # Loop over the model specifications (GAM/Linear):
      for (m in 1:2){
        # Loop over the frequency models (Poisson/NB/ZIP):
        for (df in 1:3){
          # Loop over the severity models (Gamma/IG/Pareto):
          for (ds in 1:3){
            # Determine the rank, ratio Gini coefficient and its variance:
            Temp_i[m, df, ds, ] <- rbind(apply(as.matrix(Score_Out_samp_Multi[[3]][i, j, k, 1, , df, ds, m]), 2, rank, 
                                               ties.method = 'first')[1, ], 
                                               Score_Out_samp_Multi[[3]][i, j, k, 1, , df, ds, m][1], 
                                               Score_Out_samp_Multi[[3]][i, j, k, 2, , df, ds, m][1])
          }
        }
      }
      # Collect all output:
      Temp_i
    }
    # Store parallel output:
    Temp_rGini <- t(sapply(Temp_rGini, rbind))
    dim(Temp_rGini) <- c(length(ell_0_h), dim(rGini_Out_samp_Multi)[4:7])
    rGini_Out_samp_Multi[1:length(ell_0_h), j, k, , , , , 3] <- Temp_rGini
  }
}

### T
# Loop over the maximum score levels (s):
for (j in 1:length(s)){
  # Only consider jump parameters where Psi < s:
  Psi_h <- Psi[which(Psi < s[j])]
  
  # Loop over the feasible jump parameters (Psi_h):
  for (k in 1:length(Psi_h)){
    # Only consider initial score levels where ell_0 < s:
    ell_0_h <- ell_0[which(ell_0 < s[j])]
    
    # Keep track of the progress:
    print(paste('T - Max ', j, '/', length(s),
                ', Malus ', k, '/', length(Psi_h),
                ', s (', length(ell_0_h), ')',
                sep = ''))
    
    # Loop over the feasible initial score levels (ell_0_h) - Parallel:
    Temp_rGini <- foreach(i=1:length(ell_0_h)) %dopar% {
      Temp_i <- array(NA, dim = c(2, 3, 3, 3))
      # Loop over the model specifications (GAM/Linear):
      for (m in 1:2){
        # Loop over the frequency models (Poisson/NB/ZIP):
        for (df in 1:3){
          # Loop over the severity models (Gamma/IG/Pareto):
          for (ds in 1:3){
            # Determine the rank, ratio Gini coefficient and its variance:
            Temp_i[m, df, ds, ] <- rbind(apply(as.matrix(Score_Out_samp_Multi[[4]][i, j, k, 1, , df, ds, m]), 2, rank, 
                                               ties.method = 'first')[1, ], 
                                               Score_Out_samp_Multi[[4]][i, j, k, 1, , df, ds, m][1], 
                                               Score_Out_samp_Multi[[4]][i, j, k, 2, , df, ds, m][1])
          }
        }
      }
      # Collect all output:
      Temp_i
    }
    # Store parallel output:
    Temp_rGini <- t(sapply(Temp_rGini, rbind))
    dim(Temp_rGini) <- c(length(ell_0_h), dim(rGini_Out_samp_Multi)[4:7])
    rGini_Out_samp_Multi[1:length(ell_0_h), j, k, , , , , 4] <- Temp_rGini
  }
}

### Stop clusters
stopCluster(Clusters)

### Claim score parameters - GL
# (ell_0 x s x Psi):
Opt_rGini_GAM_PG_GL <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 1, 1, 1, , 1], Exp_Buckets_rGini[, , , 1])
# (ell_0 x s x Psi):
Opt_rGini_GAM_PIG_GL <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 1, 1, 2, , 1], Exp_Buckets_rGini[, , , 1])
# (ell_0 x s x Psi):
Opt_rGini_GAM_PP_GL <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 1, 1, 3, , 1], Exp_Buckets_rGini[, , , 1])
# (ell_0 x s x Psi):
Opt_rGini_GAM_NBG_GL <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 1, 2, 1, , 1], Exp_Buckets_rGini[, , , 1])
# (ell_0 x s x Psi):
Opt_rGini_GAM_NBIG_GL <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 1, 2, 2, , 1], Exp_Buckets_rGini[, , , 1])
# (ell_0 x s x Psi):
Opt_rGini_GAM_NBP_GL <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 1, 2, 3, , 1], Exp_Buckets_rGini[, , , 1])
# (ell_0 x s x Psi):
Opt_rGini_GAM_ZIPG_GL <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 1, 3, 1, , 1], Exp_Buckets_rGini[, , , 1])
# (ell_0 x s x Psi):
Opt_rGini_GAM_ZIPIG_GL <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 1, 3, 2, , 1], Exp_Buckets_rGini[, , , 1])
# (ell_0 x s x Psi):
Opt_rGini_GAM_ZIPP_GL <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 1, 3, 3, , 1], Exp_Buckets_rGini[, , , 1])
# (ell_0 x s x Psi):
Opt_rGini_Lin_PG_GL <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 2, 1, 1, , 1], Exp_Buckets_rGini[, , , 1])
# (ell_0 x s x Psi):
Opt_rGini_Lin_PIG_GL <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 2, 1, 2, , 1], Exp_Buckets_rGini[, , , 1])
# (ell_0 x s x Psi):
Opt_rGini_Lin_PP_GL <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 2, 1, 3, , 1], Exp_Buckets_rGini[, , , 1])
# (ell_0 x s x Psi):
Opt_rGini_Lin_NBG_GL <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 2, 2, 1, , 1], Exp_Buckets_rGini[, , , 1])
# (ell_0 x s x Psi):
Opt_rGini_Lin_NBIG_GL <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 2, 2, 2, , 1], Exp_Buckets_rGini[, , , 1])
# (ell_0 x s x Psi):
Opt_rGini_Lin_NBP_GL <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 2, 2, 3, , 1], Exp_Buckets_rGini[, , , 1])
# (ell_0 x s x Psi):
Opt_rGini_Lin_ZIPG_GL <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 2, 3, 1, , 1], Exp_Buckets_rGini[, , , 1])
# (ell_0 x s x Psi):
Opt_rGini_Lin_ZIPIG_GL <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 2, 3, 2, , 1], Exp_Buckets_rGini[, , , 1])
# (ell_0 x s x Psi):
Opt_rGini_Lin_ZIPP_GL <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 2, 3, 3, , 1], Exp_Buckets_rGini[, , , 1])
# (18) x (ell_0 x s x Psi)
Opt_Scores_GL <- as.data.frame(rbind(Opt_rGini_GAM_PG_GL, Opt_rGini_GAM_PIG_GL, Opt_rGini_GAM_PP_GL,
                                      Opt_rGini_GAM_NBG_GL, Opt_rGini_GAM_NBIG_GL, Opt_rGini_GAM_NBP_GL,
                                      Opt_rGini_GAM_ZIPG_GL, Opt_rGini_GAM_ZIPIG_GL, Opt_rGini_GAM_ZIPP_GL,
                                      Opt_rGini_Lin_PG_GL, Opt_rGini_Lin_PIG_GL, Opt_rGini_Lin_PP_GL,
                                      Opt_rGini_Lin_NBG_GL, Opt_rGini_Lin_NBIG_GL, Opt_rGini_Lin_NBP_GL,
                                      Opt_rGini_Lin_ZIPG_GL, Opt_rGini_Lin_ZIPIG_GL, Opt_rGini_Lin_ZIPP_GL))
colnames(Opt_Scores_GL) <- c('ell_0', 's', 'Psi')
Opt_Scores_GL$ell_0 <- as.numeric(as.character(Opt_Scores_GL$ell_0)) + 1
Opt_Scores_GL$s <- as.numeric(as.character(Opt_Scores_GL$s)) + 2
Opt_Scores_GL$Psi <- as.numeric(as.character(Opt_Scores_GL$Psi)) + 0

### Claim score parameters - HC
# (ell_0 x s x Psi):
Opt_rGini_GAM_PG_HC <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 1, 1, 1, , 2], Exp_Buckets_rGini[, , , 2])    
# (ell_0 x s x Psi):
Opt_rGini_GAM_PIG_HC <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 1, 1, 2, , 2], Exp_Buckets_rGini[, , , 2])     
# (ell_0 x s x Psi):
Opt_rGini_GAM_PP_HC <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 1, 1, 3, , 2], Exp_Buckets_rGini[, , , 2])  
# (ell_0 x s x Psi):
Opt_rGini_GAM_NBG_HC <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 1, 2, 1, , 2], Exp_Buckets_rGini[, , , 2])   
# (ell_0 x s x Psi):
Opt_rGini_GAM_NBIG_HC <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 1, 2, 2, , 2], Exp_Buckets_rGini[, , , 2])    
# (ell_0 x s x Psi):
Opt_rGini_GAM_NBP_HC <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 1, 2, 3, , 2], Exp_Buckets_rGini[, , , 2])  
# (ell_0 x s x Psi):
Opt_rGini_GAM_ZIPG_HC <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 1, 3, 1, , 2], Exp_Buckets_rGini[, , , 2])   
# (ell_0 x s x Psi):
Opt_rGini_GAM_ZIPIG_HC <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 1, 3, 2, , 2], Exp_Buckets_rGini[, , , 2])    
# (ell_0 x s x Psi):
Opt_rGini_GAM_ZIPP_HC <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 1, 3, 3, , 2], Exp_Buckets_rGini[, , , 2]) 
# (ell_0 x s x Psi):
Opt_rGini_Lin_PG_HC <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 2, 1, 1, , 2], Exp_Buckets_rGini[, , , 2])    
# (ell_0 x s x Psi):
Opt_rGini_Lin_PIG_HC <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 2, 1, 2, , 2], Exp_Buckets_rGini[, , , 2])    
# (ell_0 x s x Psi):
Opt_rGini_Lin_PP_HC <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 2, 1, 3, , 2], Exp_Buckets_rGini[, , , 2])   
# (ell_0 x s x Psi):
Opt_rGini_Lin_NBG_HC <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 2, 2, 1, , 2], Exp_Buckets_rGini[, , , 2])   
# (ell_0 x s x Psi):
Opt_rGini_Lin_NBIG_HC <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 2, 2, 2, , 2], Exp_Buckets_rGini[, , , 2])   
# (ell_0 x s x Psi):
Opt_rGini_Lin_NBP_HC <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 2, 2, 3, , 2], Exp_Buckets_rGini[, , , 2])   
# (ell_0 x s x Psi):
Opt_rGini_Lin_ZIPG_HC <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 2, 3, 1, , 2], Exp_Buckets_rGini[, , , 2])   
# (ell_0 x s x Psi):
Opt_rGini_Lin_ZIPIG_HC <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 2, 3, 2, , 2], Exp_Buckets_rGini[, , , 2])   
# (ell_0 x s x Psi):
Opt_rGini_Lin_ZIPP_HC <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 2, 3, 3, , 2], Exp_Buckets_rGini[, , , 2])   
# (18) x (ell_0 x s x Psi)
Opt_Scores_HC <- as.data.frame(rbind(Opt_rGini_GAM_PG_HC, Opt_rGini_GAM_PIG_HC, Opt_rGini_GAM_PP_HC,
                                      Opt_rGini_GAM_NBG_HC, Opt_rGini_GAM_NBIG_HC, Opt_rGini_GAM_NBP_HC,
                                      Opt_rGini_GAM_ZIPG_HC, Opt_rGini_GAM_ZIPIG_HC, Opt_rGini_GAM_ZIPP_HC,
                                      Opt_rGini_Lin_PG_HC, Opt_rGini_Lin_PIG_HC, Opt_rGini_Lin_PP_HC,
                                      Opt_rGini_Lin_NBG_HC, Opt_rGini_Lin_NBIG_HC, Opt_rGini_Lin_NBP_HC,
                                      Opt_rGini_Lin_ZIPG_HC, Opt_rGini_Lin_ZIPIG_HC, Opt_rGini_Lin_ZIPP_HC))
colnames(Opt_Scores_HC) <- c('ell_0', 's', 'Psi')
Opt_Scores_HC$ell_0 <- as.numeric(as.character(Opt_Scores_HC$ell_0)) + 1
Opt_Scores_HC$s <- as.numeric(as.character(Opt_Scores_HC$s)) + 2
Opt_Scores_HC$Psi <- as.numeric(as.character(Opt_Scores_HC$Psi)) + 0

### Claim score parameters - H
# (ell_0 x s x Psi):
Opt_rGini_GAM_PG_H <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 1, 1, 1, , 3], Exp_Buckets_rGini[, , , 3])    
# (ell_0 x s x Psi):
Opt_rGini_GAM_PIG_H <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 1, 1, 2, , 3], Exp_Buckets_rGini[, , , 3])    
# (ell_0 x s x Psi):
Opt_rGini_GAM_PP_H <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 1, 1, 3, , 3], Exp_Buckets_rGini[, , , 3])   
# (ell_0 x s x Psi):
Opt_rGini_GAM_NBG_H <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 1, 2, 1, , 3], Exp_Buckets_rGini[, , , 3])   
# (ell_0 x s x Psi):
Opt_rGini_GAM_NBIG_H <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 1, 2, 2, , 3], Exp_Buckets_rGini[, , , 3])   
# (ell_0 x s x Psi):
Opt_rGini_GAM_NBP_H <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 1, 2, 3, , 3], Exp_Buckets_rGini[, , , 3])   
# (ell_0 x s x Psi):
Opt_rGini_GAM_ZIPG_H <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 1, 3, 1, , 3], Exp_Buckets_rGini[, , , 3])   
# (ell_0 x s x Psi):
Opt_rGini_GAM_ZIPIG_H <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 1, 3, 2, , 3], Exp_Buckets_rGini[, , , 3])   
# (ell_0 x s x Psi):
Opt_rGini_GAM_ZIPP_H <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 1, 3, 3, , 3], Exp_Buckets_rGini[, , , 3])  
# (ell_0 x s x Psi):
Opt_rGini_Lin_PG_H <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 2, 1, 1, , 3], Exp_Buckets_rGini[, , , 3])    
# (ell_0 x s x Psi):
Opt_rGini_Lin_PIG_H <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 2, 1, 2, , 3], Exp_Buckets_rGini[, , , 3])    
# (ell_0 x s x Psi):
Opt_rGini_Lin_PP_H <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 2, 1, 3, , 3], Exp_Buckets_rGini[, , , 3])   
# (ell_0 x s x Psi):
Opt_rGini_Lin_NBG_H <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 2, 2, 1, , 3], Exp_Buckets_rGini[, , , 3])   
# (ell_0 x s x Psi):
Opt_rGini_Lin_NBIG_H <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 2, 2, 2, , 3], Exp_Buckets_rGini[, , , 3])   
# (ell_0 x s x Psi):
Opt_rGini_Lin_NBP_H <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 2, 2, 3, , 3], Exp_Buckets_rGini[, , , 3])   
# (ell_0 x s x Psi):
Opt_rGini_Lin_ZIPG_H <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 2, 3, 1, , 3], Exp_Buckets_rGini[, , , 3])   
# (ell_0 x s x Psi):
Opt_rGini_Lin_ZIPIG_H <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 2, 3, 2, , 3], Exp_Buckets_rGini[, , , 3])   
# (ell_0 x s x Psi):
Opt_rGini_Lin_ZIPP_H <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 2, 3, 3, , 3], Exp_Buckets_rGini[, , , 3])  
# (18) x (ell_0 x s x Psi)
Opt_Scores_H <- as.data.frame(rbind(Opt_rGini_GAM_PG_H, Opt_rGini_GAM_PIG_H, Opt_rGini_GAM_PP_H,
                                      Opt_rGini_GAM_NBG_H, Opt_rGini_GAM_NBIG_H, Opt_rGini_GAM_NBP_H,
                                      Opt_rGini_GAM_ZIPG_H, Opt_rGini_GAM_ZIPIG_H, Opt_rGini_GAM_ZIPP_H,
                                      Opt_rGini_Lin_PG_H, Opt_rGini_Lin_PIG_H, Opt_rGini_Lin_PP_H,
                                      Opt_rGini_Lin_NBG_H, Opt_rGini_Lin_NBIG_H, Opt_rGini_Lin_NBP_H,
                                      Opt_rGini_Lin_ZIPG_H, Opt_rGini_Lin_ZIPIG_H, Opt_rGini_Lin_ZIPP_H))
colnames(Opt_Scores_H) <- c('ell_0', 's', 'Psi')
Opt_Scores_H$ell_0 <- as.numeric(as.character(Opt_Scores_H$ell_0)) + 1
Opt_Scores_H$s <- as.numeric(as.character(Opt_Scores_H$s)) + 2
Opt_Scores_H$Psi <- as.numeric(as.character(Opt_Scores_H$Psi)) + 0

### Claim score parameters - T
# (ell_0 x s x Psi):
Opt_rGini_GAM_PG_T <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 1, 1, 1, , 4], Exp_Buckets_rGini[, , , 4])    
# (ell_0 x s x Psi):
Opt_rGini_GAM_PIG_T <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 1, 1, 2, , 4], Exp_Buckets_rGini[, , , 4])    
# (ell_0 x s x Psi):
Opt_rGini_GAM_PP_T <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 1, 1, 3, , 4], Exp_Buckets_rGini[, , , 4])   
# (ell_0 x s x Psi):
Opt_rGini_GAM_NBG_T <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 1, 2, 1, , 4], Exp_Buckets_rGini[, , , 4])   
# (ell_0 x s x Psi):
Opt_rGini_GAM_NBIG_T <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 1, 2, 2, , 4], Exp_Buckets_rGini[, , , 4])   
# (ell_0 x s x Psi):
Opt_rGini_GAM_NBP_T <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 1, 2, 3, , 4], Exp_Buckets_rGini[, , , 4])   
# (ell_0 x s x Psi):
Opt_rGini_GAM_ZIPG_T <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 1, 3, 1, , 4], Exp_Buckets_rGini[, , , 4])   
# (ell_0 x s x Psi):
Opt_rGini_GAM_ZIPIG_T <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 1, 3, 2, , 4], Exp_Buckets_rGini[, , , 4])   
# (ell_0 x s x Psi):
Opt_rGini_GAM_ZIPP_T <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 1, 3, 3, , 4], Exp_Buckets_rGini[, , , 4])  
# (ell_0 x s x Psi):
Opt_rGini_Lin_PG_T <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 2, 1, 1, , 4], Exp_Buckets_rGini[, , , 4])    
# (ell_0 x s x Psi):
Opt_rGini_Lin_PIG_T <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 2, 1, 2, , 4], Exp_Buckets_rGini[, , , 4])    
# (ell_0 x s x Psi):
Opt_rGini_Lin_PP_T <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 2, 1, 3, , 4], Exp_Buckets_rGini[, , , 4])   
# (ell_0 x s x Psi):
Opt_rGini_Lin_NBG_T <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 2, 2, 1, , 4], Exp_Buckets_rGini[, , , 4])   
# (ell_0 x s x Psi):
Opt_rGini_Lin_NBIG_T <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 2, 2, 2, , 4], Exp_Buckets_rGini[, , , 4])   
# (ell_0 x s x Psi):
Opt_rGini_Lin_NBP_T <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 2, 2, 3, , 4], Exp_Buckets_rGini[, , , 4])   
# (ell_0 x s x Psi):
Opt_rGini_Lin_ZIPG_T <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 2, 3, 1, , 4], Exp_Buckets_rGini[, , , 4])   
# (ell_0 x s x Psi):
Opt_rGini_Lin_ZIPIG_T <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 2, 3, 2, , 4], Exp_Buckets_rGini[, , , 4])   
# (ell_0 x s x Psi):
Opt_rGini_Lin_ZIPP_T <- Diagnostic_Adj(rGini_Out_samp_Multi[, , , 2, 3, 3, , 4], Exp_Buckets_rGini[, , , 4]) 
# (18) x (ell_0 x s x Psi)
Opt_Scores_T <- as.data.frame(rbind(Opt_rGini_GAM_PG_T, Opt_rGini_GAM_PIG_T, Opt_rGini_GAM_PP_T,
                                       Opt_rGini_GAM_NBG_T, Opt_rGini_GAM_NBIG_T, Opt_rGini_GAM_NBP_T,
                                       Opt_rGini_GAM_ZIPG_T, Opt_rGini_GAM_ZIPIG_T, Opt_rGini_GAM_ZIPP_T,
                                       Opt_rGini_Lin_PG_T, Opt_rGini_Lin_PIG_T, Opt_rGini_Lin_PP_T,
                                       Opt_rGini_Lin_NBG_T, Opt_rGini_Lin_NBIG_T, Opt_rGini_Lin_NBP_T,
                                       Opt_rGini_Lin_ZIPG_T, Opt_rGini_Lin_ZIPIG_T, Opt_rGini_Lin_ZIPP_T))
colnames(Opt_Scores_T) <- c('ell_0', 's', 'Psi')
Opt_Scores_T$ell_0 <- as.numeric(as.character(Opt_Scores_T$ell_0)) + 1
Opt_Scores_T$s <- as.numeric(as.character(Opt_Scores_T$s)) + 2
Opt_Scores_T$Psi <- as.numeric(as.character(Opt_Scores_T$Psi)) + 0

### Save output again
