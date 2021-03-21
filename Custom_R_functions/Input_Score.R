##### Credentials #####

### Author:       R.M. (Robert) Verschuren
### Institution:  Amsterdam School of Economics, 
###               University of Amsterdam
### Date:         26/08/2020

##### Description #####

### This function creates the input in matrix form
### for the constructing the claim score in the 
### multi-product claim score model.
### The function accepts the following arguments:

# Data     = The dataset containing the policies. 

# Note that Data must contain the columns Customer
# with a customer index, Count and dt_Start. In 
# addition, dt_Start must be in the format of 
# 'YYYY/MM/DD'.

##### Function Input_Score() #####

Input_Score <- function(Data) {
  ### Preparations
  # Determine how many periods we observe for each customer:
  Periods <- aggregate(1:nrow(Data) ~ Customer, data = Data, length)
  # Determine which customers we exactly observe:
  Cust <- unique(Data$Customer)
  # (Cust) x (Periods) x (Score_Main, Exposure, Count, dt_Start)
  Input_Score <- array(NA, dim = c(length(Cust), max(Periods[, 2]), 4))
  
  ### Store all relevant information in matrix form
  # A single matrix containing Score_Main, Exposure, Count and
  # dt_Start for each customer in every observed period
  for (C in Cust) {
    C_i <- which(Cust == C)
    C_Ind <- which(Data$Customer == C)
    Data_Hist <- Data$Count[C_Ind]
    Prd_Ind <- Periods[which(Periods$Customer == C), 2]
    Input_Score[C_i, 1:Prd_Ind, 2] <- Data$Exposure[C_Ind]
    Input_Score[C_i, 1:Prd_Ind, 3] <- Data$Count[C_Ind]
    Input_Score[C_i, 1:Prd_Ind, 4] <- as.numeric(as.Date(Data$dt_Start[C_Ind]))
  }
  
  ### Store output
  Output <- Input_Score
  
  ### Return output
  return(Output)
}
