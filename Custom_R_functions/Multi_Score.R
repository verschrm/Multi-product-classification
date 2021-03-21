##### Credentials #####

### Author:       R.M. (Robert) Verschuren
### Institution:  Amsterdam School of Economics, 
###               University of Amsterdam
### Date:         26/08/2020

##### Description #####

### This function creates the multi claim score
### for the multi-product claim score model.
### The function accepts the following arguments:

# Input_Score_A = The 4-dimensional array from Input_Score() for product A;
# Data_A        = The dataset containing the policies for product A;
# Data_B        = The dataset containing the policies for product B;
# l_0_B         = The initial level of the claim score for product B.

# Note that Data_A and Data_B must contain the columns Customer with a
# customer index, Count and dt_Start. In addition, dt_Start 
# must be in the format of 'YYYY/MM/DD' and Data_B must also contain
# the column Score_Main that results from the function Score().

##### Function Multi_Score() #####

Multi_Score <- function(Input_Score_A, Data_A, Data_B, l_0_B) {
  ### Preparations
  # Determine which customers we exactly observe for the main product category A:
  Cust_A <- as.character(unique(Data_A$Customer))
  # Determine which customers we exactly observe for both the main product category A 
  # and the additional product category B:
  Cust_A_B <- intersect(Cust_A, unique(Data_B$Customer))
  # Select the relevant data from the additional product category B for the selected customers:
  Data_B <- Data_B[which(Data_B$Customer %in% Cust_A_B), 
                   c('Customer', 'dt_Start', 'Score_Main')]
  # Determine how many periods we observe for each customer:
  Periods <- apply(Input_Score_A[, , 3], 1, function(x) sum(!is.na(x)))
  # Store the claim scores and dt_Start for every selected customer from the 
  # additional product category B:
  N_Cust <- as.matrix(1:nrow(Input_Score_A))
  Scores_B <- apply(N_Cust, 1, function(x) Data_B$Score_Main[which(Data_B$Customer == Cust_A[x])])
  Dates_B <- apply(N_Cust, 1, function(x) as.numeric(as.Date(Data_B$dt_Start[which(Data_B$Customer == Cust_A[x])])))
  
  ### Construct multi claim score
  # Now loop through the number of periods for all selected customers at once 
  # by exploiting the matrix setup:
  for (Prd in 1:max(Periods)) {
    Input_Score_A[, Prd, 1] <- unlist(sapply(1:nrow(Input_Score_A), function(x) max(tail(Scores_B[[x]][which(Dates_B[[x]] <= 
                                      Input_Score_A[x, Prd, 4])], 1), 0)))
  }
  # Manually set the claim scores for customers without any claims experience 
  # on the additional product category B:
  Input_Score_A[which(Input_Score_A[, , 1] == 0)] <- l_0_B
  # Transform the claim scores back into the original format of a column-vector:
  Multi_Score <- unlist(sapply(1:nrow(Input_Score_A), function(x) Input_Score_A[x, 1:Periods[x], 1]))
  
  ### Store output
  Output <- Multi_Score
  
  ### Return output
  return(Output)
}
