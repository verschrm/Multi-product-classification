##### Credentials #####

### Author:       R.M. (Robert) Verschuren
### Institution:  Amsterdam School of Economics, 
###               University of Amsterdam
### Date:         26/08/2020

##### Description #####

### This function creates the claim score for the
### multi-product claim score model.
### The function accepts the following arguments:

# Psi           = The jump parameter of the claim score;
# s             = The maximum level of the claim score;
# l_0           = The initial level of the claim score;
# L_Min         = The minimum level of the claim score;
# L_Bonus       = The reward for a claim-free period;
# Input_Score   = The 4-dimensional array from Input_Score().

##### Function Score() #####

Score <- function(Psi, s, l_0, L_Min = 1, L_Bonus = 1, Input_Score) {
  ### Preparations
  # Determine how many periods we observe for each customer:
  Periods <- apply(Input_Score[, , 3], 1, function(x) sum(!is.na(x)))
  
  ### Construct claim score
  # Initialize for customers without any claims experience:
  Input_Score[, 1, 1] <- l_0
  # Now loop through the number of periods for all customers at once 
  # by exploiting the matrix setup:
  for (Prd in 2:max(Periods)) {
    Input_Score[, Prd, 1] <- pmax(pmin(Input_Score[, Prd - 1, 1] + 
                                       Input_Score[, Prd, 2] * L_Bonus * (Input_Score[, Prd - 1, 3] == 0) -
                                       Psi * Input_Score[, Prd - 1, 3] / Input_Score[, Prd, 2], s), L_Min)
  }
  # Transform the claim scores back into the original format of a column-vector:
  Score_Main <- unlist(sapply(1:nrow(Input_Score), function(x) Input_Score[x, 1:Periods[x], 1]))
  
  ### Store output
  Output <- Score_Main
  
  ### Return output
  return(Output)
}
