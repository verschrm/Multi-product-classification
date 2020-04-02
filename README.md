# Predictive Claim Scores for Dynamic Multi-Product Risk Classification in Insurance
The aim of this paper is to explore how to best incorporate <i>a posteriori</i> claims experience of individual customers over several product lines in non-life insurance. The framework introduced considers a dynamic claim score for each product category and is applied to a Dutch property and casualty insurance portfolio with seven years of data on four different product lines. My findings indicate that the claims experience from a single product category already substantially improves a standard Generalized Linear Model. More importantly, I identify conditions under which it is even optimal or most profitable to account for a customer's claims experience from all product lines.

The multi-product framework introduced in this paper is applied to a confidential property and casualty insurance portfolio from a large Dutch insurer. More specifically, it contains policies on general liability, home contents, home and travel insurance in the period of 2012 up to and including 2018. This portfolio additionally contains the individual claim counts and severities, if any, from the period of 2005 up to and including 2011 for customers that were already a customer at the insurer in 2012.

The relevant R code for estimating all the different model specifications of the multi-product framework is provided in the Zip file `R_Code_Paper_Verschuren_R_M_2020.zip`. To begin with, custom R functions are provided in the folder `Custom_R_functions` that allow us to calculate the claim scores at a much faster rate for the grid searches. The grid searches themselves, as well as estimation of the standard Generalized Linear Models, are contained in the folder `Single_product` for every product line, denoted by X = GL for general liability insurance, by X = HC for home contents insurance, by X = H for home insurance and by X = T for travel insurance in the corresponding files `Grid_search_X.R`. These grid searches are combined under an exposure restriction in the same folder to determine the optimal claim score parameters, in the file `Optimal_claim_scores.R`. The output of these optimal single-product claim score models are produced with the files `Output_univariate_X.R` for X = {GL, HC, H, T}. The multi-product analogues of these models, given the optimal claim score parameters, are provided in the folder `Multi_product`, in the files `Output_multivariate_X.R` for X = {GL, HC, H, T}. Finally, the folder `Piecewise_linear` contains the piecewise linear simplifications of the cubic multi-product models for each product line, denoted by the files `Output_Piecewise_linear_X.R` for X = {GL, HC, H, T}. For more details on the theoretical and empirical results, we refer the reader to the corresponding paper.