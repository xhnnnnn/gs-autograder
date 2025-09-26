# Author:  Jarad Niemi
# Date:    2025-09-20
# Purpose: HW05_TidyPart01 solutions
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# DNase solutions
DNase_n <- DNase |> nrow() # number of observations
DNase_p <- DNase |> ncol() # number of variables

DNase_names        <- DNase |> names()       # variable names

DNase_nlevels_Run  <- DNase$Run     |> nlevels() # levels of Run
DNase_min_conc     <- DNase$conc    |> min()     # minimum concentration
DNase_mean_density <- DNase$density |> mean()    # mean density
DNase_sd_density   <- DNase$density |> sd()      # sd of density

# ------------------------------------------------------------------------------
# women solutions
women_n <- women |> nrow() # number of observations
women_p <- women |> ncol() # number of variables

women_names        <- women |> names()       # variable names

women_median_height             <- women$height |> median() # median height
women_variance_weight           <- women$weight |> var()    # variance of weight

# correlation between height and weight
women_height_weight_correlation <- cor(women$height, women$weight)      



# ------------------------------------------------------------------------------
# HW05data solutions
HW05data_n <- HW05data |> nrow() # number of observations
HW05data_p <- HW05data |> ncol() # number of variables

HW05data_names        <- HW05data |> names()       # variable names

HW05data_var1_nlevels <- HW05data$var1 |> nlevels()
HW05data_var1_levels  <- HW05data$var1 |> levels()
HW05data_var2_mean    <- HW05data$var2 |> mean()
HW05data_var2_sd      <- HW05data$var2 |> sd()
