

## CATEGORICAL VARIABLE EXAMPLE

library(lavaan)  # Load the lavaan package for SEM analysis

#Example 1 - Factor Model

# Declare selected variables in the 'categorical' dataset as ordered factors (ordinal variables)
categorical[, c("sui", "mdd", "dys", "soc", "ptsd", "alc", "sub", "anti")] <-
  lapply(categorical[, c("sui", "mdd", "dys", "soc", "ptsd", "alc", "sub", "anti")], ordered)

# Define the CFA model using lavaan syntax
cat.ie <- '
int =~ NA*mdd + dys + soc + ptsd       # Define latent variable "int" (internalizing) loading on 4 items
ext =~ NA*alc + sub + anti             # Define latent variable "ext" (externalizing) loading on 3 items
int ~~ 1*int                           # Fix variance of "int" to 1 for identification
ext ~~ 1*ext                           # Fix variance of "ext" to 1 for identification
'

# Fit the model using the sem() function
# Specify that all variables are ordered categorical and include a mean structure
cat.ie.fit <- sem(
  cat.ie,
  data = categorical,
  ordered = c("sui", "mdd", "dys", "soc", "ptsd", "alc", "sub", "anti"),
  meanstructure = TRUE,
  estimator = "WLSMV"  # Specify WLSMV estimator
)

# Display the summary of the fitted model
# Includes fit indices and standardized estimates
summary(cat.ie.fit, fit.measures = TRUE, standardized = TRUE)


#Example 2 - Full categorical SEM

# Define the SEM model
sui.sem <- '
int =~ NA*mdd + dys + soc + ptsd       # Define latent variable "int" (internalizing) loading on 4 items
ext =~ NA*alc + sub + anti             # Define latent variable "ext" (externalizing) loading on 3 items
int ~~ 1*int                           # Fix variance of "int" to 1 for identification
ext ~~ 1*ext                           # Fix variance of "ext" to 1 for identification

sui ~ int + ext                       # regress suicidality on each factor
'

# Fit the model using the sem() function
# Specify that all variables are ordered categorical and include a mean structure
sui.sem.fit <- sem(
  sui.sem,
  data = categorical,
  ordered = c("sui", "mdd", "dys", "soc", "ptsd", "alc", "sub", "anti"),
  meanstructure = TRUE,
  estimator = "WLSMV"  # Specify WLSMV estimator
)

# Display the summary of the fitted model
# Includes fit indices and standardized estimates
summary(sui.sem.fit, fit.measures = TRUE, standardized = TRUE)
