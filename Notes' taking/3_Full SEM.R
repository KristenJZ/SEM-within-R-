## FULL SEM EXAMPLE ##
library(lavaan)  # Load the lavaan package for latent variable analysis (SEM/CFA)

##STEP 1 - ESTIMATE CFA (Confirmatory Factor Analysis)

# Define the CFA model using lavaan syntax as a character string
cfa1 <- 'pos =~ pa_s + pa_x + pa_y    # Latent factor "pos" (positive affect) is measured by 3 indicators
                                       # =~ means "is measured by" (defines factor loadings)
neg =~ na_s + na_x + na_y              # Latent factor "neg" (negative affect) with 3 indicators
car =~ car1 + car2 + car3              # Latent factor "car" (awakening response) with 3 indicators
slope =~ slope1 + slope2 + slope3      # Latent factor "slope" with 3 indicators

# Factor covariances - allow latent variables to correlate with each other
pos ~~ neg                             # ~~ means covariance/correlation between factors
car ~~ slope                           # Covariance between car and slope factors
car ~~ pos                             # Covariance between car and pos factors
car ~~ neg                             # Covariance between car and neg factors
slope ~~ pos                           # Covariance between slope and pos factors
slope ~~ neg                           # Covariance between slope and neg factors

# Residual covariances - allow measurement errors to correlate
pa_s ~~ na_s                           # Correlated errors between positive and negative affect measured by self s
pa_x ~~ na_x                           # Correlated errors by rater x
pa_y ~~ na_y                           # Correlated errors by rater y
car1 ~~ slope1                         # Correlated errors between car and slope indicators at same time point
car2 ~~ slope2                         # Correlated errors at time 2
car3 ~~ slope3'                        # Correlated errors at time 3

# Estimate the CFA model
cfa.fit <- sem(cfa1,                   # The model specification defined above
               data=pncsem,            # Dataset containing the observed variables
               meanstructure=TRUE,     # Estimate means of observed and latent variables
               missing="ML")           # Handle missing data using Maximum Likelihood (FIML)

# Display comprehensive output for the CFA
summary(cfa.fit,                       # Summarize the fitted model
        fit.measures=TRUE,             # Include model fit indices (CFI, TLI, RMSEA, etc.)
        standardized=TRUE)             # Show standardized parameter estimates (betas, correlations)

modindices(cfa.fit)                    # Modification indices - suggest parameters to add to improve fit
# High MI values indicate potential model improvements

residuals(cfa.fit, type="standardized") # Standardized residuals - differences between observed and model-implied covariances
# Values > |2.58| suggest local areas of misfit


## STEP 2 - CHANGE SOME CORRELATIONS TO REGRESSIONS
# Convert the CFA to a full structural equation model (SEM)

fullsem <- 'pos =~ pa_s + pa_x + pa_y  # Same measurement model for positive affect
neg =~ na_s + na_x + na_y              # Same measurement model for negative affect
car =~ car1 + car2 + car3              # Same measurement model for car
slope =~ slope1 + slope2 + slope3      # Same measurement model for slope

pos ~~ neg                             # Pos and neg still allowed to covary (exogenous variables)
car ~~ slope                           # Car and slope residuals allowed to covary (endogenous variables)

# KEY CHANGE: Convert correlations to directional regression paths
car ~ pos + neg                        # ~ means regression: car is predicted BY pos and neg
                                       # Tests if positive and negative affect predict car outcomes
slope ~ pos + neg                      # Slope is predicted BY pos and neg
                                       # These are now structural (causal) paths, not just correlations

# Same residual covariances as before
pa_s ~~ na_s                           # Correlated measurement errors across constructs
pa_x ~~ na_x                           
pa_y ~~ na_y                           
car1 ~~ slope1                         # Correlated measurement errors for outcome variables
car2 ~~ slope2                         
car3 ~~ slope3'                        

# Estimate the full SEM
full.fit <- sem(fullsem,               # The structural model specification
                data=pncsem,           # Same dataset
                meanstructure=TRUE,    # Estimate means/intercepts
                missing="ML")          # Maximum Likelihood for missing data

# Display comprehensive output for the SEM
summary(full.fit,                      # Summarize fitted structural model
        fit.measures=TRUE,             # Model fit statistics
        standardized=TRUE)             # Standardized regression coefficients and correlations

modindices(full.fit)                   # Modification indices for potential model improvements

residuals(full.fit, type="standardized") # Check for localized areas of strain in the model