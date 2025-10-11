library(lavaan)  # Load the lavaan package for latent variable analysis

## CFA WITH MEANS ##
# Define the measurement model as a character string
iemean <- '
  # FACTOR LOADINGS (measurement model)
  # Define latent factor "int" (internalizing disorders)
  # NA* allows the first loading to be freely estimated (not fixed to 1)
  int =~ NA*dys + mdd + gad
  
  # Define latent factor "ext" (externalizing disorders)
  # NA* allows the first loading to be freely estimated (not fixed to 1)
  ext =~ NA*alc + anti + sub
  
  # FACTOR VARIANCES
  # Fix the variance of "int" factor to 1 (standardization)
  # This is needed because were estimating all loadings freely
int ~~ 1*int

# Fix the variance of "ext" factor to 1 (standardization)
ext ~~ 1*ext

# RESIDUAL VARIANCES (measurement error)
# Estimate residual variance for each observed variable
# Double tilde (~) on same variable = variance/covariance
mdd ~~ mdd    # Residual variance for major depressive disorder
dys ~~ dys    # Residual variance for dysthymia
gad ~~ gad    # Residual variance for generalized anxiety disorder
anti ~~ anti  # Residual variance for antisocial behavior
alc ~~ alc    # Residual variance for alcohol use
sub ~~ sub    # Residual variance for substance use

# FACTOR COVARIANCE
# Estimate the covariance between internalizing and externalizing factors
int ~~ ext

# FACTOR INTERCEPTS (means)
# Fix latent factor means to 0 for identification
# Single tilde (~) with 1 on right side = intercept/mean
int ~ 0*1
ext ~ 0*1

# OBSERVED VARIABLE INTERCEPTS
# Fix intercepts to 1 (estimate the means of observed variables)
# This allows the model to account for mean structure
mdd ~ 1
dys ~ 1
gad ~ 1
anti ~ 1
alc ~ 1
sub ~ 1
'

# Estimate the model using sem() function
mean.fit <- sem(
  iemean,                    # Model specification
  data = cfamean,            # Dataset name
  meanstructure = TRUE       # Include means/intercepts in the model
)

# Display comprehensive model results
summary(
  mean.fit,
  fit.measures = TRUE,       # Include fit indices (CFI, TLI, RMSEA, etc.)
  standardized = TRUE        # Include standardized parameter estimates
)

# Request modification indices
# Shows potential model improvements (parameters that could be freed)
# Large MI values suggest adding parameters would improve fit
modificationIndices(mean.fit)

# Examine standardized residuals
# Large residuals indicate local areas of misfit
# Values > |2.58| suggest significant misfit
residuals(mean.fit, type = "standardized")


## MULTIGROUP MODEL ##
# Test whether the relationship between depression and alcohol use
# differs by sex (moderation analysis)

# BASELINE FREE MODEL
# Estimate parameters separately for each group (no constraints)
free <- 'alc ~ mdd'  # Regress alcohol use on major depressive disorder

free.fit <- sem(
  free,
  data = cfamean,
  meanstructure = TRUE,
  group = "sex"              # Separate parameter estimates for each sex
)

summary(free.fit, fit.measures = TRUE, standardized = TRUE)


# TESTING SEX EQUIVALENCE (Constrained Model)
# Test if the regression path is equal across sex groups
equi <- '
# c(b1, b1) constrains the path to be equal across both groups
# Same label "b1" forces equality constraint
alc ~ c(b1, b1)*mdd
'

equi.fit <- sem(
  equi,
  data = cfamean,
  meanstructure = TRUE,
  group = "sex"
)

summary(equi.fit, fit.measures = TRUE, standardized = TRUE)

# To test if sex moderates the relationship, compare models:
# anova(free.fit, equi.fit)
# Significant chi-square difference = paths differ by sex (moderation exists)
# Non-significant = paths are equivalent (no moderation)