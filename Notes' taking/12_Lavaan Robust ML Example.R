library(lavaan)   # Load lavaan for SEM modeling
library(psych)    # Load psych for descriptive statistics

## Using Robust ML (MLR) ##

describe(cfamean)  # View descriptive statistics for variables in the 'cfamean' dataset

# Define CFA model with two latent variables (internalizing and externalizing)
ie <- '
int =~ NA*mdd + dys + gad        # Internalizing factor, fixing loading of mdd to NA for identification
ext =~ NA*anti + alc + sub       # Externalizing factor, fixing loading of anti to NA

int ~~ 1*int                     # Fix variance of int to 1 (latent variable identification)
ext ~~ 1*ext                     # Fix variance of ext to 1

mdd ~~ mdd                       # Estimate residual variance of mdd
dys ~~ dys                       # Estimate residual variance of dys
gad ~~ gad                       # Estimate residual variance of gad
anti ~~ anti                     # Estimate residual variance of anti
alc ~~ alc                       # Estimate residual variance of alc
sub ~~ sub                       # Estimate residual variance of sub

int ~~ ext                       # Estimate covariance between latent factors

int ~ 0*1                        # Fix latent mean of int to 0 (default, made explicit)
ext ~ 0*1                        # Fix latent mean of ext to 0

mdd ~ 1                          # Estimate observed variable intercepts
dys ~ 1
gad ~ 1
anti ~ 1
alc ~ 1
sub ~ 1
'

# Fit the CFA model using robust ML estimator (MLR = maximum likelihood with robust standard errors and scaled test statistics)
ie.fit <- sem(ie, data = cfamean, meanstructure = TRUE, estimator = "MLR")

# Summarize the model: includes fit indices and standardized estimates
summary(ie.fit, fit.measures = TRUE, standardized = TRUE)

modificationIndices(ie.fit)             # Display modification indices (suggest potential model improvements)
residuals(ie.fit, type = "standardized")# Show standardized residuals (helpful for diagnosing misfit)
