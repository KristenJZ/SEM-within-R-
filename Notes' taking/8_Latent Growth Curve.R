library(lavaan)

## BASIC LINEAR GROWTH MODEL (LGM) EXAMPLE ##
## The goal of LGM is to explain the relationship between a single indicator at multiple time points. 

# Define the model specification as a string
e_lgm1 <-'
# Define latent intercept factor where all factor loadings are fixed to 1
# This represents the initial status/starting point for each individual
# The "1*" syntax fixes the loading to 1 (all time points contribute equally)
i_e =~ 1*e1 + 1*e2 + 1*e3 + 1*e4 + 1*e5

# Define latent slope factor with time-coded loadings (0 to 4)
# These loadings represent linear time progression (0, 1, 2, 3, 4)
# The slope captures the rate of change over time
# "0*" at e1 sets the starting point, increments of 1 represent equal time intervals
s_e =~ 0*e1 + 1*e2 + 2*e3 + 3*e4 + 4*e5

# Estimate variances for intercept and slope
# "~~" notation specifies variance/covariance
# These estimate individual differences in starting points (i_e) and rates of change (s_e)
i_e ~~ i_e
s_e ~~ s_e

# Estimate covariance between intercept and slope
# This tests whether initial status is related to rate of change
# Negative covariance = higher starters grow slower; positive = higher starters grow faster
s_e ~~ i_e

# Estimate means for intercept and slope factors
# "~ 1" estimates the mean (intercept in regression terms) - normative change, i.e. means of intercept and slope
# Mean of i_e = average starting point across all individuals
# Mean of s_e = average rate of change across all individuals
i_e ~ 1
s_e ~ 1

# Fix means of observed indicators to 0
# "~0*1" fixes the intercept to 0
# This is necessary because growth is captured by latent factors
# If we estimated these, the model would be unidentified (redundant parameters)
e1 ~0*1
e2 ~0*1
e3 ~0*1
e4 ~0*1
e5 ~0*1
'

# Fit the basic linear growth model
# sem() fits structural equation models
# data=fivewaves: specifies the dataset
# meanstructure=TRUE: estimates means of latent variables (required for growth models)
# missing="ML": uses maximum likelihood for missing data (FIML estimation)
e_lgm1.fit <- sem(e_lgm1, data=fivewaves, meanstructure=TRUE, missing = "ML")

# Display comprehensive model results
# fit.measures=TRUE: includes fit indices (CFI, TLI, RMSEA, etc.)
# standardized=TRUE: provides standardized estimates alongside unstandardized
summary(e_lgm1.fit, fit.measures=TRUE, standardized=TRUE)

# Check for possible model improvements
# Modification indices suggest parameters that, if freed, would improve fit
# High MI values (>3.84 typically) suggest constraint may be problematic
modificationIndices(e_lgm1.fit)

# Inspect residuals to identify areas of poor fit
# type="standardized": standardized residuals (values >|1.96| suggest misfit)
# Large residuals indicate the model doesn't reproduce observed covariances well
residuals(e_lgm1.fit, type="standardized")


## QUADRATIC (CURVILINEAR) GROWTH MODEL EXAMPLE ##

e_lgm2 <-'
# Intercept and slope as before (linear component)
i_e =~ 1*e1 + 1*e2 + 1*e3 + 1*e4 + 1*e5
s_e =~ 0*e1 + 1*e2 + 2*e3 + 3*e4 + 4*e5

# Add quadratic growth factor with squared time loadings
# Loadings are squares of time points: 0², 1², 2², 3², 4² = 0, 1, 4, 9, 16
# This captures acceleration/deceleration in the growth trajectory (quadratic slope is acceleration; linear slope is rate of change at intercept)
# Positive mean = upward curve (accelerating growth); negative = downward curve (decelerating)
q_e =~ 0*e1 + 1*e2 + 4*e3 + 9*e4 + 16*e5

# Estimate variances for each latent factor
# Now we have three sources of individual differences:
# - Starting point (i_e), linear rate (s_e), and curvature (q_e)
i_e ~~ i_e
s_e ~~ s_e
q_e ~~ q_e

# Estimate all pairwise covariances among latent factors
# These show how intercept, slope, and quadratic components relate to each other
# Example: s_e ~~ q_e tests if linear growth rate relates to curvature
s_e ~~ i_e
s_e ~~ q_e
q_e ~~ i_e

# Estimate means for latent growth factors
# i_e mean = average starting point
# s_e mean = average linear rate of change
# q_e mean = average curvature (acceleration/deceleration)
i_e ~ 1
s_e ~ 1
q_e ~ 1

# Fix observed variable means to 0 (same rationale as linear model)
e1 ~0*1
e2 ~0*1
e3 ~0*1
e4 ~0*1
e5 ~0*1
'

# Fit the quadratic growth model
# Note: missing="ML" not specified here but could be added if needed
e_lgm2.fit <- sem(e_lgm2, data=fivewaves, meanstructure=TRUE)
summary(e_lgm2.fit, fit.measures=TRUE, standardized=TRUE)
modificationIndices(e_lgm2.fit)
residuals(e_lgm2.fit, type="standardized")

# Compare linear and quadratic models using likelihood ratio test
# Tests if quadratic term significantly improves model fit
# Significant p-value indicates quadratic model fits better (curvature exists)
# Also compares AIC and BIC (lower values = better fit)
anova(e_lgm1.fit, e_lgm2.fit)


## PIECEWISE (MULTI-PHASE) GROWTH MODEL EXAMPLE ##

e_lgm3 <-'
# Intercept factor as before (starting point)
i_e =~ 1*e1 + 1*e2 + 1*e3 + 1*e4 + 1*e5

# Define first slope with increasing loadings up to time 3 (change point)
# Loadings: 0, 1, 2, 2, 2 - growth stops changing after time point 3
# This captures rate of change in the FIRST phase (time 1-3)
s1_e =~ 0*e1 + 1*e2 + 2*e3 + 2*e4 + 2*e5

# Define second slope for later time period (change point at time 3)
# Loadings: 0, 0, 0, 1, 2 - growth only occurs after time point 3
# This captures rate of change in the SECOND phase (time 3-5)
# Useful for modeling intervention effects or developmental transitions
s2_e =~ 0*e1 + 0*e2 + 0*e3 + 1*e4 + 2*e5

# Estimate variances for all three latent factors
# Individual differences in: starting point, phase 1 slope, and phase 2 slope
i_e ~~ i_e
s1_e ~~ s1_e
s2_e ~~ s2_e

# Estimate all pairwise covariances
# Tests relationships between phases: do fast growers in phase 1 remain fast in phase 2?
s1_e ~~ i_e
s1_e ~~ s2_e
s2_e ~~ i_e

# Estimate means for all three factors
# s1_e mean = average growth rate in phase 1
# s2_e mean = average growth rate in phase 2
i_e ~ 1
s1_e ~ 1
s2_e ~ 1

# Fix means of indicators (same as before)
e1 ~0*1
e2 ~0*1
e3 ~0*1
e4 ~0*1
e5 ~0*1
'

# Fit piecewise linear growth model
e_lgm3.fit <- sem(e_lgm3, data=fivewaves, meanstructure=TRUE)
summary(e_lgm3.fit, fit.measures=TRUE, standardized=TRUE)
modificationIndices(e_lgm3.fit)
residuals(e_lgm3.fit, type="standardized")


## LATENT BASIS (FREELY ESTIMATED TIME SCORES) MODEL EXAMPLE ##

e_lgm4 <-'
# Intercept factor (same as all previous models)
i_e =~ 1*e1 + 1*e2 + 1*e3 + 1*e4 + 1*e5

# Latent slope factor with fixed first (0*) and last (1*) loadings
# Middle loadings (e2, e3, e4) are FREE to be estimated from the data
# This allows for non-linear growth without specifying the exact form
# The model determines the "shape" of time empirically
# Results show proportional distance between time points (not assumed equal)
ls_e =~ 0*e1 + e2 + e3 + e4 + 1*e5

# Estimate variances
# Individual differences in starting point and rate of change
i_e ~~ i_e
ls_e ~~ ls_e

# Estimate covariance between intercept and latent slope
ls_e ~~ i_e

# Estimate means
# ls_e mean represents average growth from time 1 to time 5
i_e ~ 1
ls_e ~ 1

# Fix means of indicators (growth captured by latent factors)
e1 ~0*1
e2 ~0*1
e3 ~0*1
e4 ~0*1
e5 ~0*1
'

# Fit latent basis growth model
# This model is more flexible than linear but more constrained than quadratic
# Useful when you don't want to assume equal intervals or specific polynomial form
e_lgm4.fit <- sem(e_lgm4, data=fivewaves, meanstructure=TRUE)
summary(e_lgm4.fit, fit.measures=TRUE, standardized=TRUE)
modificationIndices(e_lgm4.fit)
residuals(e_lgm4.fit, type="standardized")


## LGM WITH TIME-INVARIANT PREDICTOR (BETWEEN-PERSON COVARIATE) ##

# Add time-invariant covariate (sex) as predictor of growth parameters
e_lgm5 <-'
# Latent factors: intercept, slope, quadratic (full quadratic model)
i_e =~ 1*e1 + 1*e2 + 1*e3 + 1*e4 + 1*e5
s_e =~ 0*e1 + 1*e2 + 2*e3 + 3*e4 + 4*e5
q_e =~ 0*e1 + 1*e2 + 4*e3 + 9*e4 + 16*e5

# Variances and covariances (same as quadratic model above)
i_e ~~ i_e
s_e ~~ s_e
q_e ~~ q_e

s_e ~~ i_e
s_e ~~ q_e
q_e ~~ i_e

# Means (these now represent intercepts controlling for sex)
i_e ~ 1
s_e ~ 1
q_e ~ 1

# Fix indicator means
e1 ~0*1
e2 ~0*1
e3 ~0*1
e4 ~0*1
e5 ~0*1

# Regress latent factors on sex (time-invariant covariate)
# "~" indicates regression (factor predicted by covariate)
# i_e ~ sex: Does sex predict starting point?
# s_e ~ sex: Does sex predict linear rate of change?
# q_e ~ sex: Does sex predict curvature?
# Sex is constant across time (between-person variable)
i_e ~ sex
s_e ~ sex
q_e ~ sex
'

# Fit model with between-person predictor
# This tests whether groups (e.g., males vs females) differ in growth trajectories
e_lgm5.fit <- sem(e_lgm5, data=fivewaves, meanstructure=TRUE)
summary(e_lgm5.fit, fit.measures=TRUE, standardized=TRUE)
modificationIndices(e_lgm5.fit)
residuals(e_lgm5.fit, type="standardized")


## LGM WITH TIME-VARYING COVARIATES (WITHIN-PERSON PREDICTORS) ##

# Add time-varying covariates that change across measurement occasions
e_lgm6 <-'
# Latent intercept, slope, and quadratic factors (same structure)
i_e =~ 1*e1 + 1*e2 + 1*e3 + 1*e4 + 1*e5
s_e =~ 0*e1 + 1*e2 + 2*e3 + 3*e4 + 4*e5
q_e =~ 0*e1 + 1*e2 + 4*e3 + 9*e4 + 16*e5

# Variances and covariances (unchanged)
i_e ~~ i_e
s_e ~~ s_e
q_e ~~ q_e

s_e ~~ i_e
s_e ~~ q_e
q_e ~~ i_e

# Means
i_e ~ 1
s_e ~ 1
q_e ~ 1

# Fix observed means
e1 ~0*1
e2 ~0*1
e3 ~0*1
e4 ~0*1
e5 ~0*1

# Regress each observed score on time-specific predictors (time-varying covariates)
# Each time point has its own predictor measured at that same time
# e1 ~ dysfunc1: Does dysfunction at time 1 predict outcome at time 1?
# This controls for within-person effects that vary over time
# These are CONCURRENT predictors (measured at same time as outcome)
# Useful for testing if fluctuations in predictor relate to fluctuations in outcome
e1 ~ dysfunc1
e2 ~ dysfunc2
e3 ~ dysfunc3
e4 ~ dysfunc4
e5 ~ dysfunc5
'

# Fit model with time-varying covariates
# This tests whether within-person changes in predictor relate to outcome
# After controlling for these, growth factors represent "pure" developmental change
e_lgm6.fit <- sem(e_lgm6, data=fivewaves, meanstructure=TRUE)
summary(e_lgm6.fit, fit.measures=TRUE, standardized=TRUE)
modificationIndices(e_lgm6.fit)
residuals(e_lgm6.fit, type="standardized")