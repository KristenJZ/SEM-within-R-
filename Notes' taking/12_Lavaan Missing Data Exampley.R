## FIML Example ##

# Specify a CFA model with two latent factors: internalizing and externalizing
miss <- '
int =~ NA*dys + mdd + gad     # Define internalizing factor with fixed scaling on dys
ext =~ NA*alc + anti + sub    # Define externalizing factor with fixed scaling on alc

int ~~ 1*int                  # Fix variance of int to 1 for identification
ext ~~ 1*ext                  # Fix variance of ext to 1 for identification
'

# Fit the model WITHOUT specifying a missing data method (default behavior)
miss.fit <- sem(miss, data = missex, meanstructure = TRUE)  # Will use listwise deletion by default
summary(miss.fit, fit.measures = TRUE, standardized = TRUE) # Show model summary with fit indices and standardized estimates


# Now fit the same model using Full Information Maximum Likelihood (FIML)
fiml <- '
int =~ NA*dys + mdd + gad     # Same internalizing factor specification
ext =~ NA*alc + anti + sub    # Same externalizing factor specification

int ~~ 1*int                  # Fix variance of int to 1
ext ~~ 1*ext                  # Fix variance of ext to 1
'

fiml.fit <- sem(fiml, data = missex, meanstructure = TRUE, missing = "ML")  # Use FIML to handle missing data
summary(fiml.fit, fit.measures = TRUE, standardized = TRUE)  # Model summary with FIML estimation


fiml.fit<-sem(fiml, data=missex, meanstructure=TRUE, missing = "ML")
summary(fiml.fit,fit.measures=TRUE,standardized=TRUE)




