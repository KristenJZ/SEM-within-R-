library(lavaan)


## Full Longitudinal Mediation ##

fullm <- '

  #############################
  # Autoregressive (Lag-1) Paths
  #############################

  c2 ~ c1
  c3 ~ c2

  n2 ~ n1
  n3 ~ n2

  dysfunc2 ~ dysfunc1
  dysfunc3 ~ dysfunc2

  #############################
  # Within-Wave Residual Covariances
  #############################

  # Wave 1 residual correlations
  c1 ~~ n1 + dysfunc1
  n1 ~~ dysfunc1

  # Wave 2 residual correlations
  c2 ~~ n2 + dysfunc2
  n2 ~~ dysfunc2

  # Wave 3 residual correlations
  c3 ~~ n3 + dysfunc3
  n3 ~~ dysfunc3

  #############################
  # Mediation Paths with Labels
  #############################

  # Paths from c to n
  n2 ~ a1*c1
  n3 ~ a2*c2

  # Paths from n to dysfunction
  dysfunc2 ~ b1*n1
  dysfunc3 ~ b2*n2

  # Direct effect from c1 to dysfunction
  dysfunc3 ~ c*c1

  #############################
  # Indirect Effect
  #############################

  # This defines the indirect effect from c1 to dysfunc3
  dysfunc3 := a1*b2 + c  # total effect (indirect via n2 + direct)

  # If you want just the indirect portion:
  ind_c1_dysfunc3 := a1*b2

'

fullm.fit <- sem(fullm, data=fivewaves, meanstructure=TRUE, missing = "ML")
summary(fullm.fit, fit.measures=TRUE)




## Half Longitudinal Mediation ##

halfm <- '

  #############################
  # Autoregressive Paths
  #############################

  n2 ~ n1
  dysfunc2 ~ dysfunc1

  #############################
  # Within-Wave Residual Covariances
  #############################

  # Wave 1 residual correlations
  c1 ~~ n1 + dysfunc1
  n1 ~~ dysfunc1

  # Wave 2 residual correlation
  n2 ~~ dysfunc2

  #############################
  # Mediation Paths with Labels
  #############################

  # Mediator paths
  n2 ~ a1*c1
  dysfunc2 ~ b1*n1

  #############################
  # Indirect Effect Constraint
  #############################

  ab := a1 * b1  # Indirect effect from c1 to dysfunc2 via n1

'

halfm.fit <- sem(halfm, data=fivewaves, meanstructure=TRUE, missing = "ML")
summary(halfm.fit, fit.measures=TRUE)





## Change Predicting Change Model with LDS ##

c2c <-'

#############################
# Latent Difference Scores: Neuroticism
#############################

# Step 1: Wave 3 regressed on wave 2 (fixed to 1)
n3 ~ 1*n2

# Step 2: Latent difference score at wave 3 (nd2)
nd2 =~ 1*n3
n3 ~~ 0*n3
n3 ~0*1
nd2 ~ 1 
nd2 ~ n2  # latent change regressed on prior level

# Step 3: Wave 2 regressed on wave 1 (fixed to 1)
n2 ~ 1*n1

# Step 4: Latent difference score at wave 2 (nd1)
nd1 =~ 1*n2
n2 ~~ 0*n2
n2 ~0*1
nd1 ~ 1 
nd1 ~ n1  # latent change regressed on prior level

#############################
# Latent Difference Scores: Dysfunction
#############################

# Step 1: Wave 3 regressed on wave 2 (fixed to 1)
dysfunc3 ~ 1*dysfunc2

# Step 2: Latent difference score at wave 3 (dysd2)
dysd2 =~ 1*dysfunc3
dysfunc3 ~~ 0*dysfunc3
dysfunc3 ~0*1
dysd2 ~ 1 
dysd2 ~ dysfunc2

# Step 3: Wave 2 regressed on wave 1 (fixed to 1)
dysfunc2 ~ 1*dysfunc1

# Step 4: Latent difference score at wave 2 (dysd1)
dysd1 =~ 1*dysfunc2
dysfunc2 ~~ 0*dysfunc2
dysfunc2 ~0
dysd1 ~ 1 
dysd1 ~ dysfunc1

#############################
# Cross-lagged Regressions (Level-to-Change)
#############################

nd2 ~ dysfunc2
nd1 ~ dysfunc1
dysd2 ~ n2
dysd1 ~ n1

#############################
# Correlations of Latent Changes
#############################

dysd1 ~~ nd1
dysd2 ~~ nd2

#############################
# Change-to-Change Regressions
#############################

dysd2 ~ dysd1 + nd1
nd2 ~ nd1 + dysd1
'


c2c.fit <- sem(c2c, data=fivewaves, meanstructure=TRUE, missing = "ML")
summary(c2c.fit, fit.measures=TRUE)





## Multiple Indicator LGM ##

milgm <- '

  #############################
  # Measurement Model with Invariance
  #############################

  # Latent factors
  f1 =~ y11 + c(l1)*y21 + c(l2)*y31
  f2 =~ y12 + c(l1)*y22 + c(l2)*y32
  f3 =~ y13 + c(l1)*y23 + c(l2)*y33

  # Intercepts (labeled and constrained equal across time)
  y11 ~ c(i1)*1
  y12 ~ c(i1)*1
  y13 ~ c(i1)*1

  y21 ~ c(i2)*1
  y22 ~ c(i2)*1
  y23 ~ c(i2)*1

  y31 ~ c(i3)*1
  y32 ~ c(i3)*1
  y33 ~ c(i3)*1

  #############################
  # Latent Growth Model
  #############################

  i =~ 1*f1 + 1*f2 + 1*f3
  s =~ 0*f1 + 1*f2 + 2*f3

  # Fix factor means to zero for identification
  f1 ~ 0*1
  f2 ~ 0*1
  f3 ~ 0*1

  # Intercept growth factor mean fixed, slope mean free
  i ~ 0*1
  s ~ 1

  # Residual variances for latent factors
  f1 ~~ f1
  f2 ~~ f2
  f3 ~~ f3

  # Covariance between growth factors
  i ~~ s
'

milgm.fit <- sem(milgm, data=multigrowth, meanstructure=TRUE, missing = "ML")
summary(milgm.fit, fit.measures=TRUE)
