library(lavaan)  # Load the lavaan package

## MULTIGROUP MODEL ##

# Fully free configural model (baseline model for testing measurement invariance)
multi.free <- '
int =~ mdd + dys + gad                 # Internalizing factor
ext =~ anti + alc + sub                # Externalizing factor

mdd ~~ mdd                             # Residual variances
dys ~~ dys
gad ~~ gad
anti ~~ anti
alc ~~ alc
sub ~~ sub

int ~~ ext                             # Covariance between factors

int ~ 0*1                              # Fix latent means to 0 (by default for configural model)
ext ~ 0*1

mdd ~ 1                                # Estimate intercepts
dys ~ 1
gad ~ 1
anti ~ 1
alc ~ 1
sub ~ 1
'

free.fit <- sem(multi.free, data = cfamean, meanstructure = TRUE, group = "sex")  # Fit multigroup configural model
summary(free.fit, fit.measures = TRUE)
modificationIndices(free.fit)
residuals(free.fit, type = "standardized")



# Metric invariance model (factor loadings constrained equal across groups)
multi.metric <- '
int =~ mdd + c(l2, l2)*dys + c(l3, l3)*gad      # Constrain dys and gad loadings equal across groups
ext =~ anti + c(l5, l5)*alc + c(l6, l6)*sub     # Same for externalizing indicators

mdd ~~ mdd
dys ~~ dys
gad ~~ gad
anti ~~ anti
alc ~~ alc
sub ~~ sub

int ~~ ext

int ~ 0*1
ext ~ 0*1

mdd ~ 1
dys ~ 1
gad ~ 1
anti ~ 1
alc ~ 1
sub ~ 1
'

metric.fit <- sem(multi.metric, data = cfamean, meanstructure = TRUE, group = "sex")  # Fit metric invariance model
summary(metric.fit, fit.measures = TRUE)
modificationIndices(metric.fit)
lavTestScore(metric.fit)                          # Lagrange multiplier tests for constraints
residuals(metric.fit, type = "standardized")

anova(free.fit, metric.fit)                       # Compare configural vs metric model (chi-square diff test)


# Scalar invariance model (equal loadings + intercepts)
multi.scalar <- '
int =~ mdd + c(l2, l2)*dys + c(l3, l3)*gad
ext =~ anti + c(l5, l5)*alc + c(l6, l6)*sub

mdd ~~ mdd
dys ~~ dys
gad ~~ gad
anti ~~ anti
alc ~~ alc
sub ~~ sub

int ~~ ext

int ~ c(NA,0)*1                           # Estimate latent mean in group 1, fix to 0 in group 2
ext ~ c(NA,0)*1

mdd ~ c(t1, t1)*1                         # Constrain intercepts to be equal across groups
dys ~ c(t2, t2)*1
gad ~ c(t3, t3)*1
anti ~ c(t4, t4)*1
alc ~ c(t5, t5)*1
sub ~ c(t6, t6)*1
'

scalar.fit <- sem(multi.scalar, data = cfamean, meanstructure = TRUE, missing = "ML", group = "sex")  # Fit scalar model
summary(scalar.fit, fit.measures = TRUE)
modificationIndices(scalar.fit)
lavTestScore(scalar.fit)
residuals(scalar.fit, type = "standardized")

anova(scalar.fit, metric.fit)  # Compare scalar vs metric model


# Scalar model with fixed means (e.g., for testing latent mean differences)
multi.scalar2 <- '
int =~ mdd + c(l2, l2)*dys + c(l3, l3)*gad
ext =~ anti + c(l5, l5)*alc + c(l6, l6)*sub

mdd ~~ mdd
dys ~~ dys
gad ~~ gad
anti ~~ anti
alc ~~ alc
sub ~~ sub

int ~~ ext

int ~ c(0,0)*1                            # Fix latent means to 0 in both groups
ext ~ c(0,0)*1

mdd ~ c(t1, t1)*1                         # Constrain intercepts to equality
dys ~ c(t2, t2)*1
gad ~ c(t3, t3)*1
anti ~ c(t4, t4)*1
alc ~ c(t5, t5)*1
sub ~ c(t6, t6)*1
'

scalar.fit2 <- sem(multi.scalar2, data = cfamean, meanstructure = FALSE, group = "sex")  # No meanstructure estimated
summary(scalar.fit2, fit.measures = TRUE)
modificationIndices(scalar.fit2)
residuals(scalar.fit2, type = "standardized")

anova(scalar.fit, scalar.fit2)  # Test whether freeing means improves fit



