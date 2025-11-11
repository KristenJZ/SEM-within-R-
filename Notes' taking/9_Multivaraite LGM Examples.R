library(lavaan)

#first estimate a univariate growth curve for each variable
soc_lgm <-'
i_soc =~ 1*soc1 + 1*soc2 + 1*soc3 + 1*soc4
s_soc =~ 0*soc1 + 1*soc2 + 2*soc3 + 3*soc4

i_soc ~~ i_soc
s_soc ~~ s_soc
s_soc ~~ i_soc

i_soc ~ 1
s_soc ~ 1

soc1 ~0*1
soc2 ~0*1
soc3 ~0*1
soc4 ~0*1
'

soc_lgm.fit<-sem(soc_lgm, data=devel, meanstructure=TRUE, missing="ML")
summary(soc_lgm.fit,fit.measures=TRUE,standardized=TRUE)
modificationIndices(soc_lgm.fit) #Modification indices
residuals(soc_lgm.fit, type="standardized")

acad_lgm <-'
i_acad =~ 1*acad1 + 1*acad2 + 1*acad3 + 1*acad4
s_acad =~ 0*acad1 + 1*acad2 + 2*acad3 + 3*acad4

i_acad ~~ i_acad
s_acad ~~ s_acad
s_acad ~~ i_acad

i_acad ~ 1
s_acad ~ 1

acad1 ~0*1
acad2 ~0*1
acad3 ~0*1
acad4 ~0*1
'

acad_lgm.fit<-sem(acad_lgm, data=devel, meanstructure=TRUE, missing="ML")
summary(acad_lgm.fit,fit.measures=TRUE,standardized=TRUE)
modificationIndices(acad_lgm.fit) #Modification indices
residuals(acad_lgm.fit, type="standardized")


#combine univariate growth curves to form a parallel process/multivaraite growth curve
parallel_lgm <-'
i_acad =~ 1*acad1 + 1*acad2 + 1*acad3 + 1*acad4
s_acad =~ 0*acad1 + 1*acad2 + 2*acad3 + 3*acad4

i_acad ~ 1
s_acad ~ 1

acad1 ~0*1
acad2 ~0*1
acad3 ~0*1
acad4 ~0*1

i_soc =~ 1*soc1 + 1*soc2 + 1*soc3 + 1*soc4
s_soc =~ 0*soc1 + 1*soc2 + 2*soc3 + 3*soc4

i_soc ~ 1
s_soc ~ 1

soc1 ~0*1
soc2 ~0*1
soc3 ~0*1
soc4 ~0*1

i_acad ~~ i_acad
s_acad ~~ s_acad
s_acad ~ i_acad

i_soc ~~ i_soc
s_soc ~~ s_soc
s_soc ~ i_soc

i_soc ~~ i_acad
s_soc ~~ s_acad
i_soc ~~ s_acad
s_soc ~~ i_acad

soc1 ~~ acad1
soc2 ~~ acad2
soc3 ~~ acad3
soc4 ~~ acad4
'

parallel_lgm.fit<-sem(parallel_lgm, data=devel, meanstructure=TRUE, missing="ML")
summary(parallel_lgm.fit,fit.measures=TRUE,standardized=TRUE)
modificationIndices(parallel_lgm.fit) #Modification indices
residuals(parallel_lgm.fit, type="standardized")


#estiamting a cross-lagged panel model
clpm <-'

#make latent versions of each variable

lacad1 =~ acad1
lacad2 =~ acad2
lacad3 =~ acad3
lacad4 =~ acad4

acad1 ~~ 0*acad1
acad2 ~~ 0*acad2
acad3 ~~ 0*acad3
acad4 ~~ 0*acad4

lsoc1 =~ soc1
lsoc2 =~ soc2
lsoc3 =~ soc3
lsoc4 =~ soc4

soc1 ~~ 0*soc1
soc2 ~~ 0*soc2
soc3 ~~ 0*soc3
soc4 ~~ 0*soc4

#autoregressive paths

lsoc2 ~ lsoc1
lsoc3 ~ lsoc2
lsoc4 ~ lsoc3

lacad2 ~ lacad1
lacad3 ~ lacad2
lacad4 ~ lacad3

#crosslagged paths

lsoc2 ~ lacad1
lsoc3 ~ lacad2
lsoc4 ~ lacad3

lacad2 ~ lsoc1
lacad3 ~ lsoc2
lacad4 ~ lsoc3

#covariances

lsoc1 ~~ lacad1
lsoc2 ~~ lacad2
lsoc3 ~~ lacad3
lsoc4 ~~ lacad4


'

clpm.fit<-sem(clpm, data=devel, meanstructure=TRUE, missing="ML")
summary(clpm.fit,fit.measures=TRUE,standardized=TRUE)
modificationIndices(clpm.fit) #Modification indices
residuals(clpm.fit, type="standardized")

#estimating a random intercept cross-lagged panel model
riclpm <-'
#estimate random intercepts

ri_acad =~ 1*acad1 + 1*acad2 + 1*acad3 + 1*acad4
ri_soc =~ 1*soc1 + 1*soc2 + 1*soc3 + 1*soc4

ri_acad ~~ ri_soc

#make latent versions of each variable

lacad1 =~ acad1
lacad2 =~ acad2
lacad3 =~ acad3
lacad4 =~ acad4

acad1 ~~ 0*acad1
acad2 ~~ 0*acad2
acad3 ~~ 0*acad3
acad4 ~~ 0*acad4

lsoc1 =~ soc1
lsoc2 =~ soc2
lsoc3 =~ soc3
lsoc4 =~ soc4

soc1 ~~ 0*soc1
soc2 ~~ 0*soc2
soc3 ~~ 0*soc3
soc4 ~~ 0*soc4

#autoregressive paths

lsoc2 ~ lsoc1
lsoc3 ~ lsoc2
lsoc4 ~ lsoc3

lacad2 ~ lacad1
lacad3 ~ lacad2
lacad4 ~ lacad3

#crosslagged paths

lsoc2 ~ lacad1
lsoc3 ~ lacad2
lsoc4 ~ lacad3

lacad2 ~ lsoc1
lacad3 ~ lsoc2
lacad4 ~ lsoc3

#covariances among innovations

lsoc1 ~~ lacad1
lsoc2 ~~ lacad2
lsoc3 ~~ lacad3
lsoc4 ~~ lacad4

#fix correlations among exogenous latent variables to 0

ri_acad ~~ 0*lacad1 + 0*lsoc1
ri_soc ~~ 0*lacad1 + 0*lsoc1
'

riclpm.fit<-sem(riclpm, data=devel, meanstructure=TRUE, missing="ML")
summary(riclpm.fit,fit.measures=TRUE,standardized=TRUE)
modificationIndices(riclpm.fit) #Modification indices
residuals(riclpm.fit, type="standardized")


#estimating a latent growth curve with structured residuals
lgm_sr <-'

#estimate intercepts and slopes, free means, and covariances

ri_acad =~ 1*acad1 + 1*acad2 + 1*acad3 + 1*acad4
s_acad =~ 0*acad1 + 1*acad2 + 2*acad3 + 3*acad4

ri_soc =~ 1*soc1 + 1*soc2 + 1*soc3 + 1*soc4
s_soc =~ 0*soc1 + 1*soc2 + 2*soc3 + 3*soc4

acad1 ~ 0*1
acad2 ~ 0*1
acad3 ~ 0*1
acad4 ~ 0*1

soc1 ~ 0*1
soc2 ~ 0*1
soc3 ~ 0*1
soc4 ~ 0*1

ri_acad ~ 1
s_acad ~ 1

ri_soc ~ 1
s_soc ~ 1

s_acad ~~ ri_acad 
ri_acad ~~ ri_soc
s_acad ~~ ri_soc 
s_soc ~~ s_acad
s_soc ~~ ri_soc
s_soc ~~ ri_acad

#make latent versions of each variable

lacad1 =~ acad1
lacad2 =~ acad2
lacad3 =~ acad3
lacad4 =~ acad4

acad1 ~~ 0*acad1
acad2 ~~ 0*acad2
acad3 ~~ 0*acad3
acad4 ~~ 0*acad4

lsoc1 =~ soc1
lsoc2 =~ soc2
lsoc3 =~ soc3
lsoc4 =~ soc4

soc1 ~~ 0*soc1
soc2 ~~ 0*soc2
soc3 ~~ 0*soc3
soc4 ~~ 0*soc4

#autoregressive paths

lsoc2 ~ a*lsoc1
lsoc3 ~ a*lsoc2
lsoc4 ~ a*lsoc3

lacad2 ~ b*lacad1
lacad3 ~ b*lacad2
lacad4 ~ b*lacad3

#crosslagged paths

lsoc2 ~ c*lacad1
lsoc3 ~ c*lacad2
lsoc4 ~ c*lacad3

lacad2 ~ d*lsoc1
lacad3 ~ d*lsoc2
lacad4 ~ d*lsoc3

#constrain variances

lacad1 ~~ e*lacad1
lacad2 ~~ e*lacad2
lacad3 ~~ e*lacad3
lacad4 ~~ e*lacad4

lsoc1 ~~ f*lsoc1
lsoc2 ~~ f*lsoc2
lsoc3 ~~ f*lsoc3
lsoc4 ~~ f*lsoc4

#covariances

lsoc1 ~~ g*lacad1
lsoc2 ~~ g*lacad2
lsoc3 ~~ g*lacad3
lsoc4 ~~ g*lacad4

# fix exogenous latent correlations to 0

ri_soc ~~ 0*lacad1
ri_soc ~~ 0*lsoc1

ri_acad ~~ 0*lacad1
ri_acad ~~ 0*lsoc1

s_soc ~~ 0*lacad1
s_soc ~~ 0*lsoc1

s_acad ~~ 0*lacad1
s_acad ~~ 0*lsoc1


'

lgm_sr.fit<-sem(lgm_sr, data=devel, meanstructure=TRUE, missing="ML")
summary(lgm_sr.fit,fit.measures=TRUE,standardized=TRUE)
modificationIndices(lgm_sr.fit) #Modification indices
residuals(lgm_sr.fit, type="standardized")

anova(parallel_lgm.fit, riclpm.fit)