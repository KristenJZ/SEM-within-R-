library(lavaan)
midus <- read.csv("midus.csv")

## TWO-WAVE CHANGE EXAMPLES ##
#This code provides an example of the steps of two wave models using latent variables
#It begins with establishing measurement invariance, then moves on to autoregressive and latent difference score models. 

### Measurement invariance
#Configural measurement invariance - freely estimated factors at each time point

configural <-' 

#coding in factors at each wave, in this case using unit loading identification
neur =~ moody + worrying + nervous  
neur2 =~ moody2 + worrying2 + nervous2 

#covaraince among residuals at each wave
moody ~~ moody2 
worrying ~~ worrying2
nervous ~~ nervous2

#estimate observed intercepts at each wave
moody ~1
worrying ~1
nervous ~1

moody2 ~1
worrying2 ~1
nervous2 ~1

#fix latent means to 0 to identify models
neur ~ 0*1
neur2 ~ 0*1

#lavaan default is to allow for free factor variances and covariances

'

configural.fit<-sem(configural, #refer to model code
                    data=midus, #refer to data to be used
                    meanstructure = TRUE, #include the means vector
                    missing = "ML") #include FIML
summary(configural.fit, fit.measures=TRUE, standardized=TRUE)
modificationIndices(configural.fit) #Modification indices
residuals(configural.fit, type="standardized") #request residual matrices


#Weak/metric Measurement Invariance - fixed factor loadings

weak <-' 

#same model as before, only difference is factor loadings are fixed across waves
#first loading still assumed to be 1.0, but use code (l2* and l3*) to constrain
#remaining loadings to equality
#rest of code is the same

neur =~ moody + l2*worrying + l3*nervous
neur2 =~ moody2 + l2*worrying2 + l3*nervous2

moody ~~ moody2
worrying ~~ worrying2
nervous ~~ nervous2

moody ~1
worrying ~1
nervous ~1

moody2 ~1
worrying2 ~1
nervous2 ~1

neur ~ 0*1
neur2 ~ 0*1
'

weak.fit<-sem(weak, data=midus, meanstructure=TRUE, missing = "ML")
summary(weak.fit,fit.measures=TRUE,standardized=TRUE)
modificationIndices(weak.fit) #Modification indices
lavTestScore(weak.fit) #Modification indices for constrained parameters
residuals(weak.fit, type="standardized")
#code for comparing nested models using LRT and BIC/AIC
anova(configural.fit, weak.fit)
parameterEstimates(weak.fit) #can request this to get the parameter numbers


#Strong/Scalar Measurement Invariance - fixed factor loadings
#now add constraints on intercepts
strong <-' 
neur =~ moody + l2*worrying + l3*nervous
neur2 =~ moody2 + l2*worrying2 + l3*nervous2

moody ~~ moody2
worrying ~~ worrying2
nervous ~~ nervous2

moody ~ t1*1
worrying ~ t2*1
nervous ~ t3*1

moody2 ~ t1*1
worrying2 ~ t2*1
nervous2 ~ t3*1

#when fixing intercepts to equality, remember to free one latent mean
#if you do not do this, you are also testing that means are equivalent
neur ~ 0*1
neur2 ~ 1

neur2 ~~ neur
'

strong.fit<-sem(strong, data=midus, meanstructure=TRUE, missing = "ML")
summary(strong.fit,fit.measures=TRUE,standardized=TRUE)
modificationIndices(strong.fit) #Modification indices
lavTestScore(strong.fit) #Modification indices for constrained parameters
residuals(strong.fit, type="standardized")
describe(midus)

anova(weak.fit, strong.fit)


### autoregressive model - AR model (Residualized Change model)
##Residualized change
#in the residualized change model, the only change to the code is shifting factor correlation to a regression path, regressing wave 2 on wave 1
res_c <-' 
neur =~ moody + l2*worrying + l3*nervous
neur2 =~ moody2 + l2*worrying2 + l3*nervous2

moody ~~ moody2 
worrying ~~ worrying2
nervous ~~ nervous2

moody ~ t1*1
worrying ~ t2*1
nervous ~ t3*1

moody2 ~ t1*1
worrying2 ~ t2*1
nervous2 ~ t3*1

neur ~ 0*1
neur2 ~ 1

neur2 ~ neur ##Here is the difference, it means that we use the latent variance at t1 as the predictor
'

res_c.fit<-sem(res_c, data=midus, meanstructure=TRUE, missing = "ML")
summary(res_c.fit,fit.measures=TRUE,standardized=TRUE)
modificationIndices(res_c.fit) #Modification indices
residuals(res_c.fit, type="standardized")


### LDS: Latent Difference Score Model
#this model is a difference score model, but uses latent variables so no compounding of error
#in addition, latent difference is now isolated as a variable, so can predict other things with it

lds1 <-' 
neur =~ moody + l2*worrying + l3*nervous
neur2 =~ moody2 + l2*worrying2 + l3*nervous2

moody ~~ moody2
worrying ~~ worrying2
nervous ~~ nervous2

moody ~ t1*1
worrying ~ t2*1
nervous ~ t3*1

moody2 ~ t1*1
worrying2 ~ t2*1
nervous2 ~ t3*1

neur ~ 0*1

#fix time2 latent mean to 0
neur2 ~ 0*1

#fix regression path to 1
neur2 ~ 1*neur

#fix time 2 latent residual to 0
neur2 ~~ 0*neur2

#define latent difference variable
diff =~ 1*neur2

#correlate latent difference with initial values
diff ~~ neur

#free diff mean
diff ~1

'

lds1.fit<-sem(lds1, data=midus, meanstructure=TRUE, missing = "ML")
summary(lds1.fit,fit.measures=TRUE,standardized=TRUE)
modificationIndices(lds1.fit) #Modification indices
residuals(lds1.fit, type="standardized")

#this model shifts the correlation between initial values and change to a regression path
lds2 <-' 
neur =~ moody + l2*worrying + l3*nervous
neur2 =~ moody2 + l2*worrying2 + l3*nervous2

moody ~~ moody2
worrying ~~ worrying2
nervous ~~ nervous2

moody ~ t1*1
worrying ~ t2*1
nervous ~ t3*1

moody2 ~ t1*1
worrying2 ~ t2*1
nervous2 ~ t3*1

neur ~ 0*1

#fix time2 latent mean to 0
neur2 ~ 0*1

#fix regression path to 1
neur2 ~ 1*neur

#fix time 2 latent residual to 0
neur2 ~~ 0*neur2

#define latent difference variable
diff =~ 1*neur2

#regress latent difference on initial values(change it from correlation to regression；Let change be predicted from the starting point)
diff ~ neur

#free diff mean
diff ~1

'

lds2.fit<-sem(lds2, data=midus, meanstructure=TRUE, missing = "ML")
summary(lds2.fit,fit.measures=TRUE,standardized=TRUE)
modificationIndices(lds2.fit) #Modification indices
residuals(lds2.fit, type="standardized")


#alternative specifications of LDS
lds3 <-' 
neur =~ moody + l2*worrying + l3*nervous
neur2 =~ moody2 + l2*worrying2 + l3*nervous2

moody ~~ moody2
worrying ~~ worrying2
nervous ~~ nervous2

moody ~ t1*1
worrying ~ t2*1
nervous ~ t3*1

moody2 ~ t1*1
worrying2 ~ t2*1
nervous2 ~ t3*1



#fix time1 and time2 latent means to 0
neur ~ 0*1
neur2 ~ 0*1

#estimate intercept factor
intercept =~ 1*neur + 1*neur2

#define latent difference variable (slope)
diff =~ 0*neur + 1*neur2

#fix first order variances to 0
neur ~~ 0*neur
neur2 ~~ 0*neur2

#fix latent intercept factor to 0, free latent difference factor
intercept ~ 0*1
diff ~ 1
'

lds3.fit<-sem(lds3, data=midus, meanstructure=TRUE, missing = "ML")
summary(lds3.fit,fit.measures=TRUE,standardized=TRUE)
modificationIndices(lds3.fit) #Modification indices
residuals(lds3.fit, type="standardized")


lds4 <-' 
neur =~ moody + l2*worrying + l3*nervous
neur2 =~ moody2 + l2*worrying2 + l3*nervous2

moody ~~ moody2
worrying ~~ worrying2
nervous ~~ nervous2

moody ~ t1*1
worrying ~ t2*1
nervous ~ t3*1

moody2 ~ t1*1
worrying2 ~ t2*1
nervous2 ~ t3*1

#fix time1 latent mean to 0 free time 2 latent means
neur ~ 0*1
neur2 ~ 1

#estimate intercept
intercept =~ 1*neur + 1*neur2

#free first order variances
neur ~~ neur
neur2 ~~ neur2

#fix latent intercept factor to 0
intercept ~ 0*1

'

lds4.fit<-sem(lds4, data=midus, meanstructure=TRUE, missing = "ML")
summary(lds4.fit,fit.measures=TRUE,standardized=TRUE)


###Some summary: LDS1：看平均变化。LDS2：看个体间差异。LDS3：看是否“高水平的人变化更快/更慢”(变化依赖于之前的状态）。LDS4：看这种关系是否是“比例式”的，比如某人水平越高，变化速度越快或越慢(变化速度和水平成比例）。