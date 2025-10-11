# Set up the environment and read csv
setwd("/Users/kristen/Desktop/UMich/学习coursework/25FALL Special Seminar - Structural Equation Modelling/Assignments/take-home exercise/Take Home 2 - CFA Exercise")
data <- read.csv("cfa_exercise.csv")
View(data)
library(lavaan)

# model 
cfa <-'AM =~ mdd+dys+gad; fear =~ soc+simp+ago+panic; external =~ alc+sub+anti; internal =~ NA*AM+fear; internal ~~ 1*internal; internal~~external'
cfa.fit <- sem(cfa, data=data, meanstructure=TRUE)
summary(cfa.fit, fit.measures = TRUE, standardized = TRUE)

# Model evaluation and identification indices
residuals(cfa.fit, type = "standardized")
mi <- modindices(cfa.fit)
mi  # Show MIs

#draw diagram
library(semPlot)

semPaths(cfa.fit,
         what="std",       
         whatLabels="std",  
         residuals=TRUE,   
         intercepts=FALSE,  
         nCharNodes=0,
         edge.label.cex=0.8,
         sizeMan=6,        
         sizeLat=8,         
         mar=c(5,5,5,5))