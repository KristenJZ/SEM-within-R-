# Set up
setwd("/Users/kristen/Desktop/UMich/学习coursework/25FALL Special Seminar - Structural Equation Modelling/Assignments/take-home exercise/Take Home 1 - Path Analysis Exercise")
library(lavaan)
library(semPlot)
library(dplyr)
data <- read.csv("pa_exercise.csv")
View(data)

#PartA
model1 <- 'peer_mid ~ pa_mid + na_mid; pa_mid ~ warm_prim+bull_prim+teach_prim; na_mid ~ warm_prim + bull_prim + teach_prim'
fit1 <- sem(model1, data=data, meanstructure=FALSE)
summary(fit1, fit.measure=TRUE, standardized=TRUE)

model2 <- 'peer_mid ~ pa_mid + na_mid + warm_prim + bull_prim + teach_prim; pa_mid ~ warm_prim+bull_prim+teach_prim; na_mid ~ warm_prim + bull_prim + teach_prim'
fit2 <- sem(model2, data=data, meanstructure=FALSE)
summary(fit2, fit.measures=TRUE, standardized=TRUE)

model3 <- 'peer_mid ~ pa_mid + na_mid + 0*warm_prim + bull_prim + 0*teach_prim; pa_mid ~ warm_prim+bull_prim+teach_prim; na_mid ~ warm_prim + bull_prim + teach_prim'
fit3 <- sem(model3, data=data, meanstructure=FALSE)
summary(fit3, fit.measure=TRUE, standardized=TRUE)

model4 <- 'peer_mid ~ pa_mid + na_mid + bull_prim + 0*warm_prim + 0*teach_prim; pa_mid ~ warm_prim + bull_prim + teach_prim; na_mid ~ warm_prim + bull_prim + teach_prim ;pa_mid ~~ na_mid'
fit4 <- sem(model4, data=data, meanstructure=FALSE)
summary(fit4, fit.measure=TRUE, standardized=TRUE)

anova(fit1, fit2)
anova(fit2,fit3)
anova(fit3,fit4)
anova(fit2,fit4)

semPaths(fit3, whatLabels = "std", layout="tree", residuals = FALSE, intercepts = FALSE, exoCov = TRUE, nCharNodes = 0)

#PartB
model3_indirect <- '
  # regressions with labels
  peer_mid ~ b1*pa_mid + b2*na_mid + c*bull_prim
  peer_mid ~ 0*warm_prim
  peer_mid ~ 0*teach_prim
  pa_mid ~ a1*bull_prim + a2*warm_prim + a3*teach_prim
  na_mid ~ a4*bull_prim + a5*warm_prim + a6*teach_prim

  # correlations among exogenous variables
  warm_prim ~~ bull_prim + teach_prim
  bull_prim ~~ teach_prim

  # indirect effects via pa_mid and na_mid
  ind_bully_pa := a1 * b1
  ind_bully_na := a4 * b2
  total_ind_bully := ind_bully_pa + ind_bully_na
'

fit3_boot <- sem(model3_indirect, data = data,
                 se = "bootstrap", bootstrap = 5000)

parameterEstimates(fit3_boot, standardized = TRUE) %>%
  subset(op == ":=")

# 提取标准化路径系数
std_sol <- standardizedSolution(fit3_boot)

# 提取 a1, b1, a4, b2 的标准化估计
std_a1 <- std_sol$est.std[std_sol$label == "a1"]
std_b1 <- std_sol$est.std[std_sol$label == "b1"]
std_a4 <- std_sol$est.std[std_sol$label == "a4"]
std_b2 <- std_sol$est.std[std_sol$label == "b2"]

# 计算标准化间接效应
std_ind_bully_pa <- std_a1 * std_b1
std_ind_bully_na <- std_a4 * std_b2
std_total_ind_bully <- std_ind_bully_pa + std_ind_bully_na

# 打印结果
cat("标准化间接效应:\n")
cat("ind_bully_pa:", round(std_ind_bully_pa, 3), "\n")
cat("ind_bully_na:", round(std_ind_bully_na, 3), "\n")
cat("total_ind_bully:", round(std_total_ind_bully, 3), "\n")

  
