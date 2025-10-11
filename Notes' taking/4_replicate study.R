##THIS IS CODE FOR CREATING COVARIANCE MATRIX FROM COR and SDs

library(lavaan)


# copy the correlation matrix of the observed variables
cor_matrix1 <- matrix(c(
  1.00, 0.50, 0.30,
  0.50, 1.00, 0.40,
  0.30, 0.40, 1.00
), nrow = 3, byrow = TRUE)

# Add variable names
colnames(cor_matrix1) <- c("var1", "var2", "var3")
rownames(cor_matrix1) <- c("var1", "var2", "var3")
print(cor_matrix1)

# Standard deviations for each variable
sds <- c(2.5, 3.2, 1.8)

# Convert correlation matrix to covariance matrix
cov_matrix1 <- cor2cov(cor_matrix1, sds)


#define SEM
# here's an example
#model <- 'lone =~ PI+NI; MH =~ GAI_SF + GDS_SF; resilien =~ RS+GSE; MPQL =~ PSY+PHY;
MH~lone+resilien; resilien~lone;MPQL~MH+resilien'
#fit <- sem(model,sample.cov = cov_matrix1, sample.nobs = 290) #sample size in the original paper
#summary(fit,standardized=TRUE, fit.measures=TRUE)





