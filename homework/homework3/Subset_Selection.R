########################################################################
##                          Subset Selection
########################################################################
library(MASS)
?Boston
library(leaps)


########################################################################
##                             Load Data
########################################################################
data(Boston)
dim(Boston)
head(Boston)

?regsubsets

# regsubsets() function, which has the tuning parameter nvmax specifying the 
# maximal number of predictors to incorporate in the model. It returns multiple 
# models with different size up to nvmax. You need to compare the performance of 
# the different models for choosing the best one. regsubsets() has the option method, 
# which can take the values “backward”, “forward” and “seqrep” (seqrep = sequential 
# replacement, combination of forward and backward selections).

########################################################################
##                        Best Subset Selection
########################################################################
regfit.full=regsubsets(medv~., data = Boston, nvmax = 13, method = "exhaustive")
summary(regfit.full)


########################################################################
##                Forward, Backward, and Sequential Selection
########################################################################
regfit.fwd <- regsubsets(medv~., data = Boston, nvmax = 13, method = "forward")
regfit.bwd <- regsubsets(medv~., data = Boston, nvmax = 13, method = "backward")
regfit.ss <- regsubsets(medv~., data = Boston, nvmax = 13, method = "seqrep")
summary(regfit.fwd)
summary(regfit.bwd)
summary(regfit.ss)

# Examine the best 7 variable models
summary(regfit.full)$outmat[7,]
summary(regfit.fwd)$outmat[7,]
summary(regfit.bwd)$outmat[7,]
summary(regfit.ss)$outmat[7,]

# Look at the regression models determined by the different methods
coef(regfit.full, 5)
coef(regfit.fwd, 5)
coef(regfit.bwd, 5)
coef(regfit.ss, 5)

coef(regfit.full, 7)
coef(regfit.fwd, 7)
coef(regfit.bwd, 7)
coef(regfit.ss, 7)





