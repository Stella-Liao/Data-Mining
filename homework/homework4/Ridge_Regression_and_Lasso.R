##########################################################
#     Ridge Regression and Lasso
##########################################################
library(ElemStatLearn)
library(glmnet)  #install.packages("glmnet)

##########################################################
# Load the prostate data
##########################################################
data(prostate)
?prostate
names(prostate)
train=subset(prostate,train==TRUE)[,1:9]
test=subset(prostate,train==FALSE)[,1:9]
dim(train)
dim(test)

##########################################################
## Fit a Ridge Regression Model
##########################################################
?glmnet
ridge.mod = glmnet(as.matrix(train[,1:8]), as.vector(train[,9]), 
                   family="gaussian",alpha=0)
coef(ridge.mod)
dim(coef(ridge.mod))

# Look at different lambdas
ridge.mod$lambda[10]
coef(ridge.mod)[,10]
l2_norm <- sqrt(sum(coef(ridge.mod)[2:9,10]^2))
l2_norm


# Look at different lambdas
ridge.mod$lambda[50]
coef(ridge.mod)[,50]
l2_norm <- sqrt(sum(coef(ridge.mod)[2:9,50]^2))
l2_norm

# Look at different lambdas
ridge.mod$lambda[100]
coef(ridge.mod)[,100]
l2_norm <- sqrt(sum(coef(ridge.mod)[2:9,100]^2))
l2_norm

# Predict the model for a "new value" of lamba
?predict.glmnet
predict(ridge.mod, s = .0005, type = "coefficient")
predict(ridge.mod, s = .75, type = "coefficient")


# Predict the response for "new values" of x
predicted.values=predict(ridge.mod, newx=as.matrix(test[,1:8]), 
                         type = "response")
prediction.errors=rep(NA,length(ridge.mod$lambda))
for(i in 1:length(ridge.mod$lambda))
{
  prediction.errors[i]=mean((predicted.values[,i]-test[,9])^2)
}
plot(prediction.errors, type="l")



##########################################################
## Fit a Lasso Regression Model
##########################################################
lasso.mod <- glmnet(as.matrix(train[,1:8]), as.vector(train[,9]), 
                    family="gaussian",alpha=1)
plot(lasso.mod, label=T)

# Lets look at coefficients....
lasso.mod$lambda[70]
coef(lasso.mod)[,70]

lasso.mod$lambda[50]
coef(lasso.mod)[,50]

lasso.mod$lambda[20]
coef(lasso.mod)[,20]

# Predict the model for a "new value" of lamba
?predict.glmnet
predict(lasso.mod, s = .0005, type = "coefficient")
predict(lasso.mod, s = .75, type = "coefficient")


# Predict the response for "new values" of x
predicted.values=predict(lasso.mod, newx=as.matrix(test[,1:8]), 
                         type = "response")
prediction.errors=rep(NA,length(lasso.mod$lambda))
for(i in 1:length(lasso.mod$lambda))
{
  prediction.errors[i]=mean((predicted.values[,i]-test[,9])^2)
}
plot(prediction.errors, type="l")


# l1 (lasso) regularization -----------------------------------------------

# See Tibshirani (1996) for the source, or Murphy PML (2012) for a nice overview
# (watch for typos in depictions). A more conceptual depiction of the lasso can
# be found in penalized_ML.R

# coordinate descent
lasso <- function(
  X,                   # model matrix
  y,                   # target
  lambda = .1,         # penalty parameter
  soft = T,            # soft vs. hard thresholding
  tol = 1e-6,          # tolerance
  iter = 100,          # number of max iterations
  verbose = T          # print out iteration number
) {
  
  # soft thresholding function
  soft_thresh <- function(a, b) {
    out = rep(0, length(a))
    out[a >  b] = a[a > b] - b
    out[a < -b] = a[a < -b] + b
    out
  }
  
  w = rep(0, ncol(X))
  tol_curr = 1
  J = ncol(X)
  a = rep(0, J)
  c_ = rep(0, J)
  i = 1
  
  while (tol < tol_curr && i < iter) {
    w_old = w 
    
    #a = colSums(X^2)
    #l = length(y)*lambda  # for consistency with glmnet approach
    #c_ = sapply(1:J, function(j)  sum( X[,j] * (y - X[,-j] %*% w_old[-j]) ))
    for (j in 1:J) {
        x_j = X[,j]
        y_pred = X %*% w
        rho = x_j %*% (y - y_pred + w[j] * x_j)
        w[j] = soft_thresh(rho, lambda)
    }
    
    tol_curr = crossprod(w - w_old)  
    i = i + 1
  }
  
  w
}


set.seed(8675309)
N = 500
p = 10
X = scale(matrix(rnorm(N*p), ncol=p))
b = c(.5, -.5, .25, -.25, .125, -.125, rep(0, p-6))
y = scale(X %*% b + rnorm(N, sd=.5))
lambda = .1


# debugonce(lasso)

# note, if lambda=0, result is lm.fit
result_soft = lasso(
  X,
  y,
  lambda = lambda,
  tol = 1e-12
)


library(glmnet)

# glmnet is by default a mixture of ridge and lasso penalties, setting alpha = 1
# reduces to lasso (alpha=0 would be ridge); We set the lambda to a couple
# values while only wanting the one set to the same lambda value as above (s)

glmnet_res = coef(
  glmnet(
    X,
    y,
    alpha = 1,
    lambda = c(10, 1, lambda),
    thresh = 1e-12,
    intercept = F
  ),
  s = lambda
)



# comparison

data.frame(
  lasso_soft = result_soft,
  glmnet = glmnet_res[-1, 1]
)

