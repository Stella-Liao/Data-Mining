###########################################################
## linear regression
###########################################################
library(ElemStatLearn)

###########################################################
# Load the data
###########################################################
data(prostate)
?prostate
names(prostate)
train=subset(prostate,train==TRUE)[,1:9]
test=subset(prostate,train==FALSE)[,1:9]

###########################################################
# Data Visualization
###########################################################
pairs( prostate[,1:9], col="blue" )


###########################################################
# Linear Regression
###########################################################
fit1=lm(lpsa ~ ., data = train)
summary(fit1)

# Compute confidence interval
confint(fit1,level=0.95)
confint(fit1,level=0.90)

# Test whether the coefficients of age and gleason are 0
fit2=lm(lpsa~lcavol+lweight+lbph+svi+lcp+pgg45, data=train)
rss0=sum(fit2$residuals^2)
rss1=sum(fit1$residuals^2)
F.stat=((rss0-rss1)/2)/(rss1/(nrow(train)-9))
1-pf(F.stat,2,nrow(train)-9) #p-value


# Prediction
prediction1=predict(fit1, newdata = test[,1:8])
prediction2=predict(fit2, newdata = test[,c(1,2,4,5,6,8)])

# Calculate the training error and the test error
mean(fit1$residuals^2)
mean((prediction1-test[,9])^2)
mean(fit2$residuals^2)
mean((prediction2-test[,9])^2)
