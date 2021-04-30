##########################################################
#     Methods Using Derived Input Directions
##########################################################
library(ElemStatLearn)
library(pls)  #install.packages("pls")

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
## Principal Component Regression
##########################################################
?pcr
pcr.mod = pcr(lpsa~., data=train, method = pls.options()$pcralg,
              scale=F, center=T,validation="none")
pcr.mod.s=pcr(lpsa~., data=train, method = pls.options()$pcralg,
              scale=T, center=T,validation="none")
# Predict the response for "new values" of x
prediction.errors=rep(NA,8)
prediction.errors.s=rep(NA,8)
for(i in 1:8)
{
  predicted.values=as.vector(predict(pcr.mod, newdata=as.matrix(test[,1:8]),ncomp=i))
  prediction.errors[i]=mean((predicted.values-test[,9])^2)
  predicted.values.s=as.vector(predict(pcr.mod.s, newdata=as.matrix(test[,1:8]),ncomp=i))
  prediction.errors.s[i]=mean((predicted.values.s-test[,9])^2)
}
par(mfrow=c(1,2))
plot(prediction.errors, type="o", ylim=c(0.4,1.2),
     main="PCR using the Original Data")
plot(prediction.errors.s, type="o", ylim=c(0.4,1.2), 
     main="PCR using the Scaled Data")



##########################################################
## Partial Least Squares
##########################################################
?plsr
plsr.mod = plsr(lpsa~., data=train, method = pls.options()$plsralg,
                scale=F, center=T,validation="none")
plsr.mod.s = plsr(lpsa~., data=train, method = pls.options()$plsralg,
                scale=T, center=T,validation="none")

# Predict the response for "new values" of x
prediction.errors=rep(NA,8)
prediction.errors.s=rep(NA,8)
for(i in 1:8)
{
  predicted.values=as.vector(predict(plsr.mod, newdata=as.matrix(test[,1:8]),ncomp=i))
  prediction.errors[i]=mean((predicted.values-test[,9])^2)
  predicted.values.s=as.vector(predict(plsr.mod.s, newdata=as.matrix(test[,1:8]),ncomp=i))
  prediction.errors.s[i]=mean((predicted.values.s-test[,9])^2)
}
par(mfrow=c(1,2))
plot(prediction.errors, type="o", ylim=c(0.4,1.2),
     main="PLS using the Original Data")
plot(prediction.errors.s, type="o", ylim=c(0.4,1.2), 
     main="PLS using the Scaled Data")

##########################################################
# Compare LS, PCR, and PLS
##########################################################
ls.mod=lm(lpsa~., data=train)
ls.predicted.values=as.vector(predict(ls.mod, newdata=test[,1:8]))
pcr.predicted.values=as.vector(predict(pcr.mod, newdata=as.matrix(test[,1:8]),ncomp=8))
pls.predicted.values=as.vector(predict(plsr.mod, newdata=as.matrix(test[,1:8]), ncomp=8))
pcr.s.predicted.values=as.vector(predict(pcr.mod.s, newdata=as.matrix(test[,1:8]),ncomp=8))
pls.s.predicted.values=as.vector(predict(plsr.mod.s, newdata=as.matrix(test[,1:8]), ncomp=8))
data.frame(LS=ls.predicted.values,PCR=pcr.predicted.values,PCR.s=pcr.s.predicted.values,
           PLS=pls.predicted.values,PLS.s=pls.s.predicted.values)
