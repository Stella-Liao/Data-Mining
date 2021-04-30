###########################################################
##  K-nearest neighbors (K-NN) Example
##  Identifying the Gender of a Voice using Machine Learning
##  This data was collected by Korey Becker 
###########################################################
# The following acoustic properties of each voice are measured:
# meanfreq: mean frequency (in kHz)
# sd: standard deviation of frequency
# median: median frequency (in kHz)
# Q25: first quantile (in kHz)
# Q75: third quantile (in kHz)
# IQR: interquantile range (in kHz)
# skew: skewness (see note in specprop description)
# kurt: kurtosis (see note in specprop description)
# sp.ent: spectral entropy
# sfm: spectral flatness
# mode: mode frequency
# centroid: frequency centroid (see specprop)
# meanfun: average of fundamental frequency measured across acoustic signal
# minfun: minimum fundamental frequency measured across acoustic signal
# maxfun: maximum fundamental frequency measured across acoustic signal
# meandom: average of dominant frequency measured across acoustic signal
# mindom: minimum of dominant frequency measured across acoustic signal
# maxdom: maximum of dominant frequency measured across acoustic signal
# dfrange: range of dominant frequency measured across acoustic signal
# modindx: modulation index. Calculated as the accumulated absolute difference between 
#          adjacent measurements of fundamental frequencies divided by the frequency range


###########################################################
# Load the data 
###########################################################
setwd("/Users/guanyu/Box/Courses/STA_545_Fall2020/R_Examples")
voice=read.csv("voice.csv",header=T)
voice=voice[,-c(11,19,20)] # delete variables with missing data
dim(voice)
head(voice)


###########################################################
# Divide the data into a training set and a testing set 
###########################################################
#Set the random number generator seed for reproducibility
set.seed(123)

# Set the fraction of the training data
training.fraction=0.75

# Train and Test Split
sample.size=nrow(voice)
index=sample(1:sample.size)
n.train=floor(sample.size*training.fraction)
training.data=voice[index[1:n.train],]
testing.data=voice[-index[1:n.train],]
dim(training.data)
dim(testing.data)

###########################################################
# Use K-NN with different values of K
###########################################################
# Use knn function 
library(class)
?knn

# Set the maximum value of K
n.K=30

train.error=rep(NA,n.K)
test.error=rep(NA,n.K)

# Calculate the training error and test error for each K
for(i in 1:n.K){
  knn.pred=knn(train = training.data[,1:17], test = testing.data[,1:17], 
               cl = training.data[,18], k = i)
  test.error[i]=mean(knn.pred!=testing.data[,18])
  
  knn.train=knn(train = training.data[,1:17], test = training.data[,1:17], 
                cl = training.data[,18], k = i)
  train.error[i]=mean(knn.train!=training.data[,18])
}

# Plot the training errors and test errors
par(mfrow=c(1,2))
plot(test.error,type="o",xlab="k", ylab="Test Error")
plot(train.error,type="o",,xlab="k", ylab="Training Error")


