########################################################################
##### Classification by Multivariate Linear Regression, LDA and QDA ####
#####                    Exmple: Iris Data                          ####
########################################################################

# This famous (Fisher's or Anderson's) iris data set gives the measurements 
# in centimeters of the variables sepal length and width and petal length 
# and width, respectively, for 50 flowers from each of 3 species of iris. 
# The species are Iris setosa, versicolor, and virginica.

# Load the data
data(iris)
?iris
dim(iris)
head(iris)

# Randomly divide the data into a training set and a test set
set.seed(10000)
train.index=sample(1:150,75)
train=iris[train.index,]
dim(train)
test=iris[-train.index,]
dim(test)
summary(train$Species)
summary(test$Species)

########### Classification by Multivariate Linear Regression ###########
## Step 1: Construct the indicator response matrix Y
unique(train$Species)
nrow(train)
Y=matrix(NA,75,3)
index1=which(train$Species=="setosa")
length(index1)
index2=which(train$Species=="versicolor")
length(index2)
index3=which(train$Species=="virginica")
length(index3)
for(i in index1){Y[i,]=c(1,0,0)}
for(i in index2){Y[i,]=c(0,1,0)}
for(i in index3){Y[i,]=c(0,0,1)}
apply(Y,1,sum) # Each row of Y has a single 1


## Step 2: Calculate the coefficient matrix B
X=as.matrix(cbind(rep(1,75), train[,-5]))
B=solve(t(X)%*%X)%*%t(X)%*%Y
dim(B)


# The predicted type of each training data point
f=X%*%B
dim(f)
head(f)
apply(f,1,sum)
predicted.type=rep(NA,75)
for(i in 1:75){predicted.type[i]=which.max(f[i,])}
predicted.type[predicted.type==1]="setosa"
predicted.type[predicted.type==2]="versicolor"
predicted.type[predicted.type==3]="virginica"

# The training error is 
mean(predicted.type!=train$Species)

# The R functoin table() can be used to produce a confusion matrix in order to 
# determine how many observations were correctly or incorrectly classified.
table(predicted.type, train$Species)

# For any new input xnew, we compute f(x)=[1 xnew]*B, and classify 
# it as G(x)=argmax f_k(x)

# Calculate the test error
test.predicted.type=rep(NA,75)
f.test=as.matrix(cbind(rep(1,75),test[,-5]))%*%B
for(i in 1:75){test.predicted.type[i]=which.max(f.test[i,])}
test.predicted.type[test.predicted.type==1]="setosa"
test.predicted.type[test.predicted.type==2]="versicolor"
test.predicted.type[test.predicted.type==3]="virginica"
lm.test.error=mean(test.predicted.type!=test$Species)
lm.test.error

# Confusion matrix
table(test.predicted.type, test$Species)


########### Linear Discriminant Analysis ###########
library (MASS)
lda.fit=lda(train[,-5], train[,5])

# The test error is 
test.pred=predict(lda.fit,test[,-5])$class
lda.test.error=mean(test.pred!=test$Species)
lda.test.error

# Confusion matrix
table(test.pred, test$Species)

########### Quadratic Discriminant Analysis ###########
qda.fit=qda(train[,-5], train[,5])


# The test error is 
test.pred=predict(qda.fit,test[,-5])$class
qda.test.error=mean(test.pred!=test$Species)
qda.test.error

# Confusion matrix
table(test.pred, test$Species)

########### KNN method ###########
# Use knn function 
library(class)
?knn

# Set the maximum value of K
n.K=10
knn.test.error=rep(NA,n.K)

# Calculate the training error and test error for each K
for(i in 1:n.K){
  knn.pred=knn(train = train[,1:4], test = test[,1:4], 
               cl = train[,5], k = i)
  knn.test.error[i]=mean(knn.pred!=test[,5])
}

knn.test.error


########### Compare the performance of different methods ###########
lm.test.error
lda.test.error
qda.test.error
knn.test.error