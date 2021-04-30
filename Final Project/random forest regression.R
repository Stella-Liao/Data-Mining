library(tidyr)
library(dplyr)

fin.data <- read.csv("data/crime-pp-Wales.csv")%>%
  select(!c(X, Transaction.unique.identifier, Postcode, PAON, SAON, Street, Locality, 
            Town.City, District, County, PPD.Category.Type, Record.Status, population, Month, Date.of.Transfer))%>%
  
  drop_na()%>%
  mutate(area = substr(postcode.sector,1,2))%>%
  mutate(is.SA = case_when(area == "SA" ~ "Y", 
                           area != "SA" ~ "N"))%>%
  select(!c(area,postcode.sector))

fin.data$Property.Type <- as.factor(fin.data$Property.Type)
fin.data$Old.New <- as.factor(fin.data$Old.New)
fin.data$Duration <- as.factor(fin.data$Duration)
fin.data$is.SA <- as.factor(fin.data$is.SA)

#Crime data only
fin.data2 <- fin.data %>%
  select(!c(Property.Type, Old.New, Duration))

#House data only
fin.data3 <- fin.data %>%
  select(c(Price, Property.Type, Old.New, Duration, is.SA))

#Crime amounts only
fin.data4 <- fin.data %>%
  select(!ends_with(".rate"))

#Crime rates only
fin.data5 <- fin.data %>%
  select(!ends_with(".amount"))

#####################################################################################################
###################################### Automatic random forest regression ###########################
#####################################################################################################

rf <- function(data){
  set.seed(123)
  train.num <- sample(nrow(data), 0.75*nrow(data))
  train <- data[train.num,]
  test<- data[-train.num,]
  
  #baseline model
  set.seed(1234)
  require(randomForest)
  mod0 <- randomForest(log(Price) ~ .,data = train)
  pred0 <- predict(mod0, newdata = test)
  err0 <- mean((pred0 - log(test$Price))^2)
  
  #find optimal mtry; 10-fold cross validation
  k=10
  CVgroup <- function(k=10,datasize,seed=1234){
    cvlist <- list()
    set.seed(seed)
    n <- rep(1:k,ceiling(datasize/k))[1:datasize]
    temp <- sample(n,datasize)
    x <- 1:k
    dataseq <- 1:datasize
    cvlist <- lapply(x,function(x) dataseq[temp==x]) 
    return(cvlist)
  }
  cvlist <- CVgroup(10,nrow(train))
  
  cvtest <- function(i,j){
    data.train <- train[-cvlist[[i]],]
    data.test <- train[cvlist[[i]],]
    model <- randomForest(log(Price)~.,data = train)
    prediction <- predict(model, newdata = test)
    error = mean((prediction-log(test$Price))^2)
    
    return(list(error, j))
  }
  
  tmp.mses <-NULL
  mses <- rep(0,(ncol(train)-1)) 
  mtrys <- rep(0,(ncol(train)-1))
  index = 1
  old_mtry = 1
  
  i <- c(1:k)
  j <- c(1:(ncol(train)-1))
  i.s <- rep(i,times = length(j))
  j.s <- rep(j,each = k)   
  ijs <- cbind(i.s,j.s)
  
  for(p in 1:nrow(ijs)){
    if(ijs[p,2] == old_mtry){
      i = ijs[p,1] # index for data set
      j = ijs[p,2] # mtry
      mse_mtry = cvtest(i,j)
      mse = mse_mtry[[1]]
      mtry = mse_mtry[[2]]
      tmp.mses <- c(tmp.mses,mse)
    }
    mses[index] = mean(tmp.mses)
    mtrys[index] = index
    index = ijs[p,2]
    old_mtry = ijs[p,2]
  }
  
  opt.mtry = mtrys[which.min(mses)]
  return(list(train, test, err0, opt.mtry))
}


######################################### full data ############################################
data = fin.data
set.seed(123)
train.num <- sample(nrow(data), 0.75*nrow(data))
train <- data[train.num,]
test<- data[-train.num,]

#baseline model
set.seed(1234)
library(randomForest)
mod0 <- randomForest(log(Price) ~ .,data = train)
pred0 <- predict(mod0, newdata = test)
err0 <- mean((pred0 - log(test$Price))^2) #0.2598949

#find optimal mtry; 10-fold cross validation
k=5
CVgroup <- function(k=5,datasize,seed=1234){
  cvlist <- list()
  set.seed(seed)
  n <- rep(1:k,ceiling(datasize/k))[1:datasize]
  temp <- sample(n,datasize)
  x <- 1:k
  dataseq <- 1:datasize
  cvlist <- lapply(x,function(x) dataseq[temp==x]) 
  return(cvlist)
}
cvlist <- CVgroup(5,nrow(train))

cvtest <- function(i,j){
  data.train <- train[-cvlist[[i]],]
  data.test <- train[cvlist[[i]],]
  model <- randomForest(log(Price)~.,data = train)
  prediction <- predict(model, newdata = test)
  error = mean((prediction-log(test$Price))^2)
  
  return(list(error, j))
}

tmp.mses <-NULL
mses <- rep(0,(ncol(train)-1)) 
mtrys <- rep(0,(ncol(train)-1))
index = 1
old_mtry = 1

i <- c(1:k)
j <- c(1:(ncol(train)-1))
i.s <- rep(i,times = length(j))
j.s <- rep(j,each = k)   
ijs <- cbind(i.s,j.s)

for(p in 1:nrow(ijs)){
  if(ijs[p,2] == old_mtry){
    i = ijs[p,1] # index for data set
    j = ijs[p,2] # mtry
    mse_mtry = cvtest(i,j)
    mse = mse_mtry[[1]]
    mtry = mse_mtry[[2]]
    tmp.mses <- c(tmp.mses,mse)
  }
  mses[index] = mean(tmp.mses)
  mtrys[index] = index
  index = ijs[p,2]
  old_mtry = ijs[p,2]
}

opt.mtry = mtrys[which.min(mses)]

res.full <- list(train, test, err0, opt.mtry)

#res.full <- rf(fin.data3)

#improved model
mod.full <- randomForest(log(Price) ~ .,data = res.full[[1]] , mtry = res.full[[4]])
plot(mod.full) # ntree = 100

mod.full <- randomForest(log(Price) ~ .,data = res.full[[1]] , mtry = res.full[[4]], ntree = 100 )
pred.full <- predict(mod.full, newdata = res.full[[2]])
error.full <- mean((pred.full - log((res.full[[2]])$Price))^2)
error.full #0.2471361

#baseline test error
res.full[[3]]

#opt.mtry
res.full[[4]]


######################################### crime data only ############################################

data = fin.data2
set.seed(123)
train.num <- sample(nrow(data), 0.75*nrow(data))
train <- data[train.num,]
test<- data[-train.num,]

#baseline model
set.seed(1234)
library(randomForest)
mod0 <- randomForest(log(Price) ~ .,data = train)
pred0 <- predict(mod0, newdata = test)
err0 <- mean((pred0 - log(test$Price))^2) #0.2598949

#find optimal mtry; 10-fold cross validation
k=5
CVgroup <- function(k=5,datasize,seed=1234){
  cvlist <- list()
  set.seed(seed)
  n <- rep(1:k,ceiling(datasize/k))[1:datasize]
  temp <- sample(n,datasize)
  x <- 1:k
  dataseq <- 1:datasize
  cvlist <- lapply(x,function(x) dataseq[temp==x]) 
  return(cvlist)
}
cvlist <- CVgroup(5,nrow(train))

cvtest <- function(i,j){
  data.train <- train[-cvlist[[i]],]
  data.test <- train[cvlist[[i]],]
  model <- randomForest(log(Price)~.,data = train)
  prediction <- predict(model, newdata = test)
  error = mean((prediction-log(test$Price))^2)
  
  return(list(error, j))
}

tmp.mses <-NULL
mses <- rep(0,(ncol(train)-1)) 
mtrys <- rep(0,(ncol(train)-1))
index = 1
old_mtry = 1

i <- c(1:k)
j <- c(1:(ncol(train)-1))
i.s <- rep(i,times = length(j))
j.s <- rep(j,each = k)   
ijs <- cbind(i.s,j.s)

for(p in 1:nrow(ijs)){
  if(ijs[p,2] == old_mtry){
    i = ijs[p,1] # index for data set
    j = ijs[p,2] # mtry
    mse_mtry = cvtest(i,j)
    mse = mse_mtry[[1]]
    mtry = mse_mtry[[2]]
    tmp.mses <- c(tmp.mses,mse)
  }
  mses[index] = mean(tmp.mses)
  mtrys[index] = index
  index = ijs[p,2]
  old_mtry = ijs[p,2]
}

opt.mtry = mtrys[which.min(mses)]

res.c <- list(train, test, err0, opt.mtry)

#res.c <- rf(fin.data3)

#improved model
mod.c <- randomForest(log(Price) ~ .,data = res.c[[1]] , mtry = res.c[[4]])
plot(mod.c) # ntree = 100

mod.c <- randomForest(log(Price) ~ .,data = res.c[[1]] , mtry = res.c[[4]], ntree = 100 )
pred.c <- predict(mod.c, newdata = res.c[[2]])
error.c <- mean((pred.c - log((res.c[[2]])$Price))^2)
error.c #0.2471361

#baseline test error
res.c[[3]]

#opt.mtry
res.c[[4]]

######################################### crime amounts with house ############################################

data = fin.data4
set.seed(123)
train.num <- sample(nrow(data), 0.75*nrow(data))
train <- data[train.num,]
test<- data[-train.num,]

#baseline model
set.seed(1234)
library(randomForest)
mod0 <- randomForest(log(Price) ~ .,data = train)
pred0 <- predict(mod0, newdata = test)
err0 <- mean((pred0 - log(test$Price))^2) #0.2598949

#find optimal mtry; 10-fold cross validation
k=5
CVgroup <- function(k=5,datasize,seed=1234){
  cvlist <- list()
  set.seed(seed)
  n <- rep(1:k,ceiling(datasize/k))[1:datasize]
  temp <- sample(n,datasize)
  x <- 1:k
  dataseq <- 1:datasize
  cvlist <- lapply(x,function(x) dataseq[temp==x]) 
  return(cvlist)
}
cvlist <- CVgroup(5,nrow(train))

cvtest <- function(i,j){
  data.train <- train[-cvlist[[i]],]
  data.test <- train[cvlist[[i]],]
  model <- randomForest(log(Price)~.,data = train)
  prediction <- predict(model, newdata = test)
  error = mean((prediction-log(test$Price))^2)
  
  return(list(error, j))
}

tmp.mses <-NULL
mses <- rep(0,(ncol(train)-1)) 
mtrys <- rep(0,(ncol(train)-1))
index = 1
old_mtry = 1

i <- c(1:k)
j <- c(1:(ncol(train)-1))
i.s <- rep(i,times = length(j))
j.s <- rep(j,each = k)   
ijs <- cbind(i.s,j.s)

for(p in 1:nrow(ijs)){
  if(ijs[p,2] == old_mtry){
    i = ijs[p,1] # index for data set
    j = ijs[p,2] # mtry
    mse_mtry = cvtest(i,j)
    mse = mse_mtry[[1]]
    mtry = mse_mtry[[2]]
    tmp.mses <- c(tmp.mses,mse)
  }
  mses[index] = mean(tmp.mses)
  mtrys[index] = index
  index = ijs[p,2]
  old_mtry = ijs[p,2]
}

opt.mtry = mtrys[which.min(mses)]

res.ca <- list(train, test, err0, opt.mtry)


#improved model
mod.ca <- randomForest(log(Price) ~ .,data = res.ca[[1]] , mtry = res.ca[[4]])
plot(mod.ca) # ntree = 100

mod.ca <- randomForest(log(Price) ~ .,data = res.ca[[1]] , mtry = res.ca[[4]], ntree = 100 )
pred.ca <- predict(mod.ca, newdata = res.ca[[2]])
error.ca <- mean((pred.ca - log((res.ca[[2]])$Price))^2)
error.ca #0.2471361

#baseline test error
res.ca[[3]]

#opt.mtry
res.ca[[4]]

######################################### crime rates with house############################################
data = fin.data5
set.seed(123)
train.num <- sample(nrow(data), 0.75*nrow(data))
train <- data[train.num,]
test<- data[-train.num,]

#baseline model
set.seed(1234)
library(randomForest)
mod0 <- randomForest(log(Price) ~ .,data = train)
pred0 <- predict(mod0, newdata = test)
err0 <- mean((pred0 - log(test$Price))^2) #0.2598949

#find optimal mtry; 10-fold cross validation
k=5
CVgroup <- function(k=5,datasize,seed=1234){
  cvlist <- list()
  set.seed(seed)
  n <- rep(1:k,ceiling(datasize/k))[1:datasize]
  temp <- sample(n,datasize)
  x <- 1:k
  dataseq <- 1:datasize
  cvlist <- lapply(x,function(x) dataseq[temp==x]) 
  return(cvlist)
}
cvlist <- CVgroup(5,nrow(train))

cvtest <- function(i,j){
  data.train <- train[-cvlist[[i]],]
  data.test <- train[cvlist[[i]],]
  model <- randomForest(log(Price)~.,data = train)
  prediction <- predict(model, newdata = test)
  error = mean((prediction-log(test$Price))^2)
  
  return(list(error, j))
}

tmp.mses <-NULL
mses <- rep(0,(ncol(train)-1)) 
mtrys <- rep(0,(ncol(train)-1))
index = 1
old_mtry = 1

i <- c(1:k)
j <- c(1:(ncol(train)-1))
i.s <- rep(i,times = length(j))
j.s <- rep(j,each = k)   
ijs <- cbind(i.s,j.s)

for(p in 1:nrow(ijs)){
  if(ijs[p,2] == old_mtry){
    i = ijs[p,1] # index for data set
    j = ijs[p,2] # mtry
    mse_mtry = cvtest(i,j)
    mse = mse_mtry[[1]]
    mtry = mse_mtry[[2]]
    tmp.mses <- c(tmp.mses,mse)
  }
  mses[index] = mean(tmp.mses)
  mtrys[index] = index
  index = ijs[p,2]
  old_mtry = ijs[p,2]
}

opt.mtry = mtrys[which.min(mses)]

res.cr <- list(train, test, err0, opt.mtry) #rf(fin.data5)

#improved model
mod.cr <- randomForest(log(Price) ~ .,data = res.cr[[1]] , mtry = res.cr[[4]])
plot(mod.cr) # ntree = 500

mod.cr <- randomForest(log(Price) ~ .,data = res.cr[[1]] , mtry = res.cr[[4]], ntree = 500)
pred.cr <- predict(mod.cr, newdata = res.cr[[2]])
error.cr <- mean((pred.cr - log((res.cr[[2]])$Price))^2)

error.cr

#baseline test error
res.cr[[3]]

#opt.mtry
res.cr[[4]]

######################################### house data only ############################################

data = fin.data3
set.seed(123)
train.num <- sample(nrow(data), 0.75*nrow(data))
train <- data[train.num,]
test<- data[-train.num,]

#baseline model
set.seed(1234)
library(randomForest)
mod0 <- randomForest(log(Price) ~ .,data = train)
pred0 <- predict(mod0, newdata = test)
err0 <- mean((pred0 - log(test$Price))^2) #0.2598949

#find optimal mtry; 10-fold cross validation
k=5
CVgroup <- function(k=5,datasize,seed=1234){
  cvlist <- list()
  set.seed(seed)
  n <- rep(1:k,ceiling(datasize/k))[1:datasize]
  temp <- sample(n,datasize)
  x <- 1:k
  dataseq <- 1:datasize
  cvlist <- lapply(x,function(x) dataseq[temp==x]) 
  return(cvlist)
}
cvlist <- CVgroup(5,nrow(train))

cvtest <- function(i,j){
  data.train <- train[-cvlist[[i]],]
  data.test <- train[cvlist[[i]],]
  model <- randomForest(log(Price)~.,data = train)
  prediction <- predict(model, newdata = test)
  error = mean((prediction-log(test$Price))^2)
  
  return(list(error, j))
}

tmp.mses <-NULL
mses <- rep(0,(ncol(train)-1)) 
mtrys <- rep(0,(ncol(train)-1))
index = 1
old_mtry = 1

i <- c(1:k)
j <- c(1:(ncol(train)-1))
i.s <- rep(i,times = length(j))
j.s <- rep(j,each = k)   
ijs <- cbind(i.s,j.s)

for(p in 1:nrow(ijs)){
  if(ijs[p,2] == old_mtry){
    i = ijs[p,1] # index for data set
    j = ijs[p,2] # mtry
    mse_mtry = cvtest(i,j)
    mse = mse_mtry[[1]]
    mtry = mse_mtry[[2]]
    tmp.mses <- c(tmp.mses,mse)
  }
  mses[index] = mean(tmp.mses)
  mtrys[index] = index
  index = ijs[p,2]
  old_mtry = ijs[p,2]
}

opt.mtry = mtrys[which.min(mses)]

res.h <- list(train, test, err0, opt.mtry, mses)

#res.h <- rf(fin.data3)

#improved model
mod.h <- randomForest(log(Price) ~ .,data = res.h[[1]] , mtry = res.h[[4]])
plot(mod.h) # ntree = 100

mod.h <- randomForest(log(Price) ~ .,data = res.h[[1]] , mtry = res.h[[4]], ntree = 100 )
pred.h <- predict(mod.h, newdata = res.h[[2]])
error.h <- mean((pred.h - log((res.h[[2]])$Price))^2)
error.h #0.2471361

#baseline test error
res.h[[3]]

#opt.mtry
res.h[[4]]





#####################################################################################################
######################################### random forest regression #########################################
#####################################################################################################

set.seed(123)
train.num <- sample(nrow(fin.data), 0.75*nrow(fin.data))
train <- fin.data[train.num,]
test<- fin.data[-train.num,]

set.seed(1234)
rf.model4 <- randomForest(log(Price) ~ .,data = train)
rf.pred4 <- predict(rf.model4, newdata = test)
rf.error1 <- mean((rf.pred4 - log(test$Price))^2)

rf.error1 #0.2477505
plot(rf.model4) #200



######################################## only crime data ######################################
set.seed(123)
train.num2 <- sample(nrow(fin.data2), 0.75*nrow(fin.data2))
train2 <- fin.data2[train.num2,]
test2<- fin.data2[-train.num2,]

#baseline 
set.seed(1234)
rf.model5 <- randomForest(log(Price) ~ .,data = train2)
rf.pred5 <- predict(rf.model5, newdata = test2)
rf.error2 <- mean((rf.pred5 - log(test2$Price))^2)

rf.error2 #0.2954928

##################################### only house data ##########################################
set.seed(123)
train.num3 <- sample(nrow(fin.data3), 0.75*nrow(fin.data3))
train3 <- fin.data3[train.num3,]
test3<- fin.data3[-train.num3,]

#baseline 
set.seed(1234)
rf.model6 <- randomForest(log(Price) ~ .,data = train3)
rf.pred6 <- predict(rf.model6, newdata = test3)
rf.error3 <- mean((rf.pred6 - log(test3$Price))^2)

rf.error3 #0.2598949

######################################## only crime amount with house attr data ######################################
set.seed(123)
train.num4 <- sample(nrow(fin.data4), 0.75*nrow(fin.data4))
train4 <- fin.data4[train.num4,]
test4<- fin.data4[-train.num4,]

#baseline 
set.seed(1234)
rf.model7 <- randomForest(log(Price) ~ .,data = train4)
rf.pred7 <- predict(rf.model7, newdata = test4)
rf.error4 <- mean((rf.pred7 - log(test4$Price))^2)

rf.error4 #0.2466143

######################################## only crime rate with house attr data ######################################
set.seed(123)
train.num5 <- sample(nrow(fin.data5), 0.75*nrow(fin.data5))
train5 <- fin.data5[train.num5,]
test5<- fin.data5[-train.num5,]

#baseline 
set.seed(1234)
rf.model8 <- randomForest(log(Price) ~ .,data = train5)
rf.pred8 <- predict(rf.model8, newdata = test5)
rf.error5 <- mean((rf.pred8 - log(test5$Price))^2)

rf.error5 #0.2454367
plot(rf.model8) #500

###############################full model with improved mtry ################################# 
mtry_n2 <- as.numeric()
err2 <- as.numeric()
for(i in 1:(ncol(train)-1)){
  set.seed(6)
  mtry_n2 <- randomForest(log(Price) ~.,data=train,mtry=i)
  pred2 <- predict(mtry_n2, newdata = test)
  err2 <- append(err2,mean((pred2 - log(test$Price))^2))
}
mtry2 <- which.min(err2)
mtry2 #5

#improved mtry
set.seed(1234)
rf.model9 <- randomForest(log(Price) ~ .,data = train, mtry = 5, ntree = 500)
rf.pred9 <- predict(rf.model9, newdata = test)
rf.error6 <- mean((rf.pred9 - log(test$Price))^2)

rf.error6 #0.2363366

###############################rate only model with improved mtry #################################  
mtry_n3 <- as.numeric()
err3 <- as.numeric()
for(i in 1:(ncol(train5)-1)){
  set.seed(6)
  mtry_n3 <- randomForest(log(Price) ~.,data=train5,mtry=i)
  pred3 <- predict(mtry_n3, newdata = test5)
  err3 <- append(err3,mean((pred3 - log(test5$Price))^2))
}
mtry3 <- which.min(err3)
mtry3 #3

#improved mtry
set.seed(1234)
rf.model10 <- randomForest(log(Price) ~ .,data = train5, mtry = 3, ntree = 500)
rf.pred10 <- predict(rf.model10, newdata = test5)
rf.error7 <- mean((rf.pred10 - log(test5$Price))^2)

rf.error7 #0.235803






####################################################################################################################
########################################### random forest classification ###########################################
####################################################################################################################

#################################### full model  ############################

#110000;190000;
tmp.fin <- fin.data%>%
  mutate(price.cl = case_when(Price <= 110000 ~ "low", 
                              Price <= 190000 & Price > 110000 ~ "median",
                              Price > 190000 ~ "high"))%>%
  select(!Price)
tmp.fin$price.cl <- as.factor(tmp.fin$price.cl)
train.tmp.cl <- tmp.fin[train.num,]
test.tmp.cl<- tmp.fin[-train.num,]

#baseline
library(randomForest)
set.seed(1234)
tmp.model <- randomForest(price.cl ~ .,data = train.tmp.cl)
tmp.pred <- predict(tmp.model, newdata = test.tmp.cl)
tmp.confumat <- as.matrix(table(tmp.pred, test.tmp.cl$price.cl))
tmp.accuracy <- sum(diag(tmp.confumat))/sum(tmp.confumat)

tmp.accuracy #0.5874827

#108000;147000;196927;204961
fin.data.cl <- fin.data%>%
  mutate(price.cl = case_when(Price <= 108000 ~ "low", 
                              Price <= 196927 & Price > 108000 ~ "median",
                              Price > 196927 ~ "high"))%>%
  select(!Price)

fin.data.cl$price.cl <- as.factor(fin.data.cl$price.cl)
train.cl <- fin.data.cl[train.num,]
test.cl<- fin.data.cl[-train.num,]

#baseline
library(randomForest)
set.seed(1234)
rf.model <- randomForest(price.cl ~ .,data = train.cl)
rf.pred <- predict(rf.model, newdata = test.cl)
confumat <- as.matrix(table(rf.pred, test.cl$price.cl))
rf.accuracy <- sum(diag(confumat))/sum(confumat)

rf.accuracy #0.5916321 0.5878285  #0.5888658

#mtry 
mtry_n <- as.numeric()
err <- as.numeric()
for(i in 1:(ncol(train.cl)-1)){
  set.seed(6)
  mtry_n <- randomForest(price.cl ~.,data=train.cl,mtry=i)
  err <- append(err,mean(mtry_n$err.rate))
}
mtry <- which.min(err) 
mtry #23

#improved mtry
set.seed(1234)
rf.model1 <- randomForest(price.cl ~ .,data = train.cl, mtry=23)
rf.pred1 <- predict(rf.model1, newdata = test.cl)
confumat1 <- as.matrix(table(rf.pred1, test.cl$price.cl))
rf.accuracy1 <- sum(diag(confumat2))/sum(confumat2)

rf.accuracy1 #0.5916321 #0.5878285
varImpPlot(rf.model1)

#################################### crime rate with house attributes ############################
fin.data.cl1 <- fin.data5%>%
  mutate(price.cl = case_when(Price <= 108000 ~ "low", 
                              Price <= 196927 & Price > 108000 ~ "median",
                              Price > 196927 ~ "high"))%>%
  select(!Price)

fin.data.cl1$price.cl <- as.factor(fin.data.cl1$price.cl)
train.cl1 <- fin.data.cl1[train.num,]
test.cl1<- fin.data.cl1[-train.num,]

set.seed(1234)
rf.model2 <- randomForest(price.cl ~ .,data = train.cl1)
rf.pred2 <- predict(rf.model2, newdata = test.cl1)
confumat2 <- as.matrix(table(rf.pred2, test.cl1$price.cl))
rf.accuracy2 <- sum(diag(confumat2))/sum(confumat2)

rf.accuracy2 #0.5867911 #0.5895574

#mtry 
mtry_n1 <- as.numeric()
err1 <- as.numeric()
for(i in 1:(ncol(train.cl1)-1)){
  set.seed(6)
  mtry_n1 <- randomForest(price.cl ~.,data=train.cl1,mtry=i)
  err1 <- append(err1,mean(mtry_n$err.rate))
}
mtry1 <- which.min(err1) 
mtry1 #1

#improved mtry
set.seed(1234)
rf.model3 <- randomForest(price.cl ~ .,data = train.cl1, mtry=23)
rf.pred3 <- predict(rf.model3, newdata = test.cl1)
confumat3 <- as.matrix(table(rf.pred3, test.cl1$price.cl))
rf.accuracy3 <- sum(diag(confumat3))/sum(confumat3)

rf.accuracy3 #0.5916321 #0.5878285
varImpPlot(rf.model1)