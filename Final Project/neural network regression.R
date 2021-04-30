#####################################################################################################
############################################ data ############################################
#####################################################################################################
library(tidyr)
library(dplyr)
library(fastDummies)
library(MASS)
library(neuralnet)

fin.data <- read.csv("data/crime-pp-Wales.csv")%>%
  dplyr::select(!c(X, Transaction.unique.identifier, Postcode, PAON, SAON, Street, Locality, 
                   Town.City, District, County, PPD.Category.Type, Record.Status, population, Month, Date.of.Transfer))%>%
  drop_na()%>%
  mutate(area = substr(postcode.sector,1,2))%>%
  mutate(is.SA = case_when(area == "SA" ~ "Y", 
                           area != "SA" ~ "N"))%>%
  dplyr::select(!c(area,postcode.sector))%>%
  dummy_cols(c("Property.Type", "Old.New", "Duration", "is.SA"))%>%
  mutate(price.log = log(Price))%>%
  dplyr::select(!c(Price, Property.Type, Old.New, Duration, is.SA))

fin.data2 <- read.csv("data/crime-pp-Wales.csv")%>%
  dplyr::select(!c(X, Transaction.unique.identifier, Postcode, PAON, SAON, Street, Locality, 
                   Town.City, District, County, PPD.Category.Type, Record.Status, population, Month, Date.of.Transfer))%>%
  drop_na()%>%
  mutate(area = substr(postcode.sector,1,2))%>%
  mutate(is.SA = case_when(area == "SA" ~ "Y", 
                             area != "SA" ~ "N"))%>%
  dplyr::select(!c(area,postcode.sector))%>%
  dummy_cols(c("is.SA"))%>%
  mutate(price.log = log(Price))%>%
  dplyr::select(!c(Price, Property.Type, Old.New, Duration, is.SA))

fin.data3 <- read.csv("data/crime-pp-Wales.csv")%>%
  dplyr::select(!c(X, Transaction.unique.identifier, Postcode, PAON, SAON, Street, Locality, 
                   Town.City, District, County, PPD.Category.Type, Record.Status, population, Month, Date.of.Transfer))%>%
  drop_na()%>%
  mutate(area = substr(postcode.sector,1,2))%>%
  mutate(is.SA = case_when(area == "SA" ~ "Y", 
                           area != "SA" ~ "N"))%>%
  dplyr::select(!c(area,postcode.sector))%>%
  dplyr::select(c(Price, Property.Type, Old.New, Duration, is.SA))%>%
  dummy_cols(c("Property.Type", "Old.New", "Duration", "is.SA"))%>%
  mutate(price.log = log(Price))%>%
  dplyr::select(!c(Price, Property.Type, Old.New, Duration, is.SA))

fin.data4 <- fin.data %>%
  dplyr::select(!ends_with(".rate"))

fin.data5 <- fin.data %>%
  dplyr::select(!ends_with(".amount"))

#####################################################################################################
###################################### Automatic neural network regression ###########################
#####################################################################################################

nn <- function(data){
  set.seed(123)
  train.num <- sample(nrow(data), 0.75*nrow(data))
  train <- data[train.num,]
  test <- data[-train.num,]
  
  names=names(data)
  f = as.formula(paste("price.log ~", paste(names[!names %in% "price.log"], collapse = " + ")))
  
  #scaled
  train.original <- train
  test.original <- test
  
  maxs=apply(train.original, 2, max) 
  mins=apply(train.original, 2, min)
  
  train.scaled=as.data.frame(scale(train.original, center = mins, scale = maxs - mins))
  test.scaled=as.data.frame(scale(test.original, center = mins, scale = maxs - mins))
  
  #baseline model
  set.seed(1234)
  nn0=neuralnet(f,data=train.scaled, hidden=1, threshold=0.01,
               rep=1, startweights = NULL, algorithm = "rprop+",
               err.fct="sse", act.fct="logistic",linear.output=T)
  
  tmpInd <- ncol(train)

  pred0 = compute(nn0,test.scaled[,-tmpInd])
  nn.pred0 = pred0$net.result*(max(train.original$price.log)
                               -min(train.original$price.log))+min(train.original$price.log)
  err0 <- mean((nn.pred0 - test.original[,tmpInd])^2) #0.2469199;2 #0.2459615;1 #0.8256499
  
  
  #find optimal number of neurons in the single layer; 10-fold cross validation
  # separate data for 5-fold cross validation;  
  k = 5
  cv.group<-function(k=5,datasize,seed){
    cvlist<-list()
    set.seed(seed)
    n<-rep(1:k,ceiling(datasize/k))[1:datasize]
    temp<-sample(n,datasize)
    x<-1:k
    dataseq<-1:datasize
    cvlist<-lapply(x,function(x) dataseq[temp==x])  
    return(cvlist)
  }
  whole.cv.list = cv.group(5,nrow(train.scaled),2)
  
  #implement neural network model and get the test error one time
  require(neuralnet)
  cv.test.nn<-function(i,j){
    
    #i refers to i-th dataset, the testing data
    #j refers to the number of neurons
    data.train <- train.scaled[-whole.cv.list[[i]],]
    data.test<- train.scaled[whole.cv.list[[i]],]
    
    nn = neuralnet(f, data = data.train, hidden = j, threshold=0.01, 
                   algorithm = "rprop+", err.fct="sse", 
                   act.fct="logistic",linear.output=T)
    

    Ind <- ncol(data.train)
    pred = compute(nn,data.test[,-Ind])
    nn.pred = pred$net.result*(max(data.train$price.log)
                               -min(data.train$price.log))+min(data.train$price.log)
    mse <- mean((nn.pred - data.test[,Ind])^2)
    return(list(mse,j))
  }
  
  #get the optimal number of neurons in the single layer for neural network model
  i <- c(1:5)
  j <- c(1:(ncol(train.scaled)-1)) # the number of neurons that we need to test
  
  i.s<-rep(i,times=length(j))
  j.s<-rep(j,each=5) 
  ijs <- cbind(i.s,j.s)	
  
  #i = 0
  #j = 0
  
  tmp.mses <-NULL
  mses <- rep(0,(ncol(train.scaled)-1)) 
  neus <- rep(0,(ncol(train.scaled)-1))
  index = 1
  old_neu = 10
  
  for(p in 1:nrow(ijs)){
    if(ijs[p,2] == old_neu){
      i = ijs[p,1]
      j = ijs[p,2]
      mse_neu = cv.test.nn(i,j)
      mse = mse_neu[[1]]
      neu = mse_neu[[2]]
      tmp.mses <- c(tmp.mses,mse)
    }
    mses[index] = mean(tmp.mses)
    neus[index] = index
    index = ijs[p,2]
    old_neu = ijs[p,2]
  }
  
  opt.neu = neus[which.min(mses)]
  

  return(list(f, train.scaled, test.scaled,
              train.original, test.original,
              err0, opt.neu))
}


######################################### full data ############################################
#res.full <- nn(fin.data)
data = fin.data
set.seed(123)
train.num <- sample(nrow(data), 0.75*nrow(data))
train <- data[train.num,]
test <- data[-train.num,]

names=names(data)
f = as.formula(paste("price.log ~", paste(names[!names %in% "price.log"], collapse = " + ")))

#scaled
train.original <- train
test.original <- test

maxs=apply(train.original, 2, max) 
mins=apply(train.original, 2, min)

train.scaled=as.data.frame(scale(train.original, center = mins, scale = maxs - mins))
test.scaled=as.data.frame(scale(test.original, center = mins, scale = maxs - mins))

#baseline model
set.seed(1234)
nn0=neuralnet(f,data=train.scaled, hidden=1, threshold=0.01,
              rep=1, startweights = NULL, algorithm = "rprop+",
              err.fct="sse", act.fct="logistic",linear.output=T)


pred0 = predict(nn0,test.scaled)
nn.pred0 = pred0*(max(train.original$price.log)
                             -min(train.original$price.log))+min(train.original$price.log)
err0 <- mean((nn.pred0 - test.original$price.log)^2) #0.2538712

#improved model
mod.full <- neuralnet(res.full[[1]], data = res.full[[2]], hidden = res.full[[7]], 
                      threshold=0.01, algorithm = "rprop+", err.fct="sse", act.fct="logistic",linear.output=T)


Ind.full <- ncol(res.full[[3]])
pred.full = compute(nn,res.full[[3]][,-Ind.full])
pred.full2 = pred.full$net.result*(max(res.full[[4]]$price.log)
                                   -min(res.full[[4]]$price.log))+min(res.full[[4]]$price.log)

mse.full <- mean((pred.full2 - res.full[[5]][,Ind])^2)

#baseline test error
res.full[[6]]

######################################### crime data only ############################################

#res.c <- nn(fin.data2)
data = fin.data2
set.seed(123)
train.num <- sample(nrow(data), 0.75*nrow(data))
train <- data[train.num,]
test <- data[-train.num,]

names=names(data)
f = as.formula(paste("price.log ~", paste(names[!names %in% "price.log"], collapse = " + ")))

#scaled
train.original <- train
test.original <- test

maxs=apply(train.original, 2, max) 
mins=apply(train.original, 2, min)

train.scaled=as.data.frame(scale(train.original, center = mins, scale = maxs - mins))
test.scaled=as.data.frame(scale(test.original, center = mins, scale = maxs - mins))

#baseline model
set.seed(1234)
nn0=neuralnet(f,data=train.scaled, hidden=1, threshold=0.01,
              rep=1, startweights = NULL, algorithm = "rprop+",
              err.fct="sse", act.fct="logistic",linear.output=T)


pred0 = predict(nn0,test.scaled)
nn.pred0 = pred0*(max(train.original$price.log)
                  -min(train.original$price.log))+min(train.original$price.log)
err0 <- mean((nn.pred0 - test.original$price.log)^2) # 0.3088335

data = fin.data2
set.seed(123)
train.num <- sample(nrow(data), 0.75*nrow(data))
train.original <- data[train.num,]
test.original <- data[-train.num,]

names=names(data)
f = as.formula(paste("price.log ~", paste(names[!names %in% "price.log"], collapse = " + ")))
maxs=apply(train.original, 2, max) 
mins=apply(train.original, 2, min)
train.scaled=as.data.frame(scale(train.original, center = mins, scale = maxs - mins))
test.scaled=as.data.frame(scale(test.original, center = mins, scale = maxs - mins))
test.err.all.ca=rep(NA,(ncol(train.original)-1))

for(i in 1:(ncol(train.original)-1)){
  nn=neuralnet(f,data=train.scaled, hidden=i, threshold=0.01,
               err.fct="sse", act.fct="logistic",linear.output=T)
  
  nn.scaled.prediction=predict(nn,test.scaled)
  
  nn.prediction=nn.scaled.prediction*(max(train.original$Y.train)-min(train.original$Y.train))+min(train.original$Y.train)
  
  test.err.all.ca[i] = mean((nn.prediction-test.original$price.log)^2)
}

plot(1:(ncol(train.original)-1),test.err.all.c,xlab="The number of neurons",ylab="Errors",type="l")


#improved model
mod.c <- neuralnet(res.c[[1]], data = res.c[[2]], hidden = res.c[[7]], 
                      threshold=0.01, algorithm = "rprop+", err.fct="sse", act.fct="logistic",linear.output=T)


Ind.c <- ncol(res.c[[3]])
pred.c = compute(nn,res.c[[3]][,-Ind.c])
pred.c2 = pred.c$net.result*(max(res.c[[4]]$price.log)
                                   -min(res.c[[4]]$price.log))+min(res.c[[4]]$price.log)

mse.c <- mean((pred.c2 - res.c[[5]][,Ind])^2)

#baseline test error
res.c[[6]]

######################################### house data only ############################################
data = fin.data3
set.seed(123)
train.num <- sample(nrow(data), 0.75*nrow(data))
train <- data[train.num,]
test <- data[-train.num,]

names=names(data)
f = as.formula(paste("price.log ~", paste(names[!names %in% "price.log"], collapse = " + ")))

#scaled
train.original <- train
test.original <- test

maxs=apply(train.original, 2, max) 
mins=apply(train.original, 2, min)

train.scaled=as.data.frame(scale(train.original, center = mins, scale = maxs - mins))
test.scaled=as.data.frame(scale(test.original, center = mins, scale = maxs - mins))

#baseline model
set.seed(1234)
nn0=neuralnet(f,data=train.scaled, hidden=1, threshold=0.01,
              rep=1, startweights = NULL, algorithm = "rprop+",
              err.fct="sse", act.fct="logistic",linear.output=T)

pred0 = predict(nn0,test.scaled)
nn.pred0 = pred0*(max(train.original$price.log)
                  -min(train.original$price.log))+min(train.original$price.log)
err0 <- mean((nn.pred0 - test.original$price.log)^2) #0.2500527


#find optimal number of neurons in the single layer; 10-fold cross validation
# separate data for 5-fold cross validation;  
k = 5
cv.group<-function(k=5,datasize,seed){
  cvlist<-list()
  set.seed(seed)
  n<-rep(1:k,ceiling(datasize/k))[1:datasize]
  temp<-sample(n,datasize)
  x<-1:k
  dataseq<-1:datasize
  cvlist<-lapply(x,function(x) dataseq[temp==x])  
  return(cvlist)
}
whole.cv.list = cv.group(5,nrow(train.scaled),2)

#implement neural network model and get the test error one time
library(neuralnet)
cv.test.nn2<-function(i,j){
  
  #i refers to i-th dataset, the testing data
  #j refers to the number of neurons
  data.train <- train.scaled[-whole.cv.list[[i]],]
  data.test<- train.scaled[whole.cv.list[[i]],]
  
  nn = neuralnet(f, data = data.train, hidden = j, threshold=0.01, 
                 algorithm = "rprop+", err.fct="sse", 
                 act.fct="logistic",linear.output=T)
  
  
  #Ind <- ncol(data.test)
  pred = predict(nn,data.test)
  nn.pred = pred*(max(data.train$price.log)
                             -min(data.train$price.log))+min(data.train$price.log)
  mse <- mean((nn.pred - data.test$price.log)^2)
  return(list(mse,j))
}
cv.test.nn<-function(i,j){
  
  #i refers to i-th dataset, the testing data
  #j refers to the number of neurons
  data.train <- train.original[-whole.cv.list[[i]],]
  data.test<- train.original[whole.cv.list[[i]],]
  
  maxs=apply(data.train, 2, max) 
  mins=apply(data.train, 2, min)
  
  data.train.scaled=as.data.frame(scale(data.train, center = mins, scale = maxs - mins))
  data.test.scaled=as.data.frame(scale(data.test, center = mins, scale = maxs - mins))
  
  nn = neuralnet(f, data = data.train.scaled, hidden = j, threshold=0.01, 
                 algorithm = "rprop+", err.fct="sse", 
                 act.fct="logistic",linear.output=T)
  
  pred = predict(nn, data.test.scaled)
  nn.pred = pred*(max(data.train$price.log)-min(data.train$price.log))+min(data.train$price.log)
  
  mse <- mean((nn.pred - data.test$price.log)^2)
  return(list(mse,j))
}
#get the optimal number of neurons in the single layer for neural network model
i <- c(1:5)
j <- c(1:(ncol(train.scaled)-1)) # the number of neurons that we need to test

i.s<-rep(i,times=length(j))
j.s<-rep(j,each=5) 
ijs <- cbind(i.s,j.s)	

#i = 0
#j = 0

tmp.mses <-NULL
mses <- rep(0,(ncol(train.scaled)-1)) 
neus <- rep(0,(ncol(train.scaled)-1))
index = 1
old_neu = 1

for(p in 1:nrow(ijs)){
  if(ijs[p,2] == old_neu){
    i = ijs[p,1]
    j = ijs[p,2]
    mse_neu = cv.test.nn(i,j)
    mse = mse_neu[[1]]
    neu = mse_neu[[2]]
    tmp.mses <- c(tmp.mses,mse)
  }
  mses[index] = mean(tmp.mses)
  neus[index] = index
  index = ijs[p,2]
  old_neu = ijs[p,2]
}

opt.neu = neus[which.min(mses)] #11
mses.h <- mses

#res.h <- nn(fin.data3)
res.h <- list(f, train.scaled, test.scaled, train.original, test.original, err0, opt.neu, mses.h)

#improved model
mod.h <- neuralnet(res.h[[1]], data = res.h[[2]], hidden = res.h[[7]], 
                      threshold=0.01, algorithm = "rprop+", err.fct="sse", act.fct="logistic",linear.output=T)

Ind.h <- ncol(res.h[[3]])
pred.h = compute(mod.h,(res.h[[3]])[,-Ind.h])
pred.h2 = pred.h$net.result*(max(res.h[[4]]$price.log)
                                   -min(res.h[[4]]$price.log))+min(res.h[[4]]$price.log)

mse.h <- mean((pred.h2 - res.h[[5]][,Ind.h])^2)
mse.h
#baseline test error
res.h[[6]] #0.2500527

#opt.neu
res.h[[7]]

######################################### crime amounts with house ############################################

data = fin.data4
set.seed(123)
train.num <- sample(nrow(data), 0.75*nrow(data))
train <- data[train.num,]
test <- data[-train.num,]

names=names(data)
f = as.formula(paste("price.log ~", paste(names[!names %in% "price.log"], collapse = " + ")))

#scaled
train.original <- train
test.original <- test

maxs=apply(train.original, 2, max) 
mins=apply(train.original, 2, min)

train.scaled=as.data.frame(scale(train.original, center = mins, scale = maxs - mins))
test.scaled=as.data.frame(scale(test.original, center = mins, scale = maxs - mins))

#baseline model
set.seed(1234)
nn0=neuralnet(f,data=train.scaled, hidden=1, threshold=0.01,
              rep=1, startweights = NULL, algorithm = "rprop+",
              err.fct="sse", act.fct="logistic",linear.output=T)


pred0 = predict(nn0,test.scaled)
nn.pred0 = pred0*(max(train.original$price.log)
                  -min(train.original$price.log))+min(train.original$price.log)
err0 <- mean((nn.pred0 - test.original$price.log)^2) #




set.seed(123)
train.num <- sample(nrow(data), 0.75*nrow(data))
train.original <- data[train.num,]
test.original <- data[-train.num,]

names=names(data)
f = as.formula(paste("price.log ~", paste(names[!names %in% "price.log"], collapse = " + ")))
maxs=apply(train.original, 2, max) 
mins=apply(train.original, 2, min)
train.scaled=as.data.frame(scale(train.original, center = mins, scale = maxs - mins))
test.scaled=as.data.frame(scale(test.original, center = mins, scale = maxs - mins))
test.err.all.ca=rep(NA,(ncol(train.original)-1))

for(i in 1:(ncol(train.original)-1)){
  nn=neuralnet(f,data=train.scaled, hidden=i, threshold=0.01,
               err.fct="sse", act.fct="logistic",linear.output=T)
  
  nn.scaled.prediction=predict(nn,test.scaled)
  
  nn.prediction=nn.scaled.prediction*(max(train.original$Y.train)-min(train.original$Y.train))+min(train.original$Y.train)
  
  test.err.all.ca[i] = mean((nn.prediction-test.original$price.log)^2)
}

plot(1:(ncol(train.original)-1),test.err.all.ca,xlab="The number of neurons",ylab="Errors",type="l")

#improved model
mod.ca <- neuralnet(res.ca[[1]], data = res.ca[[2]], hidden = res.ca[[7]], 
                      threshold=0.01, algorithm = "rprop+", err.fct="sse", act.fct="logistic",linear.output=T)


Ind.ca <- ncol(res.ca[[3]])
pred.ca = compute(nn,res.ca[[3]][,-Ind.ca])
pred.ca2 = pred.ca$net.result*(max(res.ca[[4]]$price.log)
                                   -min(res.ca[[4]]$price.log))+min(res.ca[[4]]$price.log)

mse.ca <- mean((pred.ca2 - res.ca[[5]][,Ind])^2)

#baseline test error
res.ca[[6]]


######################################### crime rates with house ############################################

data = fin.data5
set.seed(123)
train.num <- sample(nrow(data), 0.75*nrow(data))
train.original <- data[train.num,]
test.original <- data[-train.num,]

names=names(data)
f = as.formula(paste("price.log ~", paste(names[!names %in% "price.log"], collapse = " + ")))
maxs=apply(train.original, 2, max) 
mins=apply(train.original, 2, min)
train.scaled=as.data.frame(scale(train.original, center = mins, scale = maxs - mins))
test.scaled=as.data.frame(scale(test.original, center = mins, scale = maxs - mins))
test.err.all=rep(NA,10)

for(i in 1:10){
  nn=neuralnet(f,data=train.scaled, hidden=i, threshold=0.01,
               err.fct="sse", act.fct="logistic",linear.output=T)
  
  nn.scaled.prediction=predict(nn,test.scaled)
  
  nn.prediction=nn.scaled.prediction*(max(train.original$Y.train)-min(train.original$Y.train))+min(train.original$Y.train)
  
  test.err.all[i] = mean((nn.prediction-test.original$price.log)^2)
}

plot(1:10),test.err.all,xlab="The number of neurons",ylab="Errors",type="l")


####################################################another way#################################
res.cr <- list(f, train.scaled, test.scaled, train.original, test.original, err0, opt.neu, mses)

#improved model
mod.cr <- neuralnet(res.cr[[1]], data = res.cr[[2]], hidden = res.cr[[7]], 
                   threshold=0.01, algorithm = "rprop+", err.fct="sse", act.fct="logistic",linear.output=T)

pred.cr = predict(mod.cr,res.cr[[3]])
pred.cr2 = pred.cr*(max(res.cr[[4]]$price.log)
                    -min(res.cr[[4]]$price.log))+min(res.cr[[4]]$price.log)

mse.cr <- mean((pred.cr2 - res.cr[[5]]$price.log)^2)
mse.cr

#baseline test error
res.cr[[6]] #0.2500527

#opt.neu
res.cr[[7]]

#mses
res.cr[[8]]

######################################################################################################
























#####################################################################################################
######################################### neural network regression #########################################
#####################################################################################################

# Neural Network for Regression
library(MASS)
library(fastDummies)

fin.data.nn <- fin.data%>%
  dummy_cols(c("Property.Type", "Old.New", "Duration", "is.SA"))%>%
  mutate(price.log = log(Price))%>%
  dplyr::select(!c("Price","Property.Type", "Old.New", "Duration", "is.SA"))
  
# We scale and split the data before moving on.
# The min-max method is used to scale the data
index=sample(1:nrow(fin.data.nn),round(0.75*nrow(fin.data.nn)))
train.original=fin.data.nn[index,]
test.original=fin.data.nn[-index,]

maxs=apply(train.original, 2, max) 
mins=apply(train.original, 2, min)
train.scaled=as.data.frame(scale(train.original, center = mins, scale = maxs - mins))
test.scaled=as.data.frame(scale(test.original, center = mins, scale = maxs - mins))

# For some reason the formula y~. is not accepted in the neuralnet() function. 
# You need to first write the formula and then pass it as an argument in the fitting function.
names=names(train.original)
f =as.formula(paste("price.log ~", paste(names[!names %in% "price.log"], collapse = " + ")))
f
nn=neuralnet(f,data=train.scaled, hidden=4, threshold=0.01,
              rep=1, startweights = NULL, algorithm = "rprop+",
              err.fct="sse", act.fct="logistic",linear.output=T)
#plot(nn)
nn.scaled.prediction=compute(nn,test.scaled[,-42])
nn.prediction=nn.scaled.prediction$net.result*(max(train.original$price.log)
                                               -min(train.original$price.log))+min(train.original$price.log)
mean((nn.prediction-test.original[,42])^2) #0.2469199;2 #0.2459615;1 #0.8256499；3 # ；4


h <- as.numeric()
terr <- as.numeric()
for(i in 1:(2*ncol(train.scaled))){
  set.seed(6)
  tnn=neuralnet(f,data=train.scaled, hidden=i, threshold=0.01,
               rep=1, startweights = NULL, algorithm = "rprop+",
               err.fct="sse", act.fct="logistic",linear.output=T)
  tnn.scaled.prediction=compute(tnn,test.scaled[,-42])
  tnn.prediction=tnn.scaled.prediction$net.result*(max(train.original$price.log)
                                                 -min(train.original$price.log))+min(train.original$price.log)
  
  terr <- append(terr,mean((tnn.prediction-test.original[,42])^2))
}
h <- which.min(terr)
h



