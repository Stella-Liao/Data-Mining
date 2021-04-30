4)lda in package
```{r}
library (MASS)
lda.fit <- lda(V1~., data = train)
#lda.fit=lda(train[,-1],train[,1] )
# The test error is 
test.pred=predict(lda.fit,test[,-1])$class
lda.test.error=mean(test.pred != test$V1)
lda.test.error

# Confusion matrix
table(test.pred, test$V1)
```