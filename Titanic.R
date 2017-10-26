```{r}
setwd("C:/Users/sivam/Downloads/ML/DATA")
getwd()
train = read.csv("train.csv")
summary(train)
```
```{r}
test = read.csv("test.csv")
summary(test)
```
```{r}
train$Age[train$Sex == 'male' & is.na(train$Age)] = median(train$Age[train$Sex == 'male'],na.rm = TRUE)
train$Age[train$Sex == 'female' & is.na(train$Age)] = median(train$Age[train$Sex == 'female'],na.rm = TRUE)
summary(train$Age)
prop.table(table(train$Age<1,train$Survived),1)

```

```{r}
test$Survived <- rep(0, 418)
summary(test$Age)
test$Age[test$Sex == 'male' & is.na(test$Age)] = median(test$Age[test$Sex == 'male'],na.rm = TRUE)
test$Age[test$Sex == 'female' & is.na(test$Age)] = median(test$Age[test$Sex == 'female'],na.rm = TRUE)
```

```{r}
test$Survived[test$Age<1] <- 1
test$Survived[test$Sex == 'female'] <- 1
prop.table(table(test$Sex,test$Survived),1)
```

```{r}
train$Child <- 0

train$Child[train$Age < 18] <- 1
prop.table(table(train$Child,train$Survived),1)
```

```{r}
aggregate(Survived ~ Child + Sex, data=train, FUN=sum)

aggregate(Survived ~ Child + Sex, data=train, FUN=length)

aggregate(Survived ~ Child + Sex, data=train, FUN=function(x) {sum(x)/length(x)})
```
```{r}
library(randomForest)

train$Fare[is.na(train$Fare)] = median(train$Fare,na.rm = TRUE)

test$Fare[is.na(test$Fare)] = median(test$Fare,na.rm = TRUE)

TitanicRF = randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,data = train, ntree=2500, importance=TRUE)

summary(TitanicRF)

Prediction <- predict(TitanicRF , test, type = "response")
summary(Prediction)
```

```{r}
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)

write.csv(submit, file = "Titanic.csv", row.names = FALSE)
summary(test$Survived)
```
