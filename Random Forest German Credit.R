#Read In Data
data <- read.csv("C:/Users/Chris Liti/Desktop/R Data/German Credit/german_credit.csv")
View(data)
train <- read.csv("C:/Users/Chris Liti/Desktop/R Data/German Credit/Training50.csv")
test <- read.csv("C:/Users/Chris Liti/Desktop/R Data/German Credit/Test50.csv")
train <- train[,-1]
test <- test[,-1]
names(data)

cols <- c(1,2,4,5,7,8,9,10,11,12,13,15,16,17,18,19,20,21)

#convert some variables into factors
train[,cols] <- lapply(train[,cols],factor)
str(train)

test[,cols] <- lapply(test[,cols],factor)
str(test)

#Train Using Random Forests
library(randomForest)
rf_model <- randomForest(Creditability~.,data=train,mtry=5,ntree=200)
plot(rf_model)
varImpPlot(rf_model)

#Make Prediction
rf_predicted <- predict(rf_model,test)

#Confusion Matrix
table(rf_predicted,test$Creditability)
mean(rf_predicted==test$Creditability)

rf_model2 <- randomForest(Creditability~Account.Balance+Payment.Status.of.Previous.Credit+Purpose+Credit.Amount+Value.Savings.Stocks+Length.of.current.employment+Instalment.per.cent+Sex...Marital.Status+Concurrent.Credits,data=train,mtry=5,ntree=200)
#Make Prediction
rf_predicted2 <- predict(rf_model2,test)

#Confusion Matrix
table(rf_predicted2,test$Creditability)
mean(rf_predicted2==test$Creditability)
