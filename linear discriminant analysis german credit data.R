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

#Linear discriminant Analysis
library(MASS)
lda_model <- lda(Creditability~Account.Balance+Payment.Status.of.Previous.Credit+Purpose+Credit.Amount+Value.Savings.Stocks+Length.of.current.employment+Instalment.per.cent+Sex...Marital.Status+Concurrent.Credits,data=train)
lda_predicted <- predict(lda_model,test)
lda_pred_class <- lda_predicted$class

#Confusion Matrix
table(lda_pred_class,test$Creditability)
mean(lda_pred_class==test$Creditability)
