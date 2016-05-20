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

#Decision Trees
library(rpart)
tree_model <- rpart(Creditability~Account.Balance+Payment.Status.of.Previous.Credit+Purpose+Credit.Amount+Value.Savings.Stocks+Length.of.current.employment+Instalment.per.cent+Sex...Marital.Status+Concurrent.Credits,data=train)

#Plot Tree
library(rattle)
fancyRpartPlot(tree_model)
plot(tree_model)
text(tree_model,pretty = 0)



#Make Prediction
tree_model_pred <- predict(tree_model,test,type="class")

#Confusion Matrix
table(tree_model_pred,test$Creditability)
mean(tree_model_pred==test$Creditability)

#Prune tree
printcp(tree_model)
plotcp(tree_model)
summary(tree_model)

pruned_tree <- prune(tree_model,cp=tree_model$cptable[which.min(tree_model$cptable[,"xerror"]),"CP"])
pruned_tree_pred <- predict(pruned_tree,test,type="class")
table(pruned_tree_pred,test$Creditability)
mean(pruned_tree_pred==test$Creditability)

#Use tree package
library(tree)
tree_model2 <- tree(Creditability~Account.Balance+Payment.Status.of.Previous.Credit+Purpose+Credit.Amount+Value.Savings.Stocks+Length.of.current.employment+Instalment.per.cent+Sex...Marital.Status+Concurrent.Credits,data=train)
plot(tree_model2)
text(tree_model2,pretty = 0)

#Make Prediction
tree_model_pred2 <- predict(tree_model2,test,type="class")

#Confusion Matrix
table(tree_model_pred2,test$Creditability)
mean(tree_model_pred2==test$Creditability)

#Prune tree
#Cross Validate
cv_tree <- cv.tree(tree_model2,FUN = prune.misclass)
names(cv_tree)

plot(cv_tree$size,cv_tree$dev,type="b")
pruned_tree2 <- prune.misclass(tree_model2,best = 6)

pruned_tree_pred2 <- predict(pruned_tree2,test,type = "class")
table(pruned_tree_pred2,test$Creditability)
mean(pruned_tree_pred2==test$Creditability)

