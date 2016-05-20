#Read In Data
data <- read.csv("C:/Users/Chris Liti/Desktop/R Data/German Credit/german_credit.csv")
View(data)
train <- read.csv("C:/Users/Chris Liti/Desktop/R Data/German Credit/Training50.csv")
test <- read.csv("C:/Users/Chris Liti/Desktop/R Data/German Credit/Test50.csv")
train <- train[,-1]
test <- test[,-1]
names(data)

setdiff(names(train),names(data))
View(train)

#Creating Marginal Propotional tables
names(train)
str(train)

library(ggplot2)
attach(data)
qplot(x=Value.Savings.Stocks,data=train)
qplot(x=Length.of.current.employment,data=train)
qplot(x=Duration.of.Credit..month.,data=train,geom="histogram",binwidth=6)+scale_x_continuous(breaks=seq(0,60,6))
qplot(x=Payment.Status.of.Previous.Credit,data=train)
qplot(x=Purpose,data=train)
qplot(x=Credit.Amount,data=train,geom="density",color=factor(Creditability))
qplot(x=Value.Savings.Stocks,data=train)
qplot(x=Instalment.per.cent,data=train)
qplot(x=Sex...Marital.Status,data=train)
qplot(x=Sex...Marital.Status,data=train)
qplot(x=Most.valuable.available.asset,data=train)
qplot(x=Guarantors,data=train)
unique(train$Guarantors)
qplot(x=Duration.in.Current.address,data=train)
ggplot(train,aes(x=Duration.in.Current.address,fill=factor(Creditability)))+geom_bar(position = "dodge")
qplot(x=Most.valuable.available.asset,data=train)
qplot(x=Age..years.,data=train)
qplot(x=Age..years.,data=train,geom="density",color=factor(Creditability))
qplot(x=Concurrent.Credits,data=train)
qplot(x=Type.of.apartment,data=train)
ggplot(data=train,aes(x=No.of.Credits.at.this.Bank,fill=factor(Creditability)))+geom_bar(position = "dodge")
unique(train$No.of.Credits.at.this.Bank)

table(Occupation)
ggplot(data=train,aes(x=No.of.dependents,fill=factor(Creditability)))+geom_bar(position = "dodge")
table(No.of.dependents)
ggplot(data=train,aes(x=Telephone,fill=factor(Creditability)))+geom_bar(position = "dodge")
ggplot(data=train,aes(x=Foreign.Worker,fill=factor(Creditability)))+geom_bar(position = "dodge")

margin.table(prop.table(table(Duration.in.Current.address, Most.valuable.available.asset, Concurrent.Credits,No.of.Credits.at.this.Bank,Occupation,No.of.dependents,Telephone, Foreign.Worker)),1)
margin.table(prop.table(table(Duration.in.Current.address, Most.valuable.available.asset, Concurrent.Credits,No.of.Credits.at.this.Bank,Occupation,No.of.dependents,Telephone, Foreign.Worker)),2)
margin.table(prop.table(table(Duration.in.Current.address, Most.valuable.available.asset, Concurrent.Credits,No.of.Credits.at.this.Bank,Occupation,No.of.dependents,Telephone, Foreign.Worker)),3)
margin.table(prop.table(table(Duration.in.Current.address, Most.valuable.available.asset, Concurrent.Credits,No.of.Credits.at.this.Bank,Occupation,No.of.dependents,Telephone, Foreign.Worker)),4)
margin.table(prop.table(table(Duration.in.Current.address, Most.valuable.available.asset, Concurrent.Credits,No.of.Credits.at.this.Bank,Occupation,No.of.dependents,Telephone, Foreign.Worker)),5)
margin.table(prop.table(table(Duration.in.Current.address, Most.valuable.available.asset, Concurrent.Credits,No.of.Credits.at.this.Bank,Occupation,No.of.dependents,Telephone, Foreign.Worker)),6)
margin.table(prop.table(table(Duration.in.Current.address, Most.valuable.available.asset, Concurrent.Credits,No.of.Credits.at.this.Bank,Occupation,No.of.dependents,Telephone, Foreign.Worker)),7)
margin.table(prop.table(table(Duration.in.Current.address, Most.valuable.available.asset, Concurrent.Credits,No.of.Credits.at.this.Bank,Occupation,No.of.dependents,Telephone, Foreign.Worker)),8)

prop.table(table(Purpose))*100

library(gmodels)
CrossTable(Creditability, Account.Balance, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(Creditability, Payment.Status.of.Previous.Credit, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(Creditability, Purpose, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)

sum(is.na(train))
summary(train$Occupation)
logistic_model1 <- glm(Creditability~.,family = binomial(link = "logit"),data=train)
summary(logistic_model1)
head(test)
test_creditability <- factor(test$Creditability)
testing <- test[,-1]

predicted1 <- predict(logistic_model1,data=test,type="response")
predicted1 <- ifelse(predicted1>0.5,1,0)


table(predicted1,test_creditability)
mean(predicted1==test_creditability)

#Updated model
logistic_model2 <- glm(Creditability~Account.Balance+Payment.Status.of.Previous.Credit+Purpose+Credit.Amount+Value.Savings.Stocks+Length.of.current.employment+Instalment.per.cent+Sex...Marital.Status+Concurrent.Credits,family=binomial(link="logit"),data=train)
summary(logistic_model2)
predicted2 <- predict(logistic_model2,data=test,type="response")
predicted2 <- ifelse(predicted2>0.5,1,0)
table(predicted2,test_creditability)
mean(predicted2==test_creditability)

list <- ("Creditability","Account.Balance","Duration.of.Credit..month.","Payment.Status.of.Previous.Credit","Purpose","Credit.Amount","Value.Savings.Stocks","Length.of.current.employment","Instalment.per.cent", "Sex...Marital.Status" ,"Guarantors", "Duration.in.Current.address",
         "Most.valuable.available.asset"."Duration.in.Current.address", "Most.valuable.available.asset","Concurrent.Credits","Type.of.apartment","No.of.Credits.at.this.Bank","Occupation")

train1 <- lapply(train,as.factor)
str(train1)
train1$Age..years. <- as.numeric(train1$Age..years.)
train1$Credit.Amount <- as.numeric(train1$Credit.Amount)
test1 <- lapply(test,as.factor)
test1$Age..years. <- as.numeric(test1$Age..years.)
test1$Credit.Amount <- as.numeric(test1$Credit.Amount)
train1$Duration.of.Credit..month. <- as.numeric(train1$Duration.of.Credit..month.)
test1$Duration.of.Credit..month. <- as.numeric(test1$Duration.of.Credit..month.)
logistic_model3 <- glm(Creditability~Account.Balance+Payment.Status.of.Previous.Credit+Purpose+Credit.Amount+Value.Savings.Stocks+Length.of.current.employment+Instalment.per.cent+Sex...Marital.Status+Concurrent.Credits,family=binomial(link="logit"),data=train1)
summary(logistic_model3)
predicted3 <- predict(logistic_model3,data=test,type="response")
predicted3 <- ifelse(predicted2>0.5,1,0)
table(predicted3,test_creditability)
mean(predicted3==test_creditability)

library(ROCR)
pr <- prediction(predicted3,test1$Creditability)
perf <- performance(pr, "tpr","fpr")
plot(perf)
