
library(e1071)
library(rpart)
library(caTools)
library(class)
library(dplyr)
library(plyr)
library(lubridate)
set.seed(9000)
stocks <- read.csv('IND1B.csv')
View(stocks)
subset_mydata <- stocks[,c(3,4,5,6,7,8)]
View(subset_mydata)
#split


train <- subset(subset_mydata, temp_field==TRUE)
test <- subset(subset_mydata, temp_field==FALSE)
summary(train)
summary(test)
head(train)
head(test)
fit <- rpart(train$vol~.,data=train,method="class")
plot(fit)
text(fit)
pred <- predict(fit,newdata=test[,-1],type=("class"))

subset_mydata$pred <-
  apply(subset_mydata,1, function(x) ifelse(any( x[1]< 6.5 & x[2]<50 & x[3]<45 & x[4]<5 & x[5]<5),'BClean','BDirty'))
  View(subset_mydata) 
  
  count(subset_mydata,'BClean')
  
  


predictors <- cbind(lag(stocks$pH, default = 6.5), lag(stocks$solids, default = 50), lag(stocks$hardness, default = 45),lag(stocks$oil, default = 5),lag(stocks$bod, default = 5))
prediction <- knn(predictors[train, ], predictors[!train, ], stocks$vol[train], k = 1)
