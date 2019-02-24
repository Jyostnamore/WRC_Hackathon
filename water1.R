library(caTools)
library(rpart)
library(e1071)
library(datasets)
names(water)
dim(water)
mydata <-read.csv("IND1B", header = TRUE, sep=",")
View(water)
str(water)
subset_mydata<-water[,c(2,3)]
View(subset_mydata)
View(IND1B)
temp_field<-water.split(subset_mydata, SplitRatio=0.9)

train<-subset(subset_mydata, temp_field==TRUE)
test<-subset(subset_mydata, temp_field==FALSE)

summary(train)
summary(test)

head(train)
head(test)

fit<-rpart(train$vol~., data=train, method="class")
#fit<-rpart(train$Member.type~., data=train, method="class",control = rpart.control(cp=0,maxdepth = 8, minsplit = 100))
plot(fit)
text(fit)
pred<-predict(fit,newdata=test[,-4],type=("class"))


c <- mean(water$vol)
View(c)

d <- mean(IND1B$vol)
View(d)
cal <-(d/c)*100
View(cal)


