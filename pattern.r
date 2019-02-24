library(datasets)
data("iris")
#How many features are there and what are their types 
#To Know the attributes Name
names(iris)
#To know total rows and columns
dim(iris)

#To Disaply the dataset in rows and columns
View(iris)

# To know Internal structure
str(iris)

#Compute and display summary statistics for each feature available 
#in the dataset. (eg. minimum value, maximum value, mean, range, 
#standard deviation, variance and percentiles
# statistics
#To know min
min(iris$Sepal.Length)


#To know max
max(iris$Sepal.Length)

#TO know mean(Average)
mean(iris$Sepal.Length)

#To Identify Range of Values
range(iris$Sepal.Length)

#To know standard Deviation
sd(iris$Sepal.Length) 

#to Identify variance
var(iris$Sepal.Length)
#sample of 0,25,50,75,100 %
quantile(iris$Sepal.Length)

#if specific % required
quantile(iris$Sepal.Length,c(0.3,0.6))

#Data Visualization-Create a histogram for each feature in the dataset 
#to illustrate the feature distributions.  Plot each histogram
#to Create a histogram
h<-hist(iris$Sepal.Length,main="Sepal Length Frequencies-histogram",xlab="Sepal Length",
        xlim=c(3.5,8.5), col="blue")
#to display the details of the histogram
h
#using breaks and las
h<-hist(iris$Sepal.Length,main="Sepal Length Frequencies-histogram",xlab="Sepal Length",
        col="blue", labels=TRUE, breaks=3,border="green" ,las=1)

h<-hist(iris$Sepal.Length,breaks=c(4.3,4.6,4.9,5.2,5.5,5.8,6.1,6.4,6.7,7.0,7.3,7.6,7.9))

#Create a boxplot for each feature in the dataset.  
#All of the boxplots should be combined into a single plot.  
#Compare distributions and identify outliers.
#using boxplot() function
boxplot(iris$Sepal.Length)

#this will display the summary - the quartile,median,min,max..
summary(iris$Sepal.Length)

#combined boxplot for all 4 features
boxplot(iris[,-5])

#a combined boxplot, excluding the species
myboxplot<-boxplot(iris[,-5])

#identify outliers
myboxplot$out
set.seed(99)
rnum<- sample(rep(1:150))
rnum
normalize <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}
iris.new<- as.data.frame(lapply(iris[,c(1,2,3,4)],normalize))
head(iris.new)
library(class)
model1<- knn(train=iris.train, test=iris.test, cl=iris.train.target, k=16)
dia_nor <- as.data.frame(lapply(dia[,c(1,5,6,7,8,9,10)]), nor)
df <- data(iris)                          
head(iris)
ran <- sample(1:nrow(iris), 0.9 * nrow(iris)) 
nor <-function(x) { (x -min(x))/(max(x)-min(x))   }
iris_norm <- as.data.frame(lapply(iris[,c(1,2,3,4)], nor))
summary(iris_norm)
iris_train <- iris_norm[ran,] 
iris_test <- iris_norm[-ran,] 
iris_target_category <- iris[ran,5]
iris_test_category <- iris[-ran,5]
library(class)
pr <- knn(iris_train,iris_test,cl=iris_target_category,k=13)
tab <- table(pr,iris_test_category)
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)
