##########################################################################################
# K Means Clustering of Iris Data in R 
###########################################################################################
###########################################################################################


### Install packages needed

library(kernlab)
library(kknn)


### Set working directory to where file is stored
getwd()
setwd("C:/Users/beena/Downloads/Analytics Modelling")

### read iris text data for k Means clustering
iris<-read.table("iris.data.txt", sep=",")
#View(iris)



### Scale Iris data and store into matrix for k means
scaled_iris<- as.matrix(scale(iris[,1:4]))

#### select best value of k using an elbow curve
k.max <- 15
data <- scaled_iris
wss <- sapply(1:k.max,function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,type="b", pch = 19, frame = FALSE,xlab="Number of clusters K",ylab="Total within-clusters sum of squares")

###Note:use dev.off to clear plot in R
### rm(list=ls()) to clear R memory

#### 

### assign the number of predictor variables to x (Note: we have "-1" because one of our variable)
x=ncol(iris)-1

### assign an empty list av for 
av<-c()
ave<-c()

### iterate through a loop to find the best combination of predictors given a k-value of 3 
for (m in 1:x){
  for (n in 1:x){
    if (n>=m){
      
      f<-kmeans(as.matrix(iris[,m:n]),3,nstart=50,iter.max=15)
      #print(m)
      #print(n)
      r<-table(iris[,5],f$cluster)
      av[n]<-mean((apply(r,2,max))/(colSums(r)))
      
      
    }
    
    ave[m]<-max(av)
    
  }
}

### in data [,m:n] best value of n
print(which(av==max(av)))
### in data [,m:n] best value of m
print(which(ave==max(ave)))

#### below is the output of each of the 15 comnbinations of our 4 predictor variables in Iris data
### as observed the 2nd column is the worst predictor with 63% correct classifications, wheras
### a combination of the 3rd and 4th predictor give 96.04% correct classification
### even just the 4th predictor gives 96.04% correct classification

### Note for each predicted cluster we have defined correct classifications as max number
### of predictions in that cluster that fall into either of the 3 flower categories
###(Iris-setosa,Iris-versicolor,Iris-virginica ) divided by the sum of observations in the cluster
'[1] 1
[1] 1
[1] 0.7212658
[1] 1
[1] 2
[1] 0.820554
[1] 1
[1] 3
[1] 0.8856048
[1] 1
[1] 4
[1] 0.9071873
[1] 2
[1] 2
[1] 0.6298167
[1] 2
[1] 3
[1] 0.9318645
[1] 2
[1] 4
[1] 0.9543691
[1] 3
[1] 3
[1] 0.9484702
[1] 3
[1] 4
[1] 0.9604701
[1] 4
[1] 4
[1] 0.9604701'
#View(iris)

##### Running the model for best predictors 3 & 4; Accuracy is 96.04%

best_model<-kmeans(as.matrix(iris[,m:n]),3,nstart=50,iter.max=15)
best_table<-table(iris[,5],f$cluster)
Accuracy<-mean((apply(r,2,max))/(colSums(r)))
print(Accuracy)


