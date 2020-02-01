# Iris Dataset
library(rpart)
library(rpart.plot)
install.packages(table1)
iris
# Obeservations
dim(iris)
# Sample of 50 datasets
# Run it again and again to get Random Numbers
s<-sample(150,100)
s
# Repeat the above to get Random of 50 Samples
# Training and Testing
iris_train <- iris[s,]
iris_test <- iris[-s,]
dim(iris_test)
dim(iris_train)
# rpart Decision Tree Model DTM
# Classification of Species
#dtm<-(rpart(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width))
dtm<- rpart(species~., iris_train, method="class")
dtm
# * denotes terminal node-> Leaf Node
plot(dtm)
# Decision Tree without Text
text(dtm)
rpart.plot(dtm)
??rpart.plot
rpart.plot(dtm,type=4 ,extra=101)
p<-predict (dtm,iris_test, type="class")
table(iris_test[,5],p)

