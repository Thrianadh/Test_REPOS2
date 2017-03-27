#load the  data
data("Vehicle",package = "mlbench")

head(Vehicle)
str(Vehicle)
View(Vehicle)
#creating training data and testing data
library(caret)
set.seed(100)


train_rows=sample(1:nrow(Vehicle),size=0.7*nrow(Vehicle))
train=Vehicle[train_rows,]
test=Vehicle[-train_rows,]

caret::featurePlot(Vehicle[,-19],Vehicle[,19],plot="box")
names(train)
names(test)
#using KlaR  for Naive Bayes
install.packages("klaR",dependencies = TRUE)
library(klaR)
nb_nod=NaiveBayes(Class~.,data = train)
pred=predict(nb_nod,newdata = test)
mean(test$Class !=pred$class)

#confusion Matrix
tab=table(pred$class,test$Class)
caret::confusionMatrix(table)

plot(nb_nod)
