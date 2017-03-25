#installing packages
install.packages(c("RWeka","party",
                   "partykit","rJava",
                   "FSelector","C50",
                   "rpart","rattle",
                   "rpart.plot",
                   "RColorBrewer"),
                 dependencies = TRUE)

#loading the Libraries
library(RWeka,party,partykit)
library(C50,rJava)
library(FSelector,rpart,rattle)
library(rpart.plot,RColorBrewer)

data("iris")
names(iris)
str(iris)
dim(iris)
nrow(iris)
ncol(iris)
View(iris)

Model1_decision_tree=J48(Species~.,data=iris)
if(require("party",quietly = TRUE)) plot(Model1_decision_tree)
summary(Model1_decision_tree)


print(information.gain(Species~., data=iris))

print(table(iris$Species,p2))

#subset1.iris<-subset(iris,petal.width>0.6)
#information.gain(species~.,data=subset1.iris)

#subset2=subset(subaset1.iris,petal.width<=1.7)
#inforamtion.gain(Species~.,data=subset2.iris)

Predict1_ID_decision_tree<-predict(Model1_decision_tree,newdata = NULL,type = "probability")
Predict2_ID_decision_tree<-predict(Model1_decision_tree,newdata = NULL,type = "class")
print(table(iris$Species,Predict2_ID_decision_tree))
      