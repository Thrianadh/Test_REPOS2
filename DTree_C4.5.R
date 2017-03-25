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


churnTest
churnTrain
dim(churnTest)
dim(churnTrain)
names(churnTest)
names(churnTrain)
View(churnTrain)

treemodel_decision_tree=C5.0(x=churnTrain[,-20],y=churnTrain$churn)
print(treemodel_decision_tree)
summary(treemodel_decision_tree)
print(summary(treemodel_decision_tree))
plot(treemodel_decision_tree)


ruleModel=C5.0(churn~.,data=churnTrain,rules=TRUE)
print(ruleModel)
print(summary(ruleModel))


print(C5imp(treemodel_decision_tree))
print(C5imp(treemodel_decision_tree,metric = "splits"))

result=predict(ruleModel,head(churnTest[,-20]))
result_prob=predict(ruleModel,head(churnTest[,-20]),type = "prob")

print(result)
print(result_prob)
