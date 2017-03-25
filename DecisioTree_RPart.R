install.packages(c("RWeka","party",
                   "partykit","rJava",
                   "FSelector","C50",
                   "rpart","rattle",
                   "rpart.plot",
                   "RColorBrewer"),
                   dependencies = TRUE)

#loading the Libraries
library(RWeka)
library(party,partykit)
library(C50,rJava)
library(FSelector)
library(rpart,rattle)
library(rpart.plot,RColorBrewer)

library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(C50)
data(churn)
churnTrain
churnTest

fit=rpart(churn~.,data=churnTrain,method="class")
print(fit)
plot(fit)
text(fit)

fancyRpartplot(fit)

prediction=predict(fit,churnTest,type = "class")

print(table(actual=churnTest$churn,predicted=prediction))
