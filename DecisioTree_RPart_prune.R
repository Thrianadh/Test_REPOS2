install.packages(c("RWeka","party",
                   "partykit","rJava",
                   "FSelector","C50",
                   "rpart","rattle",
                   "rpart.plot",
                   "RColorBrewer"),
                 dependencies = TRUE)

install.packages("caret",dependencies = TRUE)

#loading the Libraries
library(RWeka)
library(party,partykit)
library(C50,rJava)
library(FSelector)
library(rpart,rattle)
library(rpart.plot,RColorBrewer)
library(caret)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(C50)



data("GermanCredit")
dim(GermanCredit)
head(GermanCredit[,1:10],10)





inTrain=runif(nrow(GermanCredit))<0.2


dt=rpart(Class~Duration+Amount+Age,
         method="class",data = GermanCredit[inTrain,])
print(summary(dt))
#plot(dt)
#text(dt)
as.party
plot(as.party(dt))

p=prune(dt,cp=dt$cptable[which.min(dt$cptable[, "xerror"]), "cp"])
                         