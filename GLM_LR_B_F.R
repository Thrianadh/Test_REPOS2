
getwd()
list.files()

AP=read.csv("https://raw.githubusercontent.com/PacktPublishing/R-Data-Mining-Blueprints/master/Chapter%204/Artpiece.csv",header=TRUE)
ArtPiece=AP
head(ArtPiece,n=5)
str(ArtPiece)
str(ArtPiece$IsGood.Purchase)
str(ArtPiece$Is.It.Online.Sale)
table(ArtPiece$IsGood.Purchase)
table(ArtPiece$Is.It.Online.Sale)

#data conversion
for(i in c(1,7)) {
  ArtPiece[,i]=as.factor(ArtPiece[,i])
}
## or
ArtPiece$IsGood.Purchase=as.factor(ArtPiece$IsGood.Purchase)
ArtPiece$Is.It.Online.Sale=as.factor(ArtPiece$Is.It.Online.Sale)

## removing Na Values from the data set
dim(ArtPiece)          #72983     8
ArtPiece=na.omit(ArtPiece)
dim(ArtPiece)          #72983-72668=315

trainData_AP=sample(1:nrow(ArtPiece),size = 0.7*nrow(ArtPiece))
train_AP=ArtPiece[trainData_AP,]
test_AP=ArtPiece[-trainData_AP,]

# logistic model
Model_ArtPiece1=glm(train_AP$IsGood.Purchase~.,family(logit) = binomial,data = train_AP)
print(Model_ArtPiece1)
#model results componemts
summary(Model_ArtPiece1)

Model_ArtPiece2=glm(train_AP$IsGood.Purchase~Is.It.Online.Sale1 +Critic.Ratings+Acq.Cost+CurrentAuctionAveragePrice+CollectorsAverageprice+Min.Guarantee.Cost,family = "binomial",data = train_AP)
print(Model_ArtPiece2)
#model results componemts
summary(Model_ArtPiece2)

# 95 Confidence interval for exponentiated coeffiencts
exp(confint(Model_ArtPiece1))
confint(Model_ArtPiece1)

exp(confint(Model_ArtPiece2))
confint(Model_ArtPiece2)

#Anova
anova(Model_ArtPiece1,test ="Chisq")


#plotting the model
plot(Model_ArtPiece1$fitted.values)


#predict the probability
test_AP$goodP=predict(Model_ArtPiece1,newdata =test_AP,type ="response")
test_AP$goodl=predict(Model_ArtPiece1,newdata =test_AP,type ="link")

#Automatic logistic regression model Selection
#backward Selection meathod
#forward selection meathod
#BOTH

#auto detection model
install.packages(c("stepAIC","MASS"),dependencies = TRUE)
library("stepwise","StepwiseTest","stepR","MASS")
install.packages("base")
library("MASS")
library(base)

fit_step<-stepAIC(Model_ArtPiece1,method="both")
summary(fit_step)

install.packages(c("MASS","plyr","car"),dependencies = TRUE)
library("MASS","plyr","car")
browse()
