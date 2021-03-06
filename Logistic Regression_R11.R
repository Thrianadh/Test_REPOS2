setwd("D:/Thrinadh/R Directory/Module 1/.Rproj.user/persona of visitors_Project")
getwd()

election=read.csv("election.csv",header=TRUE)
str(election)

election$Result=factor(election$Result,
                       levels = c(0,1),
                       labels = c("No","Yes"))

plot(election$Year,election$Result)

election_model=glm(Result~Amount+Year,election,family = "binomial")
summary(election_model)

#predicts=predict(election_model,election,family="binomial")
predicts=predict(election_model,election,type = "response")

compare=cbind(election,predicts)

compare$predicts_result=0
compare$predicts_result[compare$predicts>0.50]=1

print(compare)
# creating a generalized linear models with response outputs
install.packages("AER",dependencies = TRUE)
library(AER)
data(Affairs)
head(Affairs)
str(Affairs)


Affairs$yesnoaffairs[Affairs$affairs>0]=1
Affairs$yesnoaffairs[Affairs$affairs==0]=0
Affairs$yesnoaffairs=factor(Affairs$yesnoaffairs,levels = c(0,1),labels = c("EMANo","EMAYes"))

model1=glm(yesnoaffairs~.,data = Affairs, family="binomial")
summary(model1)

Affairs$prob=predict(model1,Affairs,type="response")
Affairs$affair=0
Affairs$affair[Affairs$prob>0.5]=1
table(Affairs$yesnoaffairs,Affairs$affair)

##################################################################
str(Affairs)
Affairs=Affairs[,-1]

model1=glm(yesnoaffairs~.,data = Affairs, family="binomial")
summary(model1)

Affairs$prob=predict(model1,Affairs,type="response")
Affairs$affair=0
Affairs$affair[Affairs$prob>0.5]=1
table(Affairs$yesnoaffairs,Affairs$affair)
####################################################################
model1=glm(yesnoaffairs~age+yearsmarried+religiousness+rating,data = Affairs,family = "binomial")
summary(model1)


Affairs$prob=predict(model1,Affairs,type="response")
Affairs$affair=0
Affairs$affair[Affairs$prob>0.5]=1
table(Affairs$yesnoaffairs,Affairs$affair)


(435+25)/601
########################################################################
Fr <- c(68,42,42,30, 37,52,24,43, 66,50,33,23, 47,55,23,47, 63,53,29,27,57,49,19,29)
Temp <- gl(2, 2, 24, labels = c("Low", "High"))
Comfort <- gl(3, 8, 24, labels = c("Hard","Medium","Soft"))
M.user<- gl(2, 4, 24, labels = c("N", "Y"))
Brand<- gl(2, 1,24,labels = c("X","M","L"))
deter<-data.frame(Fr,Temp,Comfort,M.user,Brand)
deter.model <- glm(Fr ~ M.user*Temp*Comfort + Brand, family = poisson, data = deter)
summary(deter.model)


deter.model1<-glm(terms(Fr ~ M.user*Temp*Comfort+Brand*M.user*Temp,keep.order = TRUE), family = poisson, data = deter)
summary(deter.model1)


###############################################################################
library(ISLR)
data("Smarket")
Smarket
names(Smarket)
dim(Smarket)
summary(Smarket)
cor(Smarket[-9])
plot(cor(Smarket[-9]))
attach(Smarket)
plot(Volume)
glm.fit<-glm(Direction???Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial)
summary(glm.fit)
coef(glm.fit)
summary (glm.fit)$coef
glm.probs=predict(glm.fit,type="response")
glm.probs [1:10]
contrasts(Direction)
glm.pred=rep("Down",1250)
glm.pred[glm.probs >.5]=" Up"
table(glm.pred,Direction)
sum(Direction==glm.probs)/length(glm.probs)

#precision =TP/TP+FP
#recall/sensitivity=TP/TP+FN  can be considered as the likelyhood of spotting a positive cases where presented with ONE "1" or the propotion of edges we find

#specifity =TN/TN+FP can be considered as the likelyhood of spotting a negative cases where presented with ONE "1" or the propotion edges we find

#ROCR Curve
library(ROCR)



####################################
gre=read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")
dim(gre)
gre$Row_Number=1:400
head(gre)
train_Rows_gre=sample(1:nrow(gre),size=0.7*nrow(gre))
#training data
training=gre[train_Rows_gre,]
#test data
test=gre[-train_Rows_gre,]
#sorting row number
Sort_Training_data=training[order(training$Row_Number),]
Sort_Test_data=test[order(test$Row_Number),]

#training GLM Model
Model_Gre1=glm(admit~gre+as.factor(rank)+gpa,data = Sort_Training_data,family = "binomial")
summary(Model_Gre1)

#TestGLM Model on validation data set
Sort_Test_data$preds=predict(Model_Gre1,Sort_Test_data,type = "response")
head(Sort_Test_data)

Sort_Test_data$adm=0
Sort_Test_data$adm[(Sort_Test_data$preds>0.5)]<-1

table(Sort_Test_data$adm,Sort_Test_data$admit)



Sort_Training_data$preds=predict(Model_Gre1,Sort_Training_data,type = "response")
head(Sort_Test_data)

Sort_Training_data$adm=0
Sort_Training_data$adm[(Sort_Test_data$preds>0.5)]<-1

table(Sort_Training_data$adm,Sort_Training_data$admit)

(196+6)/280
 
 