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
