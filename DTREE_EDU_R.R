dataset <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/pima-indians-diabetes/pima-indians-diabetes.data", sep = ",",na.strings="0.0",strip.white=TRUE, fill = TRUE)
pima <- dataset[complete.cases(dataset),] 
names(pima) <- c("numpreg", "plasmacon", "bloodpress", "skinfold", "seruminsulin", "BMI", 
                 "pedigreefunction", "age", "classvariable")

db=pima
str(db)
nrow(db)
set.seed(100)
db$classvariable=factor(db$classvariable,levels = c(0,1),
                                         labels = c("No","YES"))
train_rows=sample(1:nrow(db),size=0.7*nrow(db))
train=db[train_rows,]
test=db[-train_rows,]
View(train)
nrow(train)
nrow(test)
install.packages("rpart",dependencies = TRUE)
library(rpart)
dt=rpart(classvariable~.,data=train)
plot(dt)
text(dt)
class(dt)
structure(dt)

dt_predict=predict(dt, test,type=c("class"))
test$classvariable

table(test$classvariable,dt_predict)
(120+45)/228
