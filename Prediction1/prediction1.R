setwd('/Users/naordalal/Desktop/אוניברסיטה/שנה ד - סמסטר א/יישום שיטות לניתוח נתונים/hw2')

df <-read.csv('Titanic/train.csv',na.strings = "")

df$Pclass<- as.factor(df$Pclass)
df$Survived<- as.factor(df$Survived)
df<- df[,-c(1,4,9)]

indices <- sample(1:nrow(df),nrow(df)*0.8)
train<- df[indices,]
test<- df[-indices,]

seed=123
set.seed(seed) 
library(e1071)
nb_model <- naiveBayes(Survived~.,data = train)

df_test <-read.csv('Titanic/test.csv',na.strings = "")

ids<- df_test$PassengerId
df_test$Pclass<- as.factor(df_test$Pclass)
df_test<- df_test[,-c(1,3,8)]
nb_model$xlevels[["Cabin"]] <- union(nb_model$xlevels[["Cabin"]], levels(df_test$Cabin))

new_pred<- predict(nb_model,df_test)

res <- cbind(PassengerId=ids,Survived=as.character(new_pred))
write.csv(res,file="Prediction1/try1.csv",row.names = F)