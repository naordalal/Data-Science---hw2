setwd('/Users/naordalal/Desktop/אוניברסיטה/שנה ד - סמסטר א/יישום שיטות לניתוח נתונים/hw2')

df <-read.csv('Titanic/train.csv',na.strings = "")

df$Pclass<- as.factor(df$Pclass)
df$Survived<- as.factor(df$Survived)
df<- df[,-c(1,4,9)]

library(caret)
set.seed(123)
control <- trainControl(method="cv", number=5)
fit.c50 <- train(Survived~., data=df, method="C5.0", metric="Accuracy", trControl=control,na.action = na.pass)

df_test <-read.csv('Titanic/test.csv',na.strings = "")
ids<- df_test$PassengerId
df_test$Pclass<- as.factor(df_test$Pclass)
df_test<- df_test[,-c(1,3,8)]
fit.c50$xlevels[["Cabin"]] <- union(fit.c50$xlevels[["Cabin"]], levels(df_test$Cabin))

pred_c50 = predict(fit.c50 , df_test , na.action = na.pass)

res <- cbind(PassengerId=ids,Survived=as.character(pred_c50))
write.csv(res,file="Prediction2/try2.csv",row.names = F)
