setwd('/Users/naordalal/Desktop/אוניברסיטה/שנה ד - סמסטר א/יישום שיטות לניתוח נתונים/hw2')

df <-read.csv('Titanic/train.csv',na.strings = "")

df$Pclass<- as.factor(df$Pclass)
df$Survived<- as.factor(df$Survived)
df<- df[,-c(1,4,9)]

library(caret)
library(gbm)
library(lattice)

set.seed(123)
metric <- "Accuracy"


gbmGrid <- expand.grid(.n.trees=c(5),.interaction.depth=6,.shrinkage=c(0.1,0.3,0.5),.n.minobsinnode=5)
xgbGrid <- expand.grid(.nrounds=20,.max_depth=6,.eta=c(0.1,0.3,0.5),.gamma=0.1,.colsample_bytree=0.5,.min_child_weight=0.01,.subsample=0.7)
rpartGrid <- expand.grid(.cp = 0.001)
c50Grid <- expand.grid(.winnow = c(TRUE,FALSE) , .trials = 5 , .model = 'tree')

control <- trainControl(method = "cv" , number = 5)

fit.gbm <- train(Survived~., data=df, method="gbm", metric=metric, trControl=control,tuneGrid =gbmGrid , na.action = na.pass)

fit.xgb <- train(Survived~., data=df, method="xgbTree", metric=metric, trControl=control,tuneGrid =xgbGrid , na.action = na.pass)

fit.rpart <- train(Survived~., data=df, method="rpart", metric=metric, trControl=control,tuneGrid =rpartGrid , na.action = na.pass)

fit.c50 <- train(Survived~., data=df, method="C5.0", metric=metric, trControl=control,tuneGrid =c50Grid , na.action = na.pass)

df_test <-read.csv('Titanic/test.csv',na.strings = "")
ids<- df_test$PassengerId
df_test$Pclass<- as.factor(df_test$Pclass)
df_test<- df_test[,-c(1,3,8)]

fit.gbm$xlevels[["Cabin"]] <- union(fit.gbm$xlevels[["Cabin"]], levels(df_test$Cabin))
fit.xgb$xlevels[["Cabin"]] <- union(fit.xgb$xlevels[["Cabin"]], levels(df_test$Cabin))
fit.rpart$xlevels[["Cabin"]] <- union(fit.rpart$xlevels[["Cabin"]], levels(df_test$Cabin))
fit.c50$xlevels[["Cabin"]] <- union(fit.c50$xlevels[["Cabin"]], levels(df_test$Cabin))

pred_gbm = predict(fit.gbm , df_test , na.action = na.pass)
pred_xgb = predict(fit.xgb , df_test , na.action = na.pass)
pred_rpart = predict(fit.rpart , df_test , na.action = na.pass)
pred_c50 = predict(fit.c50 , df_test , na.action = na.pass)


df_test$pred <- mapply(function(x , y , z , t) as.numeric(as.character(x)) + as.numeric(as.character(y)) + as.numeric(as.character(z)) +  as.numeric(as.character(t)), pred_gbm , pred_xgb , pred_rpart , pred_c50)

df_test$pred <- mapply(function(x) if(x >= 2) as.factor(1) else as.factor(0), df_test$pred)

res <- cbind(PassengerId=ids,Survived=as.character(df_test$pred))
write.csv(res,file="Prediction3/try3.csv",row.names = F)
