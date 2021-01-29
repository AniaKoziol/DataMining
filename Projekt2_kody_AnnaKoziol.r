install.packages("praznik")
install.packages("verification")
install.packages("infotheo")
install.packages("AUC")
install.packages("randomForest")
install.packages("class")
install.packages("Boruta")
install.packages("factoextra")
install.packages("ROCR")
install.packages("adabag")
install.packages("caret")
library(pROC)
library(caret)
library(adabag)
library(ROCR)
library(factoextra)
library(DMwR)
library(class)
library(randomForest)
library(rpart)
library(e1071)
library(verification)
library(praznik)
library(car)
library(pROC)
library(caret)
library(corrplot)
library(Boruta)
library(pROC)
library(class)
library(DMwR)
library(e1071)
dane_train <- read.csv("train_projekt2.csv")
#--------- podzial na treningowy i testowy--------------------

df <- dane_train[sample(1:nrow(dane_train)),]
df_testowy <- df[1:400,]
df_treningowy <- data.frame(df[401:nrow(df),])

#--------sprawdzenie brakow danych --------------
#
sum(apply(dane_train  , 2, function(x){ sum(is.na(x))})) # nie ma brakow danych 

#------------------- Var kolumn ---------------------------------

wariancje <- as.data.frame(as.table(apply(df_treningowy[,-ncol(df_treningowy)], 2, var)))
head(wariancje)
wariancja_zero <- subset(as.data.frame(as.table(apply(df_treningowy[,-ncol(df_treningowy)], 2, var))), Freq < 0.01) #brak 
#-------- takie same wiersze ------------------------------------
#
sum(duplicated(df_treningowy)) #brak

#----------------- obserwacje odstajace---------------- 
cooksd <- cooks.distance(glm(Y ~ ., 
                             family = "binomial", 
                             data = dane_train))
plot(cooksd, 
     pch="*", 
     cex=2, 
     main="Influential Obs by Cooks distance")  
abline(h = 4*mean(cooksd, na.rm=T), col="red")
outliers <- rownames(dane_train[cooksd > 4*mean(cooksd, na.rm=T), ])
length(outliers) #30 outlinerów ,  nie tak du¿o 
print(outliers)


#---------------BORUTA---------------------

boruta.fs <- Boruta(Y~., data = df_treningowy, doTrace=2)
uznane <- which(boruta.fs$finalDecision== "Confirmed")
boruta.fs$finalDecision

#---------------CMIM------------------------------------------------

uznane2<- CMIM(df_treningowy[,-501],df_treningowy$Y,20) # pokrywaja sie w wieksozsci
uznane2$selection

#------------ IMPORTANCE RANDOM FOREST--------------------------
#
las <- randomForest(as.factor(Y)~., data=df_treningowy, importance=TRUE)
varImpPlot(las) # na podtsawie wykresu pokrywaj? si? z Boruta 

#------------- NOWE DANE ---------------------------------------------

nowe_dane <- df_treningowy[,c(uznane,501)]
nowe_dane <- as.data.frame(nowe_dane)
nowy_test <- df_testowy[,c(uznane,501)]
write.csv(nowe_dane, file = "C://Users//Ania//Desktop//Studia-SMAD//Data Mining//projekt 2")

korelacje <- (round(cor(nowe_dane[,-ncol(nowe_dane)]),2))
corrplot(korelacje, type = "upper", order = "hclust", tl.col = "darkblue", tl.srt = 45)
korelacje_df <- as.data.frame(as.table(korelacje))
head(korelacje_df) #df w sesnie data frame
skorelowane_zmienne <- subset(korelacje_df, (abs(Freq) > 0.5 & abs(Freq) <1)) 
length(subset(korelacje_df, (abs(Freq) > 0.8 & abs(Freq) <1))$Freq)/2 #12 

# zmienne : V29  + V49 + V65 + V106 + V129 +V154 +V242+ V282+V319 +V337+ V339+ V379+
#  V434+ V443+ V452+ V454+ V456+ V473+ V476+ V494

#  -------------VIF-------------------

vif <-as.data.frame(as.table(vif(nowe_dane[,-ncol(nowe_dane)])))
duzy_vif <- subset(vif,  (abs(Freq) > 10))

  vif(lm(Y ~., data=nowe_dane))


#---------------KORELACJE ------------------
korelacje <- (round(cor(nowe_dane[,-ncol(nowe_dane)]),2))
corrplot(korelacje, type = "upper", order = "hclust", tl.col = "darkblue", tl.srt = 45)
korelacje_df <- as.data.frame(as.table(korelacje))
head(korelacje_df) #df w sesnie data frame
skorelowane_zmienne <- subset(korelacje_df, (abs(Freq) > 0.8 & abs(Freq) <1))


#----- METODY KLASYFIKACJI-------------------------------------------------------------

y <- nowy_test$Y

# 1)-- drzewo decyzyjne 

tree <- rpart(factor(Y)~., data = nowe_dane, minsplit=5, cp= 0.001)
pred_tree <- predict(tree, nowy_test, type="prob")
pred_tree <- as.data.frame(pred_tree)
plot(tree)
text(tree, pretty = 0)
pred_tree2 <- predict(tree, nowy_test, type='class')
dokl_drzewo <- sum(nowe_dane$Y == pred_tree2)/nrow(nowe_dane) #0.500625

par(pty = "s")
roc(y,pred_tree[,2], plot = T , legacy.axes = T, print.auc = T)


#-------------------------- p?tla AUC DRZEWO ---------------------------------------

ile_prob <- 10
temp <- 0
set.seed(123)
for(i in 1:ile_prob){
  
  index <- createDataPartition(nowe_dane$Y, p = 3/4, list = FALSE)
  dane_treinigowe <- nowe_dane[index,]
  dane_validacyjne <- nowe_dane[-index,]
  
  model <- rpart(as.factor(Y)~.
                 ,data=dane_treinigowe)
 wynik = predict(model,dane_validacyjne, type="prob")
 wynik <- as.data.frame(wynik)[,2]
  labels = dane_validacyjne$Y
  
  area <- auc(labels, wynik)
  temp <- temp + area
}
area <- temp/ile_prob*100
area #84,22  

#---------------------------------------------------------------------------
# 2)-- SVM 
x <- subset(nowe_dane, select=-Y)
y <- nowy_test$Y

svm_model <- svm(Y ~ ., data=nowe_dane)
summary(svm_model)
pred <- predict(svm_model,nowy_test)

par(pty = "s")
roc(y,pred, plot = T , legacy.axes = T, print.auc = T) #0.9443 
par(pty = "s")


# jadro sinusoidalne
svm_model2 <- svm(Y ~ ., data=nowe_dane, kernel = "sigmoid")
summary(svm_model2)
pred2 <- predict(svm_model2,nowy_test)

par(pty = "s")
roc(y,pred2, plot = T , legacy.axes = T, print.auc = T) #0.5637 

# jadro polynomial
svm_model3 <- svm(Y ~ ., data=nowe_dane, kernel = "polynomial")
summary(svm_model3)
pred3 <- predict(svm_model3,nowy_test)

par(pty = "s")
roc(y,pred2, plot = T , legacy.axes = T, print.auc = T) #0.7128 

par(pty = "s")
roc(y,pred, plot = T , legacy.axes = T, print.auc = T) #0.9443 
plot.roc(y,pred2,  col= "#4daf4a", print.auc=T, add = T, print.auc.y= 0.42 )
plot.roc(y,pred3,  col= "red", print.auc=T, add = T, print.auc.y= 0.35 )

legend("bottomright", legend = c("radial","sigmoid", "polynomial"), col = c("black", "#4daf4a", "red"), lwd=2)


#-----------------------------------------------------------------------
# 3)-- Random Forest 


las2 <- randomForest(factor(Y)~., data=nowe_dane, importance=TRUE)
las3 <- randomForest(factor(Y)~., data=nowe_dane, importance=TRUE, ntree =1000)
varImpPlot(las2)
las2$importance
print(las2) # b??d = 11,69%
pred_rf <- predict(las2, nowy_test, type = 'prob')
pred_rf3 <- predict(las3, nowy_test, type = 'prob')
pred_rf<- as.data.frame(pred_rf)
confusionMatrix(pred_rf, factor(y)) # accuracy 0.8975 

roc(y,pred_rf[,2], plot = T , legacy.axes = T, print.auc = T)
roc(y,pred_rf3[,2], plot = T , legacy.axes = T, print.auc = T)

#------------------------- KLASY ---------------------


KNN <- kmeans(scale(nowe_dane[, -ncol(nowe_dane)]),2  , 100)
table(KNN$cluster)
fviz_cluster(object = KNN, data = scale(nowe_dane[, -ncol(nowe_dane)]) , 
             stand = F) 

# funkcja prawie rodzileila na dwie klasy przy uzyciu d?ch g?ownych sk?adowych 

#-------- Klasyfikacje na knn ------------

knnFit <- train(Y ~ ., data = nowe_dane, method = "knn", preProcess = c("center","scale"), 
                tuneLength = 20)
plot(knnFit)
knnPredict <- predict(knnFit,newdata = nowy_test , type="prob")
knnROC <- roc(nowy_test$Y,knnPredict[,"0"])
knnROC
plot(knnROC, type="S", las=1, print.auc=T, color="blue", main ="Klasyfikacja KNN", legacy.axes = T)

#-----------------------------KNN na skaladowych -------------------------------
knnFit2 <- train(Y ~ ., data = scores, method = "knn", preProcess = c("center","scale"), 
                tuneLength = 20)
plot(knnFit2)
knnPredict2 <- predict(knnFit2,newdata = test.scores , type="prob")
knnROC2 <- roc(test.scores$Y,knnPredict2[,"0"])
knnROC2
plot(knnROC2, type="S", las=1, print.auc=T, color="blue", main ="Klasyfikacja KNN", legacy.axes = T)

#slabiej

#--------------------------- PCA --------------------------------


#standaryzowanie danych 


scale <- scale(nowe_dane[, -ncol(nowe_dane)])
scale <- as.data.frame(scale)
pca <- princomp(~. , cor = T, data= scale)
print(summary(pca))
par(lwd = 2)
plot(pca, las = 1 , col = "pink") #5 skladowych g??wnych wyjasnia prawie 100% zmienno?ci 
biplot(pca)
pca$loadings

#--------- DATA FRAME Z PCA -----------------------------

scores <- data.frame(Y = nowe_dane$Y, 
                        pca$scores[, 1:5])

test.scores <- data.frame(Y = nowy_test$Y, 
                              predict(pca, newdata =nowy_test[, -ncol(nowy_test)])) 
test.scores <- as.data.frame(test.scores[, 1:6])

#--------------------------------------------------------------




#-------------- WSZYSKIE ROC na jednym rysunku -------------------

par(pty = "s")
roc(y,pred, plot = T , legacy.axes = T, print.auc = T) #0.9443 
plot.roc(y,pred_tree[,2],  col= "#4daf4a", print.auc=T, add = T, print.auc.y= 0.42 )
plot.roc(y,pred_rf[,2],  col= "red", print.auc=T, add = T, print.auc.y= 0.35 )
roc(y,pred.ada1000$prob[,2], plot = T , legacy.axes = T, print.auc = T, col = "orange", add = 0.2, print.auc.y= 0.35)

legend("bottomright", legend = c("SVM", "Decision Tree", "RForest"), col = c("black", "#4daf4a", "red"), lwd=2)




#--------------- DRZEWO NA SKLADOWYCH --------------------------


tree_pca <- rpart(factor(Y)~., data = scores)
pred_tree_pca <- predict(tree_pca, test.scores, type='prob')
pred_tree_pca <- as.data.frame(pred_tree)
plot(tree)
text(tree, pretty = 0)

par(pty = "s")

roc(y,pred_tree_pca[,2], plot = T , legacy.axes = T, print.auc = T)


#--------- ADA BOOST -----------------------------



nowe_dane$Y <- as.factor(nowe_dane$Y)
ada1000 <- boosting(Y~., data=nowe_dane, mfinal=1000)
pred.ada1000 <- predict(ada1000, nowy_test)

1 - pred.ada$error
100*mean(pred.ada1000$class!=nowy_test$Y ) # b?ad 13.5 
dokl_ada <- sum(nowy_test$Y == pred.ada1000$class)/nrow(nowy_test) #0.88

par(pty = "s")
roc(y,pred.ada1000$prob[,2], plot = T , legacy.axes = T, print.auc = T, col = "black", main = "Boosting", add = T)

#------------------------------------ADABoost na skladowych -------------------------
scores$Y <- as.factor(scores$Y)
ada100_pca <- boosting(Y~., data=scores, mfinal=100)
pred.ada100_pca <- predict(ada100_pca, test.scores)
1 - pred.ada$error
100*mean(pred.ada$class!=nowy_test$Y ) # b?ad 13.5 
dokl_ada <- sum(nowy_test$Y == pred.ada1000$class)/nrow(nowy_test) #0.88

par(pty = "s")
roc(y,pred.ada100_pca$prob[,2], plot = T , legacy.axes = T, print.auc = T, col = "black", main = "ada", add = T)
# AUC 0.52.

#--------------KLASYFIKACJA TESTOWEGO--------------------------


dane_test <- read.csv("test_projekt2.csv")

knnFit <- train(Y ~ ., data = nowe_dane, method = "knn", preProcess = c("center","scale"), 
                tuneLength = 20)
plot(knnFit)
knnPredict <- predict(knnFit,newdata = dane_test , type="prob")
wynniki_klasyfikacji <- knnPredict[,2]

write.csv2(wynniki_klasyfikacji, file = "AKO.csv")






