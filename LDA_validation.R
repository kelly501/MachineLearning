library(caret); library(pROC); 
library(sampling); library(dplyr); library(MASS)

install.packages("mlbench")
library(mlbench)
data(PimaIndiansDiabetes)
AvaData=PimaIndiansDiabetes
AvaData$diabetes=as.factor(AvaData$diabetes)
AvaN=nrow(AvaData)
#contrasts(AvaData$diabetes)

GN=round(table(AvaData$diabetes)*0.8,0)
set.seed(3)
Trainget=strata(AvaData,"diabetes",size=c(GN[[2]],GN[[1]]),method="srswor" ) 
TrainData=getdata(AvaData,Trainget)
TrainInx=TrainData$ID_unit
ValInx=c(1:AvaN)[-TrainInx]
TrainData=TrainData[,-c(10,11,12)]
ValData=AvaData[ValInx,]

#以training set建立LDA model
ModelLDA=lda(formula=diabetes~.,data=AvaData, subset=TrainInx) #此處放全資料
plot(ModelLDA)

#以validation set的X，代入建 構的LDA model，得到Y的預測情況並計算準確率
PredY=predict(ModelLDA, newdata=ValData[,-9],type="response")$class
confusionMatrix(PredY, ValData$diabetes)

#畫ROC curve & 計算AUC
win.graph()
plot.roc(as.numeric(ValData$diabetes),as.numeric(PredY),print.auc=TRUE)
