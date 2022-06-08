install.packages("lattice")
install.packages("ggplot2")
library(class);library(caret);library(pROC)
library(sampling); library(dplyr); library(tree)

install.packages("mlbench")
library(mlbench)
data(PimaIndiansDiabetes)
AvaData=PimaIndiansDiabetes

AvaData$diabetes=as.factor(AvaData$diabetes)
AvaN=nrow(AvaData)

contrasts(AvaData$diabetes)
GN=round(table(AvaData$diabetes)*0.8,0)
set.seed(3)
Trainget=strata(AvaData,"diabetes",size=c(GN[[2]],GN[[1]]),method="srswor" )
TrainData=getdata(AvaData,Trainget)
TrainInx=TrainData$ID_unit
ValInx=c(1:AvaN)[-TrainInx]
TrainData=TrainData[-c(10,11,12)]
ValData=AvaData[ValInx,]

#由training set建構random forest model  #此處randomforest方法，設定每次取3變數
ModelRF=randomForest(diabetes~., data=AvaData, subset=TrainInx, mtry=3, importance=T)

#由validation set的X帶入bagging model預測Y，並計算準確率
PredY=predict(ModelRF,newdata=ValData[,-9],type="response")

#觀察各個X的重要性
confusionMatrix(PredY, ValData$diabetes)
importance(ModelRF)
varImpPlot(ModelRF)

win.graph()
plot.roc(as.numeric(ValData$diabetes),as.numeric(PredY), print.auc=TRUE)
