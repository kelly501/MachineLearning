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
TrainData=TrainData[,-c(10,11,12)]
ValData=AvaData[ValInx,]

install.packages("randomForest")
library(randomForest)

#パtraining set篶bagging model  #mtry:戈X跑计计
ModelBag=randomForest(diabetes~., data=AvaData,subset=TrainInx, mtry=8, importance=T)

# パvalidation setX盿bagging model箇代Y璸衡非絋瞯
PredY=predict(ModelBag,newdata=ValData[,-9],type="response")


#芠诡X璶┦
confusionMatrix(PredY, ValData$diabetes)
importance(ModelBag)
varImpPlot(ModelBag)

win.graph()
plot.roc(as.numeric(ValData$diabetes),as.numeric(PredY), print.auc=TRUE)
