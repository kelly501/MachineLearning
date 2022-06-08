install.packages("lattice")
install.packages("qqplot2")
library(caret); library(pROC); library(nnet)
library(sampling); library(dplyr)

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

# 由training set建構neural network model
   #‧ 設定number of unit in the hidden layer (size) =50 (建議5-100)
   #‧ 設定initial weight的range為 [-0.7,0.7]
NNfit=nnet(diabetes~., data=TrainData, size=25, range=0.7)
summary(NNfit)

NNfit$wts ##所有weight的係數估計值

#validation set的X，代入建構的model，得到Y的預測情況並計算準確率
PredY=predict(NNfit,newdata=ValData[,-9],type="class")
confusionMatrix(as.factor(PredY), ValData$diabetes)
