install.packages("lattice")
install.packages("ggplot2")
library(caret); library(pROC); 
library(sampling); library(dplyr); library(e1071)

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

#Find an optimal cost

#設定各種cost(tuning parameter)的值
Costlist=c(0.001,0.01,0.1,1,5,10,100)
AccuSumm=rep(0,length(Costlist))

# 對每個tuning parameter值，以training set建構model，並以validation set的X，代入建構的model，得到Y的預測情況並計算準確率
for(i in 1:length(Costlist)){
	svmfitTemp=svm(diabetes~., data=TrainData,kernel="linear", cost=Costlist[i], scale=F)
	PredYTemp=predict(svmfitTemp, newdata=ValData[,-9],type="response")
	AccuSumm[i]=confusionMatrix(PredYTemp, ValData$diabetes)$overall["Accuracy"]
}

#找best tuning parametervalue (準確率最高)
BestC=which.max(AccuSumm)

#由vailable set 和 best tuning parameter value建構最終模型
svmfit=svm(diabetes~., data=AvaData, kernel="linear",cost=Costlist[BestC], scale=F)

#validation set的X，代入建構的model，得到Y的預測情況並計算準確率
PredY=predict(svmfit, newdata=ValData[,-9],type="response")

#畫ROC curve & 計算AUC
confusionMatrix(PredY, ValData$diabetes)
win.graph()
plot.roc(as.numeric(ValData$diabetes),as.numeric(PredY),print.auc=TRUE)
