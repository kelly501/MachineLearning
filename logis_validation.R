install.packages("lattice")
install.packages("ggplot2")
library(lattice);library(ggplot2);
library(caret); library(pROC); 
install.packages("sampling")
library(sampling); library(dplyr)

install.packages("mlbench")
library(mlbench)
data(PimaIndiansDiabetes)
AvaData=PimaIndiansDiabetes
AvaData$diabetes=as.factor(AvaData$diabetes)
AvaN=nrow(AvaData)
#contrasts(AvaData$diabetes)
#round=0 因要四捨五入到整數
GN=round(table(AvaData$diabetes)*0.8,0) 
set.seed(3)
Trainget=strata(AvaData,"diabetes",size=c(GN[[2]],GN[[1]]),method="srswor" )  #method 隨機抽樣取後不放回
TrainData=getdata(AvaData,Trainget)
TrainInx=TrainData$ID_unit
ValInx=c(1:AvaN)[-TrainInx]
TrainData=select(TrainData,-c("ID_unit","Prob","Stratum"))
ValData=AvaData[ValInx,]

#Model 1: full model
ModelLog1=glm(formula=diabetes~.,family=binomial,data=TrainData)
PreProb1=predict(ModelLog1, newdata=ValData[,-9],type="response")
PredY1=as.factor(ifelse(PreProb1>0.5, "pos", "neg")) 
confusionMatrix(PredY1, ValData$diabetes)
win.graph()
plot.roc(as.numeric(ValData$diabetes),as.numeric(PredY1), print.auc=TRUE)


