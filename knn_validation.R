install.packages("mlbench")
library(mlbench)
library(class);library(caret)
install.packages("sampling")
library(sampling); library(dplyr)
data(PimaIndiansDiabetes)
AvaData=PimaIndiansDiabetes
AvaData$diabetes=as.factor(AvaData$diabetes)
AvaDataX=AvaData[,-9]
AvaDataY=AvaData[, 9]

AvaData$diabetes=as.factor(AvaData$diabetes)
AvaN=nrow(AvaData)

#contrasts(AvaData$diabetes)
GN=round(table(AvaData$diabetes)*0.8,0)
set.seed(3)
Trainget=strata(AvaData,"diabetes",size=c(GN[[2]],GN[[1]]),method="srswor" )
TrainData=getdata(AvaData,Trainget)
TrainInx=TrainData$ID_unit
ValInx=c(1:AvaN)[-TrainInx]
TrainDataX=AvaDataX[TrainInx,]
TrainDataY=AvaDataY[TrainInx]
ValDataX=AvaDataX[ValInx,]
ValDataY=AvaDataY[ValInx]

#table(AvaData$diabetes)
#table(TrainDataY)
#table(ValDataY)

#暗綟﹡1-100┮Τ非絋瞯
#砞﹚tuning parameter (k=1,2,,100)training setミknnだ摸家
#validation setXだパk=1,2,,100篶knn家眔Y箇代薄猵璸衡岿粇瞯/非絋瞯

AccuracyAll=rep(1:100)
for(i in 1:100){
	PredY=knn(train=TrainDataX,test=ValDataX, cl=TrainDataY, k=i, prob=F)
	#AccuracyAll[i]=confusionMatrix(PredY, ValDataY) #玡タ絋岿粇
	#AccuracyAll[i]=confusionMatrix(PredY, ValDataY)$overall
	AccuracyAll[i]=confusionMatrix(PredY, ValDataY)$overall["Accuracy"]
}

#匡程非絋瞯 тvalidation error 程(┪accuracy程)k
OptimalK=which.max(AccuracyAll)

#礶瓜
win.graph()
plot(c(1:100), AccuracyAll, pch=19, xlab="K", ylab="Validation Accuracy", type="b")


# 程続k & available set 篶knn model
#knn(train=AvaDataX,test=TestDataX, cl=AvaDataY, k=OptimalK, prob=F)

#沽刚芠诡best kconfusion matrix and 贺非絋瞯
i=OptimalK
PredY=knn(train=TrainDataX,test=ValDataX, cl=TrainDataY, k=i, prob=F)
confusionMatrix(PredY, ValDataY)
win.graph()
plot.roc(as.numeric(ValDataY),as.numeric(PredY), print.auc=TRUE)


