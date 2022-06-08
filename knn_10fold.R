install.packages("mlbench")
library(mlbench)
library(class);library(caret)
AvaData=PimaIndiansDiabetes
AvaData$diabetes=as.factor(AvaData$diabetes)
AvaDataX=AvaData[,-9]
AvaDataY=AvaData[, 9]
AvaN=nrow(AvaData)

FoldK=10
set.seed(2)
CFold=createFolds(c(1:AvaN),k=FoldK, returnTrain=T)

AccuracyAll=rep(1:100)
for(i in 1:100){
	AccuracyFold=rep(NA,FoldK)
	for(j in 1:FoldK){
		TrainInxTemp=CFold[[j]]
		ValInxTemp=c(1:AvaN)[-TrainInxTemp]
		TrainDataX=AvaDataX[TrainInxTemp,]
		TrainDataY=AvaDataY[TrainInxTemp]
		ValDataTempX=AvaDataX[ValInxTemp,]
		ValDataTempY=AvaDataY[ValInxTemp]
		PredY=knn(train=TrainDataX,test=ValDataTempX, cl= TrainDataY, k=i, prob=F)
		AccuracyTemp=confusionMatrix(PredY, ValDataTempY)$overall["Accuracy"]
		AccuracyFold[j]=AccuracyTemp
	}
	AccuracyAll[i]=mean(AccuracyFold)
}
OptimalK=which.max(AccuracyAll)

win.graph()
plot(c(1:100), AccuracyAll, pch=19, xlab="K", ylab="Validation Accuracy", type="b")

#knn(train=AvaDataX,test=TestDataX, cl=AvaDataY, k=OptimalK, prob=F)


#因在K-fold方法中分成十群在找出最佳K，不用再個別畫出confution matrix