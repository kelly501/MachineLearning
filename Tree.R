install.packages("tree")
library(tree)

install.packages("mlbench")
library(mlbench)
data(PimaIndiansDiabetes)
AvaData=PimaIndiansDiabetes

AvaData$diabetes=as.factor(AvaData$diabetes)
AvaN=nrow(AvaData)

GN=round(table(AvaData$diabetes)*0.8,0) #先做一個table分成R、SO,再各取80%，才不會抽樣不均勻.................................
set.seed(3)
Trainget=strata(AvaData,"diabetes",size=c(GN[[2]],GN[[1]]),method="srswor" ) #GN[[1]],GN[[2]]要放的是原始資料R、SO的順序(誰先出現)
TrainData=getdata(AvaData,Trainget)
TrainInx=TrainData$ID_unit
ValInx=c(1:AvaN)[-TrainInx]
TrainData=TrainData[,-c(10,11,12)]
ValData=AvaData[ValInx,]

#由training set建構一棵classification tree
ModelTree=tree(diabetes~., data=AvaData, subset=TrainInx) #data放全資料
win.graph()
plot(ModelTree)
text(ModelTree)  #文字附上

# 由10-fold CV，畫出tree的size vs.模型deviance變異數的關係，來判斷是否prune tree
ModelTreeCV=cv.tree(ModelTree)  #用cross validation方法看不同尺寸的樹
win.graph()
plot(ModelTreeCV$size, ModelTreeCV$dev, type="b")

#怎麼剪，看圖當X軸多增加一個節點(size)時，Y軸(dev)沒有明顯下降時即停

# 由挑選的最適tree size修剪樹，畫出修剪後的樹 ，由樹梢開始剪
ModelPruneTree=prune.tree(ModelTree,best=6)
win.graph()
plot(ModelPruneTree)
text(ModelPruneTree)


#將validation data的X帶入修剪過後的樹，得到Y預測值並計算accuracy
PredProbY=predict(ModelPruneTree,ValData[,-9])
PredY=as.factor(ifelse(as.data.frame(PredProbY)$pos>0.5,"pos","neg"))
confusionMatrix(PredY, ValData$diabetes)

#畫ROC curve & 計算AUC
win.graph()
plot.roc(as.numeric(ValData$diabetes),as.numeric(PredY), print.auc=TRUE)
