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