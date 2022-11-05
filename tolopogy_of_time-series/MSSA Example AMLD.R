#set directory and import data
mydata<-read.csv("Sissili_Soum_Millet.csv")

#examine data
head(mydata)
summary(mydata)
dim(mydata)

#import (install first) Rssa package
library(Rssa)

#fit mssa model, 4-quarter windows
mssa_model<-ssa(mydata[1:28,2:3],kind="mssa",L=4)

#predict next quarter (Q2 of 2022)
Q2_2022<-predict(mssa_model,groups=list(1:3),method="recurrent",len=1)

#examine predicted values and actual values for Q2 of 2022
Q2_2022
mydata[29,]