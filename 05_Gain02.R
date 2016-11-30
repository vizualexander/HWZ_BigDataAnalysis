
install.packages("ModelGood")

IVF <- read.table("http://192.38.117.59/~tag/Teaching/share/data/IVF.txt",header=TRUE)

summary(IVF)

library(ROCR)
fsh.pred <- prediction(-IVF$fsh, IVF$response=="positive")
fsh.perf <- performance(fsh.pred,"tpr","fpr")
plot(fsh.perf)

library(Epi)
ROC(form = response ~ fsh,data=IVF, plot="ROC")

library(pROC)
with(IVF,roc(response,fsh,percent=TRUE, plot=TRUE, ci=TRUE,direction=">"))


library(ModelGood)
plot(Roc(response~I(-fsh),data=IVF),diag=TRUE)



