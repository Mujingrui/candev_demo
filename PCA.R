data2<-read.csv("F:/data2.csv")
head(data2)
data2.active<-data2[3:18]
#PCA
library("FactoMineR")
library("factoextra")
res.pca <- PCA(data2.active, graph = FALSE)
var<-get_pca_var(res.pca)
head(var$coord,4)
fviz_pca_var(res.pca, col.var = "black")
head(var$cos2)
library("corrplot")
# is.corr表示输入的矩阵不是相关系数矩阵
corrplot(var$cos2, is.corr=FALSE)

fviz_cos2(res.pca, choice = "var", axes = 1:2)

fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)

eig.val <- get_eigenvalue(res.pca)
eig.val

fviz_screeplot(res.pca)
#fviz_pca_var(res.pca)

var$cos2
class(var$cos2)

corr<-data.frame(var$cos2)
write.csv(corr, file ="F:\\corr.csv")

data3<-read.csv("F:/data3.csv")
library(ggplot2)
library(glmnet)
library(reshape2)
X<-data3[,2:26]
head(X)
Y<-data3[,27]
model<-cv.glmnet(X,Y,family="binomial",type.measure="class",nfolds = 10,alpha = 1)
plot(model)
model$lambda.1se
model.final<-model$glmnet.fit
model1<-glm(Y~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X14+X15+X16+X17+X18+X19+X20+X21+X22+X23+X24+X25,family =binomial(link="logit"),data=data3)
model1





