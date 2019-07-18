################三、数据探索性分析############
#采用主成分分析方法对变量进行降维
data=read.csv("D:/大数据作业/大数据统计基础考试/2017《大数据统计基础》考试题/2017《大数据统计基础》考试题/英超数据.csv")
library(psych)
library(corrplot)
summary(data)
dim(data)
#选取连续型变量分析
data_var=data[,c(2,6:31,33:34)]
mycor=cor(data_var[,-29])  
corrplot(mycor,type = "lower",tl.cex = 0.5)#查看相关性情况
########################主成分分析##################
data_s=scale(data_var[,-29])
pca_fit=princomp(data_s)
summary(pca_fit)
#我们选取累计贡献率达到82.2%的主成分个数9个
plot(pca_fit,type ="lines",ylim=c(0,8))
scores=round(pca_fit$scores,2)#因子得分
scoresdata=data.frame(data_var[29],scores[,1:9])#创建回归模型数据
#下一年进球为被解释变量，主成分为解释变量进行多元回归分析
names(scoresdata)=c('y','x1','x2','x3','x4','x5','x6','x7','x8','x9')
fit=lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9,data = scoresdata)
summary(fit)
#采用逐步回归对变量进行筛选
tstep<-step(fit)
summary(tstep)
##########################因子分析#####################
correlations=cor(data_var[,-29])
fa.parallel(correlations,n.obs =166,fa="fa",
            show.legend=FALSE)
#前5个特征值大于模拟的特征值均值，所以选取5个因子
fa=fa(data_var[,-29],nfactors=5,rotate="varimax",
      fm="gls",scores=T)
#进行因子旋转（旋转方法为“varimax”，因子提取方法设为主成分法
head(fa$scores)#因子得分
scores=round(fa$scores,2)#因子得分
scoresdata=data.frame(data_var[29],scores)#创建回归模型数据
#下一年进球为被解释变量，主成分为解释变量进行多元回归分析
names(scoresdata)=c('y','x1','x2','x3','x4','x5')
fit=lm(y~x1+x2+x3+x4+x5,data = scoresdata)
summary(fit)
#多个变量不显著，利用逐步回归筛选合适变量
tstep<-step(fit)
summary(tstep)
