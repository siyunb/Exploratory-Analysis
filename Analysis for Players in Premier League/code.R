######数据的导入与清洗
setwd("D:/大数据实验/")					#设置工作路径
dat=read.csv("英超数据.csv",header=T)		#读入英超数据
names(dat)						#查看变量名
dat0=dat 						#将数据矩阵命名为dat0
dat0=dat0[,-1]					#去掉第一列球员名称
dat0=dat0[,-31]          #去掉争顶头球失败的无效统计
dim(dat0)
summary(dat0[,4])
n=dim(dat0)[1]					#查看样本量
summary(dat0)					#数据基本的描述性数据
dat0$出场时间=dat0$出场时间/60			#出场时间转换成小时
######接下来对dat0做描述分析
######统计各个球队进球总数
######各球队替补（板凳）实力
#个人进球数统计直方图
hist(dat0$进球,xlab="个人进球数(个)",ylab="频数",main="英超个人进球数统计",col="lightblue")
#个人出场次数统计直方图
hist(dat0$出场,xlab="球员出场次数(次)",ylab="频数",main="英超球员出场次统计",col="lightblue")
#查看个人进球最高和最低的两条观测
dat0[dat0[,8]==min(dat0[,8]),]
dat0[dat0[,8]==max(dat0[,8]),]
#球队对球员进球数的分组箱线图
boxplot(进球~球队,data=dat0,col="lightblue",ylab="球员进球数(个)")	
######判断球员是否为前锋
是否前锋<-vector(mode="numeric",length=0)#构建出个空白
for(i in 1:166)
{if(dat0[i,4]=="前锋")
    是否前锋[i]=1
     else
    是否前锋[i]=0}
dat0=data.frame(dat0,是否前锋)#将新列合并到dat0矩阵中
par(mfrow=c(1,2))	#画1*2的图
#球员位置与进球数的分组箱线图
boxplot(进球~位置,data=dat0,col=c("lightblue","orange"),ylab="球员进球数(个)")	
boxplot(进球~是否前锋,data=dat0,col=c("lightblue","orange"),names=c("非前锋","前锋"),ylab="球员进球数(个)")
boxplot(进球~年龄,data=dat0,col=c("lightblue","orange"),ylab="球员进球数(个)")	
par(mfrow=c(1,1))	#画1*1的图
#进球和射门的散点图
plot(dat0$射门,dat0$进球,xlab="射门次数(次)",ylab="进球(个)")
#防守能力和犯规次数的散点图
防守能力<-vector(mode="numeric",length=0)#构建出个空白向量
for(i in 1:166)
{防守能力[i]=dat0[i,12]*2+dat0[i,26]+dat0[i,27]}   #防守能力指标
dat0=data.frame(dat0,防守能力)#将新列合并到dat0矩阵中
library(ggplot2)
ggplot(data=dat0,aes(防守能力,犯规))+geom_point(color = "darkred")
ggplot(data=dat0,aes(x=年龄))+geom_histogram(bins =20, fill = 'steelblue', colour = 'black')
#######画出球员能力的雷达图
library("devtools")
library("knitr")
library("ggradar")
devtools::install_github("ricardo-bion/ggradar",dependencies=TRUE)
#合成一个计算能力矩阵,并转化为效率
dat1=data.frame(dat0[,10]/dat0$出场时间,dat0[,11]/dat0$出场时间)
dat1=data.frame(dat1,dat0[,8]/dat0$出场时间,dat0[,9]/dat0$出场时间,dat0[,17]/dat0$出场时间,dat0[,18]/dat0$出场时间)
dat1=data.frame(dat1,dat0[,26]/dat0$出场时间,dat0[,27]/dat0$出场时间)
dat1=data.frame(dat1,dat0[,5]/dat0$出场时间,dat0[,6]/dat0$出场时间,dat0[,7])
dat1=data.frame(dat1,dat0[,12]/dat0$出场时间,dat0[,30]/dat0$出场时间)
colnames(dat1) <- c("传球","过人","进球","助攻","射门","射正","拦截","解围","出场","首发","出场时间","抢断","头球解围")
rownames(dat1) <- dat$球员
summary(dat1)   #查看dat1各指标
for(i in 1:13)
{dat1[,i]=(dat1[,i]-min(dat1[,i]))/(max(dat1[,i])-min(dat1[,i]))}    #对能力矩阵进行离差标准化
#控球能力的计算
Trap<-(dat1[,1]+dat1[,2])/2
#进攻能力的计算
Attack<-(dat1[,3]+dat1[,4]+dat1[,5]+dat1[,6])/4
#防守能力的计算
Defense<-(dat1[,7]+dat1[,8])/2
#球队重要性的计算
Importance<-(dat1[,9]+dat1[,10]+dat1[,11])/3
#推进能力的计算
Advance<-(dat1[,12]+dat1[,13])/2
dat2=data.frame(Trap,Attack,Defense,Importance,Advance)     #合并能力矩阵
#曼联中场位置分析
dat3=rbind (dat2[69,],dat2[72,], dat2[74,],dat2[76,], dat2[77,],dat2[78,],dat2[81,],dat2[82,],dat2[83,])
rownames(dat3) <- LETTERS[1:9]
Name<-c("安德森","吉格斯","埃尔南德斯","卡里克","纳尼","阿什利杨","克莱弗利","香川真司","巴伦西亚")
dat3<-data.frame(Name,dat3)
ggradar(dat3)
#曼联前锋位置分析
dat4=rbind (dat2[70,],dat2[71,], dat2[79,])
rownames(dat4) <- LETTERS[1:3]
Name<-c("范佩西","鲁尼","维尔贝克")
dat4<-data.frame(Name,dat4)
ggradar(dat4)
#曼联后卫位置分析
dat5=rbind (dat2[66,],dat2[67,], dat2[68,],dat2[73,], dat2[75,],dat2[80,])
rownames(dat5) <- LETTERS[1:6]
Name<-c("埃夫拉","费迪南德","埃文斯","斯马林","维迪奇","拉斐尔")
dat5<-data.frame(Name,dat5)
ggradar(dat5)
#画出典型位置球员能力的雷达图
dat6=rbind (dat2[75,],dat2[71,], dat2[69,])
rownames(dat6) <- LETTERS[1:3]
Name<-c("维迪奇","鲁尼","安德森")
dat6<-data.frame(Name,dat6)
ggradar(dat6)
#寻找各位置球员的进球总数、平均数、最大数
位置=c("后卫","中场","前锋")
zong=c(0,0,0)
人均进球数=c(0,0,0)
最大进球数=c(0,0,0)
for(i in 1:166)
{if(dat0[i,4]=="后卫")
  {zong[1]=zong[1]+dat0[i,8]
        if (最大进球数[1]<dat0[i,8])
          最大进球数[1]=dat0[i,8]}
 else if(dat0[i,4]=="中场")
  {zong[2]=zong[2]+dat0[i,8]
        if (最大进球数[2]<dat0[i,8])
          最大进球数[2]=dat0[i,8]}
 else
  {zong[3]=zong[3]+dat0[i,8]
        if (最大进球数[3]<dat0[i,8])
          最大进球数[3]=dat0[i,8]}
}
人均进球数[1]=zong[1]/50
人均进球数[2]=zong[2]/86
人均进球数[3]=zong[3]/30
dat7=data.frame(位置,人均进球数,最大进球数)
library("reshape2")
library("ggthemes")
dat8=melt(dat7,id.vars="位置",variable.name="指标",value.name="量")
  ggplot(dat8,aes(位置,量,fill=指标))+
  geom_bar(stat="identity",position="dodge")+
  theme_wsj()+
  scale_fill_wsj("rgby", "")+
  theme(axis.ticks.length=unit(0.4,'cm'))+
  guides(fill=guide_legend(title=NULL))+
  theme(axis.title = element_blank())
library("corrplot")
library("psych") 
  dat9=data.frame(dat[,9],dat[,10],dat[,11],dat[,12],dat[,13],dat[,14],dat[,15],dat[,18],dat[,19],dat[,27],dat[,28],dat[,31],dat[,34])
  colnames(dat9) <- c("进球","助攻","传球","过人","抢断","越位","犯规","射门","射正","拦截","解围","头球解围","下赛季进球")
  cormat<-corr.test(dat9)
  corrplot(cormat$r)
  