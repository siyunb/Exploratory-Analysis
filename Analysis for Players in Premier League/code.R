######���ݵĵ�������ϴ
setwd("D:/������ʵ��/")					#���ù���·��
dat=read.csv("Ӣ������.csv",header=T)		#����Ӣ������
names(dat)						#�鿴������
dat0=dat 						#�����ݾ�������Ϊdat0
dat0=dat0[,-1]					#ȥ����һ����Ա����
dat0=dat0[,-31]          #ȥ������ͷ��ʧ�ܵ���Чͳ��
dim(dat0)
summary(dat0[,4])
n=dim(dat0)[1]					#�鿴������
summary(dat0)					#���ݻ���������������
dat0$����ʱ��=dat0$����ʱ��/60			#����ʱ��ת����Сʱ
######��������dat0����������
######ͳ�Ƹ�����ӽ�������
######������油����ʣ�ʵ��
#���˽�����ͳ��ֱ��ͼ
hist(dat0$����,xlab="���˽�����(��)",ylab="Ƶ��",main="Ӣ�����˽�����ͳ��",col="lightblue")
#���˳�������ͳ��ֱ��ͼ
hist(dat0$����,xlab="��Ա��������(��)",ylab="Ƶ��",main="Ӣ����Ա������ͳ��",col="lightblue")
#�鿴���˽�����ߺ���͵������۲�
dat0[dat0[,8]==min(dat0[,8]),]
dat0[dat0[,8]==max(dat0[,8]),]
#��Ӷ���Ա�������ķ�������ͼ
boxplot(����~���,data=dat0,col="lightblue",ylab="��Ա������(��)")	
######�ж���Ա�Ƿ�Ϊǰ��
�Ƿ�ǰ��<-vector(mode="numeric",length=0)#���������հ�
for(i in 1:166)
{if(dat0[i,4]=="ǰ��")
    �Ƿ�ǰ��[i]=1
     else
    �Ƿ�ǰ��[i]=0}
dat0=data.frame(dat0,�Ƿ�ǰ��)#�����кϲ���dat0������
par(mfrow=c(1,2))	#��1*2��ͼ
#��Աλ����������ķ�������ͼ
boxplot(����~λ��,data=dat0,col=c("lightblue","orange"),ylab="��Ա������(��)")	
boxplot(����~�Ƿ�ǰ��,data=dat0,col=c("lightblue","orange"),names=c("��ǰ��","ǰ��"),ylab="��Ա������(��)")
boxplot(����~����,data=dat0,col=c("lightblue","orange"),ylab="��Ա������(��)")	
par(mfrow=c(1,1))	#��1*1��ͼ
#��������ŵ�ɢ��ͼ
plot(dat0$����,dat0$����,xlab="���Ŵ���(��)",ylab="����(��)")
#���������ͷ��������ɢ��ͼ
��������<-vector(mode="numeric",length=0)#���������հ�����
for(i in 1:166)
{��������[i]=dat0[i,12]*2+dat0[i,26]+dat0[i,27]}   #��������ָ��
dat0=data.frame(dat0,��������)#�����кϲ���dat0������
library(ggplot2)
ggplot(data=dat0,aes(��������,����))+geom_point(color = "darkred")
ggplot(data=dat0,aes(x=����))+geom_histogram(bins =20, fill = 'steelblue', colour = 'black')
#######������Ա�������״�ͼ
library("devtools")
library("knitr")
library("ggradar")
devtools::install_github("ricardo-bion/ggradar",dependencies=TRUE)
#�ϳ�һ��������������,��ת��ΪЧ��
dat1=data.frame(dat0[,10]/dat0$����ʱ��,dat0[,11]/dat0$����ʱ��)
dat1=data.frame(dat1,dat0[,8]/dat0$����ʱ��,dat0[,9]/dat0$����ʱ��,dat0[,17]/dat0$����ʱ��,dat0[,18]/dat0$����ʱ��)
dat1=data.frame(dat1,dat0[,26]/dat0$����ʱ��,dat0[,27]/dat0$����ʱ��)
dat1=data.frame(dat1,dat0[,5]/dat0$����ʱ��,dat0[,6]/dat0$����ʱ��,dat0[,7])
dat1=data.frame(dat1,dat0[,12]/dat0$����ʱ��,dat0[,30]/dat0$����ʱ��)
colnames(dat1) <- c("����","����","����","����","����","����","����","��Χ","����","�׷�","����ʱ��","����","ͷ���Χ")
rownames(dat1) <- dat$��Ա
summary(dat1)   #�鿴dat1��ָ��
for(i in 1:13)
{dat1[,i]=(dat1[,i]-min(dat1[,i]))/(max(dat1[,i])-min(dat1[,i]))}    #�����������������׼��
#���������ļ���
Trap<-(dat1[,1]+dat1[,2])/2
#���������ļ���
Attack<-(dat1[,3]+dat1[,4]+dat1[,5]+dat1[,6])/4
#���������ļ���
Defense<-(dat1[,7]+dat1[,8])/2
#�����Ҫ�Եļ���
Importance<-(dat1[,9]+dat1[,10]+dat1[,11])/3
#�ƽ������ļ���
Advance<-(dat1[,12]+dat1[,13])/2
dat2=data.frame(Trap,Attack,Defense,Importance,Advance)     #�ϲ���������
#�����г�λ�÷���
dat3=rbind (dat2[69,],dat2[72,], dat2[74,],dat2[76,], dat2[77,],dat2[78,],dat2[81,],dat2[82,],dat2[83,])
rownames(dat3) <- LETTERS[1:9]
Name<-c("����ɭ","����˹","�����ϵ�˹","�����","����","��ʲ����","��������","�㴨��˾","��������")
dat3<-data.frame(Name,dat3)
ggradar(dat3)
#����ǰ��λ�÷���
dat4=rbind (dat2[70,],dat2[71,], dat2[79,])
rownames(dat4) <- LETTERS[1:3]
Name<-c("������","³��","ά������")
dat4<-data.frame(Name,dat4)
ggradar(dat4)
#��������λ�÷���
dat5=rbind (dat2[66,],dat2[67,], dat2[68,],dat2[73,], dat2[75,],dat2[80,])
rownames(dat5) <- LETTERS[1:6]
Name<-c("������","�ѵ��ϵ�","����˹","˹����","ά����","��쳶�")
dat5<-data.frame(Name,dat5)
ggradar(dat5)
#��������λ����Ա�������״�ͼ
dat6=rbind (dat2[75,],dat2[71,], dat2[69,])
rownames(dat6) <- LETTERS[1:3]
Name<-c("ά����","³��","����ɭ")
dat6<-data.frame(Name,dat6)
ggradar(dat6)
#Ѱ�Ҹ�λ����Ա�Ľ���������ƽ�����������
λ��=c("����","�г�","ǰ��")
zong=c(0,0,0)
�˾�������=c(0,0,0)
��������=c(0,0,0)
for(i in 1:166)
{if(dat0[i,4]=="����")
  {zong[1]=zong[1]+dat0[i,8]
        if (��������[1]<dat0[i,8])
          ��������[1]=dat0[i,8]}
 else if(dat0[i,4]=="�г�")
  {zong[2]=zong[2]+dat0[i,8]
        if (��������[2]<dat0[i,8])
          ��������[2]=dat0[i,8]}
 else
  {zong[3]=zong[3]+dat0[i,8]
        if (��������[3]<dat0[i,8])
          ��������[3]=dat0[i,8]}
}
�˾�������[1]=zong[1]/50
�˾�������[2]=zong[2]/86
�˾�������[3]=zong[3]/30
dat7=data.frame(λ��,�˾�������,��������)
library("reshape2")
library("ggthemes")
dat8=melt(dat7,id.vars="λ��",variable.name="ָ��",value.name="��")
  ggplot(dat8,aes(λ��,��,fill=ָ��))+
  geom_bar(stat="identity",position="dodge")+
  theme_wsj()+
  scale_fill_wsj("rgby", "")+
  theme(axis.ticks.length=unit(0.4,'cm'))+
  guides(fill=guide_legend(title=NULL))+
  theme(axis.title = element_blank())
library("corrplot")
library("psych") 
  dat9=data.frame(dat[,9],dat[,10],dat[,11],dat[,12],dat[,13],dat[,14],dat[,15],dat[,18],dat[,19],dat[,27],dat[,28],dat[,31],dat[,34])
  colnames(dat9) <- c("����","����","����","����","����","Խλ","����","����","����","����","��Χ","ͷ���Χ","����������")
  cormat<-corr.test(dat9)
  corrplot(cormat$r)
  