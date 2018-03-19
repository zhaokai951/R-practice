attach(mtcars)
plot(wt,mpg)#ɢ��ͼ
abline(lm(mpg~wt),col="blue")#lm:line model �������lm(a~b)���Ƕ�a=k*b+c����������ϡ�abline���ع��ߣ���һ���line��ͬ
#col   :  ��������ɫ��lty   :  ���������ͣ�lwd  :  �����Ŀ���
title("regression of MPG on weight")
detach(mtcars)�����

#The explame of graph
does<-c(20,30,40,45,60)
drugA<-c(16,20,27,40,60)
drugB<-c(15,18,25,31,40)
plot(does,drugA,type = "b")#XΪ�����꣬YΪ�����꣬p ������,l������,b��ʾ��+��

#��ԭʼ���ò��������£��޸�ͼ�β���
opar<-par(no.readonly = TRUE)#par����������һ�ݵ�ǰ��ͼ�β������ã�no.readonly=TRUE����������һ�������б�
par(lty=2)#��������Ϊ���ߣ�lty���������ͣ�lwd:��������
par(pch=17)#����Ÿ�Ϊʵ������.pch:��ͼ����.cex���Ŵ�С
plot(does,drugA,type = "b")
par(opar)#��ԭԭʼ����


#ֱ�Ӹı�ԭʼ����
plot(does,drugA,type = "b",pch=15,cex=2,lty=3,lwd=3)


#��ɫ��col
#������̶����ֵ���ɫ��col.axis
#�������ǩ����ɫ��col.lab
#�������ɫ��col.main
#��������ɫ��col.sbu
#ͼ��ǰ��ɫ/����ɫ��fg/bg
#��ɫ�ж��ֱ�ʾ����
#��ɫ�±꣺col=1
#���ƣ�col="white
#RGB��ʽ��col=rgb(1,1,1)
#ɫ��-���Ͷ�-����:col=hsv(0,0,1)


n<-20
mycolors<-rainbow(n)#�ʺ��ɫ�壬����N����ɫ
pie(rep(1,n),labes=mycolors,col=mycolors)#pie,��ͼ��rep(x,y),x=��һ������y:�ظ����Ĵ���
#�ҽ�ɫ��
mygrays<-gray(0:n/n)#����N�׻Ҷ�
pie(rep(1,n),lables=mygrays,col=mygrays)

#http://research.stowers-institute.org/efg/R/Color/Chart��ɫ��վ


#�ı������С���ŵĲ�����cex.ͬ�ϣ�cex.axis/lab/main/sub
#�ı����壺font��׺ͬ��.2�����壬3б�塣ps���������family:�����壬����serif���߼�Time new roman,sans�޳���,mono�ȿ�

#ͼ�γߴ��С��pia=c(x,y)�����ߣ�mai=c��x,y,z,e)�£��ϣ����ҵı߽����

#ʵ��
opar<-par(no.readonly = TRUE)
par(pin=c(2,3))
par(lwd=2,cex=1.5)
par(cex.axis=0.75,font.axis=2)
plot(does,drugA,type = "b",pch=19,lty=2,col="red")
plot(does,drugB,type = "b",pch=23,lty=6,col="blue",bg="green")
par(opar)

plot(does,drugA,type = "b",
     col="red",lty=2,pch=2,lwd=2,
     main="Clinical Trails for DrugA",
     sub="This is hypothetical data",
     xlab="Dosage",ylab="Drug response",
     xlim=c(0,60),ylim=c(0,70))

#Ҳ������title���������Ա�ǩ������ǩ��x���y������ƽ��в�������

plot(does,drugA,type = "b",ann = FALSE)

title(main="Clinical Trails for DrugA",col.main="red",
       sub="This is hypothetical data",col.sub="blue",
       xlab="Dosage",ylab="Drug response",
       col.lab="green",cex.lab=0.75)#�Ȼ���ann=FALSE��ͼ��

#�Զ��������᣺axis(side,at=,labels=,pos=,lty=,col=,las=,tck=)
#side:��ͼ���ı߻��������ᣬ1,2,3,4����������
#at���̶��ߵ�λ�ã�labels���̶����Աߵ����ֱ�ǩ��pos���������ཻ�ĵ㣬las����ǩ�Ƿ�ƽ���ڣ�=0�����ߣ�=2����ֱ��������
#tck���̶��߳��ȣ�������ͼ����࣬������ͼ���ڲ࣬0���ÿ̶ȣ�1�����ߣ�Ĭ��-0.01
#�Զ���Ļ�����axes=FLASE�����Դ������ᡣxaxt="n"����X��,yaxt="n"����Y��

x<-c(1:10)
y<-x
z<-10/x
opar<-par(no.readonly = TRUE)
par(mar=c(5,4,4,8)+0.1)#Ĭ��ֵ
plot(x,y,type = "b",
     pch=21,col="red",
     yaxt="n",lty=3,ann = FALSE)#û��y���title
lines(x,z,type = "b",pch=22,col="blue",lty=2)
axis(2,at=x,labels = x,col.axis="red",las="2")#��ֱX�������ϵ��������̶ȱ�ǩΪx
axis(4,at=z,labels = round(z,digits = 2),
     col.axis="blue",las=2,cex.axis=0.7,tck=-0.01)#round������С�����������룬������С����λ��
mtext("y=1/x",side=4,line=3,cex.lab=1,las=2,col="blue") #�߽������ı�
title("An Example of Creative Axes",
      xlab="X values",
      ylab="Y=X")
par(opar)
#ͼ��
does<-c(20,30,40,45,60)
drugA<-c(16,20,27,40,60)
drugB<-c(15,18,25,31,40)
opar<-par(no.readonly = TRUE)

par(lwd=2,cex=1.5,font.lab=2)
plot(does,drugA,type = "b",
     pch=15,lty=1,col="red",ylim=c(0,60),
     main="Drug A vs. Drug B",
     xlab="Drug Dosage",ylab="Drug Response")
lines(does,drugB,type = "b",
      pch=17,lty=2,col="blue")
abline(h=c(30),lwd=1.5,lty=2,col="gray")#�����ߣ�abline(h=yvalues,v=xvalues)
library(Hmisc)
minor.tick(nx=3,ny=3,tick.ratio = 0.5)#ÿ���̶���3�ȷ֡�С�̶�Ϊ��̶�һ��
legend("topleft",inset=0.05,title="Drug Type",c("A","B"),
       lty=c(1,2),pch=c(15,17),col=c("red","blue"))
#��һ��������λ�ã�������bottom,top,left,right��ϣ�center,inset:��߽��Զ
par(opar)


#�ı���ע��text:�ı���ע��ͼ���� mtext����ע�ڱ߽���
#һ����˵ text(location,name,pose)
#Ҳ����ֱ��ָ���ڵ��ϣ�text(wt,mpg,row.names(mtcars),.....)

#ͼ�����mfrow
attach(mtcars)
opar<-par(no.readonly = TRUE)
par(mfrow=c(2,2))#��������
plot(wt,mpg,main="scatterplot of wt vs.mpg")
plot(wt,disp,main="Scatterplot of wt vs. disp")
hist(wt,main = "Histogram of wt")
boxplot(wt,main="Boxplot of wt")#��ͼ
par(opar)
detach(mtcars)

#layout����
attach(mtcars)
layout(matrix(c(1,1,2,3),2,2,byrow=TRUE),
       widths=c(3,1),heights=c(1,2))   #byrow������ת��Ϊ���� 
hist(wt)
hist(mpg)
hist(disp)


#��ϸ��ͼ��fig������
#�����ò���ͼ����������ͼ�������ǣ�0,0��-��1,1������ô�����õ�һ���ο�ͼfig=c(x1,x2,y1,y2)
#Ȼ����������ͼ.��������xy�ᣬ����һ�����ϣ��ҵ�˳��
#fig=c(x5,x6,y5,y6),new=TRUE