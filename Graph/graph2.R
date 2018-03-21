#������ͼ barplot��height��һ������
library(vcd)
count<-table(Arthritis$Improved)#table���������õ�ƴ��
count
barplot(count,
        main = "Simple Bar Plot",
        xlab = "Improvement",ylab = "Frequence")
#��Ϊ���������е�improved����һ���������ӣ����п���ֱ����plot������Ҫ��table����

#��������Ǹ����󣬻���ֶ�������ͼ
counts<-table(Arthritis$Improved,Arthritis$Treatment)#ǰ����Y��������X
counts
barplot(counts,
        main = "Stacked Bar Plot",
        xlab = "Treatment",ylab = "Frequence",
        col=c("red","yellow","green"),
        legend=rownames(counts),#barplot��ֻ��Ҫ�Զ������ּ��ɣ����������
               beside = FALSE) #beside=FLASE ������beside=TRUE ����

#���ڷ�ͼ��
par(mai=c(0.5,0.5,2,2))#����߽�
barplot(counts,
        main = "Stacked Bar Plot",
        xlab = "Treatment",ylab = "Frequence",
        col=c("red","yellow","green"),
        beside = FALSE) #beside=FLASE ������beside=TRUE ����        
text.legend=c("None","some","marked")ncol=4
legend("topright",inset=-0.22,pch=15,legend=text.legend,col=c("red","yellow","green"),xpd=TRUE,cex=1.5)
#xpd=TRUE,����ͼ����ͼ��,ncol=4����,text.withdth:����������ȣ�����̫�����
par(opar)


#������ͼ��
states<-data.frame(state.region,state.x77)#ǰһ�����ݵ��������򣬶�����������һ�����ݵ�����������Ϣ
means<-aggregate(states$Illiteracy,by=list(state.region),FUN=mean)
means
means<-means[order(means$x),]
#[] ���������Ԫ������,����order������������С����1,2...��ԭ��x�е�λ�ã�Ȼ���������ţ����ν�����������
means
barplot(means$x,names.arg = means$Group.1)#����ÿ�����³��ֵ����Ƶ�����
#names.argֻ�����ַ���������Ϊ�±꣬��axis��ͬ
title("mean illiteracy rate")#�ȼ���main

#��������order
#��R�У���������صĺ�����Ҫ��������sort()��rank()��order()��
#sort(x)�Ƕ�����x�������򣬷���ֵ��������ֵ������
#rank()�����ȵĺ��������ķ���ֵ����������ж�ӦԪ�صġ���������
#��order()�ķ���ֵ�Ƕ�Ӧ����������Ԫ�����������е�λ�á�
x<-c(97,93,85,74,32,100,99,67)
sort(x)
rank(x)
order(x)

#����x������ֵ����50��С��90��Ԫ��������x�е��±�
y<-sort(x)
which(x>50&x<90)#which��������������������ֵ���±�


#ֱ��ͼ hist
par(mfrow=c(2,2))
hist(mtcars$mpg)#��ĳĳ����ֲ���Ƶ��

hist(mtcars$mpg,
     breaks=12,#��X��ƽ���ȷ�Ϊ12��
     col="red",
     xlab="xx",
     main="xx")

hist(mtcars$mpg,
     freq=FALSE,#�����ܶ�
     breaks=12,
     col="red",
     xlab="xx",
     main="xx")
lines(density(mtcars$mpg),col="blue",lwd=2)#���ܶȹ���

x<-mtcars$mpg
h<-hist(x,
     breaks=12,
     col="red",
     xlab="xx",
     main="xx")
xfit<-seq(min(x),max(x),length=30)#seq:����һ��������length:���40��lengthout������40��
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
#dnorm��meanΪx��sdΪx����̬������xfitÿ�������ϵ���̬����ֵ�����ص���ֵ������׼��̬��ĳ��ֵ����Ӧ���ܶ�
yfit<-yfit*diff(h$mids[1:2])*length(x)#������̬�ܶ���
lines(xfit,yfit,col="blue",lwd=2)
box()#�����ⷽ��

#��ϸ����
yfit<-yfit*diff(h$mids[1:2])*length(x)
#diff(hmids[1:2])�����hist()Ĭ�ϻ�����Ƶ��ͼ��
#������ΪƵ����densityֻ���ܶȣ��ܶ�ԼΪƵ��/��࣬
#���Խ��ܶ�ͼ�񻭵�Ƶ��ͼ��Ҫ���������仯��
#Ƶ��=Ƶ��/���???���???��������=�ܶ�???���???����������
#��yfit<???yfit???diff(hmids[1:2])*length(x) 

#���ܶ�ͼ
par(mfrow=c(2,1))
d<-density(mtcars$mpg)
plot(d)#���ܶ�ͼ

plot(d,main="KDOMPG")
polygon(d,col="red",border="blue")#������������䣬polygon����Σ�border�߽�


#������ܶ�ͼ�ıȽϣ�sm��
library(sm)
par(lwd=2)
attach(mtcars)
cyl.f<-factor(cyl,levels=c(4,6,8),
              labels = c("4 cylinder","6 cylinder","8 cylinder"))
sm.density.compare(mpg,cyl,xlab="MPG",main="MD by CC")
colfill<-c(2:(1+length(levels(cyl.f))))
legend(locator(1),levels(cyl.f),fill=colfill,xpd=TRUE,cex=1.5)#locator(1)��ͨ����굥��ȷ��ͼ��λ��
colfill<-c(2:4)#��֮ǰ�ĺ���һ���������һֱ���Զ�����ɫ�����ܿ��Ը�ֱ��
detach(mtcars)
#����ͼboxplot(formulat,data=dataframe)formalt:y-A,Ϊ�������A����Y������ͼ
boxplot(mpg~cyl,data=mtcars,
        notch=TRUE,
        varwidth=TRUE,#������������С��ƽ����������
        col="red",
        main="x",
        xlab="y",
        ylab="z")#���ɺ����۵�����ͼ

boxplot(mpg~cyl,data=mtcars,#���ֻ�����֣����Բ������ӻ�������
        col="red",
        main="x",
        xlab="y",
        ylab="z")#���ɺ����۵�����ͼ

#�������ӵ�����ͼ
attach(mtcars)
cyl.f<-factor(cyl,levels=c(4,6,8),
              labels = c("4","6","8"))
am.f<-factor(am,
             levels=c(0,1),
              labels=c("auto","standard"))
boxplot(mpg~cyl.f*am.f,
        data=mtcars,
        col=c("gold","darkgreen"),
        main="MPG BY AT",
        xlab="AT")

boxplot(mpg~am.f*cyl.f,#ע��am��cylǰ�������am��ǰ���Ǳ���������4�£��Զ����ֶ������𡣵���λ�����෴
        data=mtcars,
        col=c("gold","darkgreen"),
        main="MPG BY AT",
        xlab="AT")
detach(mtcars)                   

#��ͼ��dotchart(x,labels=) xΪ��ֵ������labelsΪÿ�����ǩ������
dotchart(mtcars$mpg,labels = row.names(mtcars),cex=0.7)

#Ҳ���Խ��з�������
���򷽷�һ
x<-mtcars[order(mtcars$mpg),]
���򷽷���
y<-rank(mtcars$mpg)
z<-mtcars[order(y),]#���������ţ���ʾ���ݿ���±꣬mtcars���Ǻ�����ע������

x$cyl<-factor(x$cyl)
x$color[x$cyl==4]<-"red"#�ַ�������color�����ӵ����ݿ��У��Ƚ���Ҫ
x$color[x$cyl==6]<-"blue"
x$color[x$cyl==8]<-"darkgreen"
dotchart(x$mpg,
         labels=row.names(x),
         cex=0.7,
         groups=x$cyl,
         gcolor="black",#gcolor��ͬ���ǩ����ɫ
         color=x$color,
         pch=19,
         main="XXX",
         xlab="xxx")

#��ͳ������ɫ�ķ�ʽ�����е��1,2,3������ɫ������ʹ��
dotchart(x$mpg,
         labels=row.names(x),
         cex=0.7,
         groups=x$cyl,
         gcolor="black",#gcolor��ͬ���ǩ����ɫ
         color=c("red","green","yellow"),
         pch=19,
         main="XXX",
         xlab="xxx")

dotchart(x$mpg,
         labels=row.names(x),
         cex=0.7,
         groups=x$cyl,
         gcolor="red",#gcolor��ͬ���ǩ����,ָ4,6,8������ǩ����ɫ
         pch=19,
         main="XXX",
         xlab="xxx")