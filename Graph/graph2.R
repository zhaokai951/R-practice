#简单条形图 barplot，height是一个向量
library(vcd)
count<-table(Arthritis$Improved)#table列联表，得到拼数
count
barplot(count,
        main = "Simple Bar Plot",
        xlab = "Improvement",ylab = "Frequence")
#因为本身该例中的improved就是一个有序因子，所有可以直接用plot，不需要用table表格化

#如果对象是个矩阵，会出现堆砌条形图
counts<-table(Arthritis$Improved,Arthritis$Treatment)#前面是Y，后面是X
counts
barplot(counts,
        main = "Stacked Bar Plot",
        xlab = "Treatment",ylab = "Frequence",
        col=c("red","yellow","green"),
        legend=rownames(counts),#barplot中只需要自定义名字即可，但不够灵活
               beside = FALSE) #beside=FLASE 堆砌，beside=TRUE 相连

#灵活摆放图例
par(mai=c(0.5,0.5,2,2))#扩大边界
barplot(counts,
        main = "Stacked Bar Plot",
        xlab = "Treatment",ylab = "Frequence",
        col=c("red","yellow","green"),
        beside = FALSE) #beside=FLASE 堆砌，beside=TRUE 相连        
text.legend=c("None","some","marked")ncol=4
legend("topright",inset=-0.22,pch=15,legend=text.legend,col=c("red","yellow","green"),xpd=TRUE,cex=1.5)
#xpd=TRUE,允许图例在图外,ncol=4分列,text.withdth:文字整体宽度，字数太多可用
par(opar)


#均数等图形
states<-data.frame(state.region,state.x77)#前一个是州的所属区域，东南西北。后一个是州的其他变量信息
means<-aggregate(states$Illiteracy,by=list(state.region),FUN=mean)
means
means<-means[order(means$x),]
#[] 用做对象的元素索引,先用order函数，引出从小到大，1,2...在原来x中的位置，然后用中括号，依次将其重新排序
means
barplot(means$x,names.arg = means$Group.1)#是在每个条下出现的名称的向量
#names.arg只允许字符型向量成为下标，与axis不同
title("mean illiteracy rate")#等价于main

#排序函数，order
#在R中，和排序相关的函数主要有三个：sort()，rank()，order()。
#sort(x)是对向量x进行排序，返回值排序后的数值向量。
#rank()是求秩的函数，它的返回值是这个向量中对应元素的“排名”。
#而order()的返回值是对应“排名”的元素所在向量中的位置。
x<-c(97,93,85,74,32,100,99,67)
sort(x)
rank(x)
order(x)

#返回x中满足值大于50且小于90的元素在向量x中的下标
y<-sort(x)
which(x>50&x<90)#which函数，返回满足条件的值的下标


#直方图 hist
par(mfrow=c(2,2))
hist(mtcars$mpg)#在某某区间分布的频数

hist(mtcars$mpg,
     breaks=12,#把X轴平均等分为12份
     col="red",
     xlab="xx",
     main="xx")

hist(mtcars$mpg,
     freq=FALSE,#概率密度
     breaks=12,
     col="red",
     xlab="xx",
     main="xx")
lines(density(mtcars$mpg),col="blue",lwd=2)#核密度估计

x<-mtcars$mpg
h<-hist(x,
     breaks=12,
     col="red",
     xlab="xx",
     main="xx")
xfit<-seq(min(x),max(x),length=30)#seq:生成一组向量，length:相差40，lengthout：生成40个
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
#dnorm，mean为x和sd为x的正态函数在xfit每个向量上的正态函数值，返回的是值！即标准正态中某个值所对应的密度
yfit<-yfit*diff(h$mids[1:2])*length(x)#添加正态密度线
lines(xfit,yfit,col="blue",lwd=2)
box()#添加外方框

#详细解释
yfit<-yfit*diff(h$mids[1:2])*length(x)
#diff(hmids[1:2])是组距hist()默认画的是频数图，
#即纵轴为频数而density只是密度，密度约为频率/组距，
#所以将密度图像画到频数图中要进行伸缩变化，
#频数=频率/组距???组距???样本总数=密度???组距???样本总数，
#即yfit<???yfit???diff(hmids[1:2])*length(x) 

#核密度图
par(mfrow=c(2,1))
d<-density(mtcars$mpg)
plot(d)#核密度图

plot(d,main="KDOMPG")
polygon(d,col="red",border="blue")#将下面的面积填充，polygon多边形，border边界


#多个核密度图的比较，sm包
library(sm)
par(lwd=2)
attach(mtcars)
cyl.f<-factor(cyl,levels=c(4,6,8),
              labels = c("4 cylinder","6 cylinder","8 cylinder"))
sm.density.compare(mpg,cyl,xlab="MPG",main="MD by CC")
colfill<-c(2:(1+length(levels(cyl.f))))
legend(locator(1),levels(cyl.f),fill=colfill,xpd=TRUE,cex=1.5)#locator(1)：通过鼠标单击确定图例位置
colfill<-c(2:4)#和之前的函数一样，但如果一直是自定义颜色，可能可以更直接
detach(mtcars)
#箱线图boxplot(formulat,data=dataframe)formalt:y-A,为分类变量A生成Y的箱线图
boxplot(mpg~cyl,data=mtcars,
        notch=TRUE,
        varwidth=TRUE,#宽度与样本大小的平方根呈正比
        col="red",
        main="x",
        xlab="y",
        ylab="z")#生成含凹槽的箱线图

boxplot(mpg~cyl,data=mtcars,#如果只是数字，可以不用因子化，少量
        col="red",
        main="x",
        xlab="y",
        ylab="z")#生成含凹槽的箱线图

#交叉因子的箱线图
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

boxplot(mpg~am.f*cyl.f,#注意am和cyl前后的区别，am在前，是比在气缸是4下，自动和手动的区别。调换位置则相反
        data=mtcars,
        col=c("gold","darkgreen"),
        main="MPG BY AT",
        xlab="AT")
detach(mtcars)                   

#点图，dotchart(x,labels=) x为数值向量，labels为每个点标签的向量
dotchart(mtcars$mpg,labels = row.names(mtcars),cex=0.7)

#也可以进行分组排序
排序方法一
x<-mtcars[order(mtcars$mpg),]
排序方法二
y<-rank(mtcars$mpg)
z<-mtcars[order(y),]#运用中括号，表示数据框的下标，mtcars不是函数，注意区别

x$cyl<-factor(x$cyl)
x$color[x$cyl==4]<-"red"#字符型向量color被添加到数据框中，比较重要
x$color[x$cyl==6]<-"blue"
x$color[x$cyl==8]<-"darkgreen"
dotchart(x$mpg,
         labels=row.names(x),
         cex=0.7,
         groups=x$cyl,
         gcolor="black",#gcolor不同组标签的颜色
         color=x$color,
         pch=19,
         main="XXX",
         xlab="xxx")

#传统定义颜色的方式，会机械的1,2,3定义颜色，不能使用
dotchart(x$mpg,
         labels=row.names(x),
         cex=0.7,
         groups=x$cyl,
         gcolor="black",#gcolor不同组标签的颜色
         color=c("red","green","yellow"),
         pch=19,
         main="XXX",
         xlab="xxx")

dotchart(x$mpg,
         labels=row.names(x),
         cex=0.7,
         groups=x$cyl,
         gcolor="red",#gcolor不同组标签的颜,指4,6,8三个标签的颜色
         pch=19,
         main="XXX",
         xlab="xxx")
