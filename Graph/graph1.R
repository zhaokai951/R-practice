attach(mtcars)
plot(wt,mpg)#散点图
abline(lm(mpg~wt),col="blue")#lm:line model 线性拟合lm(a~b)就是对a=k*b+c进行线性拟合。abline即回归线，与一般的line不同
#col   :  线条的颜色；lty   :  线条的类型；lwd  :  线条的宽度
title("regression of MPG on weight")
detach(mtcars)解除绑定

#The explame of graph
does<-c(20,30,40,45,60)
drugA<-c(16,20,27,40,60)
drugB<-c(15,18,25,31,40)
plot(does,drugA,type = "b")#X为横坐标，Y为纵坐标，p 代表点,l代表线,b表示点+线

#在原始设置不变的情况下，修改图形参数
opar<-par(no.readonly = TRUE)#par函数，复制一份当前的图形参数设置；no.readonly=TRUE，用于生成一个这种列表
par(lty=2)#线条设置为虚线；lty：线条类型，lwd:线条宽度
par(pch=17)#点符号改为实心三角.pch:绘图符号.cex符号大小
plot(does,drugA,type = "b")
par(opar)#还原原始设置


#直接改变原始设置
plot(does,drugA,type = "b",pch=15,cex=2,lty=3,lwd=3)


#颜色：col
#坐标轴刻度文字的颜色：col.axis
#坐标轴标签的颜色：col.lab
#标题的颜色：col.main
#副标题颜色：col.sbu
#图形前景色/背景色：fg/bg
#白色有多种表示方法
#颜色下标：col=1
#名称：col="white
#RGB形式：col=rgb(1,1,1)
#色相-饱和度-亮度:col=hsv(0,0,1)


n<-20
mycolors<-rainbow(n)#彩虹调色板，连续N个颜色
pie(rep(1,n),labes=mycolors,col=mycolors)#pie,饼图；rep(x,y),x=任一向量，y:重复它的次数
#灰阶色彩
mygrays<-gray(0:n/n)#生成N阶灰度
pie(rep(1,n),lables=mygrays,col=mygrays)

#http://research.stowers-institute.org/efg/R/Color/Chart颜色网站


#文本字体大小缩放的参数：cex.同上，cex.axis/lab/main/sub
#文本字体：font后缀同上.2：粗体，3斜体。ps字体磅数，family:字体族，包括serif衬线即Time new roman,sans无衬线,mono等宽

#图形尺寸大小，pia=c(x,y)宽，高；mai=c（x,y,z,e)下，上，左，右的边界宽度

#实例
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

#也可以用title函数单独对标签，副标签，x轴和y轴的名称进行参数设置

plot(does,drugA,type = "b",ann = FALSE)

title(main="Clinical Trails for DrugA",col.main="red",
       sub="This is hypothetical data",col.sub="blue",
       xlab="Dosage",ylab="Drug response",
       col.lab="green",cex.lab=0.75)#先画出ann=FALSE的图形

#自定义坐标轴：axis(side,at=,labels=,pos=,lty=,col=,las=,tck=)
#side:在图的哪边汇至坐标轴，1,2,3,4，下左上右
#at：刻度线的位置，labels：刻度线旁边的文字标签。pos：坐标轴相交的点，las：标签是否平行于（=0）或者（=2）垂直于坐标轴
#tck：刻度线长度，负数：图形外侧，正数：图形内侧，0禁用刻度，1网格线，默认-0.01
#自定义的话，用axes=FLASE禁用自带坐标轴。xaxt="n"禁用X轴,yaxt="n"禁用Y轴

x<-c(1:10)
y<-x
z<-10/x
opar<-par(no.readonly = TRUE)
par(mar=c(5,4,4,8)+0.1)#默认值
plot(x,y,type = "b",
     pch=21,col="red",
     yaxt="n",lty=3,ann = FALSE)#没有y轴和title
lines(x,z,type = "b",pch=22,col="blue",lty=2)
axis(2,at=x,labels = x,col.axis="red",las="2")#垂直X轴的坐标系，坐标轴刻度标签为x
axis(4,at=z,labels = round(z,digits = 2),
     col.axis="blue",las=2,cex.axis=0.7,tck=-0.01)#round：返回小数点四舍五入，后面是小数点位数
mtext("y=1/x",side=4,line=3,cex.lab=1,las=2,col="blue") #边界添加文本
title("An Example of Creative Axes",
      xlab="X values",
      ylab="Y=X")
par(opar)
#图例
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
abline(h=c(30),lwd=1.5,lty=2,col="gray")#辅助线，abline(h=yvalues,v=xvalues)
library(Hmisc)
minor.tick(nx=3,ny=3,tick.ratio = 0.5)#每个刻度内3等分。小刻度为大刻度一半
legend("topleft",inset=0.05,title="Drug Type",c("A","B"),
       lty=c(1,2),pch=c(15,17),col=c("red","blue"))
#第一个函数是位置，可以用bottom,top,left,right组合，center,inset:离边界多远
par(opar)


#文本标注。text:文本标注在图形上 mtext：标注在边界外
#一般来说 text(location,name,pose)
#也可以直接指定在点上，text(wt,mpg,row.names(mtcars),.....)

#图形组合mfrow
attach(mtcars)
opar<-par(no.readonly = TRUE)
par(mfrow=c(2,2))#两行两列
plot(wt,mpg,main="scatterplot of wt vs.mpg")
plot(wt,disp,main="Scatterplot of wt vs. disp")
hist(wt,main = "Histogram of wt")
boxplot(wt,main="Boxplot of wt")#箱图
par(opar)
detach(mtcars)

#layout函数
attach(mtcars)
layout(matrix(c(1,1,2,3),2,2,byrow=TRUE),
       widths=c(3,1),heights=c(1,2))   #byrow把向量转换为矩阵 
hist(wt)
hist(mpg)
hist(disp)


#精细摆图：fig函数。
#先设置参照图，假设整幅图的坐标是（0,0）-（1,1），那么先设置第一个参考图fig=c(x1,x2,y1,y2)
#然后设置其他图.因左，下有xy轴，所以一般是上，右的顺序
#fig=c(x5,x6,y5,y6),new=TRUE
