vars<-c("mpg","hp","wt")
head(mtcars[vars])

#描述性统计分析
#基础，summary
summary(mtcars[vars])

#apply和sapply函数x
apply(mtcars,2,mean,na.rm=TRUE)#只能定义一个函数

mysatats<-function(x,na.rm=TRUE){
  if(na.rm)
    x<-x[!is.na(x)]
    m<-mean(x)
    n<-length(x)
    s<-sd(x)
    return(c(n=n,mean=m,stdev=s))
}
sapply(mtcars[vars],mysatats)#自定义一个function
apply(mtcars,2,mysatats)#如果说定义了函数，apply函数也可以进行多个结果的运算。apply函数只能用来进行矩阵的运算
apply(mtcars[vars],2,mysatats)#和sapply等价，可以指定矩阵中的具体行，列。

#拓展，pastecs包中的stat.desc()
library(pastecs)
stat.desc(mtcars[vars])

#分组进行描述，用reshape2包
library(reshape2)
dfm<-melt(mtcars,id.vars=c("am","cyl"), measure.vars=c("mpg","hp","wt"))
dstata1<-function(x){mean=mean(x)}#因为没有if语句,所以不用{}？
dstat2<-function(x){n=length(x)}
dstat3<-function(x){sd=sd(x)}
dcast(dfm,am+cyl+variable~.,dstata1)
dcast(dfm,am+cyl+variable~.,dstat2)
dcast(dfm,am+cyl+variable~.,dstat3)
#dcast用于数据框，且只能返回一个函数，比如mean.acast用于向量矩阵，这里的variable是指重铸的数据框中variable 
dstat4<-function(x){c(mean(x),sd(x))}
dcast(dfm,am+cyl+variable~.,dstat4)

#doby包
dstat5<-function(x){c(n=length(x),mean=mean(x),sd=sd(x))}
library(doBy)
summaryBy(mpg+hp+wt~am,data=mtcars,FUN=dstat5)
summaryBy(mpg+hp+wt~am+cyl,data=mtcars,FUN=dstat5)
#by函数
#搞不清楚

#列联表
library(vcd)
#一维列联表
mytable<-with(Arthritis,table(Improved))
mytable
prop.table(mytable)#转换为百分比

#二维列联表
mytable1<-xtabs(~Treatment+Improved,data=Arthritis)#这是公式模式，分类标量放前面
mytable1

attach(Arthritis)
mytable1<-table(Treatment,Improved)
mytable1
detach(Arthritis)#与上等价

#gmodels包中CrossTable是类SAS和SPSS
library(gmodels)
CrossTable(Arthritis$Treatment,Arthritis$Improved,prop.t=TRUE,prop.chisq =TRUE,chisq=TRUE,format = c("SPSS"))
CrossTable(Arthritis$Treatment,Arthritis$Improved,prop.t=TRUE,fisher = TRUE,format = c("SPSS"))
#prop.r = TRUE:行百分比，prop.c = TRUE：列百分比,或直接用prop.t,format = c("SPSS")表现为SPSS的模式
#prop.chisq =TRUE,chisq=TRUE == expected=TRUE,当期望值太小，用fisher=TRUE

#三维列联表
mytable<-xtabs(~Treatment+Sex+Improved,data=Arthritis)
ftable(addmargins(prop.table(mytable,c(1,2)),3))
ftable(addmargins(prop.table(mytable,c(1,3)),2))#可以灵活选择，#注意三维列表就表示，必定有两个分类变量

#独立性检验，感觉基本不用平时

#相关性，cor和cov函数
states<-state.x77[,1:6]
cov(states)
#也可以指定
cor(x1,x2)
#cov,协相关，use="everything"默认，有缺失值时不会显示相关结果。pairwise.complete.obs缺失值时成对删除，complete.obs行删除
#method=pearson默认，可选spearman,kendall
cor(states)


###相关性的可视化 corrgram包
options(digits = 2)
cor(mtcars)
library(corrgram)
corrgram(mtcars,order=TRUE,lower.panel=panel.shade,
#panel 平板。下方用阴影表示.
#order=TRUE时，从左上方开始看，第一列从上到下相关系数依次降低。后面同理。lable不用定义会自己按照顺序
         upper.panel=panel.pie,text.panel=panel.txt,#text用来控制输出变量最大最小值的名字
         main="Correlogram of mtcars intercorrelations")
#可以选择type=cor 或者cov
##结果解读，下方 蓝色 左下到右上表示正相关。红色左上到右下表示负相关，颜色越重相关性越大
##饼图 顺时针是正相关，逆时针负相关
#可供选择panel.pts, panel.pie, panel.shade, panel.bar, panel.ellipse, panel.conf. panel.cor.
#conf可以给出r和置信区间 cor只给出r 在图画的对角线上


####可以单独只画一边
corrgram(mtcars,lower.panel=panel.shade,
         upper.panel=NULL,text.panel=panel.txt,#text用来控制输出变量最大最小值的名字
         main="Car Mileage Data")
##order不等于TRUE，就会按照初始的变量顺序排列

##也可以自定义颜色
col.corrgram<-function(ncol){
     colorRampPalette(c("darkgoldenrod4","burlywood1","darkkhaki","darkgreen"))(ncol)}
corrgram(mtcars,order=TRUE,lower.panel=panel.shade,
         upper.panel=panel.cor,text.panel=panel.txt,
         main="Correlogram of mtcars intercorrelations",
         col.regions=col.corrgram)
#其实直接在函数里定义 col.regions=colorRampPalette(c())就可以了
##感觉不用function更好理解
help(corrgram)

#偏相关pcor(u,s),ggm包
library(ggm)
pcor(c(1,5,2,3,6),cov(states))
#c前两个变量为要计算的相关，后三个为控制的。s为协方差阵

#相关的显著性水平，psych包的corr.test函数
library(psych)
corr.test(states,use="complete",r)
#corr.test(r,q,n)偏相关的显著性，前面用pcor得到的偏相关系数，q为要控制的变量，与上表示相同，n为样本大小
#指导相关性系数r,也可以用corr.p(r,n,adjust=("holm),alpha=0.05)直接计算
#后面可以跟use= method=，与之前的相同.但注意，这里默认的是use=pairwise

#独立样本t检验 t.test(y~x,data)x为分类变量，或者t.test(y1,y2)
library(MASS)
head(UScrime)
str(UScrime)
t.test(Prob~So,data=UScrime,var.equal=TRUE)#假设方差齐性，var.equal=TRUE

#方差齐性检验
library(car)
UScrime$So1<-factor(UScrime$So,levels = c(0,1),labels = c(1,2))#需要严格的因子化
UScrime
leveneTest(UScrime$Prob,UScrime$So1)

#正态性检验
shapiro.test(UScrime$Prob)#适合小于2000的
library(nortest)
lillie.test(UScrime$Prob)#SPSS的Kolmogorov-Smirnov检验，nortest包，适合大于2000的，就是D检验

#非独立样本t检验（配对？还是两者之间有一定影响?,多用在社科）
sapply(UScrime[c("U1","U2")],function(x)(c(mean=mean(x),sd=sd(x))))
with(UScrime,t.test(U1,U2,paired=TRUE,var.equal=TRUE))#用这种格式，是因为数据框中，U1,U2分开的

#非参数检验，mann-whitney U就是wilcoxon
with(UScrime,by(Prob,So,median))
wilcox.test(Prob~So,data=UScrime)

#多组的非参数检验的比较独立用Kruskal-wallis
kruskal.test(y~x,data=)
posthoc.kruskal.nemenyi.test(y~x,data=)# 两两比较PMCMRplus包
friedman.test(y~x,data=)#去组设计或者是重复测量设计，不独立的情况

#题外话，R语言中数据框的合并


#1merge函数
#两个数据框拥有相同的时间或观测值，但这些列却不尽相同。处理的办法就是使用
#merge(x, y ,by.x = ,by.y = ,all = ) 函数。
#merge／合并
ID<-c(1,2,3,4)
name<-c("A","B","C","D")
score<-c(60,70,80,90)
student1<-data.frame(ID,name)
student2<-data.frame(ID,score)
total_student1<-merge(student1,student2,by="ID")
total_student1

#当我们需要将相同的观测对象得出的不同类型变量合并时，则采用cbind，也就是合并columm。
#2.cbind函数／纵向追加
ID<-c(1,2,3,4)
name<-c("A","B","C","D")
score<-c(60,70,80,90)
sex<-c("M","F","M","M")
student1<-data.frame(ID,name)
student2<-data.frame(score,sex)
total_student2<-cbind(student1,student2)
total_student2

#当我们需要将不同的观测对象，相同的观测变量合并时，则采用rbind，也就是合并row。
#3.rbind函数／横向追加
ID<-c(1,2,3,4)
name<-c("A","B","C","D")
student1<-data.frame(ID,name)
ID<-c(5,6,7,8)
name<-c("E","F","G","H")
student2<-data.frame(ID,name)
total_student3<-rbind(student1,student2)
total_student3

