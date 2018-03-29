################################ANOVA#####################

####基本术语####
#组间因子
#组内因子
#观测数目相同：均衡设计
#ANOVA, ANCOVA(协方差),MANOVA（多元，应变量不止一个）,MANCOVA.

##表达式
#anova  y~A
#ancova y~x+A(协变量放前面)
#双因素ANOVA  Y~A*B(交互)
#两个协变量的ANCOVA  y~x1+x2+A*B
#区组，y~B+A (B是区组因子)
#单因素组内ANOVA，y~A+Error(subject/A)
#单个组内因子和单个组间因子的重复测量设计  y~B*W+Error（Subject/W)
#R中的ANOVA是有顺序的 y~A*B 不等于y~B*A（序惯型），结果为A对Y的影响，控制A B对Y影响，控制AB主效应的交互作用

##排序原则
#基础效应在表达式前面
#首先是协变量，然后是主效应，然后是两因素交互，然后是三因素交互
#主效应，基础的在前面，比如性别要在处理方式前面
#SPSS和SAS是边界型，想要这种，可以用car包的Anova，aov()是第一种方法

####one way anova
##正态性检验 
shapiro.test(cholesterol$response)#可以用前面提到的
library(car)
qqPlot(lm(response~trt,data=cholesterol),main="Q-Q test",labels=FALSE,simulate = TRUE)
#或者用QQ图,simulate = TRUE给出置信区间by parametric bootstrap，数据落在95%置信区间内即可

##方差齐性
bartlett.test(response~trt,data = cholesterol)
leveneTest(cholesterol$response,cholesterol$trt)

help("qqPlot")
library(multcomp)
attach(cholesterol)
table(trt)#treatment
aggregate(response,by=list(trt),FUN=mean)
aggregate(response,by=list(trt),FUN=sd)
fit<-aov(response~trt)
summary(fit)

##绘图，带置信区间
library(gplots)
plotmeans(response~trt,xlab="TRT",ylab = "Response",main="mean plot/with 95%CI")
detach(cholesterol)


####多重比较
detach("package::HH")#该函数与HH包有兼容问题，如果之前用了HH包，先退出该包
TukeyHSD(fit)

###可视化1
par(las=2)
par(mar=c(5,8,4,2))
plot(TukeyHSD(fit))#置信区间过0，代表无意义
par(opar)
###可视化2
library(multcomp)
par(mar=c(5,4,6,2))
tuk<-glht(fit,linfct=mcp(trt="Tukey"))
#例子：glht(amod, linfct = mcp(tension = "Tukey"))set up all-pair comparisons for factor `tension'。也可以指定某几个因子比较
#也可以用在混合线性模型等
plot(cld(tuk,level=0.05),col="lightgrey")
#cld：为成对比较的展现一个简单的结果，帮助中的例子可以用来理解glht和cld两个函数
#有相同字母的表示显著性不明显
help(cld)
cld(tuk,level=0.05)


#########单因素协方差分析ancova##########
data(litter,package = "multcomp")
attach(litter)
head(litter)
table(dose)
aggregate(weight,by=list(dose),FUN=mean)
fit<-aov(weight~gesttime+dose)
summary(fit)
detach(litter)
##获得调整协变量后的组均值 effects函数
library(effects)
effect("dose",fit)
library(multcomp)
contrast<-glht(fit,linfct = mcp(dose="Tukey"))
summary(contrast)

contrasts<-rbind("no durg vs drug"=c(3,-1,-1,-1))
summary(glht(fit,linfct=mcp(dose=contrasts)))
#设定第一组和其他三组的均值（合并）的比较。依然得不出0与其他内部两两比较的结论。设定矩阵.矩阵之和为0
contrasts<-rbind("no durg vs drug"=c(1,0,-1,0))
summary(glht(fit,linfct=mcp(dose=contrasts)))
#该矩阵就是设置为第一组和第二组的比较

####ANCOVA除了正态性与方差齐性，还需要假定回归斜率相同,假设回归斜率相同，那么应该没有交互项
fit<-aov(weight~gesttime*dose,data=litter)
summary(fit)
#结果显示，交互作用不明显，支持斜率相等。如果说斜率不等，用sm.ancova

###ancova的可视化，HH包
library(HH)#先安装scales,lazyeval和htmlTable三个包
ancova(weight~gesttime+dose,data=litter)
#截距项变化，几乎平行
ancova(weight~gesttime*dose,data=litter)
#斜率也会有变化，虽然不知道怎么看

#################双因素ANOVA#######
attach(ToothGrowth)
head(ToothGrowth)
table(supp,dose)
aggregate(len,by=list(supp,dose),FUN=mean)

fit<-aov(len~supp*dose)
summary(fit)
####可视化
##1
interaction.plot(dose,supp,len,type = "b",col=c("red","blue",pch=c(16,18),main="Interaction bewteen Odse and supp"))
help("interaction.plot")#画出双因素ANOVA的交互图，顺序为x1,x2,y
##2
library(gplots)
plotmeans(len~interaction(supp,dose,sep=""),
          connect=list(c(1,3,5),c(2,4,6)),#用于排序每个变量的位置
          col=c("red","blue"),
          main="Interaction PLot with 95CI",
          xlab="Treatmen and Dose Combination")

plotmeans(len~interaction(supp,dose,sep=""),
          connect=list(c(1,2,3,4,5,6)),
#用于排序每个变量的位置,按照矩阵顺序，依次是oj0.5,vc0.5,oj1共6个，给这6个点孙旭
          col=c("red","blue"),
          main="Interaction PLot with 95CI",
          xlab="Treatmen and Dose Combination")

help("plotmeans")

##3
library(HH)
interaction2wt(len~supp*dose)
#左上的直线两点代表两种方法，左上是以剂量为基础，看方法。坐下是把剂量合起来，只看方法
#右下是剂量为基础看方法，右上是方法合起来，看剂量。左上和右下可以观察交互作用

####重复测量方差分析#一个组内因子，一个组间因子
w1b1<-subset(CO2,Treatment=='chilled')#subset从一个数据库中选出符合条件的
w1b1
attach(w1b1)
fit<-aov(uptake~conc*Type+Error(Plant/(conc)))#时间是对于组体而言的
summary(fit)
par(las=1)#坐标轴标签方向
interaction.plot(conc,Type,uptake,type = "b",col=c("red","blue",pch=c(16,18),main="Interaction bewteen Type and conc"))
##本例中co2是长格式，适用于时间相关的分析。前面的litter是宽格式，适用于一般情况


###长数据一般是指数据集中的变量没有做明确的细分，即变量中至少有一个变量中的元素存在值严重重复循环的情况（可以归为几类）
#表格整体的形状为长方形，即 变量少而观察值多 ######dcast长数据转为宽数据
###宽数据是指数据集对所有的变量进行了明确的细分，各变量的值不存在重复循环的情况也无法归类.
#数据总体的表现为 变量多而观察值少。 ####metl宽数据转为长数据


#####混合线性模型lme4包的lmer 需要Matrix包
library(Matrix)
library(lme4)
help("lme4")

######改变列的名字
#fix(x)
#colnames(x)[n]<-""


#####多元方差分析 应变量不止一个
library(MASS)
UScereal
attach(UScereal)
y<-cbind(calories,fat,sugars)
fit<-aov(y~shelf)
summary(fit)
####需要满足多远正态性与方差-协方差同质性
###多远正态性 QQ图
center<-colMeans(y)
n<-nrow(y)
p<-ncol(y)#自由度，样本中能独立自由变化的变量.该例中，三个因变量是两个独立变量，所以是列数
cov<-cov(y)
d<-mahalanobis(y,center,cov)#马氏距离，协方差距离
coord<-qqplot(qchisq(ppoints(n),df=p),#该例使用QQplot的卡方检验距离，n为样本量，先确定qchisq，再输Y,y为马氏距离
              d,main="Q-Q plot",
              ylab = "D2")
abline(a=0,b=1)#a截距，b斜率，截距是均值，斜率为标准差。如果是正态分布那么应该是一个截距为0，均值为1的直线
identify(coord$x,coord$y,labels=row.names(UScereal))#交互标出离群点

####协方差同质性
#暂时不能实现，一般来说似乎不必要？

#####用回归来做ANOVA

##ANOVA的方法
library(multcomp)
fit.aov<-aov(response~trt,data=cholesterol)
summary(fit.aov)#解释下结果中的DF为什么是4，trt有五个水平，但如果确定了4个水平的值，另一个水平是固定的。所以4

##用回归拟合
fit.lm<-lm(response~trt,data = cholesterol)
summary(fit.lm)
##当线性模型遇到因子时，会用因子对应的数值来代替因子。本例结果中，1time被当成参照组
#一般来说无序因子用对照处理，有序因子用正交多项式
fit.lm<-lm(response~trt,data = cholesterol,contrasts=list(trt="contr.helmert"))
summary(fit.lm)
#contr.  treatment 以第一水平为对照  SAS最后水平为对照  
#helmert以前面所有均值对照，注意，helmert是从第二个开始，但会显示为A1,A2这样子
# poly有序因子 正交多项式.如果说因变量有a b c 个因子，可以根据不同设置为 a= b= c=
#treatment为默认的设置，也是和SPSS一样的
