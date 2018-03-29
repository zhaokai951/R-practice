##OSL回归，普通最小二乘回归，包括简单线性回归，多项式回归和多元线性回归
#Y=B0+B1X1+B2X2。。。，BO截距项，B1,2回归系数。目的是使残差平方和列出最小
#要求，因变量的正态性，Y值之间相互独立，因变量与自变量呈线性，同方差性

#myfit<-lm(formula,data),formula:Y~X1+X2。。。
#常用符号
#x:z:X与z的交互项，例子：y~x+z+x:z
#*，x*y*z 等同于 x+y+z+x:y+x:z+y:z
#^ 交互项达到某种次数 (x+y+z)^2=x+y+z+x:y+x:z+y:z
#.  数据框中除应变量外所有变量，y~.
#- 从交互项中减去某项
# I(),算数角度重新第一，如x+(y+z)^2=x+y+z+y;z,x+I((y+z)^2)=x+h,h是y+z的平方的新的变量


##常用函数
#summary 显示模型的详细结果
#coefficients 列出模型参数（包括截距项的回归系数）
#fitted 列出模型预测值
#residuals 列出残差值
#predict：用拟合模型对新的数据集预测响应变量
#confint 列出置信区间

##x简单线性回归  x^2 多项式回归 x1,x2多元线性回归

#简单线性回归
fit<-lm(weight~height,data=women)
summary(fit)
confint(fit)
women$weight#根据下面的三个结果，这是实际值
fitted(fit)#根据每个模型，每个变量的预测值
residuals(fit)#残差即每个X(15个样本，15个值）预测值和实际值的差距，模型是让所有项的残差平方和最小
plot(women$height,women$weight,
     ylab="Height",
     xlab="Weight")#画散点图
abline(fit)#模型拟合

##多项式回归，同理
fit2<-lm(weight~height+I(height^2),data=women)
summary(fit2)
plot(women$height,women$weight,#注意先x后y
     ylab="Height",
     xlab="Weight")
lines(women$height,fitted(fit2))#注意，这里画的是用x预测y的线，并不是整个模型
abline(fit2)#因为有两项，但是图的x只有一个，所以不能直接用了
#注意，自变量有log,的依然是线性回归，
#也可以用car包的scatterplot函数
library(car)
scatterplot(weight~height,
            data=women,
            spread=FALSE,lty.smooth=1,#spread 标出标准差？
            col=c("blue"),
            pch=19,
            main="Women age",
            xlab = "Height",
            ylab="Weight")#直线：线性拟合，曲线：平滑拟合.默认画出回归曲线

##多元线性回归
state.x77
mode(state)#先检测一下模型
class(state)#先检测一下模型类型
class(state.x77)
state<-as.data.frame(state.x77[,c("Murder","Population","Illiteracy","Income","Frost")])
#重要，将矩阵转为数据框，可以选择自己想要的变量。虽然看去一样，但计算机依然需要定义矩阵为数据框
cor(state)#先检测一下相关性
library(psych)
corr.test(state,use="complete")
library(car)
scatterplot(states,spread=FALSE,lty.smooth=2,main="SPW")
fit2<-lm(Murder~Population+Illiteracy+Income+Frost,data=state)
summary(fit2)
confint(fit2)

#有交互项的回归
fit3<-lm(mpg~hp+wt+hp:wt,data = mtcars)
summary(fit3)#交互作用，比如wt取不同的值的时候，hp每改变一个单位，引起的Y变化不一致
#交互作用的可视化，effects包
library(effects)
plot(effect("hp:wt",fit3,
            xlevels=list(wt=c(2.2,3.2,4.2))),multiline=TRUE)

#函数形式，交互项，模型，xlevels=list(我们取wt的平均值和平均值±1标准差三个水平)，multiline=TRUE，表示画出直线

##回归诊断：函数是否合适
#离群点：拟合不佳，产生了巨大或正或负的残差
#高杠杆值，异常的预测变量值的组合
#强影响点对模型的产生影响过大

#建议用car包

#1.正态性：学生化残差，应该符合正态分布，QQ图应该是一条直线（car包）
library(car)
fit2<-lm(Murder~Population+Illiteracy+Income+Frost,data=state)
qqPlot(fit2,labels = row.names(state),id.method = "identify",
       simulate = TRUE,main= "Q-Q plot")
#id.method = "identify"能够交互项绘图。单击某个点，按ESC退出后，图上会显示他的标签，可以用来标注一些特殊的点
#simulate = TRUE，95%置信区间会用参数自助法
#看一下离群点,Nevada
state["Nevada",]
fitted(fit2)["Nevada"]
residuals(fit2)["Nevada"]
rstudent(fit2)["Nevada"]学生化残差
#学生化残差的正态图
residplot<-function(fit,nbreaks=10){
       z<-rstudent(fit)
       hist(z,breaks=nbreaks,freq=FALSE,xlab="SR",main="DE")#注意是breaks，之前错误，一直运行不起来
       #画正态曲线图（以标准化残差的均值及标准差画正常的正态曲线）
       curve(dnorm(x,mean=mean(z),sd=sd(z)),add=TRUE,col="blue",lwd=2)
       #拟合核密度曲线
       lines(density(z)$x,density(z)$y,col="red",lwd=2,lty=2)
}
legend("topright",  
       legend=c("Normal Curve","Kernel Density Curve"),  
       lty=1:2,col=c("blue","red"),cex=0.7)#建议，legend不要放在function里面加。运行不起来
residplot(fit2)


#2独立性，可以先验判断Y之间的独立性，或者用car包的DW检验
library(car)
durbinWatsonTest(fit2)#p大于0.05，无相关性，独立

#线性，car包的crPlot
library(car)
crPlots(fit2)#虚线为拟合曲线，实线为实际值

#同方差性
ncvTest(fit2)#P>0.05 方差齐
spreadLevelPlot(fit2)
#点在直线附近呈随机分布，说明齐性。另外，绿色曲线，表示建议幂次变换，但是最终结果幂次变换建议1.2，提示不变换
#0.5即用根号Y，代替Y。建议为0，提示Y对数变换。接近1不变换

##综合验证 gvlma包的gvlma
library(gvlma)
gvmodel<-gvlma(fit2)
summary(gvmodel)
#最上面是一般的summary(fit2)，下面根据P值>0.05，得出了Decision

##解释回归，多重共线性
#总的F检验显著，但是各个X的回归系数提示并不显著
#如用出生时间DOB和年龄预测握力，出生年龄和年龄相关性极大。DOB和握力的关系等于年龄和握力的关系
#相当于当年龄不变，预测年龄与握力的关系，矛盾
#Variance Inflation Factor方差膨胀因子检测(置信区间膨胀为与模型无关的预测变量的程度) car包的vif函数

vif(fit2)
sqrt(vif(fit2))>2#判断根号VIF是否大于2，没有大于2就行


##异常观测值 car包
outlierTest(fit2)#会出现<0.05的离群点,还有前面QQ图的方法

##高杠杆观测值（很多异常预测变量值的组合）用帽子统计量(hatvalues),p/n为帽子均值
hat.plot<-function(fit){
  p<-length(coefficients(fit))#参数数目,包含截距项,即自变量的数目+1（截距项）
  n<-length(fitted(fit))#样本量
  plot(hatvalues(fit),main="Index plot of hat values")#先画出点图
  abline(h=c(2,3)*p/n,col="red",lty=2)#h=  Y值，直接在Y轴上画水平线
  identify(1:n,hatvalues(fit),names(hatvalues(fit)))#定位函数
}
hat.plot(fit2)#2倍或者3倍以上，但高杠杆点不一定是强影响点，要看离群

##强影响点，移除会对模型产生巨大的改变
#Cook D法
cutoff<-4/(nrow(state)-length(fit2$coefficients)-2)#不包含截距项，所以减2  4/(n-k-1)样本量，自变量数目
plot(fit2,which=4,cook.levels = cutoff)#which,第4种模式
#有时候直接以1为分割点，好像更有一般性,这样的话就没有强影响点

#用来预测强影响点的影响  avPlot函数
library(car)
avPlots(fit2,ask=FALSE,onepage=TRUE,id.method="identify")
#也是交互作用的，4个变量，一次次的出来。Income的Alask是一个影响点，图中观察，删除ALASK，会向左移动

#各个点可以整合，infulencePlot
influencePlot(fit2,id.method = "identify",main="Influence Plot",
              sub="Circle size is proportial to Cook'S Distance")
#纵左边>2或<-2 离群点;水平轴超过0.2或0.3（看情况）高杠杆指，大圆圈，强影响点

##改进措施
#1删除强影响点，在数据没有错误，遵守章程和纳排标准的情况下，持保留意见

##变量更换，变换的前提是要我们能够解释，但如果变化后我们无法解释，就显得没有意义了
#应变量的变化，car包powerTransform，提高正态性
library(car)
summary(powerTransform(state$Murder))
#结果提示EST=0.6，我们可以用muder^0.5进行变化，但是本例结果显示lambad=1,p>0.05,所以没有变换必要
#自变量的变化botTidwell
boxTidwell(Murder~Population+Illiteracy,data=state)
#提示population^0.87和illiteracy^1.36能够改善，但P值也没有意义，和残差图一致

##多重共线性，删除变量，岭回归

#模型比较，anova
#本例中，income和Frost的回归系数并不显著，试试删掉他们
fit3<-lm(Murder~Population+Illiteracy,data=state)
anova(fit3,fit2)#小的，镶嵌在大的中
#结果显示p=0.994，所以可以删除

#AIC函数
AIC(fit2,fit3)#不需要考虑顺序
#最后的值越小，越要优先考虑

##多个模型的比较
#逐步回归(向前+向后) MASS包的stepAIC
library(MASS)
stepAIC(fit2,direction = "both")
#看AIC值，第一步删Frost,第二步删income,到93.76后模型不再改变(再删，值会变大)
#会找到好模型，但不一定是最佳的

#全子集回归法 leaps包的regsubsets，看调整R^2，越大越好
library(leaps)
leaps<-regsubsets(Murder~Population+Illiteracy+Income+Frost,data=state,nbest = 4)
#四个自变量，nbest=4

#可视化，2方法
plot(leaps,scale="adjr2")#scale，纵轴名字
#第一行，仅intercept和income的是最小的，最上面含P和I的是最大的

library(car)
subsets(leaps,statistic="cp",
        main="CP for ALL subsets regression")
abline(1,1,lty=2,col="red")# 鼠标点一下图，摆放图例的位置
#越好的模型，离截距项和斜率为1的直线越近
#最终还是要考虑实际情况和背景知识

###深层次分析
##评价模型的泛化能力：交叉验证，刀切法，bootstrap包的crossval
library(bootstrap)
shrinkage<-function(fit,k=10){#k重交叉的shrinkage函数，记录
            require(bootstrap)
  theta.fit<-function(x,y){lsfit(x,y)}
  theta.predict<-function(fit,x){cbind(1,x)%*%fit$coef}
 
  x<-fit$model[,2:ncol(fit$model)]
  y<-fit$model[,1]
  
  results<-crossval(x,y,theta.fit,theta.predict,ngroup=k)
  r2<-cor(y,fit$fitted.values)^2
  r2cv<-cor(y,results$cv.fit)^2
  cat("origin R-square=",r2,"\n")
  cat(k,"F cros- V R-square=",r2cv,"\n")
  cat("change=",r2-r2cv,"\n")
}

shrinkage(fit2)#原本的0.567过于乐观，应该是0.488
shrinkage(fit3)#改进后，变成0.519


##相对重要性
#标准化后，直接比较回归系数（一个标准差变化引起的响应变量的变化）
zstate<-as.data.frame(scale(state))#标准化后是矩阵，转换一下
zfit<-lm(Murder~Population+Illiteracy+Income+Frost,data=zstate)
coef(zfit)#x的一个标准差的变化，引起Y的（回归系数）个标准差的变化

##相对权重（每个预测变量对R^2的贡献）
relweights <- function(fit, ...) {
  R <- cor(fit$model)#就是应变量和自变量组成的列表的，相关系数
  nvar <- ncol(R)#所有的相关系数
  rxx <- R[2:nvar, 2:nvar]
  rxy <- R[2:nvar, 1]
  svd <- eigen(rxx)
  evec <- svd$vectors
  ev <- svd$values
  delta <- diag(sqrt(ev))
  
  # correlations between original predictors and new orthogonal variables
  lambda <- evec %*% delta %*% t(evec)#涉及到矩阵知识
  lambdasq <- lambda^2
  
  # regression coefficients of Y on orthogonal variables
  beta <- solve(lambda) %*% rxy
  rsquare <- colSums(beta^2)
  rawwgt <- lambdasq %*% beta^2
  import <- (rawwgt/rsquare) * 100
  lbls <- names(fit$model[2:nvar])
  rownames(import) <- lbls
  colnames(import) <- "Weights"
  
  # plot results
  barplot(t(import), names.arg = lbls, ylab = "% of R-Square", 
          xlab = "Predictor Variables", main = "Relative Importance of Predictor Variables", 
          sub = paste("R-Square = ", round(rsquare, digits = 3)), 
          ...)
  return(import)
}

relweights(fit2)
#结果和前面的标准化有所不同？定义不一样主要