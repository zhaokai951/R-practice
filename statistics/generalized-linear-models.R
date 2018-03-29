##########广义线性模型##########
###结局变量可以为二值或多分类变量或者为数值型变量，Y服从指数分布族中的一种分布即可
###常用形式  glo(formula, family=family(link=function),data=)
####family是指数家族的分布种类 binomial=logit possion=log  gaussion=identity就是正态分布（高斯分布）等等

##表示函数的指令与前面提到的类似。但注意的是不管是前面还是后面都没有求OR或者RR值的指令，给的是回归系数

###模型诊断，没有特别同意的标准
##plot(predict(model,type="response),residuals(model,type="deviance)))
#plot(hatvalues(model))
#plot(restudent(model))
#influencePlot(model)等都可以用试用

####logistic回归  AER包的affairs数据框作为例子

library(AER)
data("Affairs")
head(Affairs)#affairs偷情 rating对婚姻自我评分 affairs偷情次数
 #因子化
Affairs$ynaffair[Affairs$affairs>0]<-1
Affairs$ynaffair[Affairs$affairs==0]<-0
Affairs$ynaffair<-factor(Affairs$ynaffair,levels = c(0,1),
                         labels = c("NO","YES"))
table(Affairs$ynaffair)
fit.full<-glm(ynaffair~gender+age+yearsmarried+children+religiousness+education+occupation+rating,data=Affairs,
              family = binomial())#二元logistic 用binomial
summary(fit.full)
#结果中 Residual deviance 在592个自由度上601-8-1

#去掉不显著的变量
fit.full2<-glm(ynaffair~age+yearsmarried+religiousness+rating,data=Affairs,
              family = binomial())
summary(fit.full2)

#可以用ANOVA比较两个模型
anova(fit.full2,fit.full,test = "Chisq")
#没有差异，可以尝试用更简单的来拟合

coef(fit.full2)
#logistic回归中，响应变量是Y=1的对数优势比。
#回归系数的含义是其他不变，一单位预测变量的变化可以引起"响应变量对数"优势比的变化（增加或者减少），注意这里是+,-，
##对应线性回归模型+或者——！！！！。
#所以可以进行指数化
exp(coef(fit.full2))#对数和指数是互逆的
#指数化后，就可以解释，比如婚龄增加1年，婚外情优势比将X1.106。优势比就是OR值。注意指数化后，倍数！
#换句话说 OR=exp(β)，β=IN（OR）
confint(fit.full2)
exp(confint(fit.full2))#OR值的置信区间
#可以获得较高值的改变，比如婚龄10年，那么OR值就是1..106^10=2.7

#####以概率的形式来思考,比如婚姻评分对婚外情概率的影响
#感觉不要用ATTACH会不会更好？因为虽然ATTACH，比如AGE 依然是别的数据框的变量。要用的话，现summary看看对应的上比较好
testdata<-data.frame(rating=c(1,2,3,4,5),age=mean(Affairs$age),
                     yearsmarried=mean(Affairs$yearsmarried),
                     religiousness=mean(Affairs$religiousness))#新的数据框名字要和函数中的一致
testdata$prob<-predict(fit.full2,newdata = testdata,type="response")
testdata
#结果显示，婚姻评分由1-5 婚外情概率由53%降到15%

####过度离势  期望的方差=nπ（1-π）π为Y=1的概率，过度离势是响应变量的方差大于期望的方差
##太大时，依然可以用广义线性模型，但要改成类二项分布（quasibinomial ..
##计算方法 残差偏度/残差自由度,接近于1即可。
##也可以进行检验，即用类二项分布再检验一次，然后假设检验过度离势
fit.full3<-glm(ynaffair~age+yearsmarried+religiousness+rating,data=Affairs,
               family = quasibinomial())
pchisq(summary(fit.full3)$dispersion*fit.full2$df.residual,
       fit.full2$df.residual,lower=F)
###p不显著
help(pchisq)
summary(fit.full3)
##序数logistic回归 rms包的lrm
##稳健logistic回归 robust包的glmRob

####泊松回归 预测计数型（比如发作了几次）robust包的数据
data(breslow.dat,package="robust")
head(breslow.dat)
#base治疗前基础发病次数 sumY随机化后8周发病次数

##观察下响应变量的分布
opar<-par(no.readonly = TRUE)
par(mfrow=c(1,2))
attach(breslow.dat)    
hist(sumY,breaks = 20,xlab="Seizure Count")#seizure count癫痫发作
boxplot(sumY~Trt,xlab="Treatment",main="Group comparisions")
#泊松回归的特点 Y指数分布 两组比较，较小的方差伴随较小的均值，不关心方差异质性
fit<-glm(sumY~Base+Age+Trt,data=breslow.dat,family = poisson())
summary(fit)
coef(fit)
##泊松回归中 因变量以条件均值的对数来建模，表明保持其他不变，年龄增加一岁，发病次数的对数均值增加0.03
##和Logistic回归的应变量是一致的，都是对数形式。所以同样在初始尺度，指数化比较容易
##也就是说，我们求出的回归系数，解释起来只能说是发病数的对数，指数化后就是发病数，和logistic回归的做法是一样的
exp(coef(fit))
exp(confint(fit))
##根据结果，直接解释是变化1 增加x 而指数化后，年龄增加1岁，发病率乘以1.023。对于Trt从安慰剂转到治疗组，发病率是原来的80%

###过度离势
##原因，比如遗漏了重要的预测变量，与事件相关，比如一个发作39次的病人和只发作一次的病人，他们下次发作的概率不会相等
#还有就是重复测量时的，内在群聚性
##直接计算法与之前一样 残差偏度/自由度，本例为10.17存在过度离势（实际的方差比依据泊松分布预测的方差大）
##也可以用qcc包检验
library(qcc)
qcc.overdispersion.test(breslow.dat$sumY,type="poisson")
#p<0.05存在过度离势

#运用类泊松分布 quasi+xx ,这里是quasipoission
fit<-glm(sumY~Base+Age+Trt,data=breslow.dat,family = quasipoisson())
summary(fit)
##考虑过度离势，控制年龄和基础后，治疗的标准误增大，无差异性

##时间段分布的泊松回归
#比如本例中 假设病人随机分布后检测的长度在14-60天内变化
#fit<-glm(sumY~Base+Age+Trt,data=breslow.dat,offset=log(time),family = poisson()),当然，此处考虑比率不变

##零膨胀的泊松回归
##群体无任何计数行为。比如之前的婚外情模型，可能有些人从来没有过婚外情，称为结构零值
##用pscl包中的zeroinfl函数 就是logistic回归和泊松回归的组合 同时预测 1哪些人又会发生婚外情（0和1），
##排除了婚姻忠诚的人后调查对象会发生多少次婚外情（泊松）
##稳健泊松回归 robust包的glmRob
