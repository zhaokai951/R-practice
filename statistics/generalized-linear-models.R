##########��������ģ��##########
###��ֱ�������Ϊ��ֵ�������������Ϊ��ֵ�ͱ�����Y����ָ���ֲ����е�һ�ֲַ�����
###������ʽ  glo(formula, family=family(link=function),data=)
####family��ָ������ķֲ����� binomial=logit possion=log  gaussion=identity������̬�ֲ�����˹�ֲ����ȵ�

##��ʾ������ָ����ǰ���ᵽ�����ơ���ע����ǲ�����ǰ�滹�Ǻ��涼û����OR����RRֵ��ָ������ǻع�ϵ��

###ģ����ϣ�û���ر�ͬ��ı�׼
##plot(predict(model,type="response),residuals(model,type="deviance)))
#plot(hatvalues(model))
#plot(restudent(model))
#influencePlot(model)�ȶ�����������

####logistic�ع�  AER����affairs���ݿ���Ϊ����

library(AER)
data("Affairs")
head(Affairs)#affairs͵�� rating�Ի����������� affairs͵�����
 #���ӻ�
Affairs$ynaffair[Affairs$affairs>0]<-1
Affairs$ynaffair[Affairs$affairs==0]<-0
Affairs$ynaffair<-factor(Affairs$ynaffair,levels = c(0,1),
                         labels = c("NO","YES"))
table(Affairs$ynaffair)
fit.full<-glm(ynaffair~gender+age+yearsmarried+children+religiousness+education+occupation+rating,data=Affairs,
              family = binomial())#��Ԫlogistic ��binomial
summary(fit.full)
#����� Residual deviance ��592�����ɶ���601-8-1

#ȥ���������ı���
fit.full2<-glm(ynaffair~age+yearsmarried+religiousness+rating,data=Affairs,
              family = binomial())
summary(fit.full2)

#������ANOVA�Ƚ�����ģ��
anova(fit.full2,fit.full,test = "Chisq")
#û�в��죬���Գ����ø��򵥵������

coef(fit.full2)
#logistic�ع��У���Ӧ������Y=1�Ķ������Ʊȡ�
#�ع�ϵ���ĺ������������䣬һ��λԤ������ı仯��������"��Ӧ��������"���Ʊȵı仯�����ӻ��߼��٣���ע��������+,-��
##��Ӧ���Իع�ģ��+���ߡ�������������
#���Կ��Խ���ָ����
exp(coef(fit.full2))#������ָ���ǻ����
#ָ�����󣬾Ϳ��Խ��ͣ������������1�꣬���������ƱȽ�X1.106�����ƱȾ���ORֵ��ע��ָ�����󣬱�����
#���仰˵ OR=exp(��)����=IN��OR��
confint(fit.full2)
exp(confint(fit.full2))#ORֵ����������
#���Ի�ýϸ�ֵ�ĸı䣬�������10�꣬��ôORֵ����1..106^10=2.7

#####�Ը��ʵ���ʽ��˼��,����������ֶԻ�������ʵ�Ӱ��
#�о���Ҫ��ATTACH�᲻����ã���Ϊ��ȻATTACH������AGE ��Ȼ�Ǳ�����ݿ�ı�����Ҫ�õĻ�����summary������Ӧ���ϱȽϺ�
testdata<-data.frame(rating=c(1,2,3,4,5),age=mean(Affairs$age),
                     yearsmarried=mean(Affairs$yearsmarried),
                     religiousness=mean(Affairs$religiousness))#�µ����ݿ�����Ҫ�ͺ����е�һ��
testdata$prob<-predict(fit.full2,newdata = testdata,type="response")
testdata
#�����ʾ������������1-5 �����������53%����15%

####��������  �����ķ���=n�У�1-�У���ΪY=1�ĸ��ʣ�������������Ӧ�����ķ�����������ķ���
##̫��ʱ����Ȼ�����ù�������ģ�ͣ���Ҫ�ĳ������ֲ���quasibinomial ..
##���㷽�� �в�ƫ��/�в����ɶ�,�ӽ���1���ɡ�
##Ҳ���Խ��м��飬���������ֲ��ټ���һ�Σ�Ȼ���������������
fit.full3<-glm(ynaffair~age+yearsmarried+religiousness+rating,data=Affairs,
               family = quasibinomial())
pchisq(summary(fit.full3)$dispersion*fit.full2$df.residual,
       fit.full2$df.residual,lower=F)
###p������
help(pchisq)
summary(fit.full3)
##����logistic�ع� rms����lrm
##�Ƚ�logistic�ع� robust����glmRob

####���ɻع� Ԥ������ͣ����緢���˼��Σ�robust��������
data(breslow.dat,package="robust")
head(breslow.dat)
#base����ǰ������������ sumY�������8�ܷ�������

##�۲�����Ӧ�����ķֲ�
opar<-par(no.readonly = TRUE)
par(mfrow=c(1,2))
attach(breslow.dat)    
hist(sumY,breaks = 20,xlab="Seizure Count")#seizure count��﷢��
boxplot(sumY~Trt,xlab="Treatment",main="Group comparisions")
#���ɻع���ص� Yָ���ֲ� ����Ƚϣ���С�ķ�������С�ľ�ֵ�������ķ���������
fit<-glm(sumY~Base+Age+Trt,data=breslow.dat,family = poisson())
summary(fit)
coef(fit)
##���ɻع��� �������������ֵ�Ķ�������ģ�����������������䣬��������һ�꣬���������Ķ�����ֵ����0.03
##��Logistic�ع��Ӧ������һ�µģ����Ƕ�����ʽ������ͬ���ڳ�ʼ�߶ȣ�ָ�����Ƚ�����
##Ҳ����˵����������Ļع�ϵ������������ֻ��˵�Ƿ������Ķ�����ָ��������Ƿ���������logistic�ع��������һ����
exp(coef(fit))
exp(confint(fit))
##���ݽ����ֱ�ӽ����Ǳ仯1 ����x ��ָ��������������1�꣬�����ʳ���1.023������Trt�Ӱ�ο��ת�������飬��������ԭ����80%

###��������
##ԭ�򣬱�����©����Ҫ��Ԥ����������¼���أ�����һ������39�εĲ��˺�ֻ����һ�εĲ��ˣ������´η����ĸ��ʲ������
#���о����ظ�����ʱ�ģ�����Ⱥ����
##ֱ�Ӽ��㷨��֮ǰһ�� �в�ƫ��/���ɶȣ�����Ϊ10.17���ڹ������ƣ�ʵ�ʵķ�������ݲ��ɷֲ�Ԥ��ķ����
##Ҳ������qcc������
library(qcc)
qcc.overdispersion.test(breslow.dat$sumY,type="poisson")
#p<0.05���ڹ�������

#�����಴�ɷֲ� quasi+xx ,������quasipoission
fit<-glm(sumY~Base+Age+Trt,data=breslow.dat,family = quasipoisson())
summary(fit)
##���ǹ������ƣ���������ͻ��������Ƶı�׼�������޲�����

##ʱ��ηֲ��Ĳ��ɻع�
#���籾���� ���財������ֲ�����ĳ�����14-60���ڱ仯
#fit<-glm(sumY~Base+Age+Trt,data=breslow.dat,offset=log(time),family = poisson()),��Ȼ���˴����Ǳ��ʲ���

##�����͵Ĳ��ɻع�
##Ⱥ�����κμ�����Ϊ������֮ǰ�Ļ�����ģ�ͣ�������Щ�˴���û�й������飬��Ϊ�ṹ��ֵ
##��pscl���е�zeroinfl���� ����logistic�ع�Ͳ��ɻع����� ͬʱԤ�� 1��Щ���ֻᷢ�������飨0��1����
##�ų��˻����ҳϵ��˺�������ᷢ�����ٴλ����飨���ɣ�
##�Ƚ����ɻع� robust����glmRob