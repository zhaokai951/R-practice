################################ANOVA#####################

####��������####
#�������
#��������
#�۲���Ŀ��ͬ���������
#ANOVA, ANCOVA(Э����),MANOVA����Ԫ��Ӧ������ֹһ����,MANCOVA.

##����ʽ
#anova  y~A
#ancova y~x+A(Э������ǰ��)
#˫����ANOVA  Y~A*B(����)
#����Э������ANCOVA  y~x1+x2+A*B
#���飬y~B+A (B����������)
#����������ANOVA��y~A+Error(subject/A)
#�����������Ӻ͵���������ӵ��ظ��������  y~B*W+Error��Subject/W)
#R�е�ANOVA����˳��� y~A*B ������y~B*A������ͣ������ΪA��Y��Ӱ�죬����A B��YӰ�죬����AB��ЧӦ�Ľ�������

##����ԭ��
#����ЧӦ�ڱ���ʽǰ��
#������Э������Ȼ������ЧӦ��Ȼ���������ؽ�����Ȼ���������ؽ���
#��ЧӦ����������ǰ�棬�����Ա�Ҫ�ڴ�����ʽǰ��
#SPSS��SAS�Ǳ߽��ͣ���Ҫ���֣�������car����Anova��aov()�ǵ�һ�ַ���

####one way anova
##��̬�Լ��� 
shapiro.test(cholesterol$response)#������ǰ���ᵽ��
library(car)
qqPlot(lm(response~trt,data=cholesterol),main="Q-Q test",labels=FALSE,simulate = TRUE)
#������QQͼ,simulate = TRUE������������by parametric bootstrap����������95%���������ڼ���

##��������
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

##��ͼ������������
library(gplots)
plotmeans(response~trt,xlab="TRT",ylab = "Response",main="mean plot/with 95%CI")
detach(cholesterol)


####���رȽ�
detach("package::HH")#�ú�����HH���м������⣬���֮ǰ����HH�������˳��ð�
TukeyHSD(fit)

###���ӻ�1
par(las=2)
par(mar=c(5,8,4,2))
plot(TukeyHSD(fit))#���������0������������
par(opar)
###���ӻ�2
library(multcomp)
par(mar=c(5,4,6,2))
tuk<-glht(fit,linfct=mcp(trt="Tukey"))
#���ӣ�glht(amod, linfct = mcp(tension = "Tukey"))set up all-pair comparisons for factor `tension'��Ҳ����ָ��ĳ�������ӱȽ�
#Ҳ�������ڻ������ģ�͵�
plot(cld(tuk,level=0.05),col="lightgrey")
#cld��Ϊ�ɶԱȽϵ�չ��һ���򵥵Ľ���������е����ӿ�����������glht��cld��������
#����ͬ��ĸ�ı�ʾ�����Բ�����
help(cld)
cld(tuk,level=0.05)


#########������Э�������ancova##########
data(litter,package = "multcomp")
attach(litter)
head(litter)
table(dose)
aggregate(weight,by=list(dose),FUN=mean)
fit<-aov(weight~gesttime+dose)
summary(fit)
detach(litter)
##��õ���Э����������ֵ effects����
library(effects)
effect("dose",fit)
library(multcomp)
contrast<-glht(fit,linfct = mcp(dose="Tukey"))
summary(contrast)

contrasts<-rbind("no durg vs drug"=c(3,-1,-1,-1))
summary(glht(fit,linfct=mcp(dose=contrasts)))
#�趨��һ�����������ľ�ֵ���ϲ����ıȽϡ���Ȼ�ò���0�������ڲ������ȽϵĽ��ۡ��趨����.����֮��Ϊ0
contrasts<-rbind("no durg vs drug"=c(1,0,-1,0))
summary(glht(fit,linfct=mcp(dose=contrasts)))
#�þ����������Ϊ��һ��͵ڶ���ıȽ�

####ANCOVA������̬���뷽�����ԣ�����Ҫ�ٶ��ع�б����ͬ,����ع�б����ͬ����ôӦ��û�н�����
fit<-aov(weight~gesttime*dose,data=litter)
summary(fit)
#�����ʾ���������ò����ԣ�֧��б����ȡ����˵б�ʲ��ȣ���sm.ancova

###ancova�Ŀ��ӻ���HH��
library(HH)#�Ȱ�װscales,lazyeval��htmlTable������
ancova(weight~gesttime+dose,data=litter)
#�ؾ���仯������ƽ��
ancova(weight~gesttime*dose,data=litter)
#б��Ҳ���б仯����Ȼ��֪����ô��

#################˫����ANOVA#######
attach(ToothGrowth)
head(ToothGrowth)
table(supp,dose)
aggregate(len,by=list(supp,dose),FUN=mean)

fit<-aov(len~supp*dose)
summary(fit)
####���ӻ�
##1
interaction.plot(dose,supp,len,type = "b",col=c("red","blue",pch=c(16,18),main="Interaction bewteen Odse and supp"))
help("interaction.plot")#����˫����ANOVA�Ľ���ͼ��˳��Ϊx1,x2,y
##2
library(gplots)
plotmeans(len~interaction(supp,dose,sep=""),
          connect=list(c(1,3,5),c(2,4,6)),#��������ÿ��������λ��
          col=c("red","blue"),
          main="Interaction PLot with 95CI",
          xlab="Treatmen and Dose Combination")

plotmeans(len~interaction(supp,dose,sep=""),
          connect=list(c(1,2,3,4,5,6)),
#��������ÿ��������λ��,���վ���˳��������oj0.5,vc0.5,oj1��6��������6��������
          col=c("red","blue"),
          main="Interaction PLot with 95CI",
          xlab="Treatmen and Dose Combination")

help("plotmeans")

##3
library(HH)
interaction2wt(len~supp*dose)
#���ϵ�ֱ������������ַ������������Լ���Ϊ�������������������ǰѼ�����������ֻ������
#�����Ǽ���Ϊ�����������������Ƿ����������������������Ϻ����¿��Թ۲콻������

####�ظ������������#һ���������ӣ�һ���������
w1b1<-subset(CO2,Treatment=='chilled')#subset��һ�����ݿ���ѡ������������
w1b1
attach(w1b1)
fit<-aov(uptake~conc*Type+Error(Plant/(conc)))#ʱ���Ƕ���������Ե�
summary(fit)
par(las=1)#�������ǩ����
interaction.plot(conc,Type,uptake,type = "b",col=c("red","blue",pch=c(16,18),main="Interaction bewteen Type and conc"))
##������co2�ǳ���ʽ��������ʱ����صķ�����ǰ���litter�ǿ���ʽ��������һ�����


###������һ����ָ���ݼ��еı���û������ȷ��ϸ�֣���������������һ�������е�Ԫ�ش���ֵ�����ظ�ѭ������������Թ�Ϊ���ࣩ
#�����������״Ϊ�����Σ��� �����ٶ��۲�ֵ�� ######dcast������תΪ������
###��������ָ���ݼ������еı�����������ȷ��ϸ�֣���������ֵ�������ظ�ѭ�������Ҳ�޷�����.
#��������ı���Ϊ ��������۲�ֵ�١� ####metl������תΪ������


#####�������ģ��lme4����lmer ��ҪMatrix��
library(Matrix)
library(lme4)
help("lme4")

######�ı��е�����
#fix(x)
#colnames(x)[n]<-""


#####��Ԫ������� Ӧ������ֹһ��
library(MASS)
UScereal
attach(UScereal)
y<-cbind(calories,fat,sugars)
fit<-aov(y~shelf)
summary(fit)
####��Ҫ�����Զ��̬���뷽��-Э����ͬ����
###��Զ��̬�� QQͼ
center<-colMeans(y)
n<-nrow(y)
p<-ncol(y)#���ɶȣ��������ܶ������ɱ仯�ı���.�����У������������������������������������
cov<-cov(y)
d<-mahalanobis(y,center,cov)#���Ͼ��룬Э�������
coord<-qqplot(qchisq(ppoints(n),df=p),#����ʹ��QQplot�Ŀ���������룬nΪ����������ȷ��qchisq������Y,yΪ���Ͼ���
              d,main="Q-Q plot",
              ylab = "D2")
abline(a=0,b=1)#a�ؾ࣬bб�ʣ��ؾ��Ǿ�ֵ��б��Ϊ��׼��������̬�ֲ���ôӦ����һ���ؾ�Ϊ0����ֵΪ1��ֱ��
identify(coord$x,coord$y,labels=row.names(UScereal))#���������Ⱥ��

####Э����ͬ����
#��ʱ����ʵ�֣�һ����˵�ƺ�����Ҫ��

#####�ûع�����ANOVA

##ANOVA�ķ���
library(multcomp)
fit.aov<-aov(response~trt,data=cholesterol)
summary(fit.aov)#�����½���е�DFΪʲô��4��trt�����ˮƽ�������ȷ����4��ˮƽ��ֵ����һ��ˮƽ�ǹ̶��ġ�����4

##�ûع����
fit.lm<-lm(response~trt,data = cholesterol)
summary(fit.lm)
##������ģ����������ʱ���������Ӷ�Ӧ����ֵ���������ӡ���������У�1time�����ɲ�����
#һ����˵���������ö��մ�����������������������ʽ
fit.lm<-lm(response~trt,data = cholesterol,contrasts=list(trt="contr.helmert"))
summary(fit.lm)
#contr.  treatment �Ե�һˮƽΪ����  SAS���ˮƽΪ����  
#helmert��ǰ�����о�ֵ���գ�ע�⣬helmert�Ǵӵڶ�����ʼ��������ʾΪA1,A2������
# poly�������� ��������ʽ.���˵�������a b c �����ӣ����Ը��ݲ�ͬ����Ϊ a= b= c=
#treatmentΪĬ�ϵ����ã�Ҳ�Ǻ�SPSSһ����