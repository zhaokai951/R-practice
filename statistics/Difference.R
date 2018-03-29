vars<-c("mpg","hp","wt")
head(mtcars[vars])

#������ͳ�Ʒ���
#������summary
summary(mtcars[vars])

#apply��sapply����x
apply(mtcars,2,mean,na.rm=TRUE)#ֻ�ܶ���һ������

mysatats<-function(x,na.rm=TRUE){
  if(na.rm)
    x<-x[!is.na(x)]
    m<-mean(x)
    n<-length(x)
    s<-sd(x)
    return(c(n=n,mean=m,stdev=s))
}
sapply(mtcars[vars],mysatats)#�Զ���һ��function
apply(mtcars,2,mysatats)#���˵�����˺�����apply����Ҳ���Խ��ж����������㡣apply����ֻ���������о��������
apply(mtcars[vars],2,mysatats)#��sapply�ȼۣ�����ָ�������еľ����У��С�

#��չ��pastecs���е�stat.desc()
library(pastecs)
stat.desc(mtcars[vars])

#���������������reshape2��
library(reshape2)
dfm<-melt(mtcars,id.vars=c("am","cyl"), measure.vars=c("mpg","hp","wt"))
dstata1<-function(x){mean=mean(x)}#��Ϊû��if���,���Բ���{}��
dstat2<-function(x){n=length(x)}
dstat3<-function(x){sd=sd(x)}
dcast(dfm,am+cyl+variable~.,dstata1)
dcast(dfm,am+cyl+variable~.,dstat2)
dcast(dfm,am+cyl+variable~.,dstat3)
#dcast�������ݿ���ֻ�ܷ���һ������������mean.acast�����������������variable��ָ���������ݿ���variable 
dstat4<-function(x){c(mean(x),sd(x))}
dcast(dfm,am+cyl+variable~.,dstat4)

#doby��
dstat5<-function(x){c(n=length(x),mean=mean(x),sd=sd(x))}
library(doBy)
summaryBy(mpg+hp+wt~am,data=mtcars,FUN=dstat5)
summaryBy(mpg+hp+wt~am+cyl,data=mtcars,FUN=dstat5)
#by����
#�㲻���

#������
library(vcd)
#һά������
mytable<-with(Arthritis,table(Improved))
mytable
prop.table(mytable)#ת��Ϊ�ٷֱ�

#��ά������
mytable1<-xtabs(~Treatment+Improved,data=Arthritis)#���ǹ�ʽģʽ�����������ǰ��
mytable1

attach(Arthritis)
mytable1<-table(Treatment,Improved)
mytable1
detach(Arthritis)#���ϵȼ�

#gmodels����CrossTable����SAS��SPSS
library(gmodels)
CrossTable(Arthritis$Treatment,Arthritis$Improved,prop.t=TRUE,prop.chisq =TRUE,chisq=TRUE,format = c("SPSS"))
CrossTable(Arthritis$Treatment,Arthritis$Improved,prop.t=TRUE,fisher = TRUE,format = c("SPSS"))
#prop.r = TRUE:�аٷֱȣ�prop.c = TRUE���аٷֱ�,��ֱ����prop.t,format = c("SPSS")����ΪSPSS��ģʽ
#prop.chisq =TRUE,chisq=TRUE == expected=TRUE,������ֵ̫С����fisher=TRUE

#��ά������
mytable<-xtabs(~Treatment+Sex+Improved,data=Arthritis)
ftable(addmargins(prop.table(mytable,c(1,2)),3))
ftable(addmargins(prop.table(mytable,c(1,3)),2))#�������ѡ��#ע����ά�б��ͱ�ʾ���ض��������������

#�����Լ��飬�о���������ƽʱ

#����ԣ�cor��cov����
states<-state.x77[,1:6]
cov(states)
#Ҳ����ָ��
cor(x1,x2)
#cov,Э��أ�use="everything"Ĭ�ϣ���ȱʧֵʱ������ʾ��ؽ����pairwise.complete.obsȱʧֵʱ�ɶ�ɾ����complete.obs��ɾ��
#method=pearsonĬ�ϣ���ѡspearman,kendall
cor(states)


###����ԵĿ��ӻ� corrgram��
options(digits = 2)
cor(mtcars)
library(corrgram)
corrgram(mtcars,order=TRUE,lower.panel=panel.shade,
#panel ƽ�塣�·�����Ӱ��ʾ.
#order=TRUEʱ�������Ϸ���ʼ������һ�д��ϵ������ϵ�����ν��͡�����ͬ����lable���ö�����Լ�����˳��
         upper.panel=panel.pie,text.panel=panel.txt,#text��������������������Сֵ������
         main="Correlogram of mtcars intercorrelations")
#����ѡ��type=cor ����cov
##���������·� ��ɫ ���µ����ϱ�ʾ����ء���ɫ���ϵ����±�ʾ����أ���ɫԽ�������Խ��
##��ͼ ˳ʱ��������أ���ʱ�븺���
#�ɹ�ѡ��panel.pts, panel.pie, panel.shade, panel.bar, panel.ellipse, panel.conf. panel.cor.
#conf���Ը���r���������� corֻ����r ��ͼ���ĶԽ�����


####���Ե���ֻ��һ��
corrgram(mtcars,lower.panel=panel.shade,
         upper.panel=NULL,text.panel=panel.txt,#text��������������������Сֵ������
         main="Car Mileage Data")
##order������TRUE���ͻᰴ�ճ�ʼ�ı���˳������

##Ҳ�����Զ�����ɫ
col.corrgram<-function(ncol){
     colorRampPalette(c("darkgoldenrod4","burlywood1","darkkhaki","darkgreen"))(ncol)}
corrgram(mtcars,order=TRUE,lower.panel=panel.shade,
         upper.panel=panel.cor,text.panel=panel.txt,
         main="Correlogram of mtcars intercorrelations",
         col.regions=col.corrgram)
#��ʵֱ���ں����ﶨ�� col.regions=colorRampPalette(c())�Ϳ�����
##�о�����function��������
help(corrgram)

#ƫ���pcor(u,s),ggm��
library(ggm)
pcor(c(1,5,2,3,6),cov(states))
#cǰ��������ΪҪ�������أ�������Ϊ���Ƶġ�sΪЭ������

#��ص�������ˮƽ��psych����corr.test����
library(psych)
corr.test(states,use="complete",r)
#corr.test(r,q,n)ƫ��ص������ԣ�ǰ����pcor�õ���ƫ���ϵ����qΪҪ���Ƶı��������ϱ�ʾ��ͬ��nΪ������С
#ָ�������ϵ��r,Ҳ������corr.p(r,n,adjust=("holm),alpha=0.05)ֱ�Ӽ���
#������Ը�use= method=����֮ǰ����ͬ.��ע�⣬����Ĭ�ϵ���use=pairwise

#��������t���� t.test(y~x,data)xΪ�������������t.test(y1,y2)
library(MASS)
head(UScrime)
str(UScrime)
t.test(Prob~So,data=UScrime,var.equal=TRUE)#���跽�����ԣ�var.equal=TRUE

#�������Լ���
library(car)
UScrime$So1<-factor(UScrime$So,levels = c(0,1),labels = c(1,2))#��Ҫ�ϸ�����ӻ�
UScrime
leveneTest(UScrime$Prob,UScrime$So1)

#��̬�Լ���
shapiro.test(UScrime$Prob)#�ʺ�С��2000��
library(nortest)
lillie.test(UScrime$Prob)#SPSS��Kolmogorov-Smirnov���飬nortest�����ʺϴ���2000�ģ�����D����

#�Ƕ�������t���飨��ԣ���������֮����һ��Ӱ��?,��������ƣ�
sapply(UScrime[c("U1","U2")],function(x)(c(mean=mean(x),sd=sd(x))))
with(UScrime,t.test(U1,U2,paired=TRUE,var.equal=TRUE))#�����ָ�ʽ������Ϊ���ݿ��У�U1,U2�ֿ���

#�ǲ������飬mann-whitney U����wilcoxon
with(UScrime,by(Prob,So,median))
wilcox.test(Prob~So,data=UScrime)

#����ķǲ�������ıȽ϶�����Kruskal-wallis
kruskal.test(y~x,data=)
posthoc.kruskal.nemenyi.test(y~x,data=)# �����Ƚ�PMCMRplus��
friedman.test(y~x,data=)#ȥ����ƻ������ظ�������ƣ������������

#���⻰��R���������ݿ�ĺϲ�


#1merge����
#�������ݿ�ӵ����ͬ��ʱ���۲�ֵ������Щ��ȴ������ͬ�������İ취����ʹ��
#merge(x, y ,by.x = ,by.y = ,all = ) ������
#merge���ϲ�
ID<-c(1,2,3,4)
name<-c("A","B","C","D")
score<-c(60,70,80,90)
student1<-data.frame(ID,name)
student2<-data.frame(ID,score)
total_student1<-merge(student1,student2,by="ID")
total_student1

#��������Ҫ����ͬ�Ĺ۲����ó��Ĳ�ͬ���ͱ����ϲ�ʱ�������cbind��Ҳ���Ǻϲ�columm��
#2.cbind����������׷��
ID<-c(1,2,3,4)
name<-c("A","B","C","D")
score<-c(60,70,80,90)
sex<-c("M","F","M","M")
student1<-data.frame(ID,name)
student2<-data.frame(score,sex)
total_student2<-cbind(student1,student2)
total_student2

#��������Ҫ����ͬ�Ĺ۲������ͬ�Ĺ۲�����ϲ�ʱ�������rbind��Ҳ���Ǻϲ�row��
#3.rbind����������׷��
ID<-c(1,2,3,4)
name<-c("A","B","C","D")
student1<-data.frame(ID,name)
ID<-c(5,6,7,8)
name<-c("E","F","G","H")
student2<-data.frame(ID,name)
total_student3<-rbind(student1,student2)
total_student3
