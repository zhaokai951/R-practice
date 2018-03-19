#�������ݹ���
manager<-c(1,2,3,4,5)
data<-c("10/24/14","10/28/14","10/01/14","10/12/14","05/01/14")
country<-c("US","US","UK","UK","UK")
gender<-c("M","F","F","M","F")
age<-c(32,45,25,39,99)
q1<-c(5,3,3,3,2)
q2<-c(4,5,5,3,2)
q3<-c(5,2,5,4,1)
q4<-c(5,5,5,NA,2)
q5<-c(5,5,2,NA,1)
leadship<-data.frame(manager,data,country,gender,age,q1,q2,q3,q4,q5,stringsAsFactors = FALSE)
#stringsAsFactors=FALSE,�����ַ��ͱ�����Ϊ������

#�����µı��������ҿ��԰��������������ϵ�ԭ�������ݿ��У�������attach �� within����
mydata<-transform(mydata,
                  sumx=x1+x2,
                  meanx=(x1+x2)/2)

leadship$age[leadship$age==99]<-NA#�����쳣ֵ
leadship<-within(leadship,{
   agecat<-NA
   agecat[age>75]<-"Elder"
   agecat[age>=55 & age<=75]<-"Middle aged"
   agecat[age<55]  <-"Young"})#agecat������һ���������������Ƕ����һ���µ��ַ�

agecat<-factor(leadship$agecat,ordered = TRUE,levels = "Young","Middle aged","Elder")
leadship
str(leadship)
str(agecat)
leadship

#�����ļ���
library(plyr)
leadship<-rename(leadship,c(data="testdata"))
leadship

#�ַ�����ת��Ϊ����
#�Լ���д����data
strdatas<-c("01/05/2017","08/16/2016")
dates<-as.Date(strdatas,"%m/%d/%Y")
#����
myformat<-"%m/%d/%y"
leadship$testdata<-as.Date(leadship$testdata,myformat)

#ת��Ϊ�ַ�
as.character

#������
total<-merge(dataframA,dataframeB,by="ID")
#������
total<-rbind(A,B)