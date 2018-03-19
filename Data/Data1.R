#初步数据管理
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
#stringsAsFactors=FALSE,不把字符型变量认为是因子

#创建新的变量，并且可以把这两个变量整合到原来的数据框中，类似于attach 和 within函数
mydata<-transform(mydata,
                  sumx=x1+x2,
                  meanx=(x1+x2)/2)

leadship$age[leadship$age==99]<-NA#处理异常值
leadship<-within(leadship,{
   agecat<-NA
   agecat[age>75]<-"Elder"
   agecat[age>=55 & age<=75]<-"Middle aged"
   agecat[age<55]  <-"Young"})#agecat并不是一个函数，而是我们定义的一个新的字符

agecat<-factor(leadship$agecat,ordered = TRUE,levels = "Young","Middle aged","Elder")
leadship
str(leadship)
str(agecat)
leadship

#更换文件名
library(plyr)
leadship<-rename(leadship,c(data="testdata"))
leadship

#字符变量转换为日期
#自己手写输入data
strdatas<-c("01/05/2017","08/16/2016")
dates<-as.Date(strdatas,"%m/%d/%Y")
#本例
myformat<-"%m/%d/%y"
leadship$testdata<-as.Date(leadship$testdata,myformat)

#转换为字符
as.character

#添加列
total<-merge(dataframA,dataframeB,by="ID")
#添加行
total<-rbind(A,B)