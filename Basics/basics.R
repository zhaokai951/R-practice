read.csv("http://www.macalester.edu/~kaplan/ISM/datasets/swim100m.csv")
data2<-read.csv("http://www.macalester.edu/~kaplan/ISM/datasets/swim100m.csv")
summary(mtcars$mpg)
plot(mtcars$mpg,mtcars$disp)

#attaach�����������������ֱ����mtcars�ڵ������ݡ�detach������ֹ�õ���
attach(mtcars)
summary(mpg)
detach(mtcars)

#with���������ϣ���Ҫ�õ�{}��������ʾ��Ҫ���ڲ�����
with(mtcars,{
  stats <- summary(mpg)
  stats
})
stats #�ⲿ����������Ч�����������н�<����<<

diabetes<-c("Type1","Type2","Type1","Type1")
diabetes<-factor(diabetes)#���䶨λ���ӣ��������

#�����������order�������䶨��Ϊ�ȼ����ϣ�levls
status<-c("poor","improved","excellent","poor")
status<-factor(status,ordered = TRUE,levels = c("poor","improved","excellent"))

patientID<-c(1,2,3,4)
age<-c(25,34,28,52)
patientdata<-data.frame(patientID,age,diabetes,status)  #תΪ����
str(patientdata)#��ʾ��ṹ
summary(patientdata)#��ʾ��ƽ������

#�б�
g<-"My first list"
h<-c(25.26,18,39)
j<-matrix(1:10,nrow=5)
k<-c("one","two","three")
mylist<-list(title=g,ages=h,j,k)
mylist#����б�
mylist[[2]]
mylist[["ages"]]
mylist$ages


#������������
mydata<-data.frame(age=numeric(0),
                   gender=character(0),weight=numeric(0))  #numeric:��ֵ�ͣ�character:�ַ��ͣ���ֵ0������
mydata<-edit(mydata)
fix(mydata)#������ȼ�

#�����ı��ļ�



#����EXCEL��ת��ΪCSV
cd<-read.csv("G:\\R\\cd1.csv",header=TRUE)#��һ��Ϊһ���߼��Ա���

library(xlsx)
workbook<-"G:\\R\\cd1.xlsx"
mydatafram<-read.xlsx(workbook,sheetIndex = 1)

#��EXCEL�и�����Ҫ�����ݣ�Ȼ������
cd2<-read.table("clipboard")
read.table("clipboard") #�������һ������������ݲ�������data


#�����ı��ļ�
grades<-read.table("G:\\R\\TD\\SG.TXT",header = TRUE,row.names = "StudentID",sep = ",")#sep:���ã������ּ����
grades#���á�����ʹX,X��Ϊһ����Ԫ
str(grades)#���������������ַ��ͱ����Զ�ת��Ϊ����
#����������Ĵ���
grades<-read.table("G:\\R\\TD\\SG.TXT",header = TRUE,row.names = "StudentID",sep = ",",
                   colClasses = c("character","character","character","numeric","numeric","numeric"))
str(grades)
grades#����ǰ�汣����0�����ı�

