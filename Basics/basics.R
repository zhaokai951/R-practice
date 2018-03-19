read.csv("http://www.macalester.edu/~kaplan/ISM/datasets/swim100m.csv")
data2<-read.csv("http://www.macalester.edu/~kaplan/ISM/datasets/swim100m.csv")
summary(mtcars$mpg)
plot(mtcars$mpg,mtcars$disp)

#attaach函数，和上面的区别，直接在mtcars内调动数据。detach用于终止该调用
attach(mtcars)
summary(mpg)
detach(mtcars)

#with函数。如上，需要用到{}，但是显示需要在内部函数
with(mtcars,{
  stats <- summary(mpg)
  stats
})
stats #外部函数调动无效，除非在其中将<换成<<

diabetes<-c("Type1","Type2","Type1","Type1")
diabetes<-factor(diabetes)#将其定位因子，名义变量

#与上面的区别，order函数将其定义为等级资料，levls
status<-c("poor","improved","excellent","poor")
status<-factor(status,ordered = TRUE,levels = c("poor","improved","excellent"))

patientID<-c(1,2,3,4)
age<-c(25,34,28,52)
patientdata<-data.frame(patientID,age,diabetes,status)  #转为数组
str(patientdata)#显示其结构
summary(patientdata)#显示其平均数等

#列表
g<-"My first list"
h<-c(25.26,18,39)
j<-matrix(1:10,nrow=5)
k<-c("one","two","three")
mylist<-list(title=g,ages=h,j,k)
mylist#输出列表
mylist[[2]]
mylist[["ages"]]
mylist$ages


#键盘输入数据
mydata<-data.frame(age=numeric(0),
                   gender=character(0),weight=numeric(0))  #numeric:数值型；character:字符型；赋值0代表空
mydata<-edit(mydata)
fix(mydata)#与上面等价

#导入文本文件



#导入EXCEL可转化为CSV
cd<-read.csv("G:\\R\\cd1.csv",header=TRUE)#第一行为一个逻辑性变量

library(xlsx)
workbook<-"G:\\R\\cd1.xlsx"
mydatafram<-read.xlsx(workbook,sheetIndex = 1)

#在EXCEL中复制想要的数据，然后输入
cd2<-read.table("clipboard")
read.table("clipboard") #相比于上一列命令，这列数据不能生成data


#处理文本文件
grades<-read.table("G:\\R\\TD\\SG.TXT",header = TRUE,row.names = "StudentID",sep = ",")#sep:利用，来区分间隔。
grades#利用“”来使X,X认为一个单元
str(grades)#这样处理，会让字符型变量自动转化为因子
#可以用下面的代码
grades<-read.table("G:\\R\\TD\\SG.TXT",header = TRUE,row.names = "StudentID",sep = ",",
                   colClasses = c("character","character","character","numeric","numeric","numeric"))
str(grades)
grades#行名前面保留了0，纯文本


