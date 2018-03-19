#返回下标A为一个pattern
grep("A",c("c","A","b"),fixed=TRUE)
grep("A",c("c","A","b"),fixed=FALSE)

#X中搜索pattern，并以文本将其替换。sub pattern repacement x fixed
sub("\\s",".","Hello There",fixed = TRUE)#\\s被表现为字符，差找不到，所以不能替换
sub("\\s",".","Hello There",fixed = FALSE)#\s是用来查找空白的正则表达式

#toupper(x)转换为大写；tolower转换为小写

#strsplit(x,split,fixed)
y<-strsplit("ABC","")
strsplit("ABC","B")#会把这个字符去掉
unlist(y)[2]分割开的提前


#连续字符提取

x<-"abcde"
substr(x,2,3)#打出一个字符串中的某几个字符
x<-c("A","B","C")#注意上下两个的区别
unlist(x)[2]#打出某几个字符串

#连接字符串，paste（。。，sep=""SEP是指在中间的分隔符
paste("x",1:3,sep = "")
paste("x",1:3,sep="M")


#length(x),x的长度，有几个字符串
#nchar(x)一个字符串有多少个字母，数字。。
#indeices<-seq(1,10,2).seq生成一个1-10，间隔为2的序列
#rep rep(1:3,2) 1,2,3重复两次
#cut/pretty(x,n)将连续变量分割成n个水平的因子/连续变量分割成N个区间


#连接。。。的对象，并将其输出到屏幕上
firstname<-c("Jane")
cat("hello",firstname,"\n")#\n新行，\b退一格，\t制表符，\'单引号

name<-c("Bob")
cat("hello",name,"\b.\n","Isn\'t R","\t","GREAT?")

#对矩阵，数组，数据框任意纬度进行求和等统计，apply
#apply（x,margin,function,......margin是维度下标，function是函数,margin=1行， margin=2列
mydata<-matrix(rnorm(30),nrow=6)
mydata
apply(mydata,1,mean)
apply(mydata,2,mean,trim=0.2)#trim,忽视最高和最低的20%

#sapply函数：提取，插入
Firsname<-sapply(name,"[",1)#提取第一个元素

#排序，例子
roster[order(lastname,Firstname),]

#循环语句
for(i in 1:10)#直到某个值不在SEQ内；for(vlues in seq),statement
print("Hello")

i<-10
while(i>0) {print("Hello");i<-i-1}  


#if-else任选泽其一执行
grade<-c("F","G")
if(!is.factor(grade)) grade<-as.factor(grade) else print("Grade is a factor")#选择一个执行，！意识是不是
str(grade)

#ifelse，更为紧凑
score<-c(0.4)
ifelse(score>0.5,print("passed"),print("Failed"))
outcome<-ifelse(score>0.5,"passed","Failed")
outcome

#switch函数
feelings = c("sad", "afraid")
for (i in feelings){
  print(
    switch(i,
           happy  = "I am glad you are happy",
           afraid = "There is nothing to fear",
           sad    = "Cheer up",
           angry  = "Calm down now"
    )
  )
}
#注意，先找sad,再找afraid

u <- c("happly","sad")
for (i in u)
  if(i == "sad"){
    print("Cheer up")
  }else{
    print("There is nothing to fear")
  } 

outcome<-ifelse(i=="sad","cheer up","There is nothing to fear")
outcome#二元时候等价上面

#User-defined function用户自编函数
myfunction<-function(arg1,agr2){ #arglist
  statements
  return(object)
}
#定义一个函数


curve(myfunction(x,20,3,4),xlim=c(1,20))# X的起点和终点
curve(exp(x),xlim=c(1,20)) #X的起点和终点
#用到上面的例子

myfeeling = function(x){
  for (i in x){#for函数，i在x中循环，直到不在这个子集中
    print(
      switch(i,
             happy  = "I am glad you are happy",
             afraid = "There is nothing to fear",
             sad    = "Cheer up",
             angry  = "Calm down now"
      )
    )
  }
}
feelings = c("sad", "afraid")
myfeeling(feelings)

#第二种添加列的方式
roster<-cbind(roster,x)#cbind命令


#自我编程
mystats<-function(x,parametric=TRUE,print=FALSE){#parametric并不是具体函数，只是用来标书的
  if (parametric){
    center<-mean(x);spread<-sd(x)#聚集，离散
  }else{
    center<-median(x);spread<-mad(x)
  }
  if(print & parametric){#后续，print和parametric都为真
    cat("Mean=",center,"\n","SD=",spread,"\n")
  }else if(print &!parametric){
    cat("Median=",center,"\n","Mad=",spread,"\n")
  }
  result<-list(center=center,spread=spread)#这两句话似乎并不影响最终的输出结果
  return(result)
}

set.seed(1234)#意思是随机数的编号位1234，下次我们还可以生成和之前一样的随机数
x<-rnorm(500)
y<-mystats(x)#什么都不输的话，就是默认P=TRUE，PR=FLASE，这样就不会输出了，需要打一下y
y<-mystats(x,parametric = FALSE,print=TRUE)#需要自己定义到底是参数还是非参数
y<-mystats(x,parametric = TRUE,print = TRUE)



#switch函数的优势
mydata<-function(type="long"){ #long是其默认，可以空
  switch(type,
         long=format(Sys.time(),"%A %B %d %Y"),#format接受参数已指定模式输出
         short=format(Sys.time(),"%m-%d-%y"),
         cat(type,"is not a recognized type\n"))#将两者皆不是的情况放入，可以用来报错
}
mydata("long")
mydata("short")
mydata()
mydata("X")


#行/列反转，t(x),x为矩阵

#整合与重塑数据，aggdata
#该函数是用于整合数据，换句话说，我们可以用数据中的某N个分类变量来重新排列整合数据，比如我们根据VITAMIND 缺乏
#与不缺乏，两组观测PSQI的平均分数（功能可以自定义）
options(digits=3)
attach(mtcars)
aggdata<-aggregate(mtcars,by=list(cyl),FUN = mean,na.rm=TRUE)#注意 by的格式：by=list(x1,x2,....)
aggdata<-aggregate(mtcars,by=list(cyl,gear),FUN = mean,na.rm=TRUE)#na.rm=TRUE,移去缺失值
aggdata

#第二种函数reshape