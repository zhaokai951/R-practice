#�����±�AΪһ��pattern
grep("A",c("c","A","b"),fixed=TRUE)
grep("A",c("c","A","b"),fixed=FALSE)

#X������pattern�������ı������滻��sub pattern repacement x fixed
sub("\\s",".","Hello There",fixed = TRUE)#\\s������Ϊ�ַ������Ҳ��������Բ����滻
sub("\\s",".","Hello There",fixed = FALSE)#\s���������ҿհ׵��������ʽ

#toupper(x)ת��Ϊ��д��tolowerת��ΪСд

#strsplit(x,split,fixed)
y<-strsplit("ABC","")
strsplit("ABC","B")#�������ַ�ȥ��
unlist(y)[2]�ָ����ǰ


#�����ַ���ȡ

x<-"abcde"
substr(x,2,3)#���һ���ַ����е�ĳ�����ַ�
x<-c("A","B","C")#ע����������������
unlist(x)[2]#���ĳ�����ַ���

#�����ַ�����paste��������sep=""SEP��ָ���м�ķָ���
paste("x",1:3,sep = "")
paste("x",1:3,sep="M")


#length(x),x�ĳ��ȣ��м����ַ���
#nchar(x)һ���ַ����ж��ٸ���ĸ�����֡���
#indeices<-seq(1,10,2).seq����һ��1-10�����Ϊ2������
#rep rep(1:3,2) 1,2,3�ظ�����
#cut/pretty(x,n)�����������ָ��n��ˮƽ������/���������ָ��N������


#���ӡ������Ķ��󣬲������������Ļ��
firstname<-c("Jane")
cat("hello",firstname,"\n")#\n���У�\b��һ��\t�Ʊ�����\'������

name<-c("Bob")
cat("hello",name,"\b.\n","Isn\'t R","\t","GREAT?")

#�Ծ������飬���ݿ�����γ�Ƚ�����͵�ͳ�ƣ�apply
#apply��x,margin,function,......margin��ά���±꣬function�Ǻ���,margin=1�У� margin=2��
mydata<-matrix(rnorm(30),nrow=6)
mydata
apply(mydata,1,mean)
apply(mydata,2,mean,trim=0.2)#trim,������ߺ���͵�20%

#sapply��������ȡ������
Firsname<-sapply(name,"[",1)#��ȡ��һ��Ԫ��

#��������
roster[order(lastname,Firstname),]

#ѭ�����
for(i in 1:10)#ֱ��ĳ��ֵ����SEQ�ڣ�for(vlues in seq),statement
print("Hello")

i<-10
while(i>0) {print("Hello");i<-i-1}  


#if-else��ѡ����һִ��
grade<-c("F","G")
if(!is.factor(grade)) grade<-as.factor(grade) else print("Grade is a factor")#ѡ��һ��ִ�У�����ʶ�ǲ���
str(grade)

#ifelse����Ϊ����
score<-c(0.4)
ifelse(score>0.5,print("passed"),print("Failed"))
outcome<-ifelse(score>0.5,"passed","Failed")
outcome

#switch����
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
#ע�⣬����sad,����afraid

u <- c("happly","sad")
for (i in u)
  if(i == "sad"){
    print("Cheer up")
  }else{
    print("There is nothing to fear")
  } 

outcome<-ifelse(i=="sad","cheer up","There is nothing to fear")
outcome#��Ԫʱ��ȼ�����

#User-defined function�û��Աຯ��
myfunction<-function(arg1,agr2){ #arglist
  statements
  return(object)
}
#����һ������


curve(myfunction(x,20,3,4),xlim=c(1,20))# X�������յ�
curve(exp(x),xlim=c(1,20)) #X�������յ�
#�õ����������

myfeeling = function(x){
  for (i in x){#for������i��x��ѭ����ֱ����������Ӽ���
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

#�ڶ��������еķ�ʽ
roster<-cbind(roster,x)#cbind����


#���ұ��
mystats<-function(x,parametric=TRUE,print=FALSE){#parametric�����Ǿ��庯����ֻ�����������
  if (parametric){
    center<-mean(x);spread<-sd(x)#�ۼ�����ɢ
  }else{
    center<-median(x);spread<-mad(x)
  }
  if(print & parametric){#������print��parametric��Ϊ��
    cat("Mean=",center,"\n","SD=",spread,"\n")
  }else if(print &!parametric){
    cat("Median=",center,"\n","Mad=",spread,"\n")
  }
  result<-list(center=center,spread=spread)#�����仰�ƺ�����Ӱ�����յ�������
  return(result)
}

set.seed(1234)#��˼��������ı��λ1234���´����ǻ��������ɺ�֮ǰһ���������
x<-rnorm(500)
y<-mystats(x)#ʲô������Ļ�������Ĭ��P=TRUE��PR=FLASE�������Ͳ�������ˣ���Ҫ��һ��y
y<-mystats(x,parametric = FALSE,print=TRUE)#��Ҫ�Լ����嵽���ǲ������Ƿǲ���
y<-mystats(x,parametric = TRUE,print = TRUE)



#switch����������
mydata<-function(type="long"){ #long����Ĭ�ϣ����Կ�
  switch(type,
         long=format(Sys.time(),"%A %B %d %Y"),#format���ܲ�����ָ��ģʽ���
         short=format(Sys.time(),"%m-%d-%y"),
         cat(type,"is not a recognized type\n"))#�����߽Բ��ǵ�������룬������������
}
mydata("long")
mydata("short")
mydata()
mydata("X")


#��/�з�ת��t(x),xΪ����

#�������������ݣ�aggdata
#�ú����������������ݣ����仰˵�����ǿ����������е�ĳN��������������������������ݣ��������Ǹ���VITAMIND ȱ��
#�벻ȱ��������۲�PSQI��ƽ�����������ܿ����Զ��壩
options(digits=3)
attach(mtcars)
aggdata<-aggregate(mtcars,by=list(cyl),FUN = mean,na.rm=TRUE)#ע�� by�ĸ�ʽ��by=list(x1,x2,....)
aggdata<-aggregate(mtcars,by=list(cyl,gear),FUN = mean,na.rm=TRUE)#na.rm=TRUE,��ȥȱʧֵ
aggdata

#�ڶ��ֺ���reshape