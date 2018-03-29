##OSL�ع飬��ͨ��С���˻ع飬���������Իع飬����ʽ�ع�Ͷ�Ԫ���Իع�
#Y=B0+B1X1+B2X2��������BO�ؾ��B1,2�ع�ϵ����Ŀ����ʹ�в�ƽ�����г���С
#Ҫ�����������̬�ԣ�Yֵ֮���໥��������������Ա��������ԣ�ͬ������

#myfit<-lm(formula,data),formula:Y~X1+X2������
#���÷���
#x:z:X��z�Ľ�������ӣ�y~x+z+x:z
#*��x*y*z ��ͬ�� x+y+z+x:y+x:z+y:z
#^ ������ﵽĳ�ִ��� (x+y+z)^2=x+y+z+x:y+x:z+y:z
#.  ���ݿ��г�Ӧ���������б�����y~.
#- �ӽ������м�ȥĳ��
# I(),�����Ƕ����µ�һ����x+(y+z)^2=x+y+z+y;z,x+I((y+z)^2)=x+h,h��y+z��ƽ�����µı���


##���ú���
#summary ��ʾģ�͵���ϸ���
#coefficients �г�ģ�Ͳ����������ؾ���Ļع�ϵ����
#fitted �г�ģ��Ԥ��ֵ
#residuals �г��в�ֵ
#predict�������ģ�Ͷ��µ����ݼ�Ԥ����Ӧ����
#confint �г���������

##x�����Իع�  x^2 ����ʽ�ع� x1,x2��Ԫ���Իع�

#�����Իع�
fit<-lm(weight~height,data=women)
summary(fit)
confint(fit)
women$weight#����������������������ʵ��ֵ
fitted(fit)#����ÿ��ģ�ͣ�ÿ��������Ԥ��ֵ
residuals(fit)#�вÿ��X(15��������15��ֵ��Ԥ��ֵ��ʵ��ֵ�Ĳ�࣬ģ������������Ĳв�ƽ������С
plot(women$height,women$weight,
     ylab="Height",
     xlab="Weight")#��ɢ��ͼ
abline(fit)#ģ�����

##����ʽ�ع飬ͬ��
fit2<-lm(weight~height+I(height^2),data=women)
summary(fit2)
plot(women$height,women$weight,#ע����x��y
     ylab="Height",
     xlab="Weight")
lines(women$height,fitted(fit2))#ע�⣬���ﻭ������xԤ��y���ߣ�����������ģ��
abline(fit2)#��Ϊ���������ͼ��xֻ��һ�������Բ���ֱ������
#ע�⣬�Ա�����log,����Ȼ�����Իع飬
#Ҳ������car����scatterplot����
library(car)
scatterplot(weight~height,
            data=women,
            spread=FALSE,lty.smooth=1,#spread �����׼�
            col=c("blue"),
            pch=19,
            main="Women age",
            xlab = "Height",
            ylab="Weight")#ֱ�ߣ�������ϣ����ߣ�ƽ�����.Ĭ�ϻ����ع�����

##��Ԫ���Իع�
state.x77
mode(state)#�ȼ��һ��ģ��
class(state)#�ȼ��һ��ģ������
class(state.x77)
state<-as.data.frame(state.x77[,c("Murder","Population","Illiteracy","Income","Frost")])
#��Ҫ��������תΪ���ݿ򣬿���ѡ���Լ���Ҫ�ı�������Ȼ��ȥһ�������������Ȼ��Ҫ�������Ϊ���ݿ�
cor(state)#�ȼ��һ�������
library(psych)
corr.test(state,use="complete")
library(car)
scatterplot(states,spread=FALSE,lty.smooth=2,main="SPW")
fit2<-lm(Murder~Population+Illiteracy+Income+Frost,data=state)
summary(fit2)
confint(fit2)

#�н�����Ļع�
fit3<-lm(mpg~hp+wt+hp:wt,data = mtcars)
summary(fit3)#�������ã�����wtȡ��ͬ��ֵ��ʱ��hpÿ�ı�һ����λ�������Y�仯��һ��
#�������õĿ��ӻ���effects��
library(effects)
plot(effect("hp:wt",fit3,
            xlevels=list(wt=c(2.2,3.2,4.2))),multiline=TRUE)

#������ʽ�������ģ�ͣ�xlevels=list(����ȡwt��ƽ��ֵ��ƽ��ֵ��1��׼������ˮƽ)��multiline=TRUE����ʾ����ֱ��

##�ع���ϣ������Ƿ����
#��Ⱥ�㣺��ϲ��ѣ������˾޴�����򸺵Ĳв�
#�߸ܸ�ֵ���쳣��Ԥ�����ֵ�����
#ǿӰ����ģ�͵Ĳ���Ӱ�����

#������car��

#1.��̬�ԣ�ѧ�����вӦ�÷�����̬�ֲ���QQͼӦ����һ��ֱ�ߣ�car����
library(car)
fit2<-lm(Murder~Population+Illiteracy+Income+Frost,data=state)
qqPlot(fit2,labels = row.names(state),id.method = "identify",
       simulate = TRUE,main= "Q-Q plot")
#id.method = "identify"�ܹ��������ͼ������ĳ���㣬��ESC�˳���ͼ�ϻ���ʾ���ı�ǩ������������עһЩ����ĵ�
#simulate = TRUE��95%����������ò���������
#��һ����Ⱥ��,Nevada
state["Nevada",]
fitted(fit2)["Nevada"]
residuals(fit2)["Nevada"]
rstudent(fit2)["Nevada"]ѧ�����в�
#ѧ�����в����̬ͼ
residplot<-function(fit,nbreaks=10){
       z<-rstudent(fit)
       hist(z,breaks=nbreaks,freq=FALSE,xlab="SR",main="DE")#ע����breaks��֮ǰ����һֱ���в�����
       #����̬����ͼ���Ա�׼���в�ľ�ֵ����׼���������̬���ߣ�
       curve(dnorm(x,mean=mean(z),sd=sd(z)),add=TRUE,col="blue",lwd=2)
       #��Ϻ��ܶ�����
       lines(density(z)$x,density(z)$y,col="red",lwd=2,lty=2)
}
legend("topright",  
       legend=c("Normal Curve","Kernel Density Curve"),  
       lty=1:2,col=c("blue","red"),cex=0.7)#���飬legend��Ҫ����function����ӡ����в�����
residplot(fit2)


#2�����ԣ����������ж�Y֮��Ķ����ԣ�������car����DW����
library(car)
durbinWatsonTest(fit2)#p����0.05��������ԣ�����

#���ԣ�car����crPlot
library(car)
crPlots(fit2)#����Ϊ������ߣ�ʵ��Ϊʵ��ֵ

#ͬ������
ncvTest(fit2)#P>0.05 ������
spreadLevelPlot(fit2)
#����ֱ�߸���������ֲ���˵�����ԡ����⣬��ɫ���ߣ���ʾ�����ݴα任���������ս���ݴα任����1.2����ʾ���任
#0.5���ø���Y������Y������Ϊ0����ʾY�����任���ӽ�1���任

##�ۺ���֤ gvlma����gvlma
library(gvlma)
gvmodel<-gvlma(fit2)
summary(gvmodel)
#��������һ���summary(fit2)���������Pֵ>0.05���ó���Decision

##���ͻع飬���ع�����
#�ܵ�F�������������Ǹ���X�Ļع�ϵ����ʾ��������
#���ó���ʱ��DOB������Ԥ�������������������������Լ���DOB�������Ĺ�ϵ��������������Ĺ�ϵ
#�൱�ڵ����䲻�䣬Ԥ�������������Ĺ�ϵ��ì��
#Variance Inflation Factor�����������Ӽ��(������������Ϊ��ģ���޹ص�Ԥ������ĳ̶�) car����vif����

vif(fit2)
sqrt(vif(fit2))>2#�жϸ���VIF�Ƿ����2��û�д���2����


##�쳣�۲�ֵ car��
outlierTest(fit2)#�����<0.05����Ⱥ��,����ǰ��QQͼ�ķ���

##�߸ܸ˹۲�ֵ���ܶ��쳣Ԥ�����ֵ����ϣ���ñ��ͳ����(hatvalues),p/nΪñ�Ӿ�ֵ
hat.plot<-function(fit){
  p<-length(coefficients(fit))#������Ŀ,�����ؾ���,���Ա�������Ŀ+1���ؾ��
  n<-length(fitted(fit))#������
  plot(hatvalues(fit),main="Index plot of hat values")#�Ȼ�����ͼ
  abline(h=c(2,3)*p/n,col="red",lty=2)#h=  Yֵ��ֱ����Y���ϻ�ˮƽ��
  identify(1:n,hatvalues(fit),names(hatvalues(fit)))#��λ����
}
hat.plot(fit2)#2������3�����ϣ����߸ܸ˵㲻һ����ǿӰ��㣬Ҫ����Ⱥ

##ǿӰ��㣬�Ƴ����ģ�Ͳ����޴�ĸı�
#Cook D��
cutoff<-4/(nrow(state)-length(fit2$coefficients)-2)#�������ؾ�����Լ�2  4/(n-k-1)���������Ա�����Ŀ
plot(fit2,which=4,cook.levels = cutoff)#which,��4��ģʽ
#��ʱ��ֱ����1Ϊ�ָ�㣬�������һ����,�����Ļ���û��ǿӰ���

#����Ԥ��ǿӰ����Ӱ��  avPlot����
library(car)
avPlots(fit2,ask=FALSE,onepage=TRUE,id.method="identify")
#Ҳ�ǽ������õģ�4��������һ�δεĳ�����Income��Alask��һ��Ӱ��㣬ͼ�й۲죬ɾ��ALASK���������ƶ�

#������������ϣ�infulencePlot
influencePlot(fit2,id.method = "identify",main="Influence Plot",
              sub="Circle size is proportial to Cook'S Distance")
#�����>2��<-2 ��Ⱥ��;ˮƽ�ᳬ��0.2��0.3����������߸ܸ�ָ����ԲȦ��ǿӰ���

##�Ľ���ʩ
#1ɾ��ǿӰ��㣬������û�д��������³̺����ű�׼������£��ֱ������

##�����������任��ǰ����Ҫ�����ܹ����ͣ�������仯�������޷����ͣ����Ե�û��������
#Ӧ�����ı仯��car��powerTransform�������̬��
library(car)
summary(powerTransform(state$Murder))
#�����ʾEST=0.6�����ǿ�����muder^0.5���б仯�����Ǳ��������ʾlambad=1,p>0.05,����û�б任��Ҫ
#�Ա����ı仯botTidwell
boxTidwell(Murder~Population+Illiteracy,data=state)
#��ʾpopulation^0.87��illiteracy^1.36�ܹ����ƣ���PֵҲû�����壬�Ͳв�ͼһ��

##���ع����ԣ�ɾ����������ع�

#ģ�ͱȽϣ�anova
#�����У�income��Frost�Ļع�ϵ����������������ɾ������
fit3<-lm(Murder~Population+Illiteracy,data=state)
anova(fit3,fit2)#С�ģ���Ƕ�ڴ����
#�����ʾp=0.994�����Կ���ɾ��

#AIC����
AIC(fit2,fit3)#����Ҫ����˳��
#����ֵԽС��ԽҪ���ȿ���

##���ģ�͵ıȽ�
#�𲽻ع�(��ǰ+���) MASS����stepAIC
library(MASS)
stepAIC(fit2,direction = "both")
#��AICֵ����һ��ɾFrost,�ڶ���ɾincome,��93.76��ģ�Ͳ��ٸı�(��ɾ��ֵ����)
#���ҵ���ģ�ͣ�����һ������ѵ�

#ȫ�Ӽ��ع鷨 leaps����regsubsets��������R^2��Խ��Խ��
library(leaps)
leaps<-regsubsets(Murder~Population+Illiteracy+Income+Frost,data=state,nbest = 4)
#�ĸ��Ա�����nbest=4

#���ӻ���2����
plot(leaps,scale="adjr2")#scale����������
#��һ�У���intercept��income������С�ģ������溬P��I��������

library(car)
subsets(leaps,statistic="cp",
        main="CP for ALL subsets regression")
abline(1,1,lty=2,col="red")# ����һ��ͼ���ڷ�ͼ����λ��
#Խ�õ�ģ�ͣ���ؾ����б��Ϊ1��ֱ��Խ��
#���ջ���Ҫ����ʵ������ͱ���֪ʶ

###���η���
##����ģ�͵ķ���������������֤�����з���bootstrap����crossval
library(bootstrap)
shrinkage<-function(fit,k=10){#k�ؽ����shrinkage��������¼
            require(bootstrap)
  theta.fit<-function(x,y){lsfit(x,y)}
  theta.predict<-function(fit,x){cbind(1,x)%*%fit$coef}
 
  x<-fit$model[,2:ncol(fit$model)]
  y<-fit$model[,1]
  
  results<-crossval(x,y,theta.fit,theta.predict,ngroup=k)
  r2<-cor(y,fit$fitted.values)^2
  r2cv<-cor(y,results$cv.fit)^2
  cat("origin R-square=",r2,"\n")
  cat(k,"F cros- V R-square=",r2cv,"\n")
  cat("change=",r2-r2cv,"\n")
}

shrinkage(fit2)#ԭ����0.567�����ֹۣ�Ӧ����0.488
shrinkage(fit3)#�Ľ��󣬱��0.519


##�����Ҫ��
#��׼����ֱ�ӱȽϻع�ϵ����һ����׼��仯�������Ӧ�����ı仯��
zstate<-as.data.frame(scale(state))#��׼�����Ǿ���ת��һ��
zfit<-lm(Murder~Population+Illiteracy+Income+Frost,data=zstate)
coef(zfit)#x��һ����׼��ı仯������Y�ģ��ع�ϵ��������׼��ı仯

##���Ȩ�أ�ÿ��Ԥ�������R^2�Ĺ��ף�
relweights <- function(fit, ...) {
  R <- cor(fit$model)#����Ӧ�������Ա�����ɵ��б��ģ����ϵ��
  nvar <- ncol(R)#���е����ϵ��
  rxx <- R[2:nvar, 2:nvar]
  rxy <- R[2:nvar, 1]
  svd <- eigen(rxx)
  evec <- svd$vectors
  ev <- svd$values
  delta <- diag(sqrt(ev))
  
  # correlations between original predictors and new orthogonal variables
  lambda <- evec %*% delta %*% t(evec)#�漰������֪ʶ
  lambdasq <- lambda^2
  
  # regression coefficients of Y on orthogonal variables
  beta <- solve(lambda) %*% rxy
  rsquare <- colSums(beta^2)
  rawwgt <- lambdasq %*% beta^2
  import <- (rawwgt/rsquare) * 100
  lbls <- names(fit$model[2:nvar])
  rownames(import) <- lbls
  colnames(import) <- "Weights"
  
  # plot results
  barplot(t(import), names.arg = lbls, ylab = "% of R-Square", 
          xlab = "Predictor Variables", main = "Relative Importance of Predictor Variables", 
          sub = paste("R-Square = ", round(rsquare, digits = 3)), 
          ...)
  return(import)
}

relweights(fit2)
#�����ǰ��ı�׼��������ͬ�����岻һ����Ҫ