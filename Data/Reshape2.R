head(airquality)#�����ݣ�ÿһ��Ϊһ���۲������ÿһ��Ϊһ�����й۲������ֵ���ص���һ�ι۲�ʱ������������Ӧ�Ĺ۲�ֵ��
library(reshape2)
data1<-melt(airquality)
head(data1)
#������������������У�variable��value��variable�����ڴ�Ź۲������value�����ڴ�Ź۲������Ӧ�Ĺ۲�ֵ
#�������⣺�����ݸ�ʽ�������е�variable�к�value�н��۲���ϸ�֣��������ǶԵ����������з��������Ƕ�
#���б������з���ʱ����ö࣬���佫ÿһ���۲�����Ĺ۲�ֵ�ֿ��洢�����variable�к�value�нϳ�
#�����������ݼ��Եýϳ����ʳ�Ϊ������

#melt(data, varnames = names(dimnames(data)), ��, na.rm = FALSE, as.is = FALSE, value.name = ��value��)
a <- array(c(1:23, NA), c(2,3,4))
melt(a, varnames = c('X', 'Y', 'Z'))
#varnames������ָ�����ڻ������γɵ����ݿ�ı�����
#na.rm��Ĭ��ΪFALSE�������Ƴ�ȱʧֵ
#as.is��Ĭ��ΪFALSE��ΪFALSEʱ�����������type.convert������ά������ת����ΪTRUEʱ��ά�������Ի���Ϊ�ַ�������

#����
#melt(data, id.vars, measure.vars, variable.name = ��variable��, ��, na.rm = FALSE, value.name = ��value��, factorsAsStrings = TRUE)
#id.vars������ָ����ʶ���������ݱ�ʶ�����������������С��ڻ�������ʶ�������������С��ڻ�����������
#measure.vars������ָ�������������Բ����������С��ڻ������������������С��ڻ�����������
#��ָֻ����id.vars��measure.vars�е�һ����ָ��֮��ı�����Ϊ��һ��
#�����߶�δָ����������Ӻ��ַ������͵ı�����Ϊid.vars�����������Ϊmeasure.vars
data3 <- melt(airquality, id.vars = c('Month', 'Day'))
head(data3)#head,ֻ������ͷ��group1�ĵ�һ�����Ķ�Ӧ
data3

iris
data4 <- melt(iris, measure.vars = 'Species')
head(data4)

#�Բ�ͬ�����ݶ���melt�����в�ͬ��ʹ�÷�����
#��������melt.array
#������ݿ��melt.data.frame
#���������melt.default
#����б���melt.list**
#�������⣺melt��Ϊ���ڻ�������������ת��Ϊ�����ݣ���������һ�ι۲��¼�����й۲�����Ĺ۲�ֵ
#melt�����þ��Ǹ��ݹ۲�������ڻ�����Щ�۲�ֵ�����۲�ֵ�ֿ�


#cast��Ϊ�����족����������ת��Ϊ������
#������ĺ�����������melt�ͺ���ѽ����ڻ���cast���ǰ��ڻ��Ľ������죬��������Ե�

#dcast(data, formula, fun.aggregate = NULL, ��, margins = NULL, subset = NULL, fill = NULL, drop = TRUE, value.var = guess_value(data))
#formula�������족��ʽ��Ϊ�����ĺ��Ĳ������������ݹ�ʽ���С����족����ʽ��ʽΪx_variable + x_2 ~ y_variable + y_2�����Ϊ��ʶ�������ұ�Ϊ����������������melt�����е�id.vars������measure.vars����
#fun.aggregate���ۼ���������mean��median��sum
#fill���������ȱʧֵ��ֵ
#drop��Ĭ��ΪTRUE���Ƿ�ɾ��ȱʧ�����
library(reshape2)
data5 <- melt(airquality, id.vars = c('Month', 'Day'))
head(data5)
data5
data6<-dcast(data5,Month+Day~variable)
head(data6)
data6
str(data5)
