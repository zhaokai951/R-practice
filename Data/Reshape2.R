head(airquality)#宽数据，每一列为一个观测变量。每一行为一组所有观测变量的值。重点在一次观测时各个变量所对应的观测值！
library(reshape2)
data1<-melt(airquality)
head(data1)
#长数据有两个特殊的列：variable和value，variable列用于存放观测变量，value列用于存放观测变量对应的观测值
#个人理解：长数据格式用其特有的variable列和value列将观测结果细分，这在我们对单个变量进行分析而不是对
#所有变量进行分析时会简便得多，因其将每一个观测变量的观测值分开存储，造成variable列和value列较长
#以致整个数据集显得较长，故称为长数据

#melt(data, varnames = names(dimnames(data)), …, na.rm = FALSE, as.is = FALSE, value.name = “value”)
a <- array(c(1:23, NA), c(2,3,4))
melt(a, varnames = c('X', 'Y', 'Z'))
#varnames：用于指定“融化”后形成的数据框的变量名
#na.rm：默认为FALSE，用于移除缺失值
#as.is：默认为FALSE，为FALSE时，函数会调用type.convert方法将维度名称转化，为TRUE时，维度名称仍会作为字符串类型

#矩阵
#melt(data, id.vars, measure.vars, variable.name = “variable”, …, na.rm = FALSE, value.name = “value”, factorsAsStrings = TRUE)
#id.vars：用于指定标识变量，根据标识标量对其它变量进行“融化”，标识变量本身不进行“融化”。即依据
#measure.vars：用于指定测量变量，对测量变量进行“融化”，其它变量不进行“融化”。即对象
#若只指定了id.vars和measure.vars中的一项，则把指定之外的变量作为另一项
#若两者都未指定，则把因子和字符串类型的变量作为id.vars，其余变量作为measure.vars
data3 <- melt(airquality, id.vars = c('Month', 'Day'))
head(data3)#head,只给出开头，group1的第一个数的对应
data3

iris
data4 <- melt(iris, measure.vars = 'Species')
head(data4)

#对不同的数据对象，melt函数有不同的使用方法：
#针对数组的melt.array
#针对数据框的melt.data.frame
#针对向量的melt.default
#针对列表的melt.list**
#个人理解：melt意为“融化”，将宽数据转化为长数据，宽数据是一次观测记录了所有观测变量的观测值
#melt的作用就是根据观测变量“融化”这些观测值，将观测值分开


#cast意为“铸造”，将长数据转化为宽数据
#作者起的函数名很形象，melt就好像把金属融化，cast则是把融化的金属铸造，两者是相对的

#dcast(data, formula, fun.aggregate = NULL, …, margins = NULL, subset = NULL, fill = NULL, drop = TRUE, value.var = guess_value(data))
#formula：“铸造”公式，为函数的核心参数，函数根据公式进行“铸造”，公式形式为x_variable + x_2 ~ y_variable + y_2，左边为标识变量，右边为测量变量，类似于melt函数中的id.vars参数和measure.vars参数
#fun.aggregate：聚集函数，如mean、median、sum
#fill：用于填充缺失值的值
#drop：默认为TRUE，是否删除缺失的组合
library(reshape2)
data5 <- melt(airquality, id.vars = c('Month', 'Day'))
head(data5)
data5
data6<-dcast(data5,Month+Day~variable)
head(data6)
data6
str(data5)

