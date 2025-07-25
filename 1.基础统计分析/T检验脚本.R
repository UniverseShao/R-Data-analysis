#                一、单样本T检验——就是一个样本和一个平均值进行比较
#检验单个变量的均值与目标值之间是否存在差异
#如果总体均值已知
#样本均值与总体均值之间差异显著性检验属于单样本t检验
#目的
#检验单样本的均值是否和已知总体的均值相等(主要用于比较一组数据与一个特定数值之间的差异情况)
#要求是
#总体方差未知 否则就可以利用Z检验(也叫U检验,就是正态检验)
#正态数据或近似正态
#应用条件
#在单样本t检验中，总体标准差σ未知且样本含量较小（n<50）时，要求样本来自正态分布总体
#两个小样本均数比较时，要求两个样本均来自正态分布总体，且两样本总体方差相等
#如果两个样本方差不等，则用t'检验
#对于两个大样本（样本数均大于50）的均数比较
#在实际应用当中，与上述条件稍有偏差，只有数据呈单峰分布，且近似对称分布，一般影响不大

# 使用foreign包读取SPSS数据

library(foreign)

df <- read.spss('E:/各科资料/医学统计学/研究生课程/3总体均数的估计与假设检验18-9研/例03-05.sav',to.data.frame = T)

head(df)

#head() 函数用于查看数据框（data frame）、矩阵（matrix）或向量（vector）的前几行或几个元素。默认情况下，head() 函数会显示前6行或元素，但你也可以指定显示的数量
#注意以上是打开spss文件的方式
#to.data.frame = T：这是read.spss()函数的一个参数 
#设置为TRUE意味着读取的数据将被转换为R中的数据框（data frame）

st <- t.test(df$hb,mu=140,alternative = 'two.sided') # 双侧检验
#代码中的st即是对这一次t检验的赋值，就是这个st就是代表的就是这一次的t检验的结果，后续的对这一次t检验结果进行相关的查看就都可以
#针对st进行相关的函数操作
print(st)


#                 二、配对样本T检验——两配样本T检验（针对配对的人、同一人不同部位、同一样品）
library(foreign)

df <- read.spss('D:/各科资料/医学统计学/研究生课程/3总体均数的估计与假设检验18-9研/例03-06.sav',to.data.frame = T)

head(df)
#数据一共3列10行，第1列是样本编号，第2列和第3列是要比较的值
#进行配对样本t检验：
pt <- t.test(df$x1,df$x2,paired = T,var.equal = T) # 配对样本t检验
print(pt)

#配对设计均数比较又称为配对t检验。配对t检验的的目的是推断两种处理或方法的结果有无差异
#配对设计资料主要包括三种情况
#配对的两个受试者分别接受两种不同的处理之后的数据进行分析
#同一样品用两种方法检验出的结果
#同一受试对象两个部位的测定数据
#配对资料差值分布要符合正态性。首先要进行正态性检验，然后进行t检验
t.test(A1,A2,paired=T)


#               三、两独立样本的t检验（两个群体完全是互相独立的）
#两独立样本的t检验又称为两样本均数比较的t检验或成组t检验
#进行t检验之前要进行正态性检验  和方差齐性检验 
#方差齐性检验的R代码：
#leveneTest(A,B)；A数据，B是分组
#当方差齐时，设置参数var.equal=T
#方差不齐时，采用t'检验设置参数var.equal=F
library(car)
leveneTest(mtcars$mpg,mtcars$am)#注意这里mtcars$mpg是数据（因变量）mtcars$am是分组
#上行代码对mtcars$am进行方差齐性检验
#可知p<0.05，方差不齐
#方差不齐的时候采用t'检验
#t'检验代码为
t.test(mpg~am,data = mtcars,val.equal=F)
#方差齐的时候采用t检验
#t检验代码为
t.test(mpg~am,data = mtcars,val.equal=T)
#样本较小，方差齐与否的检验结果一样
#样本量较大时，检验结果就会出现差异