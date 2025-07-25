rm(list = ls())
# 1.P for trend-----------------------------------------------------------------
"
根据HR值的变化可以看出，
从CD8+ TIL阴性组到高水平组，HR值逐渐降低，提示随着CD8+ TIL水平的升高，
患者死亡风险呈现下降的趋势，
那么这个变化趋势是否有统计学显著性呢
这就是P for trend存在的意义

把连续性变量转换为分类变量(在R里转变为因子)，
设置哑变量，进行回归分析，即可得到OR值及95%的可信区间；
把转换好的分类变量当做数值型，进行回归分析，即可得到P for trend
"


##### 1.1P for trend操作的意义--------------------------------------------------
"
对于原始变量本身即为连续型变量时

为什么不将原始变量直接带入到模型中进行分析呢？

为什么还要大费周折将其转化为哑变量，然后再做一遍趋势性检验呢？

直接带入原始变量时所得的P值不是能更好的说明该变量与因变量之间的变化趋势么？

(1)临床意义上
为什么我们有时需要将连续型变量转化为哑变量，
是因为当自变量以连续型变量的形式引入模型时，其意义解释为该自变量每增加1个单位，
所引起的因变量Y的变化（β），或结局发生风险的变化（OR/HR），
但实际上这种变化效应有时是很微弱的，并没有太大的临床意义，
因此需要对连续型变量进行适当的转化

(2)模型原始假设上
如果直接将原始的连续型变量带入到回归模型中，
其前提是已经假定该连续型自变量与因变量之间存在着一定的线性关系。
但是，当自变量与因变量之间的相互变化关系不明确时，
以连续型变量带入模型会遗漏一些很重要的信息

(3)举例说明
结果显示随着蔬菜摄入量的升高，
2型糖尿病发病风险RR值分别为1、1.01、1.04、1.06、0.98，P-trend=0.78（Model 1），
提示蔬菜摄入量和2型糖尿病发病风险之间的线性趋势不具有统计学显著性，
但是从RR值的变化中我们可以推测，两者之间可能存在“抛物线（倒U字型）”的关系，
而此时如果仅仅是带入原始的连续型变量，就无法从结果中看出这样的变化关系

"
##### 1.2P for Trend的实现------------------------------------------------------


library(haven)
df16_2 <- read_sav("C:/Users/Administrator/Desktop/R脚本(SWY精心编辑版)/Medical Statistics/datasets/例16-02.sav")
View(df16_2)
"
x1：年龄，小于45岁是1,45-55是2,55-65是3,65以上是4
y：是否是冠心病，1代表是，0代表否
"
str(df16_2)
df16_2$y <- factor(df16_2$y)
df16_2$x2 <- factor(df16_2$x2)
str(df16_2)
###### 1.2.1现在我们要计算矫正x2后的x1变量的P for Trend-------------------------
f <- glm(y ~ x1 + x2, 
         data = df16_2, 
         family = binomial())
summary(f)
broom::tidy(f)

###### 1.2.2把x1变成因子型,算正常的OR-------------------------------------------
df16_2$x1 <- factor(df16_2$x1, levels = c(1,2,3,4), 
                    labels = c("青年人", "中年人", "壮年人", "老年人" ))
f1 <- glm(y ~ x1 + x2, 
         data = df16_2, 
         family = binomial())
summary(f1)
broom::tidy(f1)

# 2.p for interaction-----------------------------------------------------------

##### 2.1交互项P值得含义和作用--------------------------------------------------
"
(1)
在正常模型中得到结论：介入因素A对结局B在人群中有显著干预效应
人群进行亚组分析，添加因素B和因素A得交互项
A：B得交互项P<0.05

则说明介入因素A对结局B在人群中干预效应得程度受因素B得影响
再根据因素B得具体亚组分组可以得出B对结局得具体影响或者临床意义

(2)举例说明
举例：术后吃药可以显著降低CKM患者房颤消融术后复发
进行亚组分析，年龄分组，大于65一组，小于65一组
亚组分析中age:是否吃药 的p for interaction<0.05
这就说明根据age进行的亚组分析是有意义的，
且发现大于65一组，术后吃药风险降低，小于65一组中术后吃药风险也降低，
但是程度没有大于65一组大
得出结论
术后吃药对房颤消融术后复发在人群中干预效应得程度受年龄得影响
老年人中术后吃药复发风险更小
"

##### 2.2P for interaction的实现------------------------------------------------
###### 2.2.1多项分类的交互项P值-------------------------------------------------
df16_2$x7 <- factor(df16_2$x7, levels = c(1,2,3), 
                    labels = c("轻", "中", "胖"))
# 而对于多项分类【如血型】，产生哑变量后，相乘则会产生多个交互项
# 此时不能整体判断交互作用是否有意义
# 我们可以先构建一个无交互作用项的模型
# 再构建一个有交互作用项的模型
# 然后采用似然比检验（likelihood ratio test）进行比较有个模型差异
f3 <- glm(y ~ x1 + x7 + x1:x7, 
         family = binomial(),
         data = df16_2
)
summary(f3)
f4 <- glm(y ~ x1 + x7, 
          family = binomial(),
          data = df16_2
)
summary(f4)
# 似然比检验
lmtest::lrtest(f4,f3)
"
Likelihood ratio test

Model 1: y ~ x1 + x7
Model 2: y ~ x1 + x7 + x1:x7
  #Df  LogLik Df Chisq Pr(>Chisq)
1   6 -31.041                    
2  12 -27.230  6 7.623      0.267
P是0.267>0.05也就是交互项的P for interaction没有什么意义

"
###### 2.2.1二分类的交互项P值---------------------------------------------------
df16_2$x2 <- factor(df16_2$x2, levels = c(0,1), 
                    labels = c("无高血压", "有高血压"))
df16_2$x3 <- factor(df16_2$x3, levels = c(0,1), 
                    labels = c("无高血压家族史", "有高血压家族史"))

f5 <- glm(y ~ x2 + x3 + x2:x3,
          family = binomial(),
          data = df16_2
)
summary(f5)

# 3.per 1 sd--------------------------------------------------------------------
# 即原始数据每升高1个标准差，效应量发生的风险
# 处理方法就是将想要处理的变量标准化就行
# 新建一列weight
df16_2$weight <- rnorm(54, 70,11)

# 进行标准化
df16_2$weight.scaled <- scale(df16_2$weight)

# 进行逻辑回归
f <- glm(y ~ weight.scaled, data = df16_2)
broom::tidy(f,conf.int=T,exponentiate=T)

