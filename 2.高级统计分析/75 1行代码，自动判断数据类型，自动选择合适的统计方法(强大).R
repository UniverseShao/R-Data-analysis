rm(list = ls())
"
今天介绍一个R包CBCgrps，非常优秀，可以：

1行代码实现常见的统计分析方法，
自动判断数据是计量资料还是计数资料，
自动判断数据是不是正态分布，
自动选择合适的统计方法，
结果中会显示P值和统计量值。
"
# 1.安装------------------------------------------------------------------------
install.packages("CBCgrps")
library(CBCgrps)
# CBCgrps就只有两个函数，用于两组间的比较和多组的比较

# 2.两组间比较------------------------------------------------------------------
data(dt)
head(dt)
##   age gender lac      type vaso  wbc   crp mort
## 1  62   male 7.6   surgery   No  6.5    NA    1
## 2  67   male 4.3   medical  Yes   NA 141.1    1
## 3  93   male 5.5   surgery   No  9.3  37.0    1
## 4  71 female 4.9 emergency   No  4.4    NA    1
## 5  72 female 4.1   medical   No 12.3 212.7    1
## 6  96   male 4.3   medical   No 17.6 222.1    1
str(dt)
## 'data.frame': 500 obs. of  8 variables:
##  $ age   : int  62 67 93 71 72 96 77 51 60 63 ...
##  $ gender: Factor w/ 2 levels "female","male": 2 2 2 1 1 2 1 2 2 2 ...
##  $ lac   : num  7.6 4.3 5.5 4.9 4.1 4.3 6.5 4.1 NA 4.1 ...
##  $ type  : Factor w/ 3 levels "emergency","medical",..: 3 2 3 1 2 2 2 3 3 2 ...
##  $ vaso  : Factor w/ 2 levels "No","Yes": 1 2 1 1 1 1 2 1 2 2 ...
##  $ wbc   : num  6.5 NA 9.3 4.4 12.3 17.6 NA 11 6.7 12.8 ...
##  $ crp   : num  NA 141 37 NA 213 ...
##  $ mort  : int  1 1 1 1 1 1 1 0 0 1 ...

"
我们以mort这个变量作为分组变量，它有2个组，分别分析其他变量在这2组中有没有差异

因为这个数据中既有分类变量（比如gender），也有连续型变量，这两种变量使用的统计方
法是完全不一样的，一般来说计数资料是卡方检验，而计量资料使用的是t检验或者秩和检
验

如果要自己写代码的话，会非常繁琐，如果要使用SPSS的话，也要分别进行
但是使用CBCgrps只要1行代码即可
"
tab1 <- twogrps(dt, gvar = "mort", ShowStatistic = T)

print(tab1)

"
对于符合正态分布的计量资料，这里用的是t检验，如果不符合使用的是wilcoxon秩和检验
对于计数资料则使用的是卡方检验
"
##### 2.1传统代码验证-----------------------------------------------------------
# 结果和上面是一模一样的
t.test(dt$age ~ factor(dt$mort))

# 结果和上面一模一样的
chisq.test(dt$gender, dt$mort)

##### 2.2指定哪些变量不符合正态分布---------------------------------------------
tab1 <- twogrps(dt, gvar = "mort", ShowStatistic = T, 
                skewvar = c("age") # 指定age不符合正态分布
)

print(tab1)
# 描述性统计的部分就变成了中位数（Q1,Q3）这种显示方法
# 此时使用的是秩和检验(Mann-Whitney U 检验)

# 3.多组间比较------------------------------------------------------------------
tab2 <- multigrps(dt, gvar = "type", ShowStatistic = T) # type是3分类
print(tab2)

"
这里符合正态分布的会使用方差分析，不符合的会使用Kruskal-Wallis检验，分类变量会使用
卡方检验或者Fisher精确概率法
除此之外，本包还支持修改默认的小数点位数，修改P值的显示方式，选择需要比较的变量
等，非常方便，大家可以自己探索下
"

