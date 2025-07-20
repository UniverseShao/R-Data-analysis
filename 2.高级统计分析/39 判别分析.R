rm(list = ls())
# 1.Fisher一次判别分析----------------------------------------------------------
##### 1.1Fisher判别分析基本概念-------------------------------------------------
"
Fisher判别又称为典型判别（canonical discriminant）分析，适用于两类和多分类判别

Fisher判别使用贝叶斯定理确定每个观测属于某个类别的概率
如果你有两个类别，比如良性和恶性，
判别分析会分别计算属于两个类别的概率，然后选择概率大的类别作为正确的类别

线性判别分析假设每个类中的观测服从多元正态分布，并且不同类别之间的协方差相等
二次判别假设观测服从正态分布，每种类别都有自己的协方差
"
library(haven)
df <- read_sav("C:/Users/Administrator/Desktop/R脚本(SWY精心编辑版)/Medical Statistics/datasets/例20-01.sav")
View(df)
psych::headTail(df)
##### 1.2Fisher判别分析的实现
library(MASS)
fit <- lda(y ~ x1+x2+x3, data = df)---------------------------------------------
fit

"
Group means:
  x1 x2 x3
1 -3  4 -1
2  4 -5  1
组1的变量均值：x1 = -3，x2 = 4，x3 = -1
组2的变量均值：x1 = 4，x2 = -5，x3 = 1
**x1**：组1的均值（-3）显著低于组2（4），说明x1对区分两组有重要作用。
**x2**：组1（4）与组2（-5）差异极大，可能是最强判别变量。
**x3**：两组差异较小（-1 vs. 1），贡献可能较弱

Coefficients of linear discriminants:
          LD1
x1  0.0395150
x2 -0.1265698
x3  0.1792631
LDA通过线性组合 LD1 = 0.040*x1 - 0.127*x2 + 0.179*x3 计算判别得分
系数的绝对值反映变量对分类的贡献大小：x3 > x2 > x1
"
##### 1.3Fisher判别分析结果可视化-----------------------------------------------
plot(fit,type="both")
# 横坐标是LD1分数，纵坐标是密度（概率密度）

##### 1.4predict得到判别分析结果------------------------------------------------
"
predict用于判别分析可以得到3种类型的结果
class是类别，posterior是概率，x是线性判别评分
"
pred <- predict(fit)$class
table(df$y, pred)

# 查看概率
predict(fit)$posterior
##### 1.5ggplot2可视化----------------------------------------------------------
df.plot <- data.frame(LD1 = predict(fit)$x[,1],
                      y = factor(df$y,labels = c("早期患者","晚期患者"))
)
library(ggplot2)

ggplot(df.plot, aes(x=LD1, fill=y))+
  geom_histogram()+
  facet_wrap(~ y, ncol = 1)


##### 1.6模型预测新数据---------------------------------------------------------
tmp <- data.frame(x1 = c(-9,-7,-9),
                  x2 = c(-18,-2,6),
                  x3 = c(3,3,1)
)#只有三个样本的数据
predict(fit, newdata = tmp)

"
$class
[1] 2 2 1
Levels: 1 2
样本1：分类为 Group 2
样本2：分类为 Group 2
样本3：分类为 Group 1
$posterior
           1         2
1 0.01736557 0.9826344
2 0.35826933 0.6417307
3 0.87974275 0.1202573
           1         2
1 0.01736557 0.9826344  # 样本1：98.3%概率属于Group 2
2 0.35826933 0.6417307  # 样本2：64.2%概率属于Group 2
3 0.87974275 0.1202573  # 样本3：88.0%概率属于Group 1

$x
         LD1
1  2.4580167
2  0.5119296
3 -0.9381851
样本1：
LD1 = 2.458（高正值）→ 接近Group 2的判别得分均值（训练数据中Group 2的LD1偏向正值）。
"

##### 1.7线性判别分析的结果可视化-----------------------------------------------
str(iris)
library(MASS)

fit <- lda(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = iris)
fit
"
Call:
lda(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, 
    data = iris)

Prior probabilities of groups:
    setosa versicolor  virginica 
 0.3333333  0.3333333  0.3333333 

Group means:
           Sepal.Length Sepal.Width Petal.Length Petal.Width
setosa            5.006       3.428        1.462       0.246
versicolor        5.936       2.770        4.260       1.326
virginica         6.588       2.974        5.552       2.026

Coefficients of linear discriminants:
                    LD1         LD2
Sepal.Length  0.8293776 -0.02410215
Sepal.Width   1.5344731 -2.16452123
Petal.Length -2.2012117  0.93192121
Petal.Width  -2.8104603 -2.83918785

Proportion of trace:
   LD1    LD2 
0.9912 0.0088 
LD1 解释了 99.12% 的方差，说明它几乎可以完全区分三组。
LD2 仅解释了 0.88%，作用较小，可能仅用于微调 versicolor 和 virginica 的边界。
"

###### 1.7.1可视化评分结果------------------------------------------------------
iris$LD1 <- predict(fit)$x[,1]
iris$LD2 <- predict(fit)$x[,2]
library(ggplot2)

ggplot(iris, aes(LD1,LD2))+
  geom_point(aes(color=Species),size=3)

###### 1.7.2直方图---------------------------------------------------------------
ggplot(iris, aes(x=LD1, fill=Species))+
  geom_histogram()+
  facet_wrap(~ Species, ncol = 1)


# 2.二次判别分析----------------------------------------------------------------
fit <- qda(y ~ x1+x2+x3, data = df)
fit
pred <- predict(fit)$class
table(df$y, pred)

# 3.Bayes判别分析---------------------------------------------------------------
"
贝叶斯判别也是根据概率大小进行判别，要求各类近似服从多元正态分布。
当各类的协方差相等时，可得到线性贝叶斯判别函数，
当各类的协方差不相等时，可得到二次贝叶斯判别函数。
"
library(haven)
df2 <- read_sav("C:/Users/Administrator/Desktop/R脚本(SWY精心编辑版)/Medical Statistics/datasets/例20-04.sav")
View(df2)
df$y <- factor(df$y)
psych::headTail(df)

library(klaR)

fit <- NaiveBayes(y ~ ., data = df)
fit

pred <- predict(fit)$class
table(pred, df$y)

# ⭐判别分析和逻辑回归的区别----------------------------------------------------
"
特征            判别分析（LDA/QDA）           逻辑回归
变量类型        自变量需连续且正态分布       自变量可连续、分类或混合类型
分类边界        线性（LDA）或二次（QDA）     线性（通过logit变换）
输出结果        判别得分 + 分类              概率 + 分类
解释性          判别函数系数（难解释）       优势比（OR值，易解释）
异常值敏感性    敏感（依赖均值和方差）       较稳健
样本量要求      各类别样本量均衡             可处理不平衡数据
适用类别数      二元或多元                   二元（Logistic）或多元（Multinomial）
"
