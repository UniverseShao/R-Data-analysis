# 1.直线回归--------------------------------------------------------------------
df9_1 <- data.frame(x = c(13,11,9,6,8,10,12,7),
                    y = c(3.54,3.01,3.09,2.48,2.56,3.36,3.18,2.65))

## 1.1建立回归方程--------------------------------------------------------------
fit <- lm(y ~ x, data = df9_1)
summary(fit)

broom::tidy(fit,conf.int = T)

new_x <- data.frame(x=12)
# 总体均数的可信区间
predict(fit, newdata = new_x,interval = "confidence",level = 0.95)
# 个体Y值的预测区间
predict(fit, newdata = new_x,interval = "prediction",level = 0.95)

# 2.直线相关--------------------------------------------------------------------
df <- data.frame(
  weight = c(43,74,51,58,50,65,54,57,67,69,80,48,38,85,54),
  kv = c(217.22,316.18,231.11,220.96,254.70,293.84,263.28,271.73,263.46,
         276.53,341.15,261.00,213.20,315.12,252.08)
)

str(df)
## 2.1相关系数计算--------------------------------------------------------------
cor(df$weight, df$kv)
library(psych)
corr.test(df)

cor.test(~ weight + kv, data = df)

## 2.2可视化结果----------------------------------------------------------------
library(ggplot2)

ggplot(df, aes(weight, kv)) +
  geom_point(size = 4) +
  geom_smooth(method = "lm",se=F) +
  geom_vline(xintercept = mean(df$weight),linetype=2)+
  geom_hline(yintercept = mean(df$kv),linetype=2)+
  labs(x="体重(kg)X",y="双肾体积(ml)Y")+
  theme_classic()


# 3.秩相关----------------------------------------------------------------------
library(haven)
df9_8 <- read_sav("C:/Users/Administrator/Desktop/R脚本(SWY精心编辑版)/Medical Statistics/datasets/例09-08.sav")
View(df9_8)

cor(df9_8$x,df9_8$y, method = "spearman")
cor.test(df9_8$x,df9_8$y, method = "spearman")

# 4.多条回归直线的比较----------------------------------------------------------
# 例9-1数据
df9_1 <- data.frame(x = c(13,11,9,6,8,10,12,7),
                    y = c(3.54,3.01,3.09,2.48,2.56,3.36,3.18,2.65))

library(haven)
df9_9 <- read_sav("C:/Users/Administrator/Desktop/R脚本(SWY精心编辑版)/Medical Statistics/datasets/例09-09.sav")

## 4.1建立回归方程--------------------------------------------------------------
# 例9-1的回归方程
fit9_1 <- lm(y ~ x, data = df9_1)
# 例9-2的回归方程
fit9_9 <- lm(y ~ x, data = df9_9)

a1 <- anova(fit9_1)
a1# 检验自变量 x 对因变量 y 的影响是否显著
# 和summary(fit9_1) broom::tidy(fit9_1)中最后得到的P值是一样的


# 如果此时你直接使用anova进行F检验，会得到以下报错
anova(fit9_1,fit9_9)
# 这是因为这个函数只能处理样本量完全一样的两个模型的比较
# 我们可以把两个数据集合并到一起，添加一个交互项，查看交互项的显著性
## 4.2合并数据------------------------------------------------------------------
df9_1$group <- "group9_1"
df9_9$group <- "group9_9"

df9 <- rbind(df9_1,df9_9[,-1])
df9$group <- factor(df9$group)
str(df9)

### 4.2.1建立回归方程并比较-----------------------------------------------------
# 建立不包含交互项的模型
model_no_interaction <- lm(y ~ x + group, data = df9)

# 建立包含交互项的模型
model_interaction <- lm(y ~ x * group, data = df9)

# 使用anova函数比较两个模型
anova(model_no_interaction, model_interaction)
#最后显示的P就是 交互项的P
#P=0.4052这提示两者的斜率没有显著差异，提示两者是平行的
# 查看模型摘要，检查group的显著性
summary(model_no_interaction)

### 4.2.2单因素协方差分析的评估检验就可以---------------------------------------
### 4.2.2即检验回归斜率的同质性-------------------------------------------------
fit1 <- aov(y ~ x + group, data = df9)
summary(fit1)
fit2 <- aov(y ~ x*group, data = df9)
summary(fit2)


### 4.2.3多条回归直比较可视化---------------------------------------------------
pred <- predict(fit2)

library(ggplot2)
ggplot(data = cbind(df9, pred),
       aes(x, y)) + geom_point() +
  facet_grid(. ~ group) + geom_line(aes(y=pred)) +
  labs(title="ANCOVA for weight by x and y") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.position="none")

# 5.曲线拟合--------------------------------------------------------------------
## 第一例-----------------------------------------------------------------------
library(haven)
df9_11 <- read_sav("C:/Users/Administrator/Desktop/R脚本(SWY精心编辑版)/Medical Statistics/datasets/例09-11.sav")
str(df9_11)

## 5.1画图查看趋势--------------------------------------------------------------
library(ggplot2)

ggplot(df9_11, aes(x,y))+
  geom_point(size=4)

## 5.2对自变量X做对数转换-------------------------------------------------------
ggplot(df9_11, aes(log10(x),y))+
  geom_point(size=4)

## 5.3对数转换后的X建立直线回归方程---------------------------------------------
f9_11 <- lm(y ~ log10(x), data = df9_11)
summary(f9_11)


## 第二例-----------------------------------------------------------------------

library(haven)
df9_12 <- read_sav("C:/Users/Administrator/Desktop/R脚本(SWY精心编辑版)/Medical Statistics/datasets/例09-12.sav")
str(df9_12)
ggplot(df9_12, aes(x,y))+
  geom_point(size=4)

ggplot(df9_12, aes(x,log(y)))+
  geom_point(size=4)

f9_12 <- lm(log(y) ~ x, data = df9_12)
summary(f9_12)


