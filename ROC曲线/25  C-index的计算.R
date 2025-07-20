# C-index的一些基本概念---------------------------------------------------------
#把所有的研究对象随机地两两组对，对于一对病人，如果生存时间较长的一位，
#其预测生存时间或者生存概率也长于或高于另一位，则称之为预测结果和实际结果一致
# 25.1 logistic回归-------------------------------------------------------------
#在二分类变量中，C-statistic就是AUC，二者在数值上是一样的
#把所有的研究对象随机地两两组对，对于一对病人，如果生存时间较长的一位，
#其预测生存时间或者生存概率也长于或高于另一位，则称之为预测结果和实际结果一致
##### 数据预处理----------------------------------------------------------------
rm(list = ls())
lowbirth <- read.csv("C:\\Users\\Administrator\\Desktop\\R语言脚本(SWY精心编辑版本)\\临床预测模型\\datasets\\lowbirth.csv")

library(dplyr)

tmp <- lowbirth %>% 
  mutate(across(where(is.character),as.factor),#字符型向量转换因子
         vent = factor(vent),
         #dead = factor(dead), 下面介绍的方法2不能是因子型，所以不转换了
         race = case_when(race %in% 
                            c("native American","oriental") ~ "other",
                          .default = race),
         race = factor(race))

str(tmp)

##### 25.1.1 方法1：rms---------------------------------------------------------
library(rms)

dd <- datadist(tmp)
options(datadist="dd")

fit2 <- lrm(dead ~ birth + lowph + pltct + bwt + vent + race,
            data = tmp)

fit2
#模型结果中Rank Discrim.下面的C就是C-Statistic

##### 25.1.2 方法2：Hmisc-------------------------------------------------------
# 先计算线性预测值
tmp$predvalue<-predict(fit2)
library(Hmisc)
somers2(tmp$predvalue, tmp$dead)
# 或者用
rcorr.cens(tmp$predvalue, tmp$dead)


##### 25.1.3 方法3：ROCR--------------------------------------------------------
library(ROCR)

tmp$predvalue<-predict(fit2)

# 取出C-Statistics，和上面结果一样
pred <- prediction(tmp$predvalue, tmp$dead)

auc <- round(performance(pred, "auc")@y.values[[1]],digits = 4)
auc

##### 25.1.4 方法4：pROC--------------------------------------------------------
library(pROC)
roc(tmp$dead, tmp$predvalue, legacy.axes = T, print.auc = T, print.auc.y = 45)


# 25.2 cox回归------------------------------------------------------------------
#cox回归的C-statistic可以用survival包计算
#生存分析的C-statistic和AUC是不一样的
#生存分析的C指数一般是指Harrell’s C指数
rm(list = ls())
library(survival)
library(dplyr)

df1 <- lung %>% 
  mutate(status=ifelse(status == 1,1,0))

str(df1)
cox_fit1 <- coxph(Surv(time, status) ~ age + sex + ph.ecog,
                  data = df1,x = T, y = T)

summary(cox_fit1)
#Concordance就是C-statistic

summary(cox_fit1)$concordance
