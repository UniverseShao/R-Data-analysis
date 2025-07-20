# 35.1 方法1：dcurves-----------------------------------------------------------
rm(list = ls())
# 加载R包和数据
library(dcurves)
library(survival)
data("df_surv")

# 查看数据结构
dim(df_surv)
str(df_surv)
#划分训练集测试集
set.seed(123)
train <- sample(1:nrow(df_surv),nrow(df_surv) * 0.7)
train_df <- df_surv[train,]
test_df <- df_surv[- train,]

dim(train_df)
dim(test_df)

##### 35.1.1 训练集-------------------------------------------------------------
###### 35.1.1.1单个预测变量-----------------------------------------------------
library(ggplot2)

dcurves::dca(Surv(ttcancer, cancer) ~ famhistory,#这个~后面跟的是预测变量而不是模型自变量
             data = train_df,
             time = 1 # 时间选1年
) %>% 
  plot(smooth = T)
###### 35.1.1.2多个模型的DCA画在一起--------------------------------------------
dcurves::dca(Surv(ttcancer, cancer) ~ cancerpredmarker + marker + famhistory,#这里不是三个一起做一个模型
             #而是三个不同的模型
             data = train_df,
             as_probability = "marker", # 只有marker需要转换成概率
             time = 1,
             label = list(cancerpredmarker = "Prediction Model", 
                          marker = "Biomarker")) %>%
  plot(smooth = TRUE,show_ggplot_code = T) +
  ggplot2::labs(x = "Treatment Threshold Probability")

###### 35.1.1.3同一个模型在不同时间点的DCA--------------------------------------
# 如果你的预测变量是多个，就需要先计算预测概率
# 构建一个多元cox回归
cox_model <- coxph(Surv(ttcancer, cancer) ~ age + famhistory + marker, 
                   data = train_df)

# 计算1.5年的概率
train_df$prob1 <- c(1-(summary(survfit(cox_model,newdata=train_df), 
                               times=1.5)$surv))

# 我们分2步，先获取数据，再用ggplot2画图
x1 <- dcurves::dca(Surv(ttcancer, cancer) ~ prob1,
                   data = train_df,
                   time = 1.5
)%>% 
  dcurves::as_tibble()

# 使用自带的画图代码
ggplot(x1, aes(x=threshold, y=net_benefit,color=variable))+
  stat_smooth(method = "loess", se = FALSE, formula = "y ~ x", span = 0.2) +
  coord_cartesian(ylim = c(-0.03, 0.25)) +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1)) +
  labs(x = "Threshold Probability", y = "Net Benefit", color = "") +
  theme_bw()

##### 35.1.2 测试集-------------------------------------------------------------
# 在训练集构建一个多元cox回归
cox_model <- coxph(Surv(ttcancer, cancer) ~ age + famhistory + marker, 
                   data = train_df)

# 计算测试集1.5年的概率
test_df$prob1 <- c(1-(summary(survfit(cox_model,newdata=test_df), 
                              times=1.5)$surv))

# 先获取数据，再用ggplot2画图
x1 <- dcurves::dca(Surv(ttcancer, cancer) ~ prob1,
                   data = test_df,
                   time = 1.5
)%>% 
  dcurves::as_tibble()

# 使用自带的画图代码
ggplot(x1, aes(x=threshold, y=net_benefit,color=variable))+
  stat_smooth(method = "loess", se = FALSE, formula = "y ~ x", span = 0.2) +
  coord_cartesian(ylim = c(-0.03, 0.25)) +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1)) +
  labs(x = "Threshold Probability", y = "Net Benefit", color = "") +
  theme_bw()

# 35.2 方法2：ggDCA(包已经无用)-------------------------------------------------
# 35.3 方法3：stdca.R(基本方法)-------------------------------------------------
##### 35.3.1 训练集-------------------------------------------------------------
###### 35.3.1.1单个模型图-------------------------------------------------------
library(survival)
library(dcurves)
data("df_surv")
# 加载函数,这个是我修改过的
# 原函数有时会报错:no points selected for one or more curves...
# 获取方式：https://mp.weixin.qq.com/s/TZ7MSaPZZ0Pwomyp_7wqFw
source("C:\\Users\\Administrator\\Desktop\\R语言脚本(SWY精心编辑版本)\\临床预测模型\\stdca.R") 
# 格式准备好
train_df$cancer <- as.numeric(train_df$cancer) # stdca函数需要结果变量是0,1
train_df <- as.data.frame(train_df) # stdca函数只接受data.frame
# 构建一个多元cox回归
cox_model <- coxph(Surv(ttcancer, cancer) ~ age + famhistory + marker, 
                   data = train_df)

# 计算1.5年的概率
train_df$prob1 <- c(1-(summary(survfit(cox_model,newdata=train_df),
                               times=1.5)$surv))

# 这个函数我修改过，如果你遇到报错，可以通过添加参数 xstop=0.5 解决
dd <- stdca(data=train_df, outcome="cancer", ttoutcome="ttcancer", 
            timepoint=1.5, 
            predictors="prob1",
            smooth=TRUE
)

###### 35.3.1.2多模型图---------------------------------------------------------
# 建立多个模型
cox_fit1 <- coxph(Surv(ttcancer, cancer) ~ famhistory+marker, 
                  data = train_df)
cox_fit2 <- coxph(Surv(ttcancer, cancer) ~ age + famhistory + marker, 
                  data = train_df)
cox_fit3 <- coxph(Surv(ttcancer, cancer) ~ age + famhistory, 
                  data = train_df)

# 计算每个模型的概率
train_df$prob1 <- c(1-(summary(survfit(cox_fit1, newdata=train_df), 
                               times=1.5)$surv))
train_df$prob2 <- c(1-(summary(survfit(cox_fit2, newdata=train_df), 
                               times=1.5)$surv))
train_df$prob3 <- c(1-(summary(survfit(cox_fit3, newdata=train_df), 
                               times=1.5)$surv))

# 画图
dd <- stdca(data=train_df, outcome="cancer", ttoutcome="ttcancer", 
            timepoint=1.5, 
            predictors=c("prob1","prob2","prob3"),  
            smooth=TRUE
)

##### 35.3.2 测试集-------------------------------------------------------------
# 格式准备好
test_df$cancer <- as.numeric(test_df$cancer) # stdca函数需要结果变量是0,1
test_df <- as.data.frame(test_df) # stdca函数只接受data.frame

# 计算每个模型的概率
test_df$prob1 <- c(1-(summary(survfit(cox_fit1, newdata=test_df), 
                              times=1.5)$surv))
test_df$prob2 <- c(1-(summary(survfit(cox_fit2, newdata=test_df), 
                              times=1.5)$surv))
test_df$prob3 <- c(1-(summary(survfit(cox_fit3, newdata=test_df), 
                              times=1.5)$surv))

# 画图
dd <- stdca(data=test_df, outcome="cancer", ttoutcome="ttcancer", 
            timepoint=1.5, 
            predictors=c("prob1","prob2","prob3"),  
            smooth=TRUE
)

# 35.4 方法4：DIY---------------------------------------------------------------
##### 35.4.1 训练集-------------------------------------------------------------
###### 35.4.1.1返回画图数据-----------------------------------------------------
cox_dca <- stdca(data = train_df, outcome = "cancer", ttoutcome = "ttcancer", 
                 timepoint = 1.5, 
                 predictors = c("prob1","prob2","prob3"),
                 smooth=TRUE,
                 graph = FALSE
)
library(tidyr)
cox_dca_df <- cox_dca$net.benefit %>% 
  pivot_longer(cols = c(all,none,contains("sm")),names_to = "models",
               values_to = "net_benefit"
  )
###### 35.4.1.2画图-------------------------------------------------------------
library(ggplot2)
library(ggsci)

ggplot(cox_dca_df, aes(x=threshold,y=net_benefit))+
  geom_line(aes(color=models),linewidth=1.2)+
  scale_color_jama(name="Models Types",
                   labels=c("All","None","Model1","Model2","Model3"))+
  scale_x_continuous(labels = scales::label_percent(accuracy = 1),
                     name="Threshold Probility")+
  scale_y_continuous(limits = c(-0.05,0.2),name="Net Benefit")+
  theme_bw(base_size = 14)+
  theme(legend.background = element_blank(),
        legend.position.inside = c(0.85,0.75)
  )
##### 35.4.2 测试集-------------------------------------------------------------
###### 35.4.2.1获取画图数据-----------------------------------------------------
# 格式准备好
test_df$cancer <- as.numeric(test_df$cancer) # stdca函数需要结果变量是0,1
test_df <- as.data.frame(test_df) # stdca函数只接受data.frame

# 计算每个模型的概率
test_df$prob1 <- c(1-(summary(survfit(cox_fit1, newdata=test_df), 
                              times=1.5)$surv))
test_df$prob2 <- c(1-(summary(survfit(cox_fit2, newdata=test_df), 
                              times=1.5)$surv))
test_df$prob3 <- c(1-(summary(survfit(cox_fit3, newdata=test_df), 
                              times=1.5)$surv))

# 返回画图数据
dd <- stdca(data=test_df, outcome="cancer", ttoutcome="ttcancer", 
            timepoint=1.5, 
            predictors=c("prob1","prob2","prob3"),  
            smooth=TRUE, graph = F
)
## [1] "prob1: No observations with risk greater than 95%, and therefore net benefit not calculable in this range."

# 格式整理
cox_dca_df <- dd$net.benefit %>% 
  pivot_longer(cols = c(all,none,contains("sm")),names_to = "models",
               values_to = "net_benefit"
  )
###### 35.4.2.2画图-------------------------------------------------------------
library(ggplot2)
library(ggsci)

ggplot(cox_dca_df, aes(x=threshold,y=net_benefit))+
  geom_line(aes(color=models),linewidth=1.2)+
  scale_color_jama(name="Models Types",
                   labels=c("All","None","Model1","Model2","Model3"))+
  scale_x_continuous(labels = scales::label_percent(accuracy = 1),
                     name="Threshold Probility")+
  scale_y_continuous(limits = c(-0.05,0.2),name="Net Benefit")+
  theme_bw(base_size = 14)+
  theme(legend.background = element_blank(),
        legend.position.inside = c(0.85,0.75)
  )


