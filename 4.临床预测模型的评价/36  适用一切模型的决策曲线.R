# 36.1 多个时间点多个cox模型的数据提取------------------------------------------

rm(list = ls())
library(survival)
library(dcurves)
data("df_surv")
source("C:\\Users\\Administrator\\Desktop\\R语言脚本(SWY精心编辑版本)\\临床预测模型\\stdca.R")
df_surv$cancer <- as.numeric(df_surv$cancer) # stdca函数需要结果变量是0,1
df_surv <- as.data.frame(df_surv) # stdca函数只接受data.frame

##### 36.1.1建立多个模型,计算每个模型在不同时间点的概率-------------------------
cox_fit1 <- coxph(Surv(ttcancer, cancer) ~ famhistory+marker, 
                  data = df_surv)
cox_fit2 <- coxph(Surv(ttcancer, cancer) ~ age + famhistory + marker, 
                  data = df_surv)
cox_fit3 <- coxph(Surv(ttcancer, cancer) ~ age + famhistory, 
                  data = df_surv)

# 计算每个模型在不同时间点的死亡概率
df_surv$prob11 <- c(1-(summary(survfit(cox_fit1, newdata=df_surv), 
                               times=1)$surv))
df_surv$prob21 <- c(1-(summary(survfit(cox_fit2, newdata=df_surv), 
                               times=1)$surv))
df_surv$prob31 <- c(1-(summary(survfit(cox_fit3, newdata=df_surv), 
                               times=1)$surv))

df_surv$prob12 <- c(1-(summary(survfit(cox_fit1, newdata=df_surv), 
                               times=2)$surv))
df_surv$prob22 <- c(1-(summary(survfit(cox_fit2, newdata=df_surv), 
                               times=2)$surv))
df_surv$prob32 <- c(1-(summary(survfit(cox_fit3, newdata=df_surv), 
                               times=2)$surv))

df_surv$prob13 <- c(1-(summary(survfit(cox_fit1, newdata=df_surv), 
                               times=3)$surv))
df_surv$prob23 <- c(1-(summary(survfit(cox_fit2, newdata=df_surv), 
                               times=3)$surv))
df_surv$prob33 <- c(1-(summary(survfit(cox_fit3, newdata=df_surv), 
                               times=3)$surv))

##### 36.1.2计算threshold和net benefit------------------------------------------

cox_dca1 <- stdca(data = df_surv, 
                  outcome = "cancer", 
                  ttoutcome = "ttcancer", 
                  timepoint = 1, 
                  predictors = c("prob11","prob21","prob31"),
                  smooth=TRUE,
                  graph = FALSE
)
## [1] "prob31: No observations with risk greater than 99%, and therefore net benefit not calculable in this range."

cox_dca2 <- stdca(data = df_surv, 
                  outcome = "cancer", 
                  ttoutcome = "ttcancer", 
                  timepoint = 2, 
                  predictors = c("prob12","prob22","prob32"),
                  smooth=TRUE,
                  graph = FALSE
)

cox_dca3 <- stdca(data = df_surv, 
                  outcome = "cancer", 
                  ttoutcome = "ttcancer", 
                  timepoint = 3, 
                  predictors = c("prob13","prob23","prob33"),
                  smooth=TRUE,
                  graph = FALSE
)


library(tidyr)
library(dplyr)

##### 36.1.3第一种数据整理方法--------------------------------------------------

cox_dca_df1 <- cox_dca1$net.benefit
cox_dca_df2 <- cox_dca2$net.benefit
cox_dca_df3 <- cox_dca3$net.benefit

names(cox_dca_df1)[2] <- "all1"
names(cox_dca_df2)[2] <- "all2"
names(cox_dca_df3)[2] <- "all3"

tmp <- cox_dca_df1 %>% 
  left_join(cox_dca_df2) %>% 
  left_join(cox_dca_df3) %>% 
  pivot_longer(cols = contains(c("all","sm","none")),
               names_to = "models",
               values_to = "net_benefit"
  )

##### 36.1.4画图----------------------------------------------------------------
library(ggplot2)
library(ggsci)

ggplot(tmp, aes(x=threshold,y=net_benefit))+
  geom_line(aes(color=models),linewidth=1.2)+
  scale_x_continuous(labels = scales::label_percent(accuracy = 1),
                     name="Threshold Probility")+
  scale_y_continuous(limits = c(-0.05,0.3),name="Net Benefit")+
  theme_bw(base_size = 14)

##### 36.1.5第二种数据整理方法--------------------------------------------------

cox_dca_df1 <- cox_dca1$net.benefit
cox_dca_df2 <- cox_dca2$net.benefit
cox_dca_df3 <- cox_dca3$net.benefit

cox_dca_long_df1 <- cox_dca_df1 %>% 
  rename(mod1 = prob11_sm,
         mod2 = prob21_sm,
         mod3 = prob31_sm
  ) %>% 
  select(-4:-6) %>% 
  mutate(time = "1") %>% 
  pivot_longer(cols = c(all,none,contains("mod")),names_to = "models",
               values_to = "net_benefit"
  )

cox_dca_long_df2 <- cox_dca_df2 %>% 
  rename(mod1 = prob12_sm,
         mod2 = prob22_sm,
         mod3 = prob32_sm
  ) %>% 
  select(-4:-6) %>% 
  mutate(time = "2") %>% 
  pivot_longer(cols = c(all,none,contains("mod")),names_to = "models",
               values_to = "net_benefit"
  )


cox_dca_long_df3 <- cox_dca_df3 %>% 
  rename(mod1 = prob13_sm,
         mod2 = prob23_sm,
         mod3 = prob33_sm
  ) %>% 
  select(-4:-6) %>% 
  mutate(time = "3") %>% 
  pivot_longer(cols = c(all,none,contains("mod")),names_to = "models",
               values_to = "net_benefit"
  )

tes <- bind_rows(cox_dca_long_df1,cox_dca_long_df2,cox_dca_long_df3)

##### 36.1.6画图----------------------------------------------------------------
ggplot(tes,aes(x=threshold,y=net_benefit))+
  geom_line(aes(color=models,linetype=time),linewidth=1.2)+
  scale_x_continuous(labels = scales::label_percent(accuracy = 1),
                     name="Threshold Probility")+
  scale_y_continuous(limits = c(-0.05,0.3),name="Net Benefit")+
  theme_bw(base_size = 14)

##### 36.1.7分面画图------------------------------------------------------------
ggplot(tes,aes(x=threshold,y=net_benefit))+
  geom_line(aes(color=models),linewidth=1.2)+
  scale_y_continuous(limits = c(-0.05,0.3),name="Net Benefit")+
  scale_x_continuous(labels = scales::label_percent(accuracy = 1),
                     name="Threshold Probility")+
  scale_y_continuous(limits = c(-0.05,0.3),name="Net Benefit")+
  theme_bw(base_size = 14)+
  facet_wrap(~time)
# 36.2 lasso回归----------------------------------------------------------------
rm(list = ls())
library(glmnet)
library(tidyverse)

df <- readRDS(file = "C:\\Users\\Administrator\\Desktop\\R语言脚本(SWY精心编辑版本)\\临床预测模型\\datasets\\df_example.rds")

df <- df %>% 
  select(-c(2:3)) %>% 
  mutate(sample_type = ifelse(sample_type=="Tumor",1,0))

set.seed(123)
ind <- sample(1:nrow(df),nrow(df)*0.6)

train_df <- df[ind,]
test_df <- df[-ind,]

x <- as.matrix(train_df[,-1])
y <- train_df$sample_type

cvfit = cv.glmnet(x, y, family = "binomial")
plot(cvfit)

prob_lasso <- predict(cvfit,
                      newx = as.matrix(test_df[,-1]),
                      s="lambda.1se",
                      type="response") #返回概率

source("./datasets/dca.r")

test_df$lasso <- prob_lasso

df_lasso <- dca(data = test_df, # 指定数据集,必须是data.frame类型
                outcome="sample_type", # 指定结果变量
                predictors="lasso", # 指定预测变量
                probability = T
)

library(ggplot2)
library(ggsci)
library(tidyr)

df_lasso$net.benefit %>% 
  pivot_longer(cols = -threshold, 
               names_to = "type", 
               values_to = "net_benefit") %>% 
  ggplot(aes(threshold, net_benefit, color = type))+
  geom_line(linewidth = 1.2)+
  scale_color_jama(name = "Model Type")+ 
  scale_y_continuous(limits = c(-0.02,1),name = "Net Benefit")+ 
  scale_x_continuous(limits = c(0,1),name = "Threshold Probility")+
  theme_bw(base_size = 16)+
  theme(legend.position.inside = c(0.2,0.3),
        legend.background = element_blank()
  )

# 36.3 随机森林-----------------------------------------------------------------
library(ranger)

rf <- ranger(sample_type ~ ., data = train_df)

# 获取测试集的概率
prob_rf <- predict(rf,test_df[,-1],type = "response")$predictions

test_df$rf <- prob_rf

df_rf <- dca(data = test_df, # 指定数据集,必须是data.frame类型
             outcome="sample_type", # 指定结果变量
             predictors="rf", # 指定预测变量
             probability = T,
             graph = F
)

df_rf$net.benefit %>% 
  pivot_longer(cols = -threshold, 
               names_to = "type", 
               values_to = "net_benefit") %>% 
  ggplot(aes(threshold, net_benefit, color = type))+
  geom_line(linewidth = 1.2)+
  scale_color_jama(name = "Model Type")+ 
  scale_y_continuous(limits = c(-0.02,1),name = "Net Benefit")+ 
  scale_x_continuous(limits = c(0,1),name = "Threshold Probility")+
  theme_bw(base_size = 16)+
  theme(legend.position = c(0.2,0.3),
        legend.background = element_blank()
  )



