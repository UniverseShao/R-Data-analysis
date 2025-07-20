rm(list = ls())
# 基本概念----------------------------------------------------------------------
"临床决策曲线分析(decision curve analysis，DCA)
更佳贴近临床实际
对临床工作的开展比AUC/NRI/IDI等更具有指导意义"
#横轴是决策阈值概率（如患者愿意接受治疗的癌症风险阈值），纵轴是净获益
"图中还有2条特殊的线，一条水平的蓝色线，表示所有人都不接受治疗时
此时不管概率阈值是多少，净获益肯定都是0
一条黄色的线，表示所有人都接受治疗时
随着概率阈值的改变，其净获益的改变。这两条线代表了2种极端的情况
在给定的概率阈值下，肯定是净获益越大越好
所以一般来说，肯定是曲线距离两条特殊的线越远越好"


# 34.1 方法1：rmda--------------------------------------------------------------
library(rmda)
data("dcaData")
dim(dcaData)
head(dcaData)
str(dcaData)

set.seed(123)
train <- sample(1:nrow(dcaData), nrow(dcaData)*0.7)
train_df <- dcaData[train,]
test_df <- dcaData[- train,]
dim(train_df)
dim(test_df)

##### 34.1.1 训练集-------------------------------------------------------------
set.seed(123)

###### 34.1.1.1构建DCA----------------------------------------------------------
fit1 <- decision_curve(Cancer ~ Age + Female + Smokes, # 自变量和因变量
                       data = train_df, # 训练集数据
                       study.design = "cohort", # 选择研究类型
                       bootstraps = 50 # 重抽样次数
)

#"cohort"（队列研究）：默认选项，适用于前瞻性或回顾性队列研究
#"case-control"（病例对照研究）：如果数据是病例对照设计，需指定此选项以调整计算方式

###### 34.1.1.2画图-------------------------------------------------------------
plot_decision_curve(fit1, curve.names = "fit1",
                    cost.benefit.axis = F, # 是否需要损失：获益比 轴
                    confidence.intervals = "none" # 不画可信区间 TRUE显示
)
summary(fit1)

##### 34.1.1.3多条DCA曲线一起绘制-----------------------------------------------
# 新建立1个模型
set.seed(123)
fit2 <- decision_curve(Cancer~Age + Female + Smokes + Marker1 + Marker2,
                       data = train_df, 
                       bootstraps = 50
)

# 画图只要把多个模型放在1个列表中即可，还可以进行很多自定义调整
plot_decision_curve(list(fit1, fit2),
                    curve.names = c("fit1", "fit2"), 
                    xlim = c(0, 1), # 可以设置x轴范围
                    legend.position = "topright", # 图例位置,
                    col = c("red","blue"), # 自定义颜色
                    confidence.intervals = "none",
                    lty = c(1,2), # 线型，注意顺序
                    lwd = c(3,2,2,1) #注意顺序,先是自己的模型,然后All,然后None
)

##### 34.1.1.4绘制临床影响曲线--------------------------------------------------
# 1次只能绘制1个模型
plot_clinical_impact(fit1,
                     population.size= 1000,
                     cost.benefit.axis = T,
                     n.cost.benefits= 8,
                     col=c('red','blue'),
                     confidence.intervals= T,
                     ylim=c(0,1000),
                     legend.position="topright")

##### 34.1.2 测试集-------------------------------------------------------------

set.seed(123)

###### 34.1.2.1构建测试集DCA----------------------------------------------------
fit1 <- decision_curve(Cancer ~ Age + Female + Smokes, # 自变量和因变量
                       data = test_df, # 测试集数据
                       study.design = "cohort", # 选择研究类型
                       bootstraps = 50 # 重抽样次数
)

###### 34.1.2.2画测试集图-------------------------------------------------------
plot_decision_curve(fit1, curve.names = "fit1",
                    cost.benefit.axis = F, # 是否需要损失：获益比 轴
                    confidence.intervals = "none" # 不画可信区间
)
###### 34.1.2.3临床影响曲线-----------------------------------------------------
# 1次只能绘制1个模型
plot_clinical_impact(fit1,
                     population.size= 1000,
                     cost.benefit.axis = T,
                     n.cost.benefits= 8,
                     col=c('red','blue'),
                     confidence.intervals= T,
                     ylim=c(0,1000),
                     legend.position="topright")

# 34.2 方法2：dca.r(基石方法)---------------------------------------------------
source("C:\\Users\\Administrator\\Desktop\\R语言脚本(SWY精心编辑版本)\\临床预测模型\\datasets\\dca.r")
##### 34.2.1 训练集-------------------------------------------------------------
# 变为数据框类型
train_df <- as.data.frame(train_df)
###### 34.2.1.1单个预测变量时使用方法-------------------------------------------
dd <- dca(data = train_df, # 指定数据集,必须是data.frame类型
          outcome="Cancer", # 指定结果变量
          predictors="Smokes", # 指定预测变量
          probability = F, # Smokes这一列是0,1组成的二分类变量，不是概率，所以是F
          xstop = 0.3 # x轴范围
)
###### 34.2.1.2多个预测变量时使用方法-------------------------------------------
# 建立包含多个自变量的logistic模型
model <- glm(Cancer ~ Age + Female + Smokes, 
             family=binomial(),
             data = train_df
)

# 算出概率
train_df$prob <- predict(model, type="response")
# 绘制多个预测变量的DCA
dd <- dca(data=train_df, outcome="Cancer", predictors="prob", 
          probability = T
)

###### 34.2.1.3多个模型画在一起-------------------------------------------------
dd <- dca(data = train_df, outcome="Cancer", 
          predictors=c("prob","Smokes","Marker2"), # 这是3个模型！
          probability = c(T,F,F) # 和上面是对应的！
)

##### 34.2.2 测试集-------------------------------------------------------------
###### 34.2.2.1单个预测变量时使用方法-------------------------------------------
# 变为数据框类型
test_df <- as.data.frame(test_df)

dd <- dca(data = test_df, # 指定数据集,必须是data.frame类型
          outcome="Cancer", # 指定结果变量
          predictors="Smokes", # 指定预测变量
          probability = F, # Smokes这一列是0,1组成的二分类变量，不是概率，所以是F
          xstop = 0.3 # x轴范围
)
###### 34.2.2.2多个预测变量时使用方法-------------------------------------------
# 在训练集建立包含多个自变量的logistic模型
model <- glm(Cancer ~ Age + Female + Smokes, 
             family=binomial(),
             data = train_df
)

# 算出测试集的概率
test_df$prob <- predict(model, type="response", newdata = test_df)

# 绘制多个预测变量的DCA
dd <- dca(data=test_df, outcome="Cancer", predictors="prob", 
          probability = T)

# 34.3 方法3：DIY(支持ggplot)---------------------------------------------------
##### 34.3.1 训练集-------------------------------------------------------------
###### 34.3.1.1返回模型1的画图数据----------------------------------------------
source("C:\\Users\\Administrator\\Desktop\\R语言脚本(SWY精心编辑版本)\\临床预测模型\\datasets\\dca.r")
###### 34.3.1.2建立包含多个自变量的logistic模型---------------------------------
model <- glm(Cancer ~ Age + Female + Smokes, 
             family=binomial(),
             data = train_df
)
###### 34.3.1.3算出模型概率-----------------------------------------------------
train_df$prob <- predict(model, type="response")
# 绘制多个预测变量的DCA,返回画图数据
dca_data1 <- dca(data=train_df, outcome="Cancer", predictors="prob", 
                 probability = T, graph = F)

###### 34.3.1.4转换数据---------------------------------------------------------
library(tidyr)
dca_df1 <- dca_data1$net.benefit %>% # 画图数据就藏在这里！
  # 变成长数据,还不懂长宽数据转换这个超强操作的快去翻一下历史文章！
  pivot_longer(cols = -threshold, names_to = "type", values_to = "net_benefit") 

###### 34.3.1.5画图-------------------------------------------------------------
library(ggplot2)
library(ggsci)

# 以prob这个模型为例

ggplot(dca_df1, aes(threshold, net_benefit, color = type))+
  geom_line(linewidth = 1.2)+
  scale_color_jama(name = "Model Type")+ # c("steelblue","firebrick","green4")
  scale_y_continuous(limits = c(-0.03,0.12),name = "Net Benefit")+
  #限定y轴范围是重点，你可以去掉这句看看
  scale_x_continuous(limits = c(0,1),name = "Threshold Probility")+
  theme_bw(base_size = 16)+
  theme(legend.position.inside = c(0.8,0.8),
        legend.background = element_blank()
  )
###### 34.3.1.62个模型画在一起--------------------------------------------------
###### 34.3.1.7构建模型2--------------------------------------------------------
mod2 <- glm(Cancer ~ Marker1 + Age + Smokes, train_df, family = binomial)
train_df$model2 <- predict(mod2, type="response")

###### 34.3.1.8返回两个模型的画图数据-------------------------------------------
dca12 <- dca(data = train_df, 
             outcome="Cancer", 
             predictors=c("prob","model2") ,
             probability = c(T,T),
             graph = F
)
###### 34.3.1.9合并数据---------------------------------------------------------
library(dplyr)

dca_df_all <- dca12$net.benefit %>% 
  pivot_longer(cols = -threshold,names_to = "models",values_to = "net_benefit")
glimpse(dca_df_all)
str(dca_df_all)

###### 34.3.1.10画图------------------------------------------------------------
ggplot(dca_df_all, aes(threshold, net_benefit, color = models))+
  #geom_line(size = 1.2)+#和下一句任选其一
  stat_smooth(method = "loess", se = FALSE, formula = "y ~ x", span = 0.2)+ 
  # 灵感来自于方法5！
  scale_color_jama(name = "Model Type")+
  scale_y_continuous(limits = c(-0.03,0.12),name = "Net Benefit")+
  scale_x_continuous(limits = c(0,1),name = "Threshold Probility")+
  theme_bw(base_size = 16)+
  theme(legend.position.inside = c(0.8,0.75),
        legend.background = element_blank()
  )
##### 34.3.2 测试集-------------------------------------------------------------

###### 34.3.2.1获取测试集的预测概率---------------------------------------------
# 建立包含多个自变量的logistic模型
model <- glm(Cancer ~ Age + Female + Smokes, 
             family=binomial(),
             data = train_df
)

# 算出测试集的概率
test_df$prob <- predict(model, type="response", newdata = test_df)

# 绘制多个预测变量的DCA
dca_data1 <- dca(data=test_df, outcome="Cancer", predictors="prob", 
                 probability = T, graph = F)
###### 34.3.2.2提取数据，数据转换-----------------------------------------------
# 转换数据
library(tidyr)

dca_df1 <- dca_data1$net.benefit %>% # 画图数据就藏在这里！
  # 变成长数据,还不懂长宽数据转换这个超强操作的快去翻一下历史文章！
  pivot_longer(cols = -threshold, names_to = "type", values_to = "net_benefit") 

# 看下数据结构
str(dca_df1)

###### 34.3.2.3画图-------------------------------------------------------------
library(ggplot2)
library(ggsci)

# 以prob这个模型为例

ggplot(dca_df1, aes(threshold, net_benefit, color = type))+
  geom_line(linewidth = 1.2)+
  scale_color_jama(name = "Model Type")+ # c("steelblue","firebrick","green4")
  scale_y_continuous(limits = c(-0.03,0.12),name = "Net Benefit")+
  #限定y轴范围是重点，你可以去掉这句看看
  scale_x_continuous(limits = c(0,1),name = "Threshold Probility")+
  theme_bw(base_size = 16)+
  theme(legend.position.inside = c(0.8,0.8),
        legend.background = element_blank()
  )


