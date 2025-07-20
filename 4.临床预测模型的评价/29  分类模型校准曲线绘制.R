#校准曲线基本概念---------------------------------------------------------------
"校准曲线（calibration curve）
校准度方面目前最推荐的还是校准曲线（calibration curve）
可用于评价模型预测概率和实际概率一致性"
"拟合优度检验（Hosmer-Lemeshow）
可以用来比较预测概率和实际概率是否有显著性差异
但是这个检验也只是能说明两者有没有统计学意义"

# 29.1准备数据-----------------------------------------------------------------
rm(list = ls())
lowbirth <- read.csv("C:\\Users\\Administrator\\Desktop\\R语言脚本(SWY精心编辑版本)\\临床预测模型\\datasets\\lowbirth.csv")
dim(lowbirth) # 565行，10列
str(lowbirth) 
table(lowbirth$dead)
# 其中两个人种人数太少了
table(lowbirth$race)
library(dplyr)
##### 29.1.1把人数太少的变成other----------------------------------------------
lowbirth <- lowbirth %>%
  mutate(race = ifelse(race %in% c("oriental", "native American"), "other", race))
#lowbirth[lowbirth == "oriental"] <- "other"
#lowbirth[lowbirth == "native American"] <- "other"
#或者
#lowbirth <- lowbirth %>%
#  mutate(race = case_when(
#    race == "oriental" ~ "other",
#    race == "native American" ~ "other",
#    TRUE ~ race  # 保持其他值不变
#  ))

##### 29.1.2分类变量因子化------------------------------------------------------
lowbirth <- lowbirth %>% 
  mutate(across(where(is.character),as.factor)
         #dead = factor(dead, levels=c(1,0),labels=c("live","death"))
  )
str(lowbirth)

##### 29.1.3数据划分为训练集、测试集--------------------------------------------
set.seed(123)
ind <- sample(1:nrow(lowbirth),nrow(lowbirth)*0.7)
"1:nrow(lowbirth)：生成从1到数据集总行数的序列（所有行的索引）。
nrow(lowbirth)*0.7：计算训练集的样本量（总行数的70%）。
sample()：从所有行索引中 无放回地随机抽取 70% 的索引，存入 ind。"
train_df <- lowbirth[ind, ]  # 训练集：抽取的70%样本
test_df <- lowbirth[-ind, ]  # 测试集：剩余的30%样本（排除训练集索引）
dim(train_df)
dim(test_df)
# 绘制校准曲线------------------------------------------------------------------
# 29.2方法1：rms---------------------------------------------------------------
library(rms)
# 必须先打包数据
dd <- datadist(train_df)
options(datadist="dd")
# 建立模型
fit2 <- lrm(dead ~ birth + lowph + pltct + bwt + vent + race,
            data = train_df, x=T,y=T)
#x=T, y=T 是 rms 包中 lrm() 函数的参数，用于保存原始数据供后续分析使用
# 进行内部验证
cal2 <- calibrate(fit2, method='boot', B=500)
##### 29.2.1 训练集校准曲线-----------------------------------------------------
plot(cal2)
"Apparent为基于该模型的计算一次的风险概率与实际概率一致性
Bias-corrected指对构建模型的数据进行自举重抽样后的结果"
###### 29.2.1.1增加细节---------------------------------------------------------
plot(cal2,
     xlim = c(0,1),
     ylim = c(0,1),
     xlab = "Prediced Probability",
     ylab = "Observed Probability",
     cex.lab=1.2, cex.axis=1, cex.main=1.2, cex.sub=0.8,
     #subtitles = FALSE,
     legend = FALSE
) 
lines(cal2[,c("predy","calibrated.corrected")], 
      type = 'l', #连线的类型，可以是"p","b","o"
      lwd = 3, #连线的粗细
      pch = 16, #点的形状，可以是0-20
      col = "#2166AC") #连线的颜色
lines(cal2[,c("predy","calibrated.orig")],type="l",pch=16,lwd=3,col="tomato")
"redy：模型预测的概率（X轴）。
calibrated.corrected：Bootstrap校正后的实际观测概率（Y轴）
calibrated.orig：原始拟合的实际观测概率（可能存在过拟合乐观偏差）"
abline(0,1,#指定直线的截距和斜率
       lty = 2, #对角线为虚线
       lwd = 2, #对角线的粗细
       col = "#224444") #对角线的颜色
#abline() 函数用于 在现有图形上添加一条参考直线
legend(0.6,0.2,
       c("Ideal","Bias-corrected","Apparent"), 
       lty = c(2,1,1), 
       lwd = c(2,3,3), 
       col = c("black","#2166AC","tomato"), 
       bty = "n")
##### 29.2.2测试集校准曲线------------------------------------------------------
# 首先获取测试集的预测结果
phat <- predict(fit2, test_df, type = 'fitted')
#ype = 'fitted'：指定输出拟合值（即预测概率）。若使用 glm 模型，
 #可能需要 type = "response"，但 rms::lrm 模型支持 'fitted'
# 直接使用val.prob即可实现，statloc=F可不显示各种指标
# 赋值给aa是为了减少输出信息
aa <- val.prob(phat, test_df$dead,cex = 1)
#phat：预测概率向量
#test_df$dead：测试集中的真实二元结果（1=死亡，0=存活）
#cex = 1：图形参数，控制文本和点的大小（1为默认大小）

###### 29.2.2.1和以上测试集同种训练集校准曲线-----------------------------------
# 获取训练集的预测结果
phat_train <- predict(fit2, train_df, type = 'fitted')

# 直接使用val.prob即可实现
aa <- val.prob(phat_train, train_df$dead,cex = 1)

"Dxy：预测概率与实际概率的相关性，Dxy=2C-1
C：ROC曲线下面积。
R2：复决定系数，越大越好，最大是1。
D：discrimination index，区分度指数，越大越好。
U：unreliability index，不可靠指数，越小越好。
Q：quality index，质量指数，越大越好。
Brier：布里尔分数，预测概率与实际概率的均方误差，brier越小，校准效果越好。
Intercept：截距，为0的时候最好。
Slope：斜率，为1的时候最好。
Emax：预测概率和实际概率的最大绝对差。
E90：预测概率和实际概率差值的90%分位数
Eavg：预测概率和实际概率的平均差值。
S:z：Z检验的z值
S:p：Z检验的p值。p=0.842说明拟合效果好，P>0.05说明拟合线和参考线无统计学差异，吻合度高。"

# 29.3 方法2：riskRegression----------------------------------------------------
##### 建模----------------------------------------------------------------------
fit2 <- glm(dead ~ birth + lowph + pltct + bwt + vent + race,
            data = train_df, family = binomial)
##### 29.3.1训练集校准曲线------------------------------------------------------
library(riskRegression)
fit22 <- Score(
  list("fit" = fit2),       # 待评估的模型列表（可对比多个模型）
  formula = dead ~ 1,       # 指定结局变量（此处~1表示无分层变量）
  data = train_df,          # 数据集
  metrics = c("auc", "brier"), # 评估指标：AUC和Brier Score
  plots = "calibration",    # 绘制校准曲线
  null.model = TRUE,        # 计算零模型（仅截距项）的基准性能
  conf.int = TRUE,          # 计算置信区间
  B = 500,                  # Bootstrap重抽样次数
  M = 50                    # 校准曲线分组数（每组50人）
)
# metrics "auc"：曲线下面积（区分度，0.5=随机，1=完美）
#         "brier"：Brier分数（校准度，0=完美，0.25=最差）       
# plots   生成图形，"calibration" 表示绘制校准曲线（实际概率 vs 预测概率）
# B = 500	Bootstrap重抽样次数，次数越多置信区间越稳定，但计算时间越长
# M = 50	校准曲线的分组人数（将预测概率分箱，每组约50人计算实际概率）
# 画图
plotCalibration(fit22,col="tomato",
                method = "quantile", # 默认是nne,quantile是分组计算的传统方法
                #"quantile"：按预测风险分位数分组（传统方法，均等分组）（分位数分组法）
                # nne**：邻近法，适用于小样本、非均匀分布或需要更精细校准分析
                # 改用 nne校准曲线可能更平滑，但整体趋势应与 quantile 类似
                xlab = "Predicted Risk",
                ylab = "Observerd RISK",
                brier.in.legend = T, # 显示brier分数(百分数显示)
                auc.in.legend = T,  # 显示auc值(百分数显示)
                bars = F) # 不显示预测风险的频率分布图
###### 29.3.1.1AUC和Brier范围---------------------------------------------------
#AUC范围	模型评价	应用建议
# 0.5-0.6	无用	拒绝使用
# 0.6-0.7	较弱	需改进
# 0.7-0.8	可用	辅助决策
# 0.8-0.9	优秀	临床采纳
# >0.9	极强	金标准

# Brier分数的定义
# 预测概率与实际观测结果之间的均方误差
#值范围	解释
# 0	完美预测（所有预测概率与实际结果完全一致）。
# 0.01-0.1	优秀预测（模型概率接近真实事件发生率）。
# 0.1-0.25	可接受预测（存在一定误差，但仍有信息价值）。
# 0.25	最差预测（对于二分类事件，相当于随机猜测概率为0.5时的分数）。
# >0.25	比随机猜测更差（模型预测方向错误，如高估风险时实际事件未发生）

###### 29.3.1.2ggplot2来画训练集校准曲线----------------------------------------
plotdata <- plotCalibration(fit22,plot = F,method = "quantile"
                            #bandwidth = 0.1
)#plot = F表示不绘图，而是保留数据

library(ggplot2)

ggplot(plotdata$plotFrames$fit, aes(x=Pred,y=Obs))+
  geom_line(color="tomato",linewidth=1.5)+
  scale_x_continuous(limits = c(0,1),name = "Predicted Risk")+
  scale_y_continuous(limits = c(0,1),name = "Observerd Risk")+
  geom_abline(slope = 1,intercept = 0,lty=2)+
  geom_rug(color="grey")+
  theme_bw()

###### 29.3.1.3预测风险的频率分布图---------------------------------------------
plotCalibration(fit22,
                method = "quantile", # 默认是nne,quantile是分组计算的传统方法
                bars = T, # 这里选择TRUE即可
                q = 10 # 把风险分为10组
)
###### 29.3.1.4ggplot2来画预测风险的频率分布图----------------------------------
plotdata <- plotCalibration(fit22,plot = F,method = "quantile",
                            bars = T, 
                            q = 10 )
plotdata <- plotdata$plotFrames$fit
plotdata$risk <- rownames(plotdata)

library(tidyr)
plotdata <- plotdata %>% 
  pivot_longer(cols = 1:2,names_to = "type",values_to = "values")

library(ggplot2)

ggplot(plotdata, aes(x=risk,y=values))+
  geom_bar(position = "dodge",aes(fill=type),stat = "identity")+
  theme(axis.text.x = element_text(hjust = 1,angle = 30))

##### 29.3.2 测试集-------------------------------------------------------------
fit22 <- Score(list("fit"=fit2),
               formula = dead ~ 1,
               data = test_df, # 这里写测试集即可
               metrics = c("auc","brier"),
               #summary = c("risks","IPA","riskQuantile","ibs"),
               plots = "calibration",
               null.model = T,
               conf.int = T,
               B = 500,
               M = 50
)

# 画图
plotCalibration(fit22,col="tomato",
                method = "quantile", # 默认是nne,quantile是分组计算的传统方法
                xlab = "Predicted Risk",
                ylab = "Observerd RISK",
                brier.in.legend = F, # 不显示brier分数
                auc.in.legend = F,  # 不显示auc值
                bars = F)

# 29.4 方法3：tidymodels--------------------------------------------------------
library(tidymodels) # 注意版本，我的是1.1.1

# 先把结果变量变成因子型
lowbirth$dead <- factor(lowbirth$dead,levels=c(1,0),labels=c("death","live"))

# 划分数据
set.seed(123)
split <- initial_split(lowbirth, strata = "dead", prop = 0.7)
train <- training(split)
test <- testing(split)

# 重抽样方法选择，也就是内部验证方法
set.seed(123)
rs <- bootstraps(train, times = 500)

# 选择模型，选择预处理，建立工作流
rf_spec <- rand_forest(mode = "classification",engine = "ranger",trees = 500)
rf_rec <- recipe(dead ~ birth + lowph + pltct + bwt + vent + race,data = train)
rf_wf <- workflow(preprocessor = rf_rec, spec = rf_spec)

# 开始建模
rf_res <- fit_resamples(rf_wf,
                        resamples = rs,
                        metrics = metric_set(roc_auc, sens, spec, mcc, f_meas, 
                                             j_index, brier_class),
                        control = control_resamples(save_pred = T))


collect_metrics(rf_res)

# 计算置信区间
collect_metrics(rf_res) %>% 
  dplyr::select(1,3,5) %>% 
  mutate(lower = mean - 1.96*std_err,
         upper = mean + 1.96*std_err)

##### 29.4.1 训练集-------------------------------------------------------------
library(probably)
library(ggplot2)

# 3种方法
cal_plot_breaks(rf_res,num_breaks = 9)

cal_plot_logistic(rf_res)

cal_plot_windowed(rf_res)

##### 29.4.2 测试集-------------------------------------------------------------
pred_test <- last_fit(rf_wf, split = split)

# 查看模型在测试集的表现
collect_metrics(pred_test)

test %>% dplyr::select(dead) %>% 
  bind_cols(collect_predictions(pred_test) %>% dplyr::select(.pred_death)) %>% 
  cal_plot_breaks(dead, .pred_death,conf_level = 0.95) # 其他方法略


