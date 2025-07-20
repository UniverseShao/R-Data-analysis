#基本概念-----------------------------------------------------------------------
"模型的校准度除了使用校准曲线进行衡量外，
还可以用统计检验来评估真实概率和预测概率的差异,
比如Hosmer-Lemeshow检验（H-L检验）,
若得到的P值小于0.05，那说明模型的预测概率和真实概率之间确实有差异，
若P值大于0.05，说明通过了H-L检验，预测概率和实际概率没有明显的差异"

# 30.1 准备数据-----------------------------------------------------------------
rm(list = ls())
lowbirth <- read.csv("C:\\Users\\Administrator\\Desktop\\R语言脚本(SWY精心编辑版本)\\临床预测模型\\datasets\\lowbirth.csv")
dim(lowbirth) # 565行，10列
str(lowbirth) 
# 看下结果变量的比例
table(lowbirth$dead)

##### 30.1.1把人数太少的变成other----------------------------------------------
library(dplyr)
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
table(lowbirth$race)
##### 30.1.2分类变量因子化------------------------------------------------------
lowbirth <- lowbirth %>% 
  mutate(across(where(is.character),as.factor)
         #dead = factor(dead, levels=c(1,0),labels=c("live","death"))
  )
str(lowbirth)

# 30.2 方法1：ResourceSelection-------------------------------------------------
model_glm <- glm(dead ~ birth + lowph + pltct + bwt + vent + race,
                 data = lowbirth, family = binomial)

summary(model_glm)

##### 30.2.1使用模型真实值和预测概率H-L检验-------------------------------------
# 加载R包
library(ResourceSelection)
# hosmer-lemeshow 检验
hoslem_test <- hoslem.test(model_glm$y, fitted(model_glm), g=10) # g=10表示分成10组
hoslem_test
##### 30.2.2提取H-L检验P值------------------------------------------------------
p.hoslem <- hoslem_test$p.value
p.hoslem

##### 30.2.3把H-L检验的结果放在校准曲线中---------------------------------------
library(rms)

# 必须先打包数据
dd <- datadist(lowbirth)
options(datadist="dd")

# 构建 calibration
fit <- lrm(dead ~ birth + lowph + pltct + bwt + vent + race, 
           data = lowbirth,x = TRUE, y = TRUE)
cal <- calibrate(fit, method='boot', B=500)

plot(cal,
     xlim = c(0,1),
     ylim = c(0,1),
     xlab = "Prediced Probability",
     ylab = "Observed Probability",
     cex.lab=1.2, cex.axis=1, cex.main=1.2, cex.sub=0.8,
     #subtitles = FALSE,
     legend = FALSE
) 
## 
## n=565   Mean absolute error=0.012   Mean squared error=0.00028
## 0.9 Quantile of absolute error=0.032
lines(cal[,c("predy","calibrated.corrected")], 
      type = 'l', #连线的类型，可以是"p","b","o"
      lwd = 3, #连线的粗细
      pch = 16, #点的形状，可以是0-20
      col = "#2166AC") #连线的颜色
lines(cal[,c("predy","calibrated.orig")],type="l",pch=16,lwd=3,col="tomato")
abline(0,1,
       lty = 2, #对角线为虚线
       lwd = 2, #对角线的粗细
       col = "#224444")#对角线的颜色
legend(0.6,0.2,
       c("Ideal","Bias-corrected","Apparent"), 
       lty = c(2,1,1), 
       lwd = c(2,3,3), 
       col = c("black","#2166AC","tomato"), 
       bty = "n"
)
text(0,0.9,bquote("Hosmer-Lemeshow "~italic(P)~" = "~.(round(p.hoslem,3))),
     adj = 0)


# 30.3 方法2：generalhoslem-----------------------------------------------------
library(generalhoslem)

logitgof(model_glm$y, fitted(model_glm), g=10, ord = F)

# 30.4 方法3：performance-------------------------------------------------------
library(performance)

performance_hosmer(model_glm, n_bins = 10)

# 30.5 方法4：PredictABEL-------------------------------------------------------
library(PredictABEL)
# 首先获得预测概率,和fitted(model_glm)完全一样
predRisk <- PredictABEL::predRisk(model_glm)
rr <- plotCalibration(data=lowbirth, cOutcome=10, #结果变量在数据的第几列
                      predRisk=predRisk, 
                      groups=10)
#或者
rr <- plotCalibration(data=lowbirth, cOutcome=10, #结果变量在数据的第几列
                      predRisk=model_glm$fitted.values, 
                      groups=10)
#或者
rr <- plotCalibration(data=lowbirth, cOutcome=10, #结果变量在数据的第几列
                      predRisk=fitted(model_glm), 
                      groups=10)
# 提取H-L检验的P值：
rr$p_value
# 也是和其他方法不一样，建议别用这个结果

# 30.6 方法5：glmtoolbox--------------------------------------------------------

library(glmtoolbox)

hltest(model_glm,verbose = F)$p.value
