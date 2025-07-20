#演示数据####
data(lindner, package = "twang")
lindner[,c(3,4,6,7,8,10)] <- lapply(lindner[,c(3,4,6,7,8,10)],factor)
str(lindner)

library(cobalt)
# 选择只有协变量的数据框
covariates <- subset(lindner, select = c(1,4:10))

bal.tab(covariates,treat = lindner$abcix, s.d.denom = "pooled",
        m.threshold = 0.1, un = TRUE,
        v.threshold = 2
)
#IPTW####

psfit <- glm(abcix ~ stent + height + female + diabetic + acutemi + ejecfrac + ves1proc,
             data = lindner, family = binomial())
ps <- psfit$fitted.values

#干预组：1/ps####
#对照组：1/(1-ps)####
iptw <- ifelse(lindner$abcix == 1, 1/ps, 1/(1-ps))
lindner$iptw <- iptw
bal.tab(covariates,treat = lindner$abcix, s.d.denom = "pooled",
        weights = lindner$iptw,
        m.threshold = 0.1, un = TRUE,
        v.threshold = 2
)
#Diff.Un未加权的SMD
#Diff.Adj加权后的SMD

#为什么逆概率加权（IPTW）能使基线数据平衡？####
#核心思想
#通过数学调整模拟随机对照试验（RCT），而不是像倾向得分匹配（PSM）那样直接筛选样本
#(1) 权重的作用
#未加权数据的问题
#观察性研究中，处理组（如接受治疗）和对照组（未接受治疗）的基线特征（如年龄、性别）可能不平衡，直接比较会导致混杂偏倚
#逆概率权重的定义
#干预组：1/ps 对照组：1/(1-ps)
#效果
#放大罕见特征个体的贡献，缩小常见特征个体的贡献

#低概率事件（如倾向得分PS接近0或1的个体）会被赋予 更高的权重
#因为它们在原始数据中代表性不足

#高概率事件（如PS接近0.5的个体）会被赋予 更低的权重
#因为它们在原始数据中已充分代表

#类比解释####
#原始数据
#左侧（处理组）有10个苹果（老年人）和2个橘子（年轻人），右侧（对照组）有2个苹果和10个橘子
#两侧重量不等（基线不平衡）
#加权后
#左侧的每个苹果权重=0.5，橘子权重=2；右侧的每个苹果权重=2，橘子权重=0.5
#重新计算总重量后，两侧的“苹果+橘子”总贡献相同（基线平衡）

#回归模型（如逻辑回归、Cox回归）中加入权重参数 weights = iptw，是为了 确保分析基于加权后的平衡数据。其原理是
#加权回归####
#回归模型会通过权重调整每条观测的贡献，使得高权重个体（即原始数据中代表性不足的个体）对结果的影响更大

#IPTW 的局限性####
#对倾向得分模型敏感：
#如果 PS 模型误设（如遗漏非线性项或交互项），加权后的平衡性可能仍不理想

#IPTW后逻辑回归（可能常规会这样认为）####
lindner$sixMonthSurvive_int <- as.integer(lindner$sixMonthSurvive)
f <- glm(sixMonthSurvive_int~abcix+stent+height+female+diabetic+acutemi+ejecfrac+ves1proc,
         data = lindner, family = binomial(),
         weights = iptw # 把权重加进去
)

summary(f)
#glm(weights = iptw)是错误的！####
#使用 survey 包进行分析####

library(survey)
# 创建调查设计对象，指定权重
design <- svydesign(ids = ~1, data = lindner, weights = ~iptw)
# 加权逻辑回归
lindner$sixMonthSurvive_int <- as.integer(lindner$sixMonthSurvive)
lindner$sixMonthSurvive_int <- factor(lindner$sixMonthSurvive_int)
glm_weighted <- svyglm(sixMonthSurvive_int ~ abcix+stent+height+female+diabetic+acutemi+ejecfrac+ves1proc, design = design, family = binomial())
summary(glm_weighted)

#重叠加权####
#目标人群是两组协变量相似的人
#即PS值分布重叠的人，其估计的效应为重叠人群平均处理效应（ATO）

## 数据准备
rm(list = ls())
data(lindner, package = "twang") 
# 构建估计PS的formula
formula.ps <- abcix ~ stent + height + female + diabetic + acutemi + ejecfrac + ves1proc
#进行重叠加权
library(PSweight)
PSweight <- PSweight(ps.formula = formula.ps, data = lindner, 
                     weight = "overlap", # 重叠加权
                     yname = "cardbill", # 因变量
                     family = "gaussian", 
                     ps.method = "glm", 
                     out.method = "glm"
)

#返回结果，效应估计及其标准误、置信区间、P值
summary(PSweight)
#计算数据均衡性
SumStat<-SumStat(ps.formula = formula.ps, data = lindner, weight = "overlap")
SumStat[["ess"]] #有效样本量
summary(SumStat) #均衡性检验







