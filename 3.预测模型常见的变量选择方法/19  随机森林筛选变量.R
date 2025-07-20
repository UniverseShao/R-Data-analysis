rm(list = ls())
# 19.1 准备数据-----------------------------------------------------------------
load(file = "C:\\Users\\Administrator\\Desktop\\R语言脚本(SWY精心编辑版本)\\临床预测模型\\datasets\\lnc_expr_clin.RData")
#去掉没有生存信息的样本
lnc_expr_clin <- lnc_expr_clin[!is.na(lnc_expr_clin$time_months),]
lnc_expr_clin <- lnc_expr_clin[lnc_expr_clin$time_months>0,]

#选择其中一部分数据
dat.cox <- lnc_expr_clin[,c(72,1:59)]

#把变量命中的“-”去掉
colnames(dat.cox)<- gsub("-","",colnames(dat.cox))

#结果变量变为因子型
dat.cox$event <- factor(dat.cox$event)
dim(dat.cox)
dat.cox[1:4,1:6]

# 19.2 建立模型-----------------------------------------------------------------
library(randomForest)

set.seed(124)
fit <- randomForest(event~., data = dat.cox)

fit
# 19.3 结果探索-----------------------------------------------------------------
plot(fit)

table(dat.cox$event)

which.min(fit$err.rate[,1])

importance(fit)

#可视化变量重要性：

varImpPlot(fit)

# 19.4 交叉验证变量筛选---------------------------------------------------------
set.seed(647)
res <- rfcv(trainx = dat.cox[,-1],trainy = dat.cox[,1],
            cv.fold = 5,
            recursive = T
)
res$n.var #变量个数
res$error.cv #错误率
with(res, plot(n.var, error.cv, type="o", lwd=2))

# 19.5 Boruta筛选变量-----------------------------------------------------------

library(Boruta)

set.seed(23)

fs <- Boruta(event ~ ., data = dat.cox, doTrace=1)
table(fs$finalDecision)
getSelectedAttributes(fs)

