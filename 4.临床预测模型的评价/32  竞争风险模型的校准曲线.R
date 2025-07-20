# 32.1 安装---------------------------------------------------------------------
#install.packages("QHScrnomo")
# 32.2 准备数据-----------------------------------------------------------------
rm(list = ls())
library(QHScrnomo)

data("bmtcrr",package = "casebase")
bmtcrr[,c(1,2,3,6)] <- lapply(bmtcrr[,c(1,2,3,6)],as.factor)
str(bmtcrr)
# 32.3 拟合竞争风险模型---------------------------------------------------------
dd <- datadist(bmtcrr)
options(datadist = "dd")
##### 32.3.1拟合cox回归模型-----------------------------------------------------
# 对Age这个变量进行样条变换
fit <- cph(Surv(ftime,Status == 1) ~ Sex + rcs(Age,3)+D+Phase, data = bmtcrr,
           x = TRUE, y= TRUE, surv=TRUE, time.inc = 24)
##### 32.3.2使用crr.fit变为竞争风险模型-----------------------------------------
crr <- crr.fit(fit = fit, cencode = 0, failcode = 1)
class(crr)
summary(crr)
anova(crr)

# 32.4 内部验证-----------------------------------------------------------------
# 默认10折交叉验证
set.seed(123)
#可以计算线性预测值，可查看帮助文档
bmtcrr$preds.tenf <- tenf.crr(crr, time = 36, trace = FALSE)
str(bmtcrr$preds.tenf)

# 32.5 计算C-index--------------------------------------------------------------
cindex(prob = bmtcrr$preds.tenf,
       fstatus = bmtcrr$Status,
       ftime = bmtcrr$ftime,
       type = "crr",
       failcode = 1, cencode = 0
)

# 32.6 校准曲线-----------------------------------------------------------------
groupci(x = bmtcrr$preds.tenf,
        ftime = bmtcrr$ftime,
        fstatus = bmtcrr$Status,
        g = 5, # 分成几组
        u = 36, # 时间点
        failcode = 1,
        xlab = "Predicted 3-year mortality",
        ylab = "Actual 3-year mortality"
)

# 32.7 列线图-------------------------------------------------------------------
nomogram.crr(
  fit = crr,
  failtime = 36,
  lp = T,
  xfrac = 0.65,
  fun.at = seq(0.2, 0.45, 0.05),
  funlabel = "Predicted 3-year risk"
)

# 32.8 生成模型方程-------------------------------------------------------------

sas.cmprsk(crr,time = 36)


# 32.9 外部验证（测试集）-------------------------------------------------------

test_df <- head(bmtcrr,50)#取前50个作为测试集
prob <- predict(crr, time = 36, newdata = test_df)
head(prob)


cindex(prob = prob,
       fstatus = test_df$Status,
       ftime = test_df$ftime
)
groupci(x = prob,
        ftime = test_df$ftime,
        fstatus = test_df$Status,
        u = 36,
        g = 5
)

