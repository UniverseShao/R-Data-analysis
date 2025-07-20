# 1.如果不满足等比例风险假定该怎么办？------------------------------------------
#采用时依cox回归
# 2.时依协变量的种类------------------------------------------------------------
#(1)自定义的（difined）时依协变量
#(2)内部（internal）时依协变量
#(3)辅助的（ancillary）时依协变量
#(4)同时是内部和辅助的时依协变量


# 3.时间依存协变量的Cox回归和时间依存系数Cox回归--------------------------------
#如果不能满足PH假设，可以考虑使用时依协变量或者时依系数Cox回归
#时依协变量和时依系数是两个概念
#如果一个协变量本身会随着时间而改变，这种叫时依协变量
#协变量的系数随着时间改变，这种叫时依系数

# 4.时依协变量（time dependent covariate）的定义--------------------------------

#长期随访过程中本身会发生改变的变量
#如长期随访过程中的某段时间吸烟某段时间不吸烟
#或者体重，这个月减肥有效下个月懈怠了体重又长回来

# 5.时依系数（time dependent coefficient）--------------------------------------

#常见的等比例风险Cox回归模型前提是满足等比例风险假设（proportional hazard assumption）
#即自变量与结局之间的风险比是一定的，两组间的风险是成比例的

#往往有很多自变量与结局的关系（系数）会随时间变化，这些系数称为时依系数
#如有研究发现辅助放射治疗在大肠癌患者确诊后5.5年后与肿瘤转移风险增加有关，
#但5.5年内并不

# 6.含时依协变量的Cox回归和时依系数的Cox回归的区别------------------------------

#含时依协变量只是说自变量在随访过程中可能会发生改变，
#但是其对结局的效应值却不一定
#因此在满足等比例假设的前提下，含时依协变量的Cox回归依然属于等比例风险Cox回归

# 7.以survival包的veteran数据集为例---------------------------------------------

rm(list = ls())
library(survival)
str(veteran)
veteran$trt <- factor(veteran$trt)
View(veteran)
##### 7.1构建普通的Cox回归，进行等比例风险假设----------------------------------
fit <- coxph(Surv(time, status) ~ trt + prior + karno, data = veteran)
# 进行PH检验
zp <- cox.zph(fit)
zp
#图形化方法查看PH检验的结果
##### 7.2风险比系数时间图-------------------------------------------------------
op <- par(mfrow=c(1,3))
plot(zp)
par(op)
#图中的Beta(t)是协变量的系数估计值，它反映了协变量对事件发生风险的影响随时间的变化

#若图中的平滑曲线相对平稳且置信区间包含零，满足比例风险假设
#如果曲线有明显的趋势，且置信区间不包含零说明系数随时间变化显著，可能违反比例风险假设
###### 7.2.1ggcoxdiagnostics()绘制风险比系数时间图------------------------------
library(survminer)
ggcoxdiagnostics(fit, type = "schoenfeld")
#这个就是高级的plot(zp)表述
###### 7.2.2单独绘制karno的风险比系数时间图-------------------------------------
plot(zp[3])
abline(0,0, col="red") # 0水平线
abline(h=fit$coef[3], col="green", lwd=2, lty=2) 

# 8.对时间分层------------------------------------------------------------------

vet2 <- survSplit(Surv(time, status) ~ ., data= veteran, 
                  cut=c(90, 180), # 两个拐点把时间分为3层（3段）
                  episode= "tgroup", #新列名
                  id="id")
vet2[1:7, c("id", "tstart", "time", "status", "tgroup", "age", "karno")]

##### 8.1重新拟合cox模型--------------------------------------------------------
# 注意此时Surv()的用法！
fit2 <- coxph(Surv(tstart, time, status) ~ trt + prior + karno:strata(tgroup), data = vet2)
fit2
#注意这个分层是对时间进行分层不是对karno进行分层，所以会出现一个id不同时间有相同karno的情况
#再次进行PH检验
cox.zph(fit2)


# 9.连续性时依系数变换----------------------------------------------------------
fit3 <- coxph(Surv(time, status) ~ trt + prior + karno + tt(karno), # 对karno进行变换
              data = veteran, 
              tt = function(x, t, ...) x * log(t+20) # 具体变换方式
)
fit3
##### 9.1连续性时依系数变换结果的解读-------------------------------------------
"karno     -0.124662  0.882795  0.028785 -4.331 1.49e-05
tt(karno)  0.021310  1.021538  0.006607  3.225  0.00126
ph.karno 和tt(ph.karno) 的系数都具有统计显着性
ph.karno 的时变效应可以写为 β(t) = -0.125+0.021×log(t + 20)"


#构建时依协变量时，可以选择x * t、x * log(t)、x * log(t + 20)、x * log(t + 200)等等
##### 9.2变换后的PH检验---------------------------------------------------------
zp <- cox.zph(fit, transform = function(time) log(time + 20))

##### 9.3画风险比系数时间图-----------------------------------------------------
plot(zp[3])
abline(0,0, col="red") # 0水平线
abline(h=fit$coef[3], col="green", lwd=2, lty=2) # 整体估计
abline(coef(fit3)[3:4],lwd=2,lty=3,col="blue") # 现在的估计

###### 9.3.1将图纵坐标转换成HR--------------------------------------------------

# 变换后的PH检验
zp <- cox.zph(fit, transform = function(time) log(time + 20))

# 画图（隐藏默认y轴）
plot(zp[3], ylab = "HR", yaxt = "n")  # 修改标签并隐藏y轴

# 自定义指数变换的y轴刻度
original_ticks <- axTicks(2)  # 获取原始刻度值
axis(2, at = original_ticks, 
     labels = format(round(exp(original_ticks), 3), nsmall = 3))  # 指数变换刻度值

# 添加参考线（保持原位置不变）
abline(0, 0, col = "red")  # 0水平线（对应HR=1）
abline(h = fit$coef[3], col = "green", lwd = 2, lty = 2)  # 整体估计（beta值位置）
abline(coef(fit3)[3:4], lwd = 2, lty = 3, col = "blue")  # 当前估计（beta值位置）

# 10.timereg包附带的 timecox() 函数能够拟合具有时间固定和时变系数的 Cox 模型----
library(timereg)
fit.out <- timecox(Surv(time,status)~
                     age+sex+ph.karno,
                   data=lung,n.sim=500,
                   max.time=700)
summary(fit.out)

par(mfrow=c(2,2))
plot(fit.out)



