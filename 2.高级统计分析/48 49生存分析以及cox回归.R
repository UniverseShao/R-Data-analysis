# 生存分析的实现####------------------------------------------------------------
# 1.生存过程的描述####----------------------------------------------------------
library(survival)
library(survminer)
#数据集中1代表删失2代表死亡，先将1代表死亡，0代表删失
df <- lung
df$status <- ifelse(df$status == 2,1,0)
str(df)
##### 1.1把生存时间和生存状态用Surv()放到一起####-------------------------------
Surv(time = lung$time, event = lung$status)
#有+的就是截尾数据
# 1010+ 表示该患者在第1010天时仍未发生目标事件，数据被截尾
###### 1.1.1为什么需要 Surv() 函数####------------------------------------------
#统一格式：将时间和事件状态绑定为一个对象，供后续生存分析函数

# 2.描述生存数据####------------------------------------------------------------
##### 2.1构建生存曲线####-------------------------------------------------------
fit <- survfit(Surv(time, status) ~ 1, data = df)
#survfit()
#用于拟合生存曲线（默认使用 Kaplan-Meier 方法）
#输入：一个公式（formula）和一个数据框（data）
#~ 1
#这里的 1 代表 截距（Intercept），
#即 不分组，计算所有数据的整体生存曲线
#输出会显示 整体生存概率 随时间的变化




###### 2.1.1寿命表，surv_summary比默认的summary()更好####-----------------------
surv_summary(fit)


# 3.画出全体生存曲线####--------------------------------------------------------
ggsurvplot(fit,
           conf.int = TRUE, # 可信区间
           palette= 'blue', # 更改配色
           surv.median.line = "hv", # 中位生存时间
           ggtheme = theme_bw() # 更改主题
           
)

#surv.median.line = "hv"
#图形会标出生存概率降至 50% 时的时间点


# 4.生存过程的比较####----------------------------------------------------------
#检验不同组别之间的生存时间（生存曲线）有无差别

fit <- survdiff(Surv(time, status) ~ sex, data = df)
fit
#Expected (E)：如果两组生存曲线完全相同（零假设成立），该组预期的死亡数
##### 4.1不同组别之间生存曲线的检验K-M图示的方法####----------------------------
fit.logrank <- survfit(Surv(time, status) ~ sex, data = df)

surv_summary(fit.logrank) # 可以查看寿命表

##### 4.2ggsurvplot()进行可视化####---------------------------------------------

ggsurvplot(fit.logrank, 
           data = df,
           surv.median.line = "hv", # Add medians survival
           
           # Change legends: title & labels
           legend.title = "Sex",
           legend.labs = c("Male", "Female"),
           
           # Add p-value and tervals
           pval = TRUE, # 这里P值直接写数字也行
           conf.int = TRUE,
           
           # Add risk table显示每个时间点剩余患者的数量
           risk.table = TRUE, 
           tables.height = 0.2,
           tables.theme = theme_cleantable(),
           
           ncensor.plot = TRUE,#显示删失事件的数量（底部小图）
           
           # Color palettes. Use custom color: c("#E7B800", "#2E9FDF"),
           # or brewer color (e.g.: "Dark2"), or ggsci color (e.g.: "jco")
           palette = c("#E7B800", "#2E9FDF"),
           ggtheme = theme_bw(), # Change ggplot2 theme
           
           # Change font size, style and color
           main = "Survival curve",
           font.main = c(16, "bold", "darkblue"),
           font.x = c(14, "bold.italic", "red"),
           font.y = c(14, "bold.italic", "darkred"),
           font.tickslab = c(12, "plain", "darkgreen")
)
#自带的surv_cutpoint()可用于寻找最佳切点，但是只能用于连续性数据。
# 5.surv_cutpoint()可用于寻找最佳切点-------------------------------------------
rm(list = ls())
# 0. Load some data
data(myeloma)
head(myeloma)
#寻找最佳切点
# 1. Determine the optimal cutpoint of variables
res.cut <- surv_cutpoint(myeloma, time = "time", event = "event",
                         variables = c("DEPDC1", "WHSC1", "CRIM1") # 找这3个变量的最佳切点
)

summary(res.cut)
#查看根据最佳切点进行分组后的数据分布情况
# 2. Plot cutpoint for DEPDC1
plot(res.cut, "DEPDC1", palette = "npg")
#根据最佳切点重新划分数据，这样数据就根据最佳切点变成了高表达/低表达组
# 3. Categorize variables
res.cat <- surv_categorize(res.cut)
head(res.cat)
#根据最佳切点绘制生存曲线
# 4. Fit survival curves and visualize
library("survival")
fit <- survfit(Surv(time, event) ~DEPDC1, data = res.cat)
ggsurvplot(fit, data = res.cat, risk.table = TRUE, conf.int = TRUE)




# 6.Cox回归####-----------------------------------------------------------------
rm(list = ls())
library(survival)
library(survminer)
str(lung)

lung$sex <- factor(lung$sex, labels = c("female","male"))
lung$ph.ecog <- factor(lung$ph.ecog, labels = c("asymptomatic", "symptomatic",'in bed <50%','in bed >50%'))
str(lung)
##### 6.1数值型变量可以使用参数levels和labels来编码成因子####-------------------

##### 6.2拟合多因素Cox回归模型####----------------------------------------------
#sex/age/ph.karno3个变量做演示
fit.cox <- coxph(Surv(time, status) ~ sex + age + ph.karno, data = lung)
summary(fit.cox)
#查看cox回归最后结果的神函数broom::tidy
res <- broom::tidy(fit.cox)
res
###### 6.2.1cox回归分类变量注意事项####-----------------------------------------
#最后cox回归模型中对于分类变量得到的回归系数
#分类变量中的最后得到的HR都是和分类变量中第一组进行比较的结果

###### 6.2.2在sex分类变量中就是男性和女性比（女性是1男性是2）HR是0.61
#那么就是男性VS女性死亡风险降低39%

#在age连续变量中 HR是1.012 含义就是年龄每增加1岁，死亡风险
#增加1.2%

#注意在cox模型中对于分类变量都要就行factor处理


# 7.Cox回归的等比例风险假设检验####---------------------------------------------

ftest <- cox.zph(fit.cox)
ftest
#结果要P＞0.05才可以说明满足等比例风险假设检验

# 8.cox回归的回归系数的森林图展示####-------------------------------------------

#为了森林图好看点，多选几个变量
fit.cox <- coxph(Surv(time, status) ~ . , data = lung)
summary(fit.cox)

##### 8.1画回归系数森林图####---------------------------------------------------
ggforest(fit.cox, data = lung,
         main = "Hazard ratio",
         cpositions = c(0.01, 0.15, 0.35), # 更改前三列的相对位置
         fontsize = 0.7,#更改字体大小
         refLabel = "reference",#参考组的标签
         noDigits = 2#保留2位小数
)

# 9.如何判断cox模型是否可以适用样条回归-----------------------------------------

##### (1) 方法一：anova()检验非线性项-------------------------------------------
anova(fit)

#Wald Statistics          Response: Surv(time, status) 

#Factor     Chi-Square d.f. P     
#age        28.75      4    <.0001
#Nonlinear  7.92      3    0.048  # <- 关注这一行
#sex        12.34      1    0.0004

#Nonlinear对应的P值（此处0.048 < 0.05）
#拒绝线性假设，支持使用样条

##### (2) 方法二：似然比检验（比较线性与样条模型）------------------------------

fit_linear <- cph(Surv(time, death) ~ age + sex, data = data)
fit_spline <- cph(Surv(time, death) ~ rcs(age, 4) + sex, data = data)
lrtest(fit_linear, fit_spline)  # 若P<0.05，选择样条模型

##### (3) 方法三：图形诊断------------------------------------------------------
# 绘制HR随age的变化
ggplot(Predict(fit_spline, age, fun = exp, ref.zero = TRUE)) +
  geom_hline(yintercept = 1, linetype = 2)
#若曲线明显偏离水平线（HR=1），提示非线性

##### (4)检验比例风险假设-------------------------------------------------------
library(survival)
cox.zph(fit_linear)
#结果要P＞0.05才可以说明满足等比例风险假设检验
#若违反，尝试样条
fit_spline <- cph(Surv(time, death) ~ rcs(age, 4) + sex, data = data)
cox.zph(fit_spline)
#P＞0.05说明cox模型的样条回归符合等比例风险假设检验
#采用样条回归
"常见误区澄清
误区：“PH检验通过 → 必须用线性模型”。
正解：PH检验仅说明效应不随时间变化，但效应本身可能是非线性的（样条模型更优）。
误区：“样条模型一定破坏PH假设”。
正解：样条可以建模固定的非线性效应，同时保持比例风险（如您的案例）。"




