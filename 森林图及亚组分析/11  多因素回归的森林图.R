#GGally####
#该包能够快速可视化多种模型
#结合了broom包和ggplot2的优点
#线性回归####
library(GGally)
data(tips, package = "reshape")
mod_simple <- lm(tip ~ day + time + total_bill, data = tips)

ggcoef_model(mod_simple)

#logistic回归####
d_titanic <- as.data.frame(Titanic)
d_titanic$Survived <- factor(d_titanic$Survived, c("No", "Yes"))
mod_titanic <- glm(
  Survived ~ Sex * Age + Class,
  weights = Freq,
  data = d_titanic,
  family = binomial
)
ggcoef_model(mod_titanic, exponentiate = TRUE)

#exponentiate = TRUE####
#这个指的是是否对系数取指数（用于HR/OR）
#ggcoef_model()函数用法####
ggcoef_model(
model,             # 拟合的模型对象（如lm, glm, coxph等）
exponentiate = FALSE,  # 是否对系数取指数（用于HR/OR）
conf.int = TRUE,   # 是否显示置信区间
mapping = aes(...), # ggplot2的美学映射
...                # 其他图形参数（颜色、标题等）
)

#自定义标题####
library(labelled)
tips_labelled <- tips %>%
  set_variable_labels(
    day = "Day of the week",
    time = "Lunch or Dinner",
    total_bill = "Bill's total"
  )
mod_labelled <- lm(tip ~ day + time + total_bill, data = tips_labelled)
ggcoef_model(mod_labelled)

#set_variable_labels()####
#set_variable_labels() 是 labelled 包中的一个函数，
#用于为数据框中的变量添加标签（variable labels）

#更改主题####
ggcoef_model(mod_simple) +
  xlab("Coefficients") +
  ggtitle("Custom title") +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "right")

# scale_color_brewer(palette = "Set1")####
#为使用预定调色板中的set1调色板中已经调试好的点 线 颜色
#theme(legend.position = "right")####
#主题设置中legend.position = "right"
#指的是将图例放在图的右侧

#同时比较多个回归模型####
mod1 <- lm(Fertility ~ ., data = swiss)
mod2 <- step(mod1, trace = 0)
mod3 <- lm(Fertility ~ Agriculture + Education * Catholic, data = swiss)
models <- list("Full model" = mod1, "Simplified model" = mod2, "With interaction" = mod3)

ggcoef_compare(models)


#mod1是带所有自变量回归的模型
#mod2是逐步回归逐步剔除变量回归生成最简化的模型
#trace = 0：关闭迭代过程打印（若设为1会显示步骤细节）
#mod3是带有Education * Catholic交互项的模型也就是
#With interaction模型


#分面####
ggcoef_compare(models, type = "faceted")
#以分面的形式画图












































