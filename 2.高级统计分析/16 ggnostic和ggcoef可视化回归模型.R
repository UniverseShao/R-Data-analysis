rm(list = ls())
# 1.ggbivariate()---------------------------------------------------------------
##### 1.1展示一个结果变量和其他变量之间的关系-----------------------------------
library(GGally)
data(tips, package = "reshape")
View(tips)
ggbivariate(tips, outcome = "smoker", explanatory = c("day","time","sex","tip"))

ggbivariate(tips, outcome = "total_bill", explanatory = c("day", "time", "sex", "tip"))

ggbivariate(tips, "smoker")

##### 1.2自定义颜色-------------------------------------------------------------
library(ggplot2)
ggbivariate(tips, outcome = "smoker", explanatory = c("day","time","sex","tip")) +
  scale_fill_brewer(type = "qual")

# 2.ggnostic()------------------------------------------------------------------
##### 2.1直接可视化回归模型-----------------------------------------------------
df15_1 <- data.frame(
  cho = c(5.68,3.79,6.02,4.85,4.60,6.05,4.90,7.08,3.85,4.65,4.59,4.29,7.97,
          6.19,6.13,5.71,6.40,6.06,5.09,6.13,5.78,5.43,6.50,7.98,11.54,5.84,
          3.84),
  tg = c(1.90,1.64,3.56,1.07,2.32,0.64,8.50,3.00,2.11,0.63,1.97,1.97,1.93,
         1.18,2.06,1.78,2.40,3.67,1.03,1.71,3.36,1.13,6.21,7.92,10.89,0.92,
         1.20),
  ri = c(4.53, 7.32,6.95,5.88,4.05,1.42,12.60,6.75,16.28,6.59,3.61,6.61,7.57,
         1.42,10.35,8.53,4.53,12.79,2.53,5.28,2.96,4.31,3.47,3.37,1.20,8.61,
         6.45),
  hba = c(8.2,6.9,10.8,8.3,7.5,13.6,8.5,11.5,7.9,7.1,8.7,7.8,9.9,6.9,10.5,8.0,
          10.3,7.1,8.9,9.9,8.0,11.3,12.3,9.8,10.5,6.4,9.6),
  fpg = c(11.2,8.8,12.3,11.6,13.4,18.3,11.1,12.1,9.6,8.4,9.3,10.6,8.4,9.6,10.9,
          10.1,14.8,9.1,10.8,10.2,13.6,14.9,16.0,13.2,20.0,13.3,10.4)
)

str(df15_1)
##### 2.2建立回归方程-----------------------------------------------------------
f <- lm(fpg ~ cho + tg + ri + hba, data = df15_1)
summary(f)
##### 2.3可视化回归方程---------------------------------------------------------
ggnostic(f)
##### 2.3.1进一步可视化---------------------------------------------------------
ggnostic(f, columnsY = c("fpg", ".fitted", ".se.fit", ".resid", ".std.resid", ".hat", ".sigma", ".cooksd"))

# 3.ggcoef_model()--------------------------------------------------------------
##### 3.1线性回归---------------------------------------------------------------
data(tips, package = "reshape")
mod_simple <- lm(tip ~ day + time + total_bill, data = tips)
summary(mod_simple)
ggcoef_model(mod_simple)

##### 3.2logistic回归-----------------------------------------------------------
d_titanic <- as.data.frame(Titanic)
d_titanic$Survived <- factor(d_titanic$Survived, c("No", "Yes"))
mod_titanic <- glm(
  Survived ~ Sex * Age + Class,
  weights = Freq,
  data = d_titanic,
  family = binomial
)
ggcoef_model(mod_titanic, exponentiate = TRUE)

##### 3.3支持自定义标题---------------------------------------------------------
library(labelled)
tips_labelled <- tips %>%
  set_variable_labels(
    day = "Day of the week",
    time = "Lunch or Dinner",
    total_bill = "Bill's total"
  )
mod_labelled <- lm(tip ~ day + time + total_bill, data = tips_labelled)
ggcoef_model(mod_labelled)

##### 3.4支持更改主题-----------------------------------------------------------
ggcoef_model(mod_simple) +
  xlab("Coefficients") +
  ggtitle("Custom title") +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "right")

##### 3.5同时比较多个回归模型---------------------------------------------------
mod1 <- lm(Fertility ~ ., data = swiss)
mod2 <- step(mod1, trace = 0)
mod3 <- lm(Fertility ~ Agriculture + Education * Catholic, data = swiss)
models <- list("Full model" = mod1, "Simplified model" = mod2, "With interaction" = mod3)
ggcoef_compare(models)
ggcoef_compare(models) +
  xlab("Coefficients") +
  ggtitle("Custom title") +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "right")

##### 3.6分面展示---------------------------------------------------------------
ggcoef_compare(models, type = "faceted")
ggcoef_compare(models, type = "faceted") +
  xlab("Coefficients") +
  ggtitle("Custom title") +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "right")

