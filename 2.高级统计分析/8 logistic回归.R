library(haven)
df16_2 <- read_sav("C:/Users/Administrator/Desktop/R脚本(SWY精心编辑版)/Medical Statistics/datasets/例16-02.sav")
View(df16_2)
str(df16_2)
f <- glm(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, data = df16_2, family = binomial())
f1 <- step(f, direction = "forward")
summary(f1)
# 各种可以和上一节共通的