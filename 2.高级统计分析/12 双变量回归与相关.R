# 1.两样本相关------------------------------------------------------------------
rm(list = ls())
df <- data.frame(
  weight = c(43,74,51,58,50,65,54,57,67,69,80,48,38,85,54),
  kv = c(217.22,316.18,231.11,220.96,254.70,293.84,263.28,271.73,263.46,276.53,341.15,261.00,213.20,315.12,252.08)
)

str(df)
cor(df)
cor.test(~ weight + kv, data = df)
library(psych)
corr.test(df)

library(ggplot2)

ggplot(df, aes(weight, kv)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm") +
  theme_minimal()
