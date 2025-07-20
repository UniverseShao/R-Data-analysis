# 1.kruskal-Wallis H检验及多重比较----------------------------------------------
rm(list = ls())
death_rate <- c(32.5,35.5,40.5,46,49,16,20.5,22.5,29,36,6.5,9.0,12.5,18,24)
drug <- rep(c("Drug_A","drug_B","drug_C"),each=5)
mydata <- data.frame(death_rate,drug)

##### 1.1分类变量因子化---------------------------------------------------------
mydata$drug <- factor(mydata$drug)
str(mydata)

##### 1.2进行kruskal-Wallis H 检验----------------------------------------------
fit <- kruskal.test(death_rate ~ drug, data = mydata)
fit

##### 1.3多重检验---------------------------------------------------------------
library(PMCMRplus)

res <- kwAllPairsNemenyiTest(fit)
res <- kwAllPairsNemenyiTest(death_rate ~ drug, data = mydata)
summary(res)

# 2.Friedman M检验及多重比较----------------------------------------------------
df <- matrix(
  c(8.4, 11.6, 9.4, 9.8, 8.3, 8.6, 8.9, 7.8,
    9.6, 12.7, 9.1, 8.7, 8, 9.8, 9, 8.2,
    9.8, 11.8, 10.4, 9.9, 8.6, 9.6, 10.6, 8.5,
    11.7, 12, 9.8, 12, 8.6, 10.6, 11.4, 10.8
  ),
  byrow = F, nrow = 8,
  dimnames = list(1:8,LETTERS[1:4])
)

print(df)


##### 2.1进行Friedman M检验-----------------------------------------------------
fit <- friedman.test(df)
fit

##### 2.2q检验（quade test）进行多重比较:---------------------------------------

res <- quadeAllPairsTest(df)
summary(res)

