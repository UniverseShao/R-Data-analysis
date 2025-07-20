rm(list = ls())
# 1.四格表资料的卡方检验--------------------------------------------------------
ID<-seq(1,200)
treat<-c(rep("treated",104),rep("placebo",96))
treat<- factor(treat)
impro<-c(rep("marked",99),rep("none",5),rep("marked",75),rep("none",21))
impro<-as.factor(impro)
data1<-data.frame(ID,treat,impro)

str(data1)
table(data1$treat,data1$impro)

##### 1.1方法1------------------------------------------------------------------
library(gmodels)
CrossTable(data1$treat, data1$impro, digits = 4, expected = T, chisq = T, fisher = T, mcnemar = T, format = "SPSS")

##### 1.2方法2------------------------------------------------------------------
mytable <- table(data1$treat,data1$impro)
mytable
chisq.test(mytable,correct = F)

# 2.行 x 列表资料的卡方检验-----------------------------------------------------
library(haven)
df <- read_sav("C:/Users/Administrator/Desktop/R(SWY)/Medical Statistics/datasets/例07-06.sav")
View(df)
M <- as.table(rbind(c(199, 7), 
                    c(164, 18),
                    c(118, 26)))
dimnames(M) <- list(trt = c("物理", "药物", "外用"),
                    effect = c("有效","无效"))
kf <- chisq.test(M, correct = F)
kf

