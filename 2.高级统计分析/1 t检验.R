# 1.单样本t检验-----------------------------------------------------------------
# 使用foreign包读取SPSS数据
rm(list = ls())
library(foreign)
df <- read.spss("C:\\Users\\Administrator\\Desktop\\R脚本(SWY精心编辑版)\\Medical Statistics\\datasets\\例03-05.sav",to.data.frame = T)
head(df)
st <- t.test(df$hb,mu=140,alternative = 'two.sided') # 双侧检验
st


# 2.已配对好的样本t检验---------------------------------------------------------
library(foreign)
df <- read.spss("C:\\Users\\Administrator\\Desktop\\R脚本(SWY精心编辑版)\\Medical Statistics\\datasets\\例03-06.sav",to.data.frame = T)
head(df)
pt <- t.test(df$x1,df$x2,paired = T,var.equal = T) # 配对样本t检验
pt


# 3.两样本t检验-----------------------------------------------------------------
##### 3.1两样本非配对t检验------------------------------------------------------
library(foreign)
df <- read.spss("C:\\Users\\Administrator\\Desktop\\R脚本(SWY精心编辑版)\\Medical Statistics\\datasets\\例03-07.sav",to.data.frame = T)
df$group <- c(rep('阿卡波糖',20),rep('拜糖平',20))
head(df)
###### 3.1.1t.test(x ~ group)写法-----------------------------------------------
tt <- t.test(x ~ group, data = df, var.equal = T) 
tt
###### 3.1.2t.test(A,B)写法-----------------------------------------------------
tt <- t.test(df$x[df$group == "阿卡波糖"], 
             df$x[df$group == "拜糖平"], 
             paired = FALSE,  # 独立样本
             var.equal = TRUE)  # 方差齐性
tt
##### 3.2两样本配对t检验--------------------------------------------------------
tt <- t.test(df$x[df$group == "阿卡波糖"], 
             df$x[df$group == "拜糖平"], 
             paired = TRUE,  # 独立样本
             var.equal = TRUE)  # 方差齐性
tt
