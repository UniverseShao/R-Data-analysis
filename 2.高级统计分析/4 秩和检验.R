# 1.配对样本比较的Wilcoxon符号秩检验--------------------------------------------
test1<-c(60,142,195,80,242,220,190,25,198,38,236,95)
test2<-c(76,152,243,82,240,220,205,38,243,44,190,100)
boxplot(test1,test2)
wilcox.test(test1,test2,paired = T,alternative = "two.sided",exact = F, correct = F)


# 2.两独立样本比较的Wilcoxon符号秩检验------------------------------------------
RD1<-c(2.78,3.23,4.20,4.87,5.12,6.21,7.18,8.05,8.56,9.60)
RD2<-c(3.23,3.50,4.04,4.15,4.28,4.34,4.47,4.64,4.75,4.82,4.95,5.10)
wilcox.test(RD1,RD2,paired = F, correct = F)


# 3.完全随机设计多个样本比较的 Kruskal-Wallis H 检验----------------------------
# 各组间独立
rm(list = ls())
death_rate <- c(32.5,35.5,40.5,46,49,16,20.5,22.5,29,36,6.5,9.0,12.5,18,24)
drug <- rep(c("Drug_A","drug_B","drug_C"),each=5)
mydata <- data.frame(death_rate,drug)
mydata$drug <- factor(mydata$drug)
str(mydata)
boxplot(death_rate ~ drug, data = mydata)
kruskal.test(death_rate ~ drug, data = mydata)



# 4.随记区组设计多个样本比较的 Friedman M 检验----------------------------------
# 各组间不独立
df <- foreign::read.spss("C:\\Users\\Administrator\\Desktop\\R(SWY)\\Medical Statistics\\datasets\\例08-09.sav", to.data.frame = T)
str(df)
boxplot(df$a,df$b,df$c,df$d)

M <- as.matrix(df) # 变成矩阵
friedman.test(M)

