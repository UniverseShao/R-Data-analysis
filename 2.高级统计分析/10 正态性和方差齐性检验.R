# 1.正态性检验------------------------------------------------------------------
RD1<-c(2.78,3.23,4.20,4.87,5.12,6.21,7.18,8.05,8.56,9.60)
RD2<-c(3.23,3.50,4.04,4.15,4.28,4.34,4.47,4.64,4.75,4.82,4.95,5.10)  
df <- data.frame(
  rd = c(RD1,RD2),
  group = c(rep("rd1",length(RD1)), rep("rd2", length(RD2)))
)
df$group <- factor(df$group)
str(df)
##### 1.1shapiro wilk检验-------------------------------------------------------
shapiro.test(RD1)
# P值大于0.05，符合正态性。就是这么简单

##### 1.2kolmogorov smimov检验--------------------------------------------------
# 该函数使用要先进行标准化处理
RD1_scaled <- scale(RD1)  # 减去均值，除以标准差
ks.test(RD1_scaled, "pnorm")#指定要比较的理论分布，此处为标准正态分布

# 2.方差齐性检验----------------------------------------------------------------
##### 2.1两样本方差比较的F检验--------------------------------------------------
var.test(RD1, RD2)
# 或者
rd1_data <- df$rd[df$group == "rd1"]
rd2_data <- df$rd[df$group == "rd2"]
var_test_result <- var.test(rd1_data, rd2_data)
var_test_result
# 或者
var.test(rd ~ group, data = df)
# P值小于0.05，两样本方差不齐
# 提取 rd1 和 rd2 的数据
##### 2.2两样本方差比较的Levene检验---------------------------------------------
library(car)
leveneTest(rd ~ group, data = df)
# P值小于0.05，两样本方差不齐


# 3.多样本方差比较的Bartlett检验------------------------------------------------
trt<-c(rep("group1",30),rep("group2",30),rep("group3",30),rep("group4",30))

weight<-c(3.53,4.59,4.34,2.66,3.59,3.13,3.30,4.04,3.53,3.56,3.85,4.07,1.37,
          3.93,2.33,2.98,4.00,3.55,2.64,2.56,3.50,3.25,2.96,4.30,3.52,3.93,
          4.19,2.96,4.16,2.59,2.42,3.36,4.32,2.34,2.68,2.95,2.36,2.56,2.52,
          2.27,2.98,3.72,2.65,2.22,2.90,1.98,2.63,2.86,2.93,2.17,2.72,1.56,
          3.11,1.81,1.77,2.80,3.57,2.97,4.02,2.31,2.86,2.28,2.39,2.28,2.48,
          2.28,3.48,2.42,2.41,2.66,3.29,2.70,2.66,3.68,2.65,2.66,2.32,2.61,
          3.64,2.58,3.65,3.21,2.23,2.32,2.68,3.04,2.81,3.02,1.97,1.68,0.89,
          1.06,1.08,1.27,1.63,1.89,1.31,2.51,1.88,1.41,3.19,1.92,0.94,2.11,
          2.81,1.98,1.74,2.16,3.37,2.97,1.69,1.19,2.17,2.28,1.72,2.47,1.02,
          2.52,2.10,3.71)

data1<-data.frame(trt,weight)
data1$trt <- factor(data1$trt)

str(data1)

bartlett.test(weight ~ trt, data = data1)
#  p-value = 0.1564 结果提示满足方差齐性

# 4.多样本方差比较的Levene检验--------------------------------------------------
library(car)
leveneTest(weight ~ trt, data = data1)
# Pr(>F) 0.2201 结果提示满足方差齐性

