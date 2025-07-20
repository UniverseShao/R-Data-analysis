# 15.1准备数据-----------------------------------------------------------------
rm(list = ls())
load(file = "C:\\Users\\Administrator\\Desktop\\R语言脚本(SWY精心编辑版本)\\临床预测模型\\datasets\\lnc_expr_clin.RData")
#去掉没有生存信息的样本
lnc_expr_clin1 <- lnc_expr_clin[!is.na(lnc_expr_clin$time_months),]
lnc_expr_clin1 <- lnc_expr_clin1[lnc_expr_clin1$time_months>0,]
#选择其中一部分数据
dat.cox <- lnc_expr_clin1[,c(72:73,1:59)]
dim(dat.cox)
dat.cox[1:4,1:6]

dat_cox <- dat.cox
dat_cox[,c(3:ncol(dat_cox))] <- sapply(dat_cox[,c(3:ncol(dat_cox))],function(x){
  ifelse(x>median(x),"high","low")
})
#将该列中大于中位数的值替换为"high"
#将该列中小于或等于中位数的值替换为"low"
dat_cox[,c(3:ncol(dat_cox))] <- lapply(dat_cox[,c(3:ncol(dat_cox))],factor)
#lapply() 对第3列到最后一列应用 factor() 函数
#factor() 将这些列从字符型转换为因子型(factor)
dat_cox[1:4,1:6]

# 15.2批量单因素cox------------------------------------------------------------

library(survival)
gene <- colnames(dat_cox)[-c(1:2)]
gene <- colnames(dat_cox)[-c(1:2)]
cox.result <- list()
for (i in 1:length(gene)) {
  #print(i)
  group <- dat_cox[, i + 2]
  if (length(table(group)) == 1) next
  #if (length(grep("high", group)) < min_sample_size) next
  #if (length(grep("low", group)) < min_sample_size) next
  x <- survival::coxph(survival::Surv(time_months, event) ~ group, 
                       data = dat_cox)
  tmp1 <- broom::tidy(x, exponentiate = T, conf.int = T)
  cox.result[[i]] <- c(gene[i], tmp1)
}
#exponentiate = T：返回 风险比（HR, Hazard Ratio） 而非回归系数（exp(coef)）。
#conf.int = T：返回 95% 置信区间
res.cox <- data.frame(do.call(rbind, cox.result))
#筛选出P值小于0.1的变量
library(dplyr)

unifea <- res.cox %>% 
  filter(p.value<0.1) %>% 
  pull(V1) %>% # 提取列名为 "V1" 的列（基因名）
  unlist()# 确保输出为向量（而非列表）
unifea

# 15.3多因素cox----------------------------------------------------------------
sub_dat <- dat_cox[,c("time_months","event",unifea)]
dim(sub_dat)
sub_dat[1:4,1:6]

final.fit <- coxph(Surv(time_months,event)~., data = sub_dat)
res <- broom::tidy(final.fit)
res
##### 15.3.1查看P值小于0.05的变量-----------------------------------------------
res %>% filter(p.value<0.05)
##### 15.3.2拟合最终模型-------------------------------------------------------
fit5 <- coxph(Surv(time_months,event)~`ADAMTS9-AS1`+AC093010.3+SNHG25+
                AC025575.2+AL161431.1,data = sub_dat)
summary(fit5)
##### 15.3.3绘制森林图
library(survminer)
survminer::ggforest(fit5)

# 15.4一行代码实现--------------------------------------------------------------
library(autoReg)
#使用autoReg函数可以实现先单后多cox分析
#此时是多因素cox的形式，但是这个函数会自动帮我们提取数据
#然后先批量对每个变量做cox
##### 15.4.1autoReg()函数适用对变量名的需求-------------------------------------
#变量名字中有-，导致函数报错
fit <- coxph(Surv(time_months,event)~., data = dat_cox)

autoReg(fit,
        threshold = 0.1,#显著性阈值是0.1
        uni = T,# 单因素分析
        multi = F# 多因素分析
)
#出现报错，需要将变量中的-去掉
###### 15.4.1.1修改变量名-------------------------------------------------------
#直接把-去掉了
colnames(dat_cox)<- gsub("-","",colnames(dat_cox))

##### 15.4.2一行代码实现先单后多------------------------------------------------
fit <- coxph(Surv(time_months,event)~., data = dat_cox)
ft <- autoReg(fit,
              threshold = 0.1,
              uni = T, # 单因素分析
              multi = T, # 多因素分析
              final = F # 逐步法，向后
)
ft
###### 15.4.2.1结果导出Word或者Excel格式----------------------------------------
library(rrtable)

table2docx(ft)
