# 1.加载数据和R包---------------------------------------------------------------

#探讨骨髓移植和血液移植治疗白血病的疗效，结局事件定义为复发，
#某些患者因为移植不良反应死亡，定义为竞争风险事件
rm(list = ls())
data("bmtcrr",package = "casebase")
str(bmtcrr)
##### 1.1数据变量简介-----------------------------------------------------------

#Sex: 性别，F是女，M是男
#D: 疾病类型，ALL是急性淋巴细胞白血病，AML是急性髓系细胞白血病。
#Phase: 不同阶段，4个水平，CR1，CR2，CR3，Relapse。
#Age: 年龄。
#Status: 结局变量，0=删失，1=复发，2=竞争风险事件。
#Source: 因子变量，2个水平：BM+PB(骨髓移植+血液移植)，PB(血液移植)。
#ftime: 生存时间。

# 竞争风险分析需要用的R包
library(cmprsk)

# 2.Fine-Gray检验（单因素分析）-------------------------------------------------


##### 2.1Fine-Gray检验注意事项--------------------------------------------------
#再次强调
#复发是结局事件，移植后死亡是竞争风险事件
#使用Fine-Gray检验的时候需要保证status中除了有复发事件外，还得有竞争风险事件

#普通的生存分析中，可以用log-rank检验做单因素分析
#竞争风险模型中，使用Fine-Gray检验进行单因素分析

#比较不同疾病类型（D）有没有差异，可以进行Fine-Gray检验
bmtcrr$Status <- factor(bmtcrr$Status)
f <- cuminc(bmtcrr$ftime, bmtcrr$Status, bmtcrr$D)
f
##        stat         pv df
## 1 2.8623325 0.09067592  1
## 2 0.4481279 0.50322531  1
## $est
## $var

##### 2.2Fine-Gray检验结果解读--------------------------------------------------
###### 2.2.1 $Tests解读---------------------------------------------------------
#第1行统计量=2.8623325, P=0.09067592,表示在控制了竞争风险事件后
#两种疾病类型ALL和AML的累计复发风险无统计学差异P=0.09067592
#第2行说明ALL和AML的累计竞争风险(即移植后死亡累计风险(h(t)的积分))无统计学差异

###### 2.2.2 $est解读-----------------------------------------------------------

## $est
##              20        40        60        80       100       120
## ALL 1 0.3713851 0.3875571 0.3875571 0.3875571 0.3875571 0.3875571
## AML 1 0.2414530 0.2663827 0.2810390 0.2810390 0.2810390        NA
## ALL 2 0.3698630 0.3860350 0.3860350 0.3860350 0.3860350 0.3860350
## AML 2 0.4439103 0.4551473 0.4551473 0.4551473 0.4551473        NA


#$est表示估计的各时间点ALL和AML组的累计复发率与与累计竞争风险事件发生率
#（分别用1和2来区分，与第一行第二行一致）

###### 2.2.3 $var解读-----------------------------------------------------------

## $var
##                20          40          60          80         100         120
## ALL 1 0.003307032 0.003405375 0.003405375 0.003405375 0.003405375 0.003405375
## AML 1 0.001801156 0.001995487 0.002130835 0.002130835 0.002130835          NA
## ALL 2 0.003268852 0.003373130 0.003373130 0.003373130 0.003373130 0.003373130
## AML 2 0.002430406 0.002460425 0.002460425 0.002460425 0.002460425          NA

#$var表示估计的各时间点ALL和AML组的累计复发率与与累计竞争风险事件发生率的方差
#（分别用1和2来区分，与第一行第二行一致）

# 3. 图形展示结果---------------------------------------------------------------
plot(f,xlab = 'Month', ylab = 'CIF',lwd=2,lty=1,#注意这个xlab = 'Month'仅仅代表标签
     col = c('red','blue','black','forestgreen'))

#在控制了竞争风险事件后，ALL和AML累计复发风险无统计学差异P=0.09067592


# 4.ggplot2绘制图像-------------------------------------------------------------
#使用ggplot绘图需要先提取数，数就是图，图就是数

# 提取数据
ALL1 <- data.frame(ALL1_t = f[[1]][[1]], ALL1_C = f[[1]][[2]])
AML1 <- data.frame(AML1_t = f[[2]][[1]], AML1_C = f[[2]][[2]])
ALL2 <- data.frame(ALL2_t = f[[3]][[1]], ALL2_C = f[[3]][[2]])
AML2 <- data.frame(AML2_t = f[[4]][[1]], AML2_C = f[[4]][[2]])

library(ggplot2)
##### 4.1画图-------------------------------------------------------------------
ggplot()+
  geom_line(data = ALL1, aes(ALL1_t,ALL1_C))+
  geom_line(data = ALL2, aes(ALL2_t,ALL2_C))+
  geom_line(data = AML1, aes(AML1_t,AML1_C))+
  geom_line(data = AML2, aes(AML2_t,AML2_C))+
  labs(x="month",y="cif")+
  theme_bw()

##### 4.2图像优化--------------------------------------------------------------- 
tmp <- data.frame(month = c(ALL1$ALL1_t,AML1$AML1_t,ALL2$ALL2_t,AML2$AML2_t),
                  cif = c(ALL1$ALL1_C,AML1$AML1_C,ALL2$ALL2_C,AML2$AML2_C),
                  type = rep(c("ALL1","AML1","ALL2","AML2"), c(58,58,58,88))
)

ggplot(tmp, aes(month, cif))+
  geom_line(aes(color=type, group=type),size=1.2)+
  theme_bw()+
  theme(legend.position = "top")

# 5.竞争风险模型（多因素分析）--------------------------------------------------
covs <- subset(bmtcrr, select = - c(ftime,Status))
#从数据集 bmtcrr 中提取所有 协变量（预测变量），
#并排除生存时间（ftime）和事件状态（Status）
covs[,c(1:3,5)] <- lapply(covs[,c(1:3,5)],as.integer)
#将 covs 的第1、2、3、5列（通常是分类变量或离散变量）转换为 整数类型（integer）
#某些协变量（如性别、分组变量）可能需要作为整数输入模型，避免因子（factor）带来的额外处理
str(covs)
# 构建竞争风险模型
f2 <- crr(bmtcrr$ftime, bmtcrr$Status, covs, failcode=1, cencode=0)
summary(f2)
##           coef exp(coef) se(coef)      z p-value
## Sex     0.0494     1.051   0.2867  0.172 0.86000
## D      -0.4860     0.615   0.3040 -1.599 0.11000
## Phase   0.4144     1.514   0.1194  3.470 0.00052
## Age    -0.0174     0.983   0.0118 -1.465 0.14000
## Source  0.9526     2.592   0.5469  1.742 0.08200


##### 5.1结果解读---------------------------------------------------------------

#在控制了竞争分险事件后，phase变量，即疾病所处阶段是患者复发的独立影响因素(p =0.00052)


# 6.列线图----------------------------------------------------------------------
##### 6.1调整数据---------------------------------------------------------------
rm(list = ls())
data("bmtcrr",package = "casebase") # 还是这个数据

library(mstate) # 加权用到的R包

bmtcrr$id <- 1:nrow(bmtcrr) # 创建id

# phase变为2分类，不然列线图不好解释
bmtcrr$Phase <- factor(ifelse(bmtcrr$Phase=="Relapse",1,0)) 
str(bmtcrr)
##### 6.2 数据加权--------------------------------------------------------------
#对原数据进行加权
df.w <- crprep("ftime", "Status",
               data=bmtcrr, 
               trans=c(1,2),# 要加权的变量，1表示结局事件，2表示竞争风险事件
               cens=0, # 删失
               id="id",
               
               # 要保留的协变量
               keep=c("Age","Sex","D","Source","Phase"))

df.w
df.w$T<- df.w$Tstop - df.w$Tstart
#这时数据中会出现failcode列
#crprep() 的核心功能是 对竞争风险数据重新加权
#使得在分析主要事件（如疾病复发）时，能够调整竞争事件（如死亡）的影响

#选择failcode == 1的行
#才可以在此数据集上使用coxph()函数进行竞争风险分析
df.w2 <- df.w[df.w$failcode == 1,]
##### 6.3failcode列的作用以及含义-----------------------------------------------
###### (1)6.3.1failcode是什么---------------------------------------------------
#failcode 是 crprep() 函数生成的中间变量，
#用于标识当前行对应的原始事件类型（即 Status 的映射）
###### (2)6.3.2指定trans = c(1, 2)的含义----------------------------------------
#在 crprep() 中，你指定了 trans = c(1, 2)，表示：
#1：主要事件（如复发，对应原始 Status == 1）
#2：竞争事件（如死亡，对应原始 Status == 2）
#failcode 的作用：标记当前行是用于建模主要事件（failcode=1）还是竞争事件（failcode=2）。
###### (3)6.3.3为什么 failcode == 1 的行中会有 status = 2？---------------------

#这是 crprep() 的核心机制——数据重构。函数会将原始数据拆分为两部分：
#主要事件视角（failcode=1）：
#所有发生竞争事件（Status=2）的个体，会被赋予一个权重（IPCW），
#表示“如果竞争事件未发生，他们可能贡献的信息”。
#因此，在 failcode=1 的分组中，你会看到：
#原始 Status=1 的行（主要事件）。
#原始 Status=2 的行（竞争事件，但被重新加权，用于调整竞争风险）。
#竞争事件视角（failcode=2）：
#同理，主要事件会被当作竞争事件处理

###### (4)6.3.4df.w2 <- df.w[df.w$failcode == 1,] 的作用------------------------
#这行代码的目的是：提取所有从主要事件视角（failcode=1）分析的数据行，包括：
#(1)原始的主要事件（Status=1）。
#(2)加权的竞争事件（Status=2，但通过权重调整其影响）。
#这样做的意义是：
#专注分析主要事件：在后续模型（如加权Cox模型）中，竞争事件的影响已通过权重调整，避免偏倚。
#保持数据完整性：竞争事件未被删除，而是通过统计学方法（IPCW）处理。

##### 6.4构建cox模型------------------------------------------------------------
m.crr<- coxph(Surv(T,status==1)~Age+Sex+D+Source+Phase,
              data=df.w2,
              weight=weight.cens,
              subset=failcode==1)
summary(m.crr)

##### 6.5regplot()函数绘制nomogram---------------------------------------------
library(regplot)
regplot(m.crr,
        observation=df.w2[df.w2$id==25&df.w2$failcode==1,],
        failtime = c(36, 60), 
        prfail = T, 
        droplines=T)



