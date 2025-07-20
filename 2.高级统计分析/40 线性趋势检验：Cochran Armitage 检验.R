# 1.双向有序分组资料的线性趋势检验----------------------------------------------
ather <- matrix(c(70,22,4,2,27,24,9,3,16,23,13,7,9,20,15,14),
                nrow = 4,byrow = T,
                dimnames = list(age = c("20~","30~","40~","≥50"),
                                level = c("-","+","++","+++")
                )
)
ather

chisq.test(ather)
##### 1.1双向有序分组资料卡方检验的不足-----------------------------------------

"这种情况下做这种卡方检验并不能说明什么问题，课本是看两者之间有没有线性趋势，我们可以直接用lm()函数做，
把age作为自变量，把level作为因变量即可，由于没有原始数据"
##### 1.2 使用CMHtest（推荐）---------------------------------------------------
library(vcdExtra)
CMHtest(ather, rscores = 1:nrow(ather), cscores = 1:ncol(ather))

##### 1.3 CMHtest()结果解读-----------------------------------------------------s
"
CMHtest(ather, rscores = 1:nrow(ather), cscores = 1:ncol(ather))
Cochran-Mantel-Haenszel Statistics for age by level 

                 AltHypothesis  Chisq Df       Prob
cor        Nonzero correlation 63.389  1 1.6962e-15
rmeans  Row mean scores differ 63.450  3 1.0758e-13
cmeans  Col mean scores differ 65.669  3 3.6072e-14
general    General association 71.176  9 8.9519e-12

统计量	      原假设 (H₀)	                    适用场景
cor行      列变量无线性相关	                 |双向有序变量（检验趋势）
rmeans	   各行平均得分相等	                 |分组变量有序，指标变量无序
cmeans	   各列平均得分相等	                 |分组变量无序，指标变量有序
general    行列变量完全独立（无任何关联）	   |普通卡方检验的扩展
"

# 2.单向有序R×C表资料-----------------------------------------------------------

##### 2.1情况1：分组变量有序（如年龄），指标变量无序（如传染病类型）------------
"
研究目的：分析不同年龄组各种传染病的构成情况。
适用方法：行×列表资料的 Pearson 卡方检验（chisq.test）

"
###### 2.1.1构造数据 -----------------------------------------------------------
# 构造数据：年龄（有序） vs. 传染病类型（无序）
data1 <- matrix(
  c(20, 30, 10,   # <20岁：流感、肝炎、麻疹
    15, 25, 20,   # 20~40岁
    10, 15, 30),  # ≥40岁
  nrow = 3,
  byrow = TRUE,
  dimnames = list(
    Age = c("<20岁", "20~40岁", "≥40岁"),
    Disease = c("流感", "肝炎", "麻疹")
  )
)

# 查看数据
data1
###### 2.1.2卡方检验 -----------------------------------------------------------
# Pearson卡方检验（检验年龄组与传染病构成的关联性）
chisq.test(data1)

###### 2.1.3选择卡方检验的原因--------------------------------------------------

"
分析不同年龄组各种传染病的构成情况

统计假设：
H₀：各年龄组的传染病构成比例相同（年龄与传染病类型独立）。
H₁：构成比例不同（存在关联）。

方法选择：
Pearson卡方检验：因为指标变量（传染病）是无序的，
只需检验“比例是否相同”，无需利用年龄的顺序信息


"

##### 2.2 情况2：分组变量无序（如疗法），指标变量有序（如疗效等级）-------------

"
研究目的：比较不同疗法的疗效（有序等级）。
适用方法：秩转换的非参数检验（如 Kruskal-Wallis 检验或 CMH 行平均得分检验）


"
###### 2.2.1构造数据并装换成长格式频率表----------------------------------------
# 构造数据：疗法（无序） vs. 疗效（有序：无效、改善、显效）
data2 <- matrix(
  c(10, 20, 30,   # 疗法A：无效、改善、显效
    15, 25, 10,   # 疗法B
    5, 15, 40),   # 疗法C
  nrow = 3,
  byrow = TRUE,
  dimnames = list(
    Therapy = c("A", "B", "C"),
    Effect = c("无效", "改善", "显效")
  )
)
data2
# 转换为长格式频数表（适用于秩检验）
library(tidyr)
data2_long <- as.data.frame(as.table(data2))
names(data2_long) <- c("Therapy", "Effect", "Count")

###### 2.2.2方法1：Kruskal-Wallis 检验（全局比较）------------------------------
# 生成秩数据（按疗效等级赋分）
data2_long$Effect_score <- factor(data2_long$Effect, 
                                  levels = c("无效", "改善", "显效"),
                                  ordered = TRUE)

# Kruskal-Wallis 检验
kruskal.test(Effect_score ~ Therapy, data = data2_long)

###### 2.2.3方法2：CMH 行平均得分检验（更适用于有序分类）-----------------------
library(vcdExtra)
CMHtest(data2, rscores = 1:nrow(data2), cscores = 1:ncol(data2))

###### 2.2.4 CMHtest()结果解读--------------------------------------------------
"
CMHtest(data2, rscores = 1:nrow(data2), cscores = 1:ncol(data2))
Cochran-Mantel-Haenszel Statistics for Therapy by Effect 

                 AltHypothesis   Chisq Df       Prob
cor        Nonzero correlation  3.3252  1 6.8224e-02
rmeans  Row mean scores differ 22.8376  2 1.0987e-05
cmeans  Col mean scores differ  3.5208  2 1.7197e-01
general    General association 24.8806  4 5.3170e-05

统计量	      原假设 (H₀)	                    适用场景
cor行      列变量无线性相关	                 |双向有序变量（检验趋势）
rmeans	   各行平均得分相等	                 |分组变量有序，指标变量无序
cmeans	   各列平均得分相等	                 |分组变量无序，指标变量有序
general    行列变量完全独立（无任何关联）	   |普通卡方检验的扩展
"

##### 2.3单向有序R×C表资料两种形式检验不同的原因--------------------------------
# 因为卡方检验是检验行组之间指标变量的构成差异
# 行组是有序分组变量的时候，因为指标变量（传染病）是无序的，只需检验“比例是否相同”
# 无需利用年龄的顺序信息


# 比较不同疗法的疗效等级是否有差异（如疗法A是否更易导致“显效”）
# 比较不同疗法的疗效等级是否有差异
# 列组是有序分组变量的时候，适用卡方检验检验行组之间的列祖是否有差异
# 就算检验出来了，但是没有体现列祖变量的有序性，所以不能适用卡方检验
# 而要使用秩和检验（如Kruskal-Wallis）或CMH行平均得分检验

# 为什么不能简单互换行列后用卡方检验？
# 因为我的临床问题是探究比较不同疗法的疗效等级是否有差异（如疗法A是否更易导致“显效”）
# 行列变换之后使用卡方检验的意义就变成
# 各疗效等级之间的疗法构成是否有差异、是否显著相关、是否分布相同
# 临床研究的意义和目的都发生改变，所以不行



