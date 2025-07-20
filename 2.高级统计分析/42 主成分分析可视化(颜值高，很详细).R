# 1.PCA-------------------------------------------------------------------------
rm(list = ls())
library(factoextra)
library(FactoMineR)
pca.res <- PCA(iris[,-5], graph = F, scale.unit = T) # 简简单单1行代码实现主成分分析
pca.res
# 为什么标准化？
# PCA对变量的方差敏感，
# 若变量单位不同（如 Sepal.Length 单位是cm，Petal.Width 是mm），
# 会导致方差大的变量主导结果。标准化可消除量纲影响

# 2.特征值可视化----------------------------------------------------------------
get_eigenvalue(pca.res)
"
      eigenvalue variance.percent cumulative.variance.percent
Dim.1 2.91849782       72.9624454                    72.96245
Dim.2 0.91403047       22.8507618                    95.81321
Dim.3 0.14675688        3.6689219                    99.48213
Dim.4 0.02071484        0.5178709                   100.00000
获取特征值、方差贡献率和累积方差贡献率
(1) 特征值（Eigenvalue）
含义：每个主成分捕获的原始数据方差量。
判断标准：
特征值 > 1：该主成分保留的信息比原始变量更多（Kaiser准则）。
你的结果：
Dim.1（2.918）和 Dim.2（0.914）的特征值较高，尤其是 Dim.1
(2) 方差解释率（Variance Percent）
含义：每个主成分解释的原始数据总方差的百分比。
你的结果：
Dim.1 解释了 72.96% 的方差（主要信息集中在第一主成分）。
Dim.2 解释了 22.85%的方差。
前两个主成分累计解释 95.81% 的方差，几乎覆盖全部信息。
(3) 累计方差解释率（Cumulative Variance）
含义：前N个主成分累计解释的方差比例。
应用：
通常选择累计解释率 ≥80% 的主成分（你的数据仅需前两个主成分即可达到95.81%）。
"
##### 2.1碎石图-----------------------------------------------------------------
fviz_eig(pca.res,addlabels = T,ylim=c(0,100))
# 观察点：寻找拐点（Elbow），拐点后的主成分贡献下降平缓
# 你的数据：拐点可能在 Dim.2 处，支持保留前两个主成分

# 3.提取变量结果----------------------------------------------------------------
# 通过get_pca_var()`函数实现
res.var <- get_pca_var(pca.res)
res.var$cor# 变量和主成分的相关系数
res.var$coord# 变量在主成分投影上的坐标 
res.var$cos2# coord的平方，也是表示主成分和变量间的相关性
              # 同一个变量所有cos2的总和是1
res.var$contrib# 变量对主成分的贡献
"
 res.var$cos2结果是横着看横着比较，主成分和变量间的相关性，横着加为1
             [0, 1]（0表示不相关，1表示完全相关）

res.var$contrib结果是竖着看竖着比较，变量对主成分的贡献，竖着加为100
"

# 4.变量结果可视化--------------------------------------------------------------
##### 4.1整体向量图-------------------------------------------------------------
fviz_pca_var(pca.res)
"
res.var$coord是变量在主成分投影上的坐标，
Sepal.Width在Dim.1的坐标是-0.4601427，
在Dim.2的坐标是0.88271627，根据这两个坐标就画出来Sepal.Width那根线了，以此类推~
"
##### 4.2变量和主成分的cos2可视化(相关性可视化)---------------------------------
###### 4.2.1热图----------------------------------------------------------------
library("corrplot")
corrplot(res.var$cos2, is.corr = F)
"
可以看到Petal.Length、Petal.Width和Dim1的相关性比较强
Sepal.Width和Dim2的相关性比较强
"
###### 4.2.2直方图--------------------------------------------------------------
fviz_cos2(pca.res, choice = "var", axes = 1:2)
"
通过fviz_cos2()查看变量在不同主成分的总和
以上是不同变量在第1和第2主成分的加和
如果把axes = 1:2改成axes = 1:4，就会变成都是1
(这个数据最多4个主成分，同一变量的cos2在所有主成分的总和是1)
"
###### 4.2.3向量相关性热图------------------------------------------------------
fviz_pca_var(pca.res, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE 
)


##### 4.3变量对主成分的贡献可视化-----------------------------------------------
res.var$contrib

###### 4.3.1贡献热图------------------------------------------------------------
library("corrplot")
corrplot(res.var$contrib, is.corr=FALSE) 

###### 4.3.2贡献直方图----------------------------------------------------------
# 对第1主成分的贡献
fviz_contrib(pca.res, choice = "var", axes = 1)
# 对第1和第2主成分的贡献
fviz_contrib(pca.res, choice = "var", axes = 1:2)

###### 4.3.3向量贡献热图--------------------------------------------------------
fviz_pca_var(pca.res, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)

# 5.Dimension description(变量与主成分的Pearson相关系数)------------------------
res.desc <- dimdesc(pca.res, axes = c(1,2), proba = 0.05)
res.desc$Dim.1
# [-1, 1]（负相关/正相关）
# 变量对主成分的方向性影响（正/负）
# 和res.var$cos2相关性的区别在，
# res.desc$Dim.1提供p值检验相关性是否显著，变量对主成分的方向性影响（正/负）
# res.var$cos2无p值，纯几何意义，变量在主成分中的代表性强度
# 6.提取样本结果----------------------------------------------------------------
res.ind <- get_pca_ind(pca.res)
head(res.ind$coord)#   就是不同样本在不同主成分的上面的得分score     
##       Dim.1      Dim.2       Dim.3       Dim.4
## 1 -2.264703  0.4800266 -0.12770602 -0.02416820
## 2 -2.080961 -0.6741336 -0.23460885 -0.10300677
## 3 -2.364229 -0.3419080  0.04420148 -0.02837705
## 4 -2.299384 -0.5973945  0.09129011  0.06595556
## 5 -2.389842  0.6468354  0.01573820  0.03592281
## 6 -2.075631  1.4891775  0.02696829 -0.00660818
head(res.ind$contrib)      
##       Dim.1      Dim.2       Dim.3       Dim.4
## 1 1.1715796 0.16806554 0.074085470 0.018798188
## 2 0.9891845 0.33146674 0.250034006 0.341474919
## 3 1.2768164 0.08526419 0.008875320 0.025915633
## 4 1.2077372 0.26029781 0.037858004 0.140000650
## 5 1.3046313 0.30516562 0.001125175 0.041530572
## 6 0.9841236 1.61748779 0.003303827 0.001405371
head(res.ind$cos2)          
##       Dim.1      Dim.2        Dim.3        Dim.4
## 1 0.9539975 0.04286032 0.0030335249 1.086460e-04
## 2 0.8927725 0.09369248 0.0113475382 2.187482e-03
## 3 0.9790410 0.02047578 0.0003422122 1.410446e-04
## 4 0.9346682 0.06308947 0.0014732682 7.690193e-04
## 5 0.9315095 0.06823959 0.0000403979 2.104697e-04
## 6 0.6600989 0.33978301 0.0001114335 6.690714e-06


# 7.样本结果可视化--------------------------------------------------------------
fviz_pca_ind(pca.res)
# 图是通过res.ind$coord里面的坐标实现的，其实就是不同样本在不同主成分的上面的得分score

##### 7.1通过组别上色-----------------------------------------------------------

# 经典图形，是不是很熟悉？
fviz_pca_ind(pca.res,
             geom.ind = "point", # 只显示点，不要文字
             col.ind = iris$Species, # 按照组别上色
             palette = c("#00AFBB", "#E7B800", "#FC4E07"), # 自己提供颜色，或者使用主题
             addEllipses = TRUE, # 添加置信椭圆
             legend.title = "Groups"
)

##### 7.2样本的cos2可视化-------------------------------------------------------
fviz_pca_ind(pca.res,
             col.ind = "cos2", # 按照cos2上色
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE    
)

fviz_pca_ind(pca.res, 
             pointsize = "cos2", # 把cos2的大小映射给点的大小
             pointshape = 21, 
             fill = "#E7B800",
             repel = TRUE 
)
# 使用参数choice = "ind"可视化样本对不同主成分的cos2
# axes选择主成分
fviz_cos2(pca.res, choice = "ind", axes = 1:2)

# 8.样本对主成分的贡献可视化----------------------------------------------------
fviz_contrib(pca.res, choice = "ind", axes = 1:2)

# 9.biplot双标图(同时展示变量和样本和主成分的关系)------------------------------
fviz_pca_biplot(pca.res, 
                col.ind = iris$Species, 
                palette = "jco", 
                addEllipses = TRUE, 
                label = "var",
                col.var = "black", 
                repel = TRUE,
                legend.title = "Species"
) 


fviz_pca_biplot(pca.res, 
                # 组别映射给点的填充色
                geom.ind = "point",
                pointshape = 21,
                addEllipses = TRUE,
                pointsize = 2.5,
                fill.ind = iris$Species,
                col.ind = "black",
                # 通过自定义分组给变量上色
                col.var = factor(c("sepal", "sepal", "petal", "petal")),
                # 自定义图例标题
                legend.title = list(fill = "Species", color = "Clusters"),
                repel = TRUE        
)+
  ggpubr::fill_palette("jco")+ # 选择点的填充色的配色
  ggpubr::color_palette("npg") # 选择变量颜色的配色


fviz_pca_biplot(pca.res, 
                # 自定义样本部分
                geom.ind = "point",
                fill.ind = iris$Species, # 填充色
                col.ind = "black", # 边框色
                pointshape = 21, # 点的形状
                pointsize = 2, 
                palette = "jco",
                addEllipses = TRUE,
                # 自定义变量部分
                alpha.var ="contrib", col.var = "contrib",
                gradient.cols = "RdYlBu",
                
                # 自定义图例标题
                legend.title = list(fill = "Species", color = "Contrib",
                                    alpha = "Contrib")
)

