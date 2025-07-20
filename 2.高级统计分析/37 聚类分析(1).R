# 1.系统聚类（层次聚类,Hierarchical clustering）--------------------------------
data(nutrient, package = "flexclust")
row.names(nutrient) <- tolower(row.names(nutrient))
#将数据框 nutrient 的行名（rownames）全部转换为小写字母
dim(nutrient) 
psych::headTail(nutrient)

##### 1.1 聚类前先进行标准化----------------------------------------------------
nutrient.scaled <- scale(nutrient)
##### 1.2聚类分析---------------------------------------------------------------
h.clust <- hclust(dist(nutrient.scaled,method = "euclidean"), # 计算距离有不同方法
                  method = "average" # 层次聚类有不同方法
)

##### 1.3画图-------------------------------------------------------------------

plot(h.clust,hang = -1,main = "层次聚类", sub="", 
     xlab="", cex.lab = 1.0, cex.axis = 1.0, cex.main = 2)

##### 1.4NbClust包实现----------------------------------------------------------
library(NbClust)
nc <- NbClust(nutrient.scaled, distance = "euclidean",# 欧氏距离，多维空间中两点之间直线距离的经典方法
              min.nc = 2, # 最小聚类数
              max.nc = 10, # 最大聚类树
              method = "average"
)

###### 1.4.1条形图查看评判准则具体数量------------------------------------------
dev.off()# 关闭当前图形设备
# 重置为单图布局
par(mfrow=c(1,1))
barplot(table(nc$Best.nc[1,]),
        xlab = "聚类数目",
        ylab = "评判准则个数"
)
##### 1.5绘制聚类森林图---------------------------------------------------------
# 这里我们选择5个聚类好看
# 把聚类树划分为5类
cluster <- cutree(h.clust, k=5)

# 查看每一类有多少例
table(cluster)

plot(h.clust, hang = -1,main = "",xlab = "")
rect.hclust(h.clust, k=5) # 添加矩形，方便观察

# 2.快速聚类（划分聚类,partitioning clustering）--------------------------------
##### 2.1K-means聚类------------------------------------------------------------

##### 2.1.1首先确认类数目(先用NbClust)-----------------------------------------------
data(wine, package = "rattle")
df <- scale(wine[,-1])

psych::headTail(df)
library(NbClust)
set.seed(123)
nc <- NbClust(df, min.nc = 2, max.nc = 15, method = "kmeans")# 方法选择kmeans

###### 2.1.2画图查看聚类结果----------------------------------------------------
par(mfrow=c(1,1))
table(nc$Best.nc[1,])
barplot(table(nc$Best.nc[1,]),
        xlab = "聚类数目",
        ylab = "评判准则个数"
)

###### 2.1.3factoextra包查看最佳聚类个数fviz_cluster()(绘图)--------------------
library(factoextra)
set.seed(123)
fviz_nbclust(df, kmeans, k.max = 15)

###### 2.1.4进行K均值聚类-------------------------------------------------------
set.seed(123)
fit.km <- kmeans(df, centers = 3, nstart = 25)
fit.km
###### 2.1.5结果解释------------------------------------------------------------
"
K-means clustering with 3 clusters of sizes 51, 62, 65

Cluster means:
     Alcohol      Malic        Ash Alcalinity   Magnesium     Phenols  Flavanoids
1  0.1644436  0.8690954  0.1863726  0.5228924 -0.07526047 -0.97657548 -1.21182921
2  0.8328826 -0.3029551  0.3636801 -0.6084749  0.57596208  0.88274724  0.97506900
3 -0.9234669 -0.3929331 -0.4931257  0.1701220 -0.49032869 -0.07576891  0.02075402
  Nonflavanoids Proanthocyanins      Color        Hue   Dilution    Proline
1    0.72402116     -0.77751312  0.9388902 -1.1615122 -1.2887761 -0.4059428
2   -0.56050853      0.57865427  0.1705823  0.4726504  0.7770551  1.1220202
3   -0.03343924      0.05810161 -0.8993770  0.4605046  0.2700025 -0.7517257
解读：
数据已标准化（均值为0，标准差为1），因此：
正值：该变量在簇中的值高于数据集平均水平。
负值：该变量在簇中的值低于平均水平。
Clustering vector:
  [1] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
 [42] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 3 1 3 3 3 3 3 3 3 3 3 3 3 2 3 3 3 3 3 3 3 3
 [83] 3 1 3 3 3 3 3 3 3 3 3 3 3 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 1 3 3 2 3
[124] 3 3 3 3 3 3 3 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
[165] 1 1 1 1 1 1 1 1 1 1 1 1 1 1

K均值聚类聚为3类
每一类数量分别是51,62,65
还给出了聚类中心，每一个观测分别属于哪一个类
Available components:

[1] cluster      centers     totss        withinss     tot.withinss
[6] betweenss    size        iter         ifault     
可提取的组件（Available Components）
**cluster**：每个样本的聚类标签（同Clustering vector）
**centers**：各簇的中心点坐标（同Cluster means）
**size**：各簇的样本量
**tot.withinss**：所有簇的WCSS总和（=326.35+385.70+558.70）
**betweenss**：组间方差（between_SS）
"
###### 2.1.6factoextra包fviz_cluster()可视化结果--------------------------------
fviz_cluster(fit.km, data = df)

#图像细节调整-------------------------------------------------------------------
fviz_cluster(fit.km, data = df, 
             ellipse = T, # 增加椭圆
             ellipse.type = "t", # 椭圆类型
             geom = "point", # 只显示点不要文字
             palette = "lancet", # 支持超多配色方案
             ggtheme = theme_bw() # 支持更换主题
)


###### 2.1.7K-means方法和层次聚类区别-------------------------------------------

"
特征          **method = kmeans**         **method = average**

算法类型      划分式聚类（Partitioning）	层次聚类（Hierarchical）

核心思想      通过迭代优化将数据          通过逐层合并或分裂形成树状结构
              划分为K个球形簇	

距离计算      默认欧氏距离	              平均连接法（Average Linkage）

是否需要指定K	是(NbClust会评估不同K值)	  否(但最终仍需选择切割层数)

输出结果      直接给出最佳K值和聚类标签	  生成树状图，需手动切割或自动选择簇数
"

##### 2.2围绕中心点的划分PAM----------------------------------------------------

###### 2.2.1同K均值聚类区别-----------------------------------------------------

"
K均值聚类是基于均值的，所以对异常值很敏感。
一个更稳健的方法是围绕中心点的划分（PAM）。
用一个最有代表性的观测值代表这一类(有点类似于主成分)。
K均值聚类一般使用欧几里得距离，而PAM可以使用任意的距离来计算。
因此，PAM可以容纳混合数据类型，并且不仅限于连续变量


对比维度           K均值聚类（K-Means）      围绕中心点的划分（PAM） 
核心思想           基于均值（Mean）划分簇	   基于中心点（Medoid，即实际存在的样本点）
对异常值的敏感性   敏感（均值易受极端值影响）稳健（中心点是真实样本，不受极端值干扰）。
距离度量	         默认欧几里得距离	         支持任意距离，
                   ，仅适用于连续变量        可处理混合数据类型（连续+分类变量）

适用数据类型       仅限连续型数值数据        连续型、分类型或混合数据
                                             （如临床数据、问卷数据）。
簇形状适应性       仅适合球形簇	             可适应任意形状簇（如非球形、流形结构）
R函数	             kmeans()（基础包）	       cluster::pam() 或 fpc::pamk()
典型应用场景       客户分群、图像压缩	       医学诊断、异常检测、混合数据聚类
"

library(cluster)

set.seed(123)
fit.pam <- pam(wine[-1,], k=3 # 聚为3类
               , stand = T # 聚类前进行标准化
)
fit.pam


clusplot(fit.pam, main = "PAM cluster")

fviz_cluster(fit.pam, 
             ellipse = T, # 增加椭圆
             ellipse.type = "t", # 椭圆类型
             geom = "point", # 只显示点不要文字
             palette = "aaas", # 支持超多配色方案
             ggtheme = theme_bw() # 支持更换主题
)

