# 加个背景，更改颜色
op <- par(bg = "grey90")

plot(h.clust, main = "层次聚类", sub="", xlab = "",
     col = "#487AA1", col.main = "#45ADA8", col.lab = "#7C8071",
     col.axis = "#F38630", lwd = 2, lty = 1, hang = -1, axes = FALSE)
# add axis
axis(side = 2, at = 0:5, col = "#F38630",
     labels = FALSE, lwd = 2)
# add text in margin
mtext(0:5, side = 2, at = 0:5,
      line = 1, col = "#A38630", las = 2)



par(op)


# 扭曲分支图
dhc <- as.dendrogram(h.clust)
plot(dhc,type = "triangle") # 比如换个类型


# 修改标签的颜色
# 按照上面画出来的结果，我们可以分为5类，所以准备好5个颜色
labelColors = c("#CDB380", "#036564", "#EB6841", "#EDC951", "#487AA1")

# 把聚类树分为5个类
clusMember <- cutree(h.clust,k=5)

# 给标签增加不同的颜色
colLab <- function(n) {
  if (is.leaf(n)) {
    a <- attributes(n)
    labCol <- labelColors[clusMember[which(names(clusMember) == a$label)]]
    attr(n, "nodePar") <- c(a$nodePar,
                            list(cex=1.5, # 节点形状大小
                                 pch=20, # 节点形状
                                 col=labCol, # 节点颜色
                                 lab.col=labCol, # 标签颜色
                                 lab.font=2, # 标签字体，粗体斜体粗斜体
                                 lab.cex=1 # 标签大小
                            )
    )
  }
  n
}

# 把自定义标签颜色应用到聚类树中
diyDendro = dendrapply(dhc, colLab)    

# 画图
plot(diyDendro, main = "DIY Dendrogram")  

# 加图例
legend("topright", 
       legend = c("Cluster 1","Cluster 2","Cluster 3","Cluster 4","Cluster 5"), 
       col = c("#CDB380", "#036564", "#EB6841", "#EDC951", "#487AA1"), 
       pch = c(20,20,20,20,20), bty = "n", pt.cex = 2, cex = 1 , 
       text.col = "black", horiz = FALSE, inset = c(0, 0.1))

