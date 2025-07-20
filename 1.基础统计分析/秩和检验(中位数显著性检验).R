#秩和检验（Rank Sum Test），也称为Wilcoxon秩和检验或Mann-Whitney U检验
#秩和检验是一种不依赖于总体分布具体形式的非参数检验方法
#在进行t检验或进行方差分析的时候都需要数据群体满足方差一致和是正态分布的数据
#就是在进行行t检验或进行方差分析前都要先进行正态性检验和方差齐性分析
#
#那么针对非正态分布数据群体、方差不齐或样本量容量较小进行分析，比较两个独立样本的位置差异的时候
#可以使用秩和检验
#秩和检验检验的是两个群体的中位数是否有显著差异
#比如针对中美两个国家对工资水平进行检验
#这时候使用方差分析就没有什么意义了
#因为工资的分布水平肯定不是一个正态分布群体
#所以针对是否有显著差异这时候
#应该选择使用秩和检验
#但是两国的人群是不一样多且不一一配对的所以使用 #两独立样本比较的Wilcoxon符号秩检验
#代码如下



        boxplot(death_rate ~ drug, data = mydata)#箱线图代码(death_rate为连续变量，drug分类变量)

        
        
        
#########               配对样本比较的Wilcoxon符号秩检验#############
wilcox.test(A,B,paired = T,alternative = "two.sided",exact = F, correct = F)

        
        

#########                 两独立样本比较的Wilcoxon符号秩检验#############
wilcox.test(RD1,RD2,paired = F, correct = F)


        
        
        
#对于数据框中的数据例如生存率——药物分组（AB组）这样的数据框架
#现在要对AB这两个数据框架进行#两独立样本比较的Wilcoxon符号秩检验
#第一种方法
wilcox.test(death_rate ~ drug, data = mydata, 
    alternative = "two.sided", exact = FALSE, correct = FALSE)
#使用带有death_rate ~ drug的公式接口的写法，但是这样就不可以在（）中再出现paired = F了，因为重复
#在death_rate ~ drug出现的时候就已经默认是drug组中互相都是独立样本了
#第二种方法
drug_A <- mydata$death_rate[mydata$drug == "Drug_A"]
drug_B <- mydata$death_rate[mydata$drug == "drug_B"]
wilcox.test(drug_A,drug_B,paired = F, correct = F)
#且如果数据框中的AB组是一一对应的就是配对的那么也只能使用第二种方法，因为death_rate ~ drug
#会默认是drug是独立样本所以后续就会报错
#如果AB是配对样本就
drug_A <- mydata$death_rate[mydata$drug == "Drug_A"]
drug_B <- mydata$death_rate[mydata$drug == "drug_B"]
wilcox.test(drug_A,drug_B,paired = T,alternative = "two.sided",exact = F, correct = F)






#########        完全随机设计多个样本比较的 Kruskal-Wallis H 检验
kruskal.test(death_rate ~ drug, data = mydata)
#用于确定三个或更多独立组的中位数之间是否存在统计上的显著差异


