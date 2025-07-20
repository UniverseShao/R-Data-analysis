# 1.平滑曲线--------------------------------------------------------------------
##### 1.1加载R包和数据----------------------------------------------------------
rm(list = ls())
library(timeROC)
library(survival)
load(file = "C:\\Users\\Administrator\\Desktop\\R语言脚本(SWY精心编辑版本)\\临床预测模型\\datasets\\timeROC.RData")
##### 1.2多个时间点ROC----------------------------------------------------------

ROC <- timeROC(T = df$futime,   
               delta = df$event,   
               marker = df$riskScore,   
               cause = 1,                
               weighting = "marginal",   
               times = c(1, 2, 3),       
               iid = TRUE)

ROC   #查看模型变量信息

#画图
plot(ROC, 
     time=1, col="red", lwd=2, title = "")   #time是时间点，col是线条颜色
plot(ROC,
     time=2, col="blue", add=TRUE, lwd=2)    #add指是否添加在上一张图中
plot(ROC,
     time=3, col="orange", add=TRUE, lwd=2)

#添加标签信息
legend("bottomright",
       c(paste0("AUC at 1 year: ",round(ROC[["AUC"]][1],2)), 
         paste0("AUC at 2 year: ",round(ROC[["AUC"]][2],2)), 
         paste0("AUC at 3 year: ",round(ROC[["AUC"]][3],2))),
       col=c("red", "blue", "orange"),
       lty=1, lwd=2,bty = "n")   

##### 1.3提取数据---------------------------------------------------------------
df_plot <- data.frame(tpr = as.numeric(ROC$TP),
                      fpr = as.numeric(ROC$FP),
                      year = rep(c("1-year","2-year","3-year"),each = nrow(ROC$TP)))

head(df_plot)

##### 1.4画平滑曲线-------------------------------------------------------------

library(ggplot2)

p <- ggplot(df_plot, aes(fpr, tpr, color = year)) +
  geom_smooth(se=FALSE, size=1.2)+ # 这就是平滑曲线的关键
  geom_abline(slope = 1, intercept = 0, color = "grey10",linetype = 2) +
  scale_color_manual(values = c("#E41A1C","#377EB8","#4DAF4A"),
                     name = NULL, 
                     labels = c(paste0("AUC at 1 year: ",round(ROC[["AUC"]][1],2)), 
                                paste0("AUC at 2 year: ",round(ROC[["AUC"]][2],2)), 
                                paste0("AUC at 3 year: ",round(ROC[["AUC"]][3],2)))
  ) + 
  coord_fixed(ratio = 1) +
  labs(x = "1 - Specificity", y = "Sensitivity") +
  theme_minimal(base_size = 14, base_family = "sans") +
  theme(legend.position = c(0.7,0.15), 
        panel.border = element_rect(fill = NA),
        axis.text = element_text(color = "black"))

p

# 2.找最佳截点
library(survivalROC)

# 1年的最佳截点
roc1 <- survivalROC(Stime = df$futime,
                    status = df$event,
                    marker = df$riskScore,
                    method = "KM",
                    predict.time = 1 # 时间选1年
)

roc1$cut.values[which.max(roc1$TP - roc1$FP)] # 最佳截点的值，基于约登指数计算出来
## [1] -0.07986499
