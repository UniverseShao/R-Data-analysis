library(survival)
library(survminer)
str(lung)
fit <- survfit(Surv(time, status) ~ sex, data = lung)
# 1.基本的生存曲线-----------------------------------------------------------------
ggsurvplot(fit, data = lung)
# 2.字体及标题更改-----------------------------------------------------------------
ggsurvplot(fit, data = lung,
           title = "Survival curves", 
           subtitle = "Based on Kaplan-Meier estimates",
           caption = "created with survminer",
           font.title = c(16, "bold", "darkblue"), # 大小、粗细、颜色
           font.subtitle = c(15, "bold.italic", "purple"),
           font.caption = c(14, "plain", "orange"),
           font.x = c(14, "bold.italic", "red"),
           font.y = c(14, "bold.italic", "darkred"),
           font.tickslab = c(12, "plain", "darkgreen"))
# 3.累积风险曲线------------------------------------------------------------------------------------------------------
ggsurvplot(fit,
           fun = "cumhaz", 
           conf.int = TRUE, # 可信区间
           palette = "lancet", # 支持ggsci配色，自定义颜色，brewer palettes中的配色，等
           ggtheme = theme_bw() # 支持ggplot2及其扩展包的主题
)
#累积风险图（cumulative hazard plot）的 Y 轴表示的是累积风险函数（cumulative hazard function）
#即H(t)
#描述的是从起始时间到  时刻，某个事件发生的累积风险总和
#是通过对瞬时风险函数在时间上的积分得到的
#h(t)是已生存到 t 时刻的对象在 t 时刻发生事件的速率

###### 3.1累积风险与 HR（风险比）的关系------------------------------------------------------------------------------------
#HR 是比较两组（如治疗组与对照组）瞬时风险率h(t)的比值
#HR = h1(t)/h2(t)
#对 HR 的理解，可以类比 OR 的概念，只是 HR 是在时间上的OR，而 OR 无时间信息

#意味着在比例风险假设成立的情况下，累积风险图中两组曲线的斜率比值大致等于 HR

# 4.累积事件曲线----------------------------------------------------------------------------------
ggsurvplot(fit,
           fun = "event", 
           conf.int = TRUE, # 可信区间
           palette = "grey",
           ggtheme = theme_pubclean() 
)
# 5.增加 risk table--------------------------------------------------------------------------------------
ggsurvplot(
  fit,
  data = lung,
  size = 1,                 # 更改线条粗细
  # 配色方案，支持ggsci配色，自定义颜色，brewer palettes中的配色，等
  palette = "lancet",
  conf.int = TRUE,          # 可信区间
  pval = TRUE,              # log-rank P值，也可以提供一个数值
  pval.method = TRUE,       # 计算P值的方法，可参考https://rpkgs.datanovia.com/survminer/articles/Specifiying_weights_in_log-rank_comparisons.html
  log.rank.weights = "1",
  risk.table = TRUE,        # 增加risk table
  risk.table.col = "strata",# risk table根据分组使用不同颜色
  legend.labs = c("Male", "Female"),    # 图例标签
  risk.table.height = 0.25, # risk table高度
  ggtheme = theme_classic2()      # 主题，支持ggplot2及其扩展包的主题
)
###### 5.1更改risk table字体------------------------------------------------------------------------------------
ggsurvplot(fit, data = lung,
           title = "Survival curves", subtitle = "Based on Kaplan-Meier estimates",
           caption = "created with survminer",
           font.title = c(16, "bold", "darkblue"),
           font.subtitle = c(15, "bold.italic", "purple"),
           font.caption = c(14, "plain", "orange"),
           font.x = c(14, "bold.italic", "red"),
           font.y = c(14, "bold.italic", "darkred"),
           font.tickslab = c(12, "plain", "darkgreen"),
           ########## risk table #########,
           risk.table = TRUE,
           risk.table.title = "Note the risk set sizes",
           risk.table.subtitle = "and remember about censoring.",
           risk.table.caption = "source code: website.com",
           risk.table.height = 0.45)


# 6.调整x轴使得曲线更合理(这串好看)--------------------------------------------------
ggsurvplot(
  fit,
  data = lung,
  size = 1,                 # 更改线条粗细
  # 配色方案，支持ggsci配色，自定义颜色，brewer palettes中的配色，等
  palette = "lancet",
  conf.int = TRUE,          # 可信区间
  pval = TRUE,              # log-rank P值，也可以提供一个数值
  pval.method = TRUE,       # 计算P值的方法，可参考https://rpkgs.datanovia.com/survminer/articles/Specifiying_weights_in_log-rank_comparisons.html
  log.rank.weights = "1",
  risk.table = TRUE,        # 增加risk table
  risk.table.col = "strata",# risk table根据分组使用不同颜色
  legend.labs = c("Male", "Female"),    # 图例标签
  risk.table.height = 0.25, # risk table高度
  xlim = c(0,500),         # 横坐标轴范围，相当于局部放大
  xlab = "Time in days",   # 横坐标标题
  break.time.by = 100,     # 横坐标刻度
  ggtheme = theme_classic2()      # 主题，支持ggplot2及其扩展包的主题
)

# 7.增加删失事件表ncensor plot--------------------------------------------------

ggsurvplot(fit, data = lung, risk.table = TRUE, ncensor.plot = TRUE)

ggsurvplot(
  fit,
  data = lung,
  size = 1,                 # 更改线条粗细
  # 配色方案，支持ggsci配色，自定义颜色，brewer palettes中的配色，等
  palette = "lancet",
  conf.int = TRUE,          # 可信区间
  pval = TRUE,              # log-rank P值，也可以提供一个数值
  pval.method = TRUE,       # 计算P值的方法，可参考https://rpkgs.datanovia.com/survminer/articles/Specifiying_weights_in_log-rank_comparisons.html
  log.rank.weights = "1",
  ncensor.plot = TRUE,
  risk.table = TRUE,        # 增加risk table
  risk.table.col = "strata",# risk table根据分组使用不同颜色
  legend.labs = c("Male", "Female"),    # 图例标签
  risk.table.height = 0.25, # risk table高度
  xlim = c(0,500),         # 横坐标轴范围，相当于局部放大
  xlab = "Time in days",   # 横坐标标题
  break.time.by = 100,     # 横坐标刻度
  ggtheme = theme_classic2()      # 主题，支持ggplot2及其扩展包的主题
)

##### 7.1更加ncensor plot字体---------------------------------------------------

ggsurvplot(fit, data = lung,
           title = "Survival curves", subtitle = "Based on Kaplan-Meier estimates",
           caption = "created with survminer",
           font.title = c(16, "bold", "darkblue"),
           font.subtitle = c(15, "bold.italic", "purple"),
           font.caption = c(14, "plain", "orange"),
           font.x = c(14, "bold.italic", "red"),
           font.y = c(14, "bold.italic", "darkred"),
           font.tickslab = c(12, "plain", "darkgreen"),
           ########## risk table #########,
           risk.table = TRUE,
           risk.table.title = "Note the risk set sizes",
           risk.table.subtitle = "and remember about censoring.",
           risk.table.caption = "source code: website.com",
           risk.table.height = 0.2,
           ## ncensor plot ##
           ncensor.plot = TRUE,
           ncensor.plot.title = "Number of censorings",
           ncensor.plot.subtitle = "over the time.",
           ncensor.plot.caption = "data available at data.com",
           ncensor.plot.height = 0.25)


# 8.超级无敌精细化自定设置--------------------------------------------------------------------------------------------------
ggsurv <- ggsurvplot(
  fit,                     
  data = lung,             
  risk.table = TRUE,       
  pval = TRUE,             
  conf.int = TRUE,         
  palette = c("#E7B800", "#2E9FDF"),
  xlim = c(0,500),         
  xlab = "Time in days",   
  break.time.by = 100,     
  ggtheme = theme_light(), 
  risk.table.y.text.col = T,
  risk.table.height = 0.25, 
  risk.table.y.text = FALSE,
  ncensor.plot = TRUE,      
  ncensor.plot.height = 0.25,
  conf.int.style = "step",  # customize style of confidence intervals
  surv.median.line = "hv",  
  legend.labs = c("Male", "Female")    
)
ggsurv

##### 8.1定义图像更改函数----------------------------------------------------------------------------------
customize_labels <- function (p, font.title = NULL,
                              font.subtitle = NULL, font.caption = NULL,
                              font.x = NULL, font.y = NULL, font.xtickslab = NULL, font.ytickslab = NULL)
{
  original.p <- p
  if(is.ggplot(original.p)) list.plots <- list(original.p)
  else if(is.list(original.p)) list.plots <- original.p
  else stop("Can't handle an object of class ", class (original.p))
  .set_font <- function(font){
    font <- ggpubr:::.parse_font(font)
    ggtext::element_markdown (size = font$size, face = font$face, colour = font$color)
  }
  for(i in 1:length(list.plots)){
    p <- list.plots[[i]]
    if(is.ggplot(p)){
      if (!is.null(font.title)) p <- p + theme(plot.title = .set_font(font.title))
      if (!is.null(font.subtitle)) p <- p + theme(plot.subtitle = .set_font(font.subtitle))
      if (!is.null(font.caption)) p <- p + theme(plot.caption = .set_font(font.caption))
      if (!is.null(font.x)) p <- p + theme(axis.title.x = .set_font(font.x))
      if (!is.null(font.y)) p <- p + theme(axis.title.y = .set_font(font.y))
      if (!is.null(font.xtickslab)) p <- p + theme(axis.text.x = .set_font(font.xtickslab))
      if (!is.null(font.ytickslab)) p <- p + theme(axis.text.y = .set_font(font.ytickslab))
      list.plots[[i]] <- p
    }
  }
  if(is.ggplot(original.p)) list.plots[[1]]
  else list.plots
}

##### 8.2自行更改---------------------------------------------------------------------------------
# 更改生存曲线的标签
ggsurv$plot <- ggsurv$plot + labs(
  title    = "Survival curves",
  subtitle = "Based on Kaplan-Meier estimates",
  caption  = "created with survminer"
)

# 更改risk table的标签
ggsurv$table <- ggsurv$table + labs(
  title    = "Note the risk set sizes",
  subtitle = "and remember about censoring.",
  caption  = "source code: website.com"
)

# 更改ncensor plot的标签 
ggsurv$ncensor.plot <- ggsurv$ncensor.plot + labs(
  title    = "Number of censorings",
  subtitle = "over the time.",
  caption  = "source code: website.com"
)

# 更改生存曲线，risk table，ncensor plot的字体大小、类型、颜色

ggsurv <- customize_labels(
  ggsurv,
  font.title    = c(16, "bold", "darkblue"),
  font.subtitle = c(15, "bold.italic", "purple"),
  font.caption  = c(14, "plain", "orange"),
  font.x        = c(14, "bold.italic", "red"),
  font.y        = c(14, "bold.italic", "darkred"),
  font.xtickslab = c(12, "plain", "darkgreen")
)
ggsurv
# 9.多个组的生存曲线------------------------------------------------------------------------------
rm(list = ls())
library(survival)
library(survminer)

psych::headTail(colon)
# 两个分类变量
fit2 <- survfit( Surv(time, status) ~ rx + obstruct, data = colon )
# 结果会给出所有组合的生存曲线
ggsurvplot(fit2, pval = TRUE, 
           risk.table = TRUE,
           risk.table.height = 0.3
) 

# 10.多个分类变量分面绘制-----------------------------------------------------------
#首先构建生存函数
fit3 <- survfit(Surv(time, status) ~ sex + rx + adhere, data = colon )
#把生存曲线保存为一个对象
ggsurv <- ggsurvplot(fit3, data = colon,
                     fun = "cumhaz", conf.int = TRUE,
                     risk.table = TRUE, risk.table.col="strata",
                     ggtheme = theme_bw())
# 分面累积风险曲线
curv_facet <- ggsurv$plot + facet_grid(rx ~ adhere)
curv_facet
# 分面risk table，和上面的累积风险曲线分面方法一样
ggsurv$table + facet_grid(rx ~ adhere, scales = "free")+
  theme(legend.position = "none")
# risk table另一种分面方法，由于有3个分类变量，可以选择自己需要的分面方法
tbl_facet <- ggsurv$table + facet_grid(.~ adhere, scales = "free")
tbl_facet + theme(legend.position = "none")

# 重新安排下布局，把生存曲线和risk table画在一起
g2 <- ggplotGrob(curv_facet)
g3 <- ggplotGrob(tbl_facet)
min_ncol <- min(ncol(g2), ncol(g3))
g <- gridExtra::gtable_rbind(g2[, 1:min_ncol], g3[, 1:min_ncol], size="last")
g$widths <- grid::unit.pmax(g2$widths, g3$widths)
grid::grid.newpage()
grid::grid.draw(g)

# 11.ggsurvplot_facet()根据变量分组绘制----------------------------------------------------------------------------------
fit <- survfit( Surv(time, status) ~ sex, data = colon )
##### 11.1变量分组，展示每个组内的生存曲线----------------------------------
ggsurvplot_facet(fit, colon, 
                 facet.by = "rx",
                 palette = "jco", 
                 pval = TRUE)
##### 11.2根据多个变量进行分组展示--------------------------------------------------------------------
ggsurvplot_facet(fit, colon, facet.by = c("rx", "adhere"),
                 palette = "jco", pval = TRUE)
fit2 <- survfit( Surv(time, status) ~ sex + rx, data = colon )
ggsurvplot_facet(fit2, colon, facet.by = "adhere",
                 palette = "jco", pval = TRUE)

# 12.分类绘制和分组绘制的区别----------------------------------------------------------------------------------------------------------
#要想要进行分类绘制，如分成四类，就是有四个线，那就是Surv(time, status) ~ sex + rx
#如果想要进行分组绘制，那无论多少类，和 ~后的变量没有关系，是facet.by =
#是=后面加上分组变量
#简单点，就是一个颜色的线一般就是代表一个类，不同的框就是一个组
# 10节中就是特殊的画法，就是将多种类的线单框只画两个，放在多个框内，实现多种类的线的表达

# 13.同时绘制多个生存函数----------------------------------------------------------------------------------------------
data(colon)
f1 <- survfit(Surv(time, status) ~ adhere, data = colon)
f2 <- survfit(Surv(time, status) ~ rx, data = colon)
fits <- list(adhere = f1, rx = f2)
# 一下子画好！在循环出图时有用处
legend.title <- list("adhere", "rx")
ggsurvplot_list(fits, colon, legend.title = legend.title)

# 14.根据某一个变量分组绘制--------------------------------------------------------------------------------------------
rm(list = ls())

fit <- survfit( Surv(time, status) ~ sex, data = colon )

ggsurv.list <- ggsurvplot_group_by(fit, colon, group.by = "rx", risk.table = TRUE,
                                   pval = TRUE, conf.int = TRUE, palette = "jco")
names(ggsurv.list)

ggsurv.list

#图形和上面的分面展示中的ggsurvplot_facet画出来的图形是一样的，区别就是一个是分面，这个是分开多个图形

##### 14.1根据多个变量进行分组------------------------------------------------------------------------------------------------
# Visualize: grouped by treatment rx and adhere
ggsurv.list <- ggsurvplot_group_by(fit, colon, group.by = c("rx", "adhere"),
                                   risk.table = TRUE,
                                   pval = TRUE, conf.int = TRUE, palette = "jco")
# 6张图的名字，图没有画出来，感兴趣的可以自己试试看
names(ggsurv.list)
ggsurv.list

# 15.在原有生存曲线的基础上增加-------------------------------------------------------------------

##### 15.1原图的基础上添加新的生存曲线图--------------------------------------------------------------

library(survival)

# 注意这里的surv_fit，是survfit的封装
fit <- surv_fit(Surv(time, status) ~ sex, data = lung)

# Visualize survival curves
ggsurvplot(fit, data = lung,
           risk.table = TRUE, pval = TRUE,
           surv.median.line = "hv", palette = "jco")
# Add survival curves of pooled patients (Null model)
# Use add.all = TRUE option
ggsurvplot(fit, data = lung,
           risk.table = TRUE, pval = TRUE,
           surv.median.line = "hv", palette = "jco",
           add.all = TRUE)


# 16.多个生存函数画在一起------------------------------------------------------------------------------------------------

rm(list = ls())
# 构建一个示例数据集
set.seed(123)
demo.data <- data.frame(
  os.time = colon$time,
  os.status = colon$status,
  pfs.time = sample(colon$time),
  pfs.status = colon$status,
  sex = colon$sex, rx = colon$rx, adhere = colon$adhere
)

# 总体的PFS和OS生存曲线
pfs <- survfit( Surv(pfs.time, pfs.status) ~ 1, data = demo.data)
os <- survfit( Surv(os.time, os.status) ~ 1, data = demo.data)
# ~ 1指的是计算全体患者的总体生存曲线
# Combine on the same plot
fit <- list(PFS = pfs, OS = os)
ggsurvplot_combine(fit, demo.data)





