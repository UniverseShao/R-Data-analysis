#如何开始探索缺失值####-------------------------------------------------------------------------------
#R包visdat可以展示缺失值数据

#vis_dat()和vis_miss()####-------------------------------------------------------------------------------

library(visdat)
vis_dat(airquality)
vis_miss(airquality)

#探索缺失值的关系####-------------------------------------------------------------------------------
#naniar包####-------------------------------------------------------------------------------
#naniar包可知道这些缺失值之间有什么关系

# 先画一幅默认的图
library(ggplot2)
ggplot(airquality, 
       aes(x = Solar.R, 
           y = Ozone)) + 
  geom_point()
#这幅图会直接把缺失值删掉，并不能知道缺失值的情况

# 使用naniar可视化缺失值####-------------------------------------------------------------------------------
library(naniar)

ggplot(airquality, 
       aes(x = Solar.R, 
           y = Ozone)) + 
  geom_miss_point()
#支持刻面
ggplot(airquality, 
       aes(x = Solar.R, 
           y = Ozone)) + 
  geom_miss_point() + 
  facet_wrap(~Month)
#支持主题
ggplot(airquality, 
       aes(x = Solar.R, 
           y = Ozone)) + 
  geom_miss_point() + 
  facet_wrap(~Month) + 
  theme_dark()

#可视化变量中的缺失值####-------------------------------------------------------------------------------
gg_miss_var(airquality)#可以看到每一列有多少缺失值
#支持更换主题
gg_miss_var(airquality) + theme_bw() 
#支持ggplot2的各种特性
gg_miss_var(airquality) + labs(y = "Look at all the missing ones")
#支持分面
gg_miss_var(airquality, facet = Month)
#其他可视化方法：
gg_miss_upset(airquality)
gg_miss_upset(riskfactors)

#使用NA替换缺失值####-------------------------------------------------------------------------------
#很多缺失值用NA来表示可能会更加方便，比如N/A、N A，Not Available，-999等
#naniar中提供了replace_with_na函数把这些缺失值替换为NA
#replace_with_na -------------------------------------------------------------------------------
#基础替换函数
#replace_with_na(data, replace = list(var1 = value_to_replace, var2 = value_to_replace))
library(naniar)
# 将数据框中Ozone列的-99和SolarR列的999替换为NA
airquality_clean <- airquality %>% 
  replace_with_na(replace = list(Ozone = -999, Solar.R = 999))


#replace_with_na_all-------------------------------------------------------------------------------
#全部列替换
#replace_with_na_all(data, condition = ~.x == value_to_replace)
# 将所有列中的-99替换为NA
airquality_clean <- airquality %>% 
  replace_with_na_all(condition = ~.x == -999)
# 使用多个条件
airquality_clean <- airquality %>% 
  replace_with_na_all(condition = ~.x %in% c(-999, 999, "N/A"))



#replace_with_na_at-------------------------------------------------------------------------------
#指定列替换
#replace_with_na_at(data, .vars = vars(var1, var2), condition = ~.x == value_to_replace)
# 只在Ozone和SolarR列中将-99替换为NA
airquality_clean <- airquality %>% 
  replace_with_na_at(.vars = vars(Ozone, Solar.R),
                     condition = ~.x == -999)



#replace_with_na_if-------------------------------------------------------------------------------
#条件列替换
#replace_with_na_if(data, predicate = is.numeric, condition = ~.x == value_to_replace)
# 在所有数值型列中将-99替换为NA
airquality_clean <- airquality %>% 
  replace_with_na_if(.predicate = is.numeric,
                     condition = ~.x == -999)
# 在所有字符型列中将"N/A"替换为NA
airquality_clean <- airquality %>% 
  replace_with_na_if(.predicate = is.character,
                     condition = ~.x == "N/A")


#整洁的缺失数据：shadow matrix-------------------------------------------------------------------------------
#as_shadow()函数直接以数据框的形式返回是否是缺失值
as_shadow(airquality)
#把数据框形式的缺失值数据和原数据整合到一起：bind_shadow():
aq_shadow <- bind_shadow(airquality)
#将缺失值的影子向量和原数据整合一起
library(dplyr)
glimpse(aq_shadow)

#nabular()函数达到同样的效果：
aq_nab <- nabular(airquality)
glimpse(aq_nab)
#两种方法得到的东西是一样
all.equal(aq_shadow, aq_nab)

#对缺失值做一些简单的统计
airquality %>%
  bind_shadow() %>%
  group_by(Ozone_NA) %>%
  summarise_at(.vars = "Solar.R",
               .funs = c("mean", "sd", "var", "min", "max"),
               na.rm = TRUE)
#按照Ozone是否缺失对Solar.R进行数理统计
#结果显示没有缺失的Solar.R均值是185 缺失的Solar.R均值是190

#画图展示缺失值和非缺失值的数据分布
ggplot(aq_shadow,
       aes(x = Temp,
           colour = Ozone_NA)) + 
  geom_density()


#可视化插补后的缺失值-------------------------------------------------------------------------------
library(simputation)
library(dplyr)

airquality %>%
  impute_lm(Ozone ~ Temp + Wind) %>%#这是使用Temp(温度) 和 Wind(风速) 作为预测变量
  #建立线性回归模型来预测 Ozone(臭氧) 的缺失值
  ggplot(aes(x = Temp,
             y = Ozone)) + 
  geom_point()

aq_shadow %>%
  as.data.frame() %>% 
  impute_lm(Ozone ~ Temp + Wind) %>%
  ggplot(aes(x = Temp,
             y = Ozone,
             colour = Ozone_NA)) + #显示出哪些是插补的
  geom_point()

#缺失值的汇总函数-------------------------------------------------------------------------------
#主要通过n_miss()和n_complete()统计缺失值和非缺失值：
n_miss(airquality)#统计缺失值的数量
n_complete(airquality)
n_complete(airquality$Ozone)
miss_case_summary(airquality) # 每行的缺失值
miss_case_table(airquality)#汇总表格形式的
prop_miss_case(airquality)# 含有缺失值的行占比
pct_miss_case(airquality)# 含有缺失值的行占比变成百分比形式
prop_miss_var(airquality) # 含有缺失值的列占比
pct_miss_var(airquality)#变成百分比形式
miss_var_summary(airquality)#针对列的缺失值汇总
miss_var_table(airquality)#针对列的缺失值汇总表格形式
add_prop_miss(airquality)#计算数据框中每一行的缺失值比例，并添加为新列
#模型化缺失值-------------------------------------------------------------------------------
#模型化缺失值——添加缺失值新列-------------------------------------------------------------------------------
airquality %>%
  add_prop_miss() %>%
  head()

airquality_sample <- airquality %>%
  add_prop_miss() %>%
  head()  # 这里返回的是只有6行的数据框
#仅仅保留6行拥有新列的数据框
#因为head()不仅仅是查看函数，是返回一个新的数据框
#所有使用head()函数后会有6行截断的操作

airquality_all <- airquality %>%
  add_prop_miss()
View(airquality_all)
#这才是为所有的行都添加一个新列

airquality_sample <- airquality %>%
  add_prop_miss() %>%
  head()
airquality %>%
  add_prop_miss() %>%
  head()
#这两串代码的区别是前者是永久保存，后者是临时查看
#因为在没有airquality_sample <- 存在的时候，只有%>%
#那就只有数据传递的功能，没有最后赋值给对象的操作
#所以只是临时查看
#但是一旦有 <- 这个赋值给对象的意思，就是会进行永久修改

#决策树模型预测哪些变量及其值对于预测缺失比例是重要的-------------------------------------------------------------------------------

library(rpart)
library(rpart.plot)

airquality %>%
  add_prop_miss() %>%
  rpart(prop_miss_all ~ ., data = .) %>%
  prp(type = 4, extra = 101, prefix = "Prop. Miss = ")


airquality %>%
  add_prop_miss() %>%
  rpart(prop_miss_all ~ ., data = .) %>%
  prp(type = 4, extra = 101, prefix = "Prop. Miss = ", 
      nn = TRUE,          # 显示节点编号
      fallen.leaves = TRUE, # 所有叶节点在同一水平
      branch = 0.8,       # 控制分支曲线形状
      shadow.col = "gray", # 添加阴影增强可读性
      box.palette = "Blues", # 颜色方案
      faclen = 0,         # 不缩写因子名称
      cex = 0.8,          # 文字大小
      main = "Missing Data Pattern" )
#data = .指的是"使用管道传递过来的数据作为模型输入"
#也就是airquality %>% add_prop_miss() 得到的数据
#type = 4：绘制带有所有节点标签的树形图
#extra = 101：显示  1：每个节点的观测数百分比 0：每个节点的观测数 1：额外显示每个节点的预测值

#进一步优化图像参数

# 输出高清大图（PNG格式）
png("missing_pattern.png", width = 2400, height = 2000, res = 300) # 单位：像素
# 或输出矢量图（PDF格式）
pdf("missing_pattern.pdf", width = 16, height = 12) # 单位：英寸

