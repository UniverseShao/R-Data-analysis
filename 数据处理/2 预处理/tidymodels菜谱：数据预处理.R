#加载R包------------------------------------------------------------------------
library(recipes)
library(tidyverse)
library(AppliedPredictiveModeling)
library(caret)
data("segmentationOriginal")
segData <- subset(segmentationOriginal, Case == "Train")
cellID <- segData$Cell
class <- segData$Class
case <- segData$Case
statusColNum <- grep("Status", names(segData))#搜索含有Status列的名
statusColNum
segData <- segData[ , -statusColNum]
segData <- segData[,-(1:2)]
dim(segData) # 使用的演示数据

#中心化和标准化------------------------------------------------------------------------
#同时完成中心化和标准化
step_normalize()
# 选择数据预处理步骤

# 首先第一步是建立recipe
rec <- recipe(Class ~ ., data = segData)
# 然后一步一步地添加你想要的预处理步骤
preproc <- rec %>% 
  step_center(XCentroid,YCentroid) %>% # 中心化
  step_scale(XCentroid,YCentroid) %>% # 标准化
  prep()

preproc
segdata_scaled <- bake(preproc, new_data = NULL) 
glimpse(segdata_scaled)

#去除偏度---------------------------------------------------------------------------------------------
psych::skew(segData$AreaCh1) # 偏度
## [1] 3.525107
psych::kurtosi(segData$AreaCh1) # 峰度
## [1] 19.04179

ggplot(segData, aes(x = AreaCh1))+
  geom_histogram(bins = 10)
#Boxcox变换：
preproc <- rec %>% 
  step_center(XCentroid,YCentroid) %>% 
  step_scale(XCentroid,YCentroid) %>% 
  step_BoxCox(AreaCh1) %>% 
  prep()

preproc
segdata_boxcox <- bake(preproc, new_data = NULL)

ggplot(segdata_boxcox, aes(x = AreaCh1))+
  geom_histogram(bins = 10)

#添加交互项------------------------------------------------------------------------
step_interact()
#解决离群值----------------------------------------------------------------------------
data(biomass, package = "modeldata")

biomass_tr <- biomass[biomass$dataset == "Training", ]
biomass_te <- biomass[biomass$dataset == "Testing", ]

# 建立菜谱
rec <- recipe(
  HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
  data = biomass_tr
)

# 添加食材
ss_trans <- rec %>%
  step_center(carbon, hydrogen) %>%
  step_scale(carbon, hydrogen) %>%
  step_spatialsign(carbon, hydrogen)

# 准备好了，可以下锅了
ss_obj <- prep(ss_trans, training = biomass_tr)

# 下锅制作
transformed_te <- bake(ss_obj, biomass_te)

# 画图查看之前的
plot(biomass_te$carbon, biomass_te$hydrogen)
# 画图查看之后的
plot(transformed_te$carbon, transformed_te$hydrogen)
# 获取数据预处理步骤
tidy(ss_trans, number = 3)
tidy(ss_obj, number = 3)



































