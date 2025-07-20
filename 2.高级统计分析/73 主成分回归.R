"
简单来说就是先做主成分分析
然后用主成分作为自变量进行线性回归分析
"
# 1.pls-------------------------------------------------------------------------
# 安装
install.packages("pls")
rm(list = ls())
library(pls)
head(mtcars)
data(mtcars)
##### 1.1一个函数实现主成分回归-------------------------------------------------
set.seed(1)

model <- pcr(hp~mpg+disp+drat+wt+qsec, data=mtcars, scale=TRUE, validation="CV")
summary(model)
## Data:  X dimension: 32 5 
##  Y dimension: 32 1
## Fit method: svdpc
## Number of components considered: 5
## 
## VALIDATION: RMSEP
## Cross-validated using 10 random segments.
##        (Intercept)  1 comps  2 comps  3 comps  4 comps  5 comps
## CV           69.66    43.74    34.58    34.93    36.34    37.40
## adjCV        69.66    43.65    34.30    34.61    35.95    36.95
## 
## TRAINING: % variance explained
##     1 comps  2 comps  3 comps  4 comps  5 comps
## X     69.83    89.35    95.88    98.96   100.00
## hp    62.38    81.31    81.96    81.98    82.03
"
RMSEP
（Root Mean Squared Error of Prediction）：预测均方根误差，值越小越好
TRAINING: % variance explained方差解释率
自变量（X）的方差解释：
第1主成分解释69.83%的方差，前2个主成分累计89.35%，接近90%。
降维建议：仅需2个主成分即可保留大部分信息。
因变量（hp）的方差解释：
使用2个主成分时，hp的方差解释率达81.31%，增加更多主成分提升有限（仅从81.31%→82.03%）。
"
##### 1.2可视化主成分个数与各种模型指标的关系-----------------------------------
validationplot(model)# 纵坐标是RMSEP
validationplot(model, val.type="MSEP") # 纵坐标是MSEP，是对误差取平方对较大误差更敏感
validationplot(model, val.type="R2")# 纵坐标是R²（决定系数）
# R² = 1：模型完美拟合数据
# R² = 0：模型不优于均值预测
# R² < 0：模型表现比直接用均值预测更差

##### 1.3对新数据进行预测-------------------------------------------------------
test <- head(mtcars)
predict(model, test, ncomp = 2)


# 2.tidymodels------------------------------------------------------------------
suppressMessages(library(tidymodels))
tidymodels_prefer()
# 这个过程就稍显复杂
# 虽然复杂但是很有逻辑，都是这一套步骤
"
建立模型设定；
数据划分；
配方（预处理步骤）；
工作流；
超参数调优
"
# 模型设定
set.seed(994)
lm_spec <- linear_reg() %>% set_engine("lm")

# 数据划分
mtcars_resamples <- vfold_cv(mtcars, v = 10)

# 配方（预处理步骤）
mtcars_pca_recipe <- recipe(hp~mpg+disp+drat+wt+qsec, data = mtcars) %>%
  #step_dummy(all_nominal()) %>%
  step_normalize(all_predictors()) %>% # 肯定是要scale的
  step_pca(all_predictors(), num_comp = tune())

# 工作流
mtcars_pca_workflow <- workflow() %>%
  add_model(lm_spec) %>%
  add_recipe(mtcars_pca_recipe)

# 超参数调优
num_comp_grid <- grid_regular(num_comp(range = c(0, 5)), levels = 6)

mtcars_pca_tune <- tune_grid(mtcars_pca_workflow,
                             resamples = mtcars_resamples,
                             grid = num_comp_grid)

autoplot(mtcars_pca_tune)
show_best(mtcars_pca_tune)

mtcars_pca_workflow_final <-
  finalize_workflow(mtcars_pca_workflow,
                    select_best(mtcars_pca_tune, metric = "rmse"))

mtcars_pca_fit_final <- fit(mtcars_pca_workflow_final,data = mtcars)

predict(mtcars_pca_fit_final, new_data = head(mtcars))
# 最后发现结果是和方法一是一样的，并且相比方法一更麻烦
# 所以我推荐适用方法一
