library(tidyverse)
# 1.数据集：starwars------------------------------------------------------------
library(dplyr)
dim(starwars)
glimpse(starwars)# 用于快速浏览数据集的内容

# 2.filter()根据条件筛选行------------------------------------------------------
starwars %>% filter(skin_color == "light", eye_color == "brown")
# 注意filter只可以根据条件筛选行
# 不可以直接根据行号进行筛选
starwars %>% filter(1:3)
# 会直接报错
## 2.1 slice()根据行号筛选------------------------------------------------------
starwars %>% slice(1:3)
starwars %>% slice(5:10)
starwars %>% slice_head(n = 4)# 选择前4行
starwars %>% slice_head(prop = 0.1)# 选择前10%的人
starwars %>% slice_tail(prop = 0.1)# 选择后10%的人
starwars %>% slice_sample(prop = 0.1)# 随机选择前10%的人
starwars %>% slice_sample(n=10, replace = T)# 随机选择前10%的人，允许重复
# 3.arrange()进行排序-----------------------------------------------------------
starwars %>% arrange(height, mass)

## 3.1desc()进行倒序------------------------------------------------------------
starwars %>% arrange(desc(height))

## 3.2联合使用------------------------------------------------------------------
starwars %>% dplyr::filter(!is.na(height)) %>% 
  slice_max(height, n = 5)

# 4.select()选择列--------------------------------------------------------------

## 4.1直接根据列名选择列--------------------------------------------------------
starwars %>% select(hair_color, skin_color, eye_color)

## 4.2选择列名中以color结尾的列-------------------------------------------------
starwars %>% select(ends_with("color"))

## 4.3选择列名中包含color字样的列-----------------------------------------------
starwars %>% select(contains("color"))

## 4.4重命名列------------------------------------------------------------------
starwars %>% rename(home_world = homeworld)

# 5.mutate()新建列--------------------------------------------------------------
starwars %>% mutate(height_m = height/100, .before = 1)
## 5.1指定新建列的位置----------------------------------------------------------
# .before = 的方法和select(新列，其他列)的方法
# .before = 1指定新创建的列（这里是 height_m）应该插入到数据框的第1列位置

starwars %>% 
  mutate(height_m = height/100) %>% 
  select(height_m, everything())
"select(height_m, everything())
第一步：先选择 height_m 列 → 此时它是第一列
第二步：然后添加 everything() 代表的其他所有列 → 这些列会按原始顺序跟在后面"

## 5.2保留新列，去除其他--------------------------------------------------------
starwars %>%
  transmute(
    height_m = height / 100,
    BMI = mass / (height_m^2)
  )
# %>%是临时保存，不会彻底保存，只有starwars <- starwars %>% transmute()才可以

# 6.relocate()重排列的位置------------------------------------------------------
starwars %>% relocate(sex:homeworld, .before = height)
# 将sex到homeworld的列排到height列之前
# .before = height就是指的是在heigh之前
# 上面的.before = 2就是在第一列之前，也就是重新成为第一列

# 7.summarise()汇总-------------------------------------------------------------
starwars %>% summarise(height = mean(height, na.rm = T))



