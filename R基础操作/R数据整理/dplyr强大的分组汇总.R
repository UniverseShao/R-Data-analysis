# 1.group_by()------------------------------------------------------------------
by_species <- starwars %>% group_by(species)
by_sex_gender <- starwars %>% group_by(sex, gender)
by_species
by_sex_gender
# 数据看上去和原数据没有什么区别，但是都被分组了，后续分析都是按组进行
# 使用tally()函数进行计数
by_species %>% tally(sort = T)
# 等价于
by_species %>% summarise(n = n())  # 计算每组的行数


bmi_breaks <- c(0,18.5,25,30,Inf)

starwars %>% 
  group_by(bmi_cat = cut(mass/(height/100)^2,breaks = bmi_breaks)) %>% 
  tally(sort = T)

# 2.查看分组信息----------------------------------------------------------------
## 2.1group_keys()查看用于分组的组内有哪些类别----------------------------------
by_species %>% group_keys() 
by_sex_gender %>% group_keys()

## 2.2group_indices()查看每一行属于哪个组---------------------------------------
by_species %>% group_indices()

## 2.3group_rows()查看每个组包括哪些行------------------------------------------
by_species %>% group_rows()

## 2.4group_vars()查看用于聚合的变量名字----------------------------------------

# 3.增加或改变用于聚合的变量----------------------------------------------------
"
如果把group_by()作用于已经聚合的变量，那数据会被覆盖，
比如下面这个，by_species已经被species聚合了，再通过homeworld聚合，那结果只是homeworld的结果
"
by_species %>% 
  group_by(homeworld) %>% 
  tally()

# 通过.add = T来解决
by_species %>% 
  group_by(homeworld, .add = T) %>% 
  tally()

# 4.移除聚合的变量--------------------------------------------------------------

"一个被聚合的数据如果不解除聚合
那么后面的操作都会以聚合后的结果呈现出来
所以聚合之后一定要记得解除聚合"

by_species %>% 
  ungroup() %>% 
  tally()
## # A tibble: 1 x 1
##       n
##   <int>
## 1    87
by_sex_gender %>% 
  ungroup(sex) %>% 
  tally()
## # A tibble: 3 x 2
##   gender        n
##   <chr>     <int>
## 1 feminine     17
## 2 masculine    66
## 3 <NA>          4

# 4.group_by和其他联合使用------------------------------------------------------

## 4.1summarise()---------------------------------------------------------------
by_species %>%
  summarise(
    n = n(),
    height = mean(height, na.rm = TRUE)
  )


## 4.2select()/rename()/relocate()----------------------------------------------
by_species %>% select(mass)


by_species %>% 
  ungroup() %>% 
  select(mass)


by_species %>% 
  arrange(desc(mass)) %>%  # 按mass降序排列
  relocate(species, mass)   # 将species和mass列移到最前面


by_species %>% 
  arrange(desc(mass), .by_group = T) %>% 
  relocate(species, mass)


## 4.3muatate() and transmutate()-----------------------------------------------
starwars %>% 
  select(name, homeworld, mass) %>% 
  group_by(homeworld) %>% 
  mutate(means = mean(mass, na.rm = T), 
         standard_mass = mass - mean(mass, na.rm = T))

starwars %>% 
  select(name, homeworld, height) %>% 
  mutate(rank = min_rank(height))


starwars %>% 
  select(name, homeworld, height) %>% 
  group_by(homeworld) %>% 
  mutate(rank = min_rank(height))

## 4.4filter()------------------------------------------------------------------
# 筛选每个物种（species）中最高（height）的那一个：
by_species %>%
  select(name, species, height) %>% 
  filter(height == max(height))

# 去掉只有1个成员的物种
by_species %>%
  filter(n() != 1) %>% 
  tally()


