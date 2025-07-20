rm(list = ls())
# across可以对多列进行同一操作
# 1.一般用法--------------------------------------------------------------------
library(dplyr, warn.conflicts = FALSE)
# across()有两个基本参数：
# .cols：选择你想操作的列
# .fn：你想进行的操作，可以使一个函数或者多个函数组成的列表
starwars %>% 
  summarise(across(where(is.character), n_distinct))
# n_distinct是用于计算非重复数据的函数
# 1.1across可以直接写列名-------------------------------------------------------
starwars %>% 
  group_by(species) %>% 
  filter(n() > 1) %>% 
  summarise(across(c(sex, gender, homeworld), n_distinct))

# 1.2和where函数连用------------------------------------------------------------

starwars %>% 
  group_by(homeworld) %>% 
  filter(n() > 1) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))
# 上面的~ mean(.x, na.rm = TRUE)是跳过缺失值进行平均值的计算
# 没有缺失值直接写mean就行
library(tidyr)
starwars %>% drop_na() %>% 
  group_by(homeworld) %>% 
  filter(n() > 1) %>% 
  summarise(across(where(is.numeric), mean))
# 造成的结果的差异是上串代码会保留母星是NA的数据，使用mean就只会保留完整的数据

# 1.3across支持多个函数同时使用-------------------------------------------------
min_max <- list(
  min = ~min(.x, na.rm = TRUE), 
  max = ~max(.x, na.rm = TRUE)
)
starwars %>% summarise(across(where(is.numeric), min_max))
starwars %>% summarise(across(c(height, mass, birth_year), min_max))

# 1.4支持glue，即重新修改输出列名-----------------------------------------------
starwars %>% summarise(across(where(is.numeric), 
                              min_max, .names = "{.fn}.{.col}"))

starwars %>% summarise(across(c(height, mass, birth_year), 
                              min_max, .names = "{.fn}.{.col}"))

# 分开写也是可以的
starwars %>% summarise(
  across(c(height, mass, birth_year), ~min(.x, na.rm = TRUE), .names = "min_{.col}"),
  across(c(height, mass, birth_year), ~max(.x, na.rm = TRUE), .names = "max_{.col}")
)
"
这种情况不能使用where(is.numeric)，
因为第2个across会使用新创建的列（“min_height”, “min_mass” and “min_birth_year”）
"
# 放在tibble里解决
starwars %>% summarise(
  tibble(
    across(where(is.numeric), ~min(.x, na.rm = TRUE), .names = "min_{.col}"),
    across(where(is.numeric), ~max(.x, na.rm = TRUE), .names = "max_{.col}")  
  )
)
# 这是由于tibble()中的函数是并行执行的不是先后执行的
# 2.陷阱------------------------------------------------------------------------
df <- data.frame(x = c(1, 2, 3), y = c(1, 4, 9))

df %>% 
  summarise(n = n(), across(where(is.numeric), sd))
##    n x        y
## 1 NA 1 4.041452
# 这是因为summarize函数是顺序执行从而导致之前的n = n()计算的行数被覆盖最后成NA
# 使用以下三种方法可以解决
df %>% summarise(across(where(is.numeric), sd),
                 n = n()
)

df %>% 
  summarise(n = n(), across(where(is.numeric) & !n, sd))

df %>% 
  summarise(
    tibble(n = n(), across(where(is.numeric), sd))
  )

# 3.across其他连用--------------------------------------------------------------
# across()不能直接和filter()连用，和filter()连用的是if_any()和if_all()
# if_any()：任何一列满足条件即可
# if_all()：所有列都要满足条件
starwars %>% 
  filter(if_any(everything(), ~ !is.na(.x)))
starwars %>% 
  filter(if_all(everything(), ~ !is.na(.x)))

