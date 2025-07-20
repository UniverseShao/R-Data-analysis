# 1.简介------------------------------------------------------------------------
library(dplyr, warn.conflicts = FALSE)
"
rowwise()和group_by()很像，本身不做任何操作，
但是使用了rowwise之后，再和mutate()等函数连用时，就会变成按照行进行操作！

"
df <- tibble(x = 1:2, y = 3:4, z = 5:6)
df %>% rowwise()
# 1.1不使用rowwise()函数--------------------------------------------------------
# 不使用rowwise()函数时，不执行行操作
df %>% mutate(m = mean(c(x, y, z)))
# 1.2使用rowwise()函数----------------------------------------------------------
# 使用rowwise()函时，执行行操作
df %>% rowwise() %>% mutate(m = mean(c(x, y, z)))
# 1.3进一步利用rowwise()进行分组------------------------------------------------
df <- tibble(name = c("Mara", "Hadley"), x = 1:2, y = 3:4, z = 5:6)
df
df %>% 
  rowwise() %>% 
  summarise(m = mean(c(x, y, z)))
df %>% 
  rowwise(name) %>% # 相当于按照name分组
  summarise(m = mean(c(x, y, z)))

"
rowwise()可以看做是group_by()的特殊形式
本身也是对数据先进行聚合操作
如果要解除聚合，就使用ungroup()函数
"
# 2.对行进行汇总统计------------------------------------------------------------
df <- tibble(id = 1:6, w = 10:15, x = 20:25, y = 30:35, z = 40:45)
df
rf <- df %>% rowwise(id)
rf %>% mutate(total = sum(c(w, x, y, z)))
rf %>% summarise(total = sum(c(w, x, y, z)))
# 2.1c_across选择多列数据-------------------------------------------------------
rf %>% mutate(total = sum(c_across(w:z)))
## 2.1和where合用---------------------------------------------------------------
rf %>% mutate(total = sum(c_across(where(is.numeric))))
## 2.2和列操作一起使用----------------------------------------------------------
rf %>% 
  mutate(total = sum(c_across(w:z))) %>% 
  ungroup() %>% # 先解除行操作
  mutate(across(w:z, ~ . / total))









