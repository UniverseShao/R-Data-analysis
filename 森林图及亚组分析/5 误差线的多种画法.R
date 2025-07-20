#编一个数据####

library(tibble)
options(digits = 2)
df <- tibble(
  label = LETTERS[1:22],
  mean = rnorm(22,mean = 1, sd=0.2),
  lower = mean - 0.1,
  upper = mean + 0.2,
  group = c(rep("Group-1",7),rep("Group-2",7),rep("Group-3",8))
)
df
## # A tibble: 22 × 5
##    label  mean lower upper group  
##    <chr> <dbl> <dbl> <dbl> <chr>  
##  1 A     1.05  0.954 1.25  Group-1
##  2 B     1.15  1.05  1.35  Group-1
##  3 C     1.11  1.01  1.31  Group-1
##  4 D     0.918 0.818 1.12  Group-1
##  5 E     1.32  1.22  1.52  Group-1
##  6 F     0.979 0.879 1.18  Group-1
##  7 G     0.683 0.583 0.883 Group-1
##  8 H     0.992 0.892 1.19  Group-2
##  9 I     1.20  1.10  1.40  Group-2
## 10 J     1.11  1.01  1.31  Group-2
## # ℹ 12 more rows


library(ggplot2)
#提供一个基本图####
p <- ggplot(data = df,aes(color = group))
#geom_errorbarh()和geom_errorbar()####

#geom_errorbar()：默认是画竖直方向的误差线
#geom_errorbarh()：默认是画水平方向的误差线

#geom_errorbar()的常规用法####
p + geom_point(aes(x = label, y = mean),size=5)+
  geom_errorbar(aes(x = label,ymin = lower, ymax = upper),
                width = 0.6, # 控制上下两条短横线的长短
                linewidth = 2 # 控制线条整体粗细
  )+
  theme_bw()

#geom_errorbarh()的常规用法####
p + geom_point(aes(x=mean, y=label))+
  geom_errorbarh(aes(y=label,xmin=lower, xmax=upper),
                 height=0.5, # 控制左右端点两条小竖线的长短
                 linewidth=1)+
  theme_bw()

#geom_pointrange()和geom_linerange()####

#两端没有短线的误差线或者森林图####
p + geom_point(aes(x=mean, y=label))+
  geom_errorbarh(aes(y=label,xmin=lower, xmax=upper),
                 height=0, # 控制左右端点两条小竖线的长短
                 linewidth=1)+
  theme_bw()

#geom_pointrange()####
p+geom_pointrange(aes(x=mean,y=label,xmin=lower,xmax=upper),
                  linewidth=1, # 控制线的宽度
                  fatten = 0.6 # 控制点的大小
)

#竖直方向####

p+geom_pointrange(aes(x=mean,y=label,xmin=lower,xmax=upper),
                  linewidth=1, # 控制线的宽度
                  fatten = 0.6 # 控制点的大小
)+
  coord_flip()

p+geom_pointrange(aes(x=label,y=mean,ymin=lower,ymax=upper),
                  linewidth=1, # 控制线的宽度
                  fatten = 0.6 # 控制点的大小
)


#geom_crossbar()####

p+geom_crossbar(aes(x=mean,y=label,xmin=lower,xmax=upper))+theme_bw()































