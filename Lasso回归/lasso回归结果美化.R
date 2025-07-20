# 1.加载R包---------------------------------------------------------------------
library(glmnet)
library(survival)
data(CoxExample)
x <- CoxExample$x
y <- CoxExample$y

head(y)
# 2.默认画图--------------------------------------------------------------------
mod <- glmnet(x,y,family = "cox")
cvmod <- cv.glmnet(x,y,family="cox") # 交叉验证
plot(mod,label = T,lwd=2)
plot(mod,xvar = "lambda",label = T,lwd=2)
plot(cvmod)

# 3.提取数据--------------------------------------------------------------------
library(tidyverse)
tmp <- as_tibble(as.matrix(coef(mod)), rownames = "coef") %>% 
  pivot_longer(cols = -coef, 
               names_to = "variable", 
               names_transform = list(variable = parse_number), 
               values_to = "value") %>% 
  group_by(variable) %>% 
  mutate(lambda = mod$lambda[variable + 1], 
         norm = sum(if_else(coef == "(Intercept)", 0, abs(value))))

ggplot(tmp, aes(norm,value,color=coef,group=coef))+
  geom_line(size=1.2)+
  labs(x="Log Lambda",y="Coefficients")+
  theme_bw()

# 4.broom神包-------------------------------------------------------------------
library(broom)
# 提取数据，就是这么简单！
tidy_df <- broom::tidy(mod)
tidy_cvdf <- broom::tidy(cvmod)
head(tidy_df)
head(tidy_cvdf)
##### 4.1自定义绘图-------------------------------------------------------------
library(ggplot2)
library(RColorBrewer)
#随便定义几个颜色，多找几个，防止不够用
mypalette <- c(brewer.pal(11,"BrBG"),brewer.pal(11,"Spectral"),brewer.pal(5,"Accent"))

ggplot(tidy_df, aes(step, estimate, group = term,color=term)) +
  geom_line(size=1.2)+
  geom_hline(yintercept = 0)+
  ylab("Coefficients")+
  scale_color_manual(name="variable",values = mypalette)+
  theme_bw()

p2 <- ggplot(tidy_df, aes(lambda, estimate, group = term, color = term)) +
  geom_line(size=1.2)+
  geom_hline(yintercept = 0)+
  scale_x_log10(name = "Log Lambda")+
  ylab("Coefficients")+
  scale_color_manual(name="variable",values = mypalette)+
  theme_bw()
p2  

p3 <- ggplot()+
  geom_point(data=tidy_cvdf, aes(lambda,estimate))+
  geom_errorbar(data = tidy_cvdf, aes(x=lambda,ymin=conf.low,ymax=conf.high))+
  scale_x_log10(name = "Log Lambda")+
  ylab("Coefficients")+
  theme_bw()
p3

library(patchwork)

p2 / p3
