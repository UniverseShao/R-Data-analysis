set.seed(20220840)
ca125_1 <- c(rnorm(10,80,20),rnorm(20,50,10))
ca125_1
outcome <- c(rep(c("肿瘤","非肿瘤"),c(10,20)))
outcome
df <- data.frame(`outcome`=outcome,
                 `ca125`=ca125_1
)
psych::headtail(df)
df1 <- transform(df, pred = ifelse(ca125>60,"猜他是肿瘤","猜他不是肿瘤"))
df1
xtabs(~outcome+pred,data = df1)
cal_ROC <- function(df, cutoff){
  
  df <- transform(df, pred = ifelse(ca125>cutoff,"猜他是肿瘤","猜他不是肿瘤"))
  cm <- table(df$outcome,df$pred)
  t <- cm[,"猜他是肿瘤"]/rowSums(cm)
  list(TPR=t[[2]], FPR=t[[1]])
}
cal_ROC(df,60)
range(ca125_1)
cutoff <- seq(30,113, 2)

rocs <- purrr::map_dfr(cutoff, cal_ROC, df=df)

psych::headtail(rocs)
library(ggplot2)

ggplot(rocs, aes(FPR,TPR))+
  geom_point(size=2,color="red")+
  geom_path()+
  coord_fixed()+
  theme_bw()

