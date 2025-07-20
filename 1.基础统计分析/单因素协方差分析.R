#单因素协变量方差分析就是在单因素方差分析的基础上引入了协变量
#公式又原来的fit<-aov(y~A)变成了fit<-aov(y~A+B)
#B就是这一次引入的协变量
#从公式上来讲就是将A和B看成一个整体，对这个整体进行单因素方差分析
#公式
fit<-aov(litter$weight~litter$gesttime+litter$dose)
fit<-aov(weight~gesttime+dose, data = litter)
#注意的是这里由于多了一个变量
#所以有时候R莫名其妙的会说你的变量参数长度不一致
#所以在变量前最好是直接加上具体的data$来指定是哪个数据表中的数据
#单因素协变量方差分析画图
library(HH)
ancova(weight~gesttime+dose,data=litter)
#注意这个画图函数中不可以再在变量前面加上data$否则会报错











