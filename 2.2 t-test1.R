df <- iris

str(df)

df1 <- subset(df, Species != "setosa")

df1

table(df1$Species)

hist(df1$Sepal.Length)

ggplot(df1, aes(x = Sepal.Length))+
  geom_histogram(fill = "white", col ="black", binwidth = 0.4)+
  facet_grid(Species ~ .)

ggplot(df1, aes(x = Sepal.Length, fill = Species))+
  geom_density(alpha = 0.5)

ggplot(df1, aes(Species, Sepal.Length))+
  geom_boxplot()

shapiro.test(df1$Sepal.Length)

shapiro.test(df1$Sepal.Length[df1$Species == "versicolor"])

shapiro.test(df1$Sepal.Length[df1$Species == "virginica"])

bartlett.test(Sepal.Length ~ Species, df1)

t.test(Sepal.Length ~ Species, df1)

test1 <- t.test(Sepal.Length ~ Species, df1)

str(test1)

test1$p.value

t.test(Sepal.Length ~ Species, df1)

mean(df1$Sepal.Length)

t.test()
  
t.test(df1$Sepal.Length, mu = 8)

t.test(df1$Petal.Length, df1$Petal.Width, paired = T)

dt2 <- ToothGrowth

str(dt2)

t_stat <- t.test(dt2$len[dt2$supp =="OJ" & dt2$dose == 0.5],
                 dt2$len[dt2$supp =="VC" & dt2$dose == 2])$statistic

dt3 <- read.csv("lekarstva.csv")

str(dt3)

t.test(dt3$Pressure_before, dt3$Pressure_after, paired = T)

ggplot(df1, aes(Species, Sepal.Length))+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.1)+
  stat_summary(fun.y = mean, geom = "point", size = 4)

ggplot(df1, aes(Species, Sepal.Length))+
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange", size = 2)+
  stat_summary(fun.y = mean, geom = "point", size = 4)
               
mean_cl_normal(df$Sepal.Length)

library(Hmisc)

?wilcox.test

test2 <- wilcox.test(Petal.Length ~ Species, df1)
pv <- test2$p.value

ggplot(df1, aes(Species, Petal.Length))+
  geom_boxplot()

wilcox.test(Petal.Length ~ Species, df1)

wilcox.test(df1$Petal.Length, df1$Petal.Width, paired = T)

paired_wtest <- wilcox.test(df1$Petal.Length, df1$Petal.Width, paired = T)

paired_wtest$p.value

c <- read.table("dataset_11504_15.txt")

bartlett.test(V1 ~ V2, c)

ggplot(c, aes(V2, V1, group = V2))+
  geom_boxplot()

t.test(V1 ~ V2, c, var.equal = TRUE)

wilcox.test(c$V1, c$V2, paired = T)


k <- read.table("dataset_11504_16.txt")

bartlett.test(V1 ~ V2, k)

ggplot(k, aes(V2, V1, group = V2))+
  geom_boxplot()

t.test(k$V1, k$V2)

wilcox.test(c$V1, c$V2, paired = T)

