df <- mtcars
?mtcars
str(df)

df$vs <- factor(df$vs, labels = c("V", "S"))
df$am <- factor(df$am, labels = c("Auto", "Manual"))

median(df$mpg)
mean(df$disp)
sd(df$hp)
range(df$cyl)

mean_disp <- mean(df$disp)
mean(df$mpg[df$cyl == 6])

mean(df$mpg[df$cyl == 6 & df$vs == "V"])

sd(df$hp[df$cyl != 3 & df$am == "Auto"])

result <- sd(mtcars$qsec[mtcars$cyl != 3 & mtcars$mpg > 20])

result <- mean(mtcars$qsec[mtcars$cyl != 3 & mtcars$mpg > 20])

?aggregate

mean_hp_vs <- aggregate(x = df$hp, by = list(df$vs), FUN = mean)

colnames(mean_hp_vs) <- c("VS", "Mean HP")

aggregate(hp ~ vs, df, mean)
aggregate(hp ~ vs +am, df, mean)

aggregate(x = df$hp, by = list(df$vs, df$am), FUN = mean)

aggregate(x = df[, -c(8, 9)], by = list(df$am), FUN = median)

aggregate(df[,c(1,3)], by = list(df$am, df$vs), FUN = sd)

aggregate(cbind(mpg, disp) ~ am + vs, df, sd)

cbind(df$mpg, df$disp)

my_stats <- aggregate(cbind(mpg, disp) ~ am + vs, df, sd)

descriptions_stat <- aggregate(cbind(hp, disp) ~ am, mtcars, sd)

library(psych)

?describe

describe(x = df)

descr <- describe(x = df[, -c(8, 9)])

descr2 = describeBy(x = df[, -c(8, 9)], group = df$vs)

descr3 = describeBy(x = df[, -c(8, 9)], group = df$vs, mat = T, digits = 1)

descr4 = describeBy(x = df[, -c(8, 9)], group = df$vs, mat = T, digits = 1, fast = T)

describeBy(df$qsec, group = list(df$vs, df$am), mat = T, digits = 1, fast = T)

sum(is.na(df$mpg))
sum(is.na(df))

df$mpg[1:10] <- NA
mean(df$mpg, na.rm = T)

describe()

df1 <- subset(airquality, Month%in%c(7,8,9)) 

result <- aggregate(Ozone ~ Month, df1,length)

airquality

describeBy(x = airquality[, 1:4], group = airquality$Month)

df3 <- iris
sd(iris$Sepal.Length)
sd(iris$Sepal.Width)
sd(iris$Petal.Length)
sd(iris$Petal.Width)
sd(iris)

describe(iris)

describeBy(x = iris, group = iris$Species)

my_vector <- rnorm(30)

my_vector[sample(1:30, 10)] <- NA
  
?replace

fixed_vector <- replace(my_vector, is.na(my_vector), mean(my_vector, na.rm = T))
                        
