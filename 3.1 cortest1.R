df <- mtcars

cor.test(x = df$mpg, y = df$hp) #параметр метод меняет пирсона на другого method

fit <- cor.test(x = df$mpg, y = df$hp)

fit$p.value

?cor.test

cor.test(~ mpg + hp, df)

plot(x = df$mpg, y = df$hp)

ggplot(df, aes(x = mpg, y = hp, col = factor(cyl)))+
  geom_point(size = 5)

df_numeric <- df[,c(1,3:7)]
pairs(df_numeric)

cor(df_numeric)

fit1 <- corr.test(df_numeric, adjust = "bonferroni")
fit1$r
fit1$p

x <- cbind(df$mpg, df$hp) 

corr.calc <- function(x){
  return(c(corr.test(x)$r[2], corr.test(x)$p[2]))
}

z <- corr.calc(x)

k <- data.frame(row.names = c(1:length(fit3[,1])))
j <- 1

for(i in g){
  if(i){
    k <- cbind(k,fit3[,j])
    j <- j+1
  }
  else{j <- j+1}
}
h <- which(g)
for(i in g){
  if(i){
    fit3
    j <- j+1
  }
  else{j <- j+1}
}

filtered.cor <- function(){}

cbind(c(1:10),c(1:9))
#задача 1
fit3 <- read.csv("step6.csv")
g <- sapply(fit3, FUN=is.numeric)
h <- which(g)
fit3[,h]
fit4 <- corr.test(fit3[,h])
diag(fit4$r) <- 0
max(abs(fit4$r))

which.max(abs(fit4$r))
#задача 2
library(psych)
filtered.cor <- function(x){
  g <- sapply(x, FUN=is.numeric)
  h <- which(g)
  fit4 <- corr.test(x[,h])
  diag(fit4$r) <- 0
  return(fit4$r[which.max(abs(fit4$r))])
}
filtered.cor(fit3)

fit3[sapply(fit3,is.numeric)]

filtered.cor <- function(x){
  cm <- cor(x[sapply(x, is.numeric)])
  return(cm[which.max(abs(cm-diag(ncol(cm))))])
}
#задача 3
test_data  <- read.csv("https://stepik.org/media/attachments/course/129/test_data.csv")

library(psych)
smart_cor <- function(test_data){
ifelse(shapiro.test(test_data[,1])$p >= 0.05 &
  shapiro.test(test_data[,2])$p >= 0.05, 
  return(corr.test(test_data)$r[2]), 
  return(corr.test(test_data, method = "spearman")$r[2]))
}
smart_cor(test_data)
