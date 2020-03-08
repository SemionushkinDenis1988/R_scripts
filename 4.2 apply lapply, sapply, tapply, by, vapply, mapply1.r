library(ggplot2)
data("diamonds")
#плохой вариант
min_size <- c()

for(i in 1:nrow(diamonds)){
  min_size <- c(min_size, min(diamonds[i, 8:10]))
}

#хороший вариант

min_size <- numeric(nrow(diamonds))

min_size <- apply(diamonds[, 8:10], 1, min)

?apply
  
d <- matrix(rnorm(30), nrow = 5)

apply(d, MARGIN = 1, FUN = sd)

apply(d, MARGIN = 2, FUN = sd)

apply(mtcars, 2, sd)

#Задача 1
row_max <- apply(my_df, 1, max)
#Задача 2
col_median <- apply(my_df, 2, median)

iris_num <- iris[, 1:4]

iris_outliers <- apply(iris_num, 2, outliers_count)
str(iris_outliers)

head(airquality)
?airquality
apply(airquality, 2, mean)

apply(airquality, 2, mean(na.rm = T))

apply(airquality, 2, mean, na.rm = T)

colMeans(airquality, na.rm = T)
colSums()
rowMeans()
rowSums()

set.seed(42) #"фиксирует генератор"

d <- as.data.frame(matrix(rnorm(30), nrow = 5))

my_fun <- function(x) x*2

my_fun(1:10)

d

my_list <- list()
for(i in seq_along(d)){
  temp_col <- d[, i]
  neg_numbers <- temp_col[temp_col<0]
  my_list[[i]] <- neg_numbers
}

names(my_list) <- colnames(d)

my_list

find_negative <- function(x){
  x[x<0]
}
apply(d, 2, find_negative)

apply(d, 2, function(x) x[x<0])

d[1,1] <- NA

apply(d, 2, function(x) x[x<0 & !(is.na(x))])

#задача 3
get_negative_values <- function(x){
  z <- apply(x, 2, function(x) x[x<0 & !(is.na(x))])
  z[lapply(z,length)>0]
}

get_negative_values <- function(test_data){    
  negative_col <- apply(test_data, 2, function(x) any(x[!is.na(x)] < 0))    
  return(apply(test_data[negative_col], 2, function(x) x[!is.na(x) & x <0]))}

test_data <- as.data.frame(list(V1 = c(1.3, 0.1, -0.5, -0.1, 0.8), V2 = c(0.8, -0.7, 1.4, NA, -0.8), V3 = c(-0.5, -0.3, -0.2, 0, 1.3), V4 = c(0.1, 0.7, 0.4, -0.8, 1.4), V5 = c(-0.6, -0.5, NA, -0.7, NA), V6 = c(0.1, 0.9, NA, 0.2, 0.2), V7 = c(1.6, -0.4, NA, -0.3, 0.3)))
test_data <- as.data.frame(list(V1 = c(-9.7, -10, -10.5, -7.8, -8.9), V2 = c(NA, -10.2, -10.1, -9.3, -12.2), V3 = c(NA, NA, -9.3, -10.9, -9.8)))
z <- get_negative_values(test_data)

x <- test_data
z <- apply(x, 2, function(x) x[x<0 & !(is.na(x))])
s <- z[lapply(z,length)>0]

x[x<0 & !(is.na(x)) & length(x[x<0 & !(is.na(x))])]
                             
apply(x, 2, function(x) length(x[x<0 & !(is.na(x))])>0)
x$V6[x$V6<0 & !(is.na(x$V6))]

aov_result <- apply(iris[, 1:4], 2, function(x) aov(x ~ iris$Species))

aov_result$Sepal.Length

norm_test <- apply(iris[, 1:4], 2, function(x) shapiro.test(x)$p.value)
negative_col <- apply(test_data, 2, function(x) any(x[!is.na(x)] < 0))
test_data[negative_col]
?any

#задача 4
na_rm <- function(x){
  as.data.frame(apply(x, 2, function(x) {
    x[is.na(x)] <- mean(x, na.rm = T)
    return(x)
      }
    )
  )
}

na_rm  <- function(x){    
  na_to_mean  <- function(v){    
    v[is.na(v)]  <- mean(v , na.rm = T)    
    return(v)}    
  result  <- as.data.frame(apply(x, 2, na_to_mean))}

x <- as.data.frame(list(V1 = c(NA, NA, NA, NA, 13, 12, 9, 10, 8, 9, 11, 11, 10, 12, 9), V2 = c(NA, 12, 8, NA, 11, 11, 9, 8, 8, 10, 10, 11, 10, 10, 10), V3 = c(NA, 5, NA, 13, 12, 11, 11, 14, 8, 12, 8, 8, 10, 10, 8), V4 = c(10, 10, 10, 10, 13, 10, 11, 7, 12, 10, 7, 10, 13, 10, 9)))

na_rm(z2)

z2$V1[is.na(z2$V1)] <- mean(z2$V1, na.rm = T)
z3 <- function(x){
  x[is.na(x)] <- mean(x, na.rm = T)
  x
}
z3(z2$V1)

c <- apply(x, 2, function(x) {
  x[is.na(x)] <- mean(x, na.rm = T)
  return(x)})
typeof(c)
c1 <- as.data.frame(c)

my_list <- list(x = c(rnorm(30), NA), y = rnorm(10))

lapply(my_list, mean)

lapply(my_list, mean, na.rm = T)

my_fun <- function(x){
  x*2
}

lapply(my_list, function(x) x * 2)

sapply(my_list, mean, na.rm = T)

#Задача 1
positive_sum <- function(x){
  lapply(x, function(x) sum(x[!is.na(x) & x>0]))
}

positive_sum <- function(d) {lapply(d, function(x) sum(x[x>0], na.rm = T))}

x <- data.frame(X1 = c(-1, -2, 0), X2 = c(10, 4, NA), X3 = c(-4, NA, NA))

x[!is.na(x) & x>0]

positive_sum(x)

cars <- c("Mazda", "Volga", "Merc")
car <- "Mazda RX4"
grepl("Mazda", "Mazda RX4") #проверяет наличие первой строки во второй

sapply(cars, function(x) grepl(x, car))

lapply(cars, function(x) grepl(x, car))

iris_num <- iris[sapply(iris, is.numeric)]

sapply(c(rnorm(30)), mean)

tapply(vector, index, function(x) mean(x)) #index - "группирующая" переменная

tapply(mtcars$mpg, mtcars$am, mean)

tapply(mtcars$mpg, mtcars$am, function(x) mean(x))

aggregate(mpg ~ am, mtcars, mean)

aggregate(mpg ~ am + cyl, mtcars, function(x) mean(x))

by(iris[1:4], iris$Species, colMeans) #функция в аргументе должна работать с датафреймом

by(iris[1:4], iris$Species, function(x) sapply(x, 
                                               function(x) shapiro.test(x)$p.value))

aggregate(. ~ Species, iris, function(x) shapiro.test(x)$p.value)

vapply(mtcars, mean, FUN.VALUE = numeric(1))

mapply(rep, 1:4, 1:4)

rep(1,3) #повторяет число 1 - 3 раза

mapply(rnorm, c(20, 25, 13), c(0, 1, 2), c(3, 5, 6)) #отправляет аргументы в функцию по очереди

x <- data.frame(matrix(c(NA, 2:5, NA, 7:10, NA, 12), nrow = 3))
y <- colMeans(x, na.rm = T)
mapply(function(x, y) {x[is.na(x)] <- y; x}, x, y)

m <- matrix(rnorm(100 * 200), nrow = 100)

m_names <- mapply(paste, list("row", "col"), list(1:100, 1:200), sep = "_")

m_names[1]
rownames(m) <- m_names[[1]]
colnames(m) <- m_names[[2]]

get_sd <- function(x){
  num_var <- sapply(x, is.numeric)
  sapply(x[num_var], sd)
}

get_sd(iris)

my_df <- data.frame(x = 1:10, y = letters[1:10])
get_sd(my_df)

sapply(my_df[sapply(my_df, is.numeric)], sd)

get_sd <- function(x){
  num_var <- sapply(x, is.numeric)
  sapply(x[num_var], sd)}

#задача 1
my_names <- function (dataset, names){
  dataset[which(sapply(names, grepl, dataset$name), arr.ind = T)[,1],]
}

dataset <- as.data.frame(list(name = c("p4@HPS1", "p7@HPS2", "p4@HPS3", "p7@HPS4", "p7@HPS5", "p9@HPS6", "p11@HPS7", "p10@HPS8", "p15@HPS9"), expression = c(118.84, 90.04, 106.6, 104.99, 93.2, 66.84, 90.02, 108.03, 111.83)))

names = c("HPS5", "HPS6", "HPS9", "HPS2", "HPS3", "HPS7", "HPS4", "HPS8")

grepl(test_data$name, names)
apply(sapply(names, grepl, test_data$name), 2, any)

my_names(test_data, names)

dataset[sapply(names, grep, dataset$name),]

my_names <- function (dataset, names){    
  gs=gsub('^.*\\@','',dataset[,1])    
  return(dataset[gs %in% names,])}

sapply(names, grep, dataset$name)
?grep

#Задача 2
library(dplyr)
library(lazyeval)
find_outliers <- function(x){
  factor_vars <- names(which(sapply(x, is.factor)))
  num_var <- names(which(sapply(x, is.numeric)))
  x <-  group_by_(x, .dots = factor_vars)
  mutate_(x, is_outlier = interp(~(ifelse(abs(var - mean(var))/sd(var)>2, 1,0)), var = as.name(num_var)))
}
x <- mtcars
x1 <- find_outliers(x)

#Пример правильного решения слушателя Антон Бондаренко id 831067:    
  
  find_outliers <- function(test){    
    tr1 <- split(test[,sapply(test, is.numeric)], lapply(test[,sapply(test, is.factor)], factor))    
    tr2 <- lapply(tr1, function(x) ifelse(abs(x - mean(x)) < 2*sd(x), 0, 1))    
    test$is_outlier <- unsplit(tr2,  lapply(test[,sapply(test, is.factor)], factor))    
    return(test)}    


#Отличное решение от Ulybin Vitaliy id 16285301:    
  
  library(data.table)    
find_outliers <- function(t){    
  fvar<-colnames(t[which(sapply(t,is.factor))])    
  nvar<-colnames(t[which(sapply(t,is.numeric))])    
  t<-as.data.table(t)    
  t[,is_outlier := ifelse(((abs(get(nvar)-mean(get(nvar))))>=(2*sd(get(nvar)))),1,0), by=fvar]}    


#Решение при помощи dplyr:    
  
  library(dplyr)    
library(lazyeval)    
find_outliers <- function(t){    
  numeric_index <- colnames(t)[sapply(t, is.numeric)]    
  factor_index <- colnames(t)[sapply(t, is.factor)]    
  t <- group_by_(t, .dots = factor_index) %>%    
    mutate_(is_outlier = interp(~ifelse(abs(mean(x) - x) > 2 * sd(x), 1, 0), x = as.name(numeric_index)))    
  return(t)}

summarise(x, mean_x = mean(x))

find_outliers(x)

ncol()
test_data <- read.csv("https://stepic.org/media/attachments/course/724/hard_task.csv")
correct_answer <- read.csv("https://stepic.org/media/attachments/course/724/hard_task_ans.csv")

test_data <- read.csv("https://stepic.org/media/attachments/course/724/test_data_2.csv")
correct_answer <- read.csv("https://stepic.org/media/attachments/course/724/hard_task_ans.csv")

x <- test_data

group_by(x, x[sapply(x,is.factor)])

x <- aggregate(x[!sapply(x, is.factor)], x[sapply(x, is.factor)], mean)
x$new <- aggregate(x[sapply(x, is.factor)], x[!sapply(x, is.factor)], function(x) x)

x$new <- x$x
x$new <- z4
sapply(z1[length(z1)], function(x) x)

for(col in ncol(z1[length(z1)])){
  x$new <- z1[length(z1)][, col]
}

x <- test_data

#задача 3
smart_lm <- function(x){
  ifelse(length(x[sapply(x, shapiro.test)['p.value',]>0.05]) > 0, 
         return(lm(x[,1] ~., x[sapply(x, shapiro.test)['p.value',]>0.05][-1])$coefficients), 
            "There are no normal variables in the data"
    )
}

x <- read.csv("https://stepik.org/media/attachments/course/724/test.csv")

smart_lm <- function(x){
  sapply(x, shapiro.test)
}
swiss$religious <- NULL
typeof(smart_lm(x))

smart_lm(x)

x <- as.data.frame(list(V1 = c(20.9, 19.6, 20.6, 19.4, 17.2, 21.9, 19.6, 20.3, 18.6, 23.2, 21.7, 22.5, 18.4, 18.4, 21.5, 25.2, 23, 17.7, 17.9, 18.1, 17.9, 19.3, 18.9, 18.9, 20.5, 19.1, 18.7, 20.7, 20.6, 18.6), V2 = c(23.3, 20.8, 20.3, 17.6, 18.8, 19.2, 19.6, 18.8, 22.3, 22.2, 18.2, 20.6, 20.3, 18.3, 22.5, 20.8, 20.7, 19.1, 20, 18.8, 18.7, 24.6, 22, 20.6, 21.5, 15.9, 19.6, 20.4, 20.5, 20.4), V3 = c(22.4, 20, 14.3, 20.4, 20.2, 20.2, 23.3, 22.3, 19.8, 21.7, 18.6, 17.2, 21.9, 23.1, 19.9, 21.8, 17.7, 20.9, 20.3, 17.4, 19.9, 22.1, 18.6, 22.2, 17.1, 20.7, 18.3, 16.3, 19.4, 18.3)))

lm(x[,1] ~., x[sapply(x, shapiro.test)['p.value',]>0.05][-1])$coefficients

#задача 4
one_sample_t <- function(x, y){
  lapply(x[sapply(x,is.numeric)], function(x){
    c(t.test(x,mu=y)$statistic,
      t.test(x,mu=y)$parameter,
      t.test(x,mu=y)$p.value)
    }
  )
}

z <- one_sample_t(iris[, 1:4], 4)
x <- iris[, 1:4]
z$Sepal.Length$p.value
z1 <- unlist(z)

t.test(iris[,1])
z[1]

lapply(z)
list(z[[1]]$statistic, z[[1]]$parameter, z[[1]]$p.value)

#задача 5
get_p_value <- function(x){
  lapply(x, function(x) x$p.value)
}

normality_tests <- lapply(iris[, 1:4], shapiro.test)
get_p_value(normality_tests)
