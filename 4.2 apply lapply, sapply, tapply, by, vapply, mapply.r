# steps 2 - 3 for vs apply 
library(ggplot2)

data(diamonds)
str(diamonds)

min_size <- numeric(nrow(diamonds))
for (i in 1:nrow(diamonds)){
  min_size[i] <-  min(diamonds[i, 8:10])
}

min_size_2 <- apply(diamonds[, 8:10], 1, min)


# steps 4 and 7 apply function
?apply(array, margin, ...)

apply(X, MARGIN, FUN, ...)

d <- matrix(rnorm(30), nrow = 5)

apply(d, MARGIN = 1, FUN = sd)
apply(d, MARGIN = 2, FUN = sd)

apply(mtcars, 2, sd)
apply(mtcars, 1, sd)

s <- apply(d, MARGIN = 2, FUN = sd)
range(1:10)

my_range <- apply(d, MARGIN = 2, FUN = range)
my_range


# step 8 apply advanced
outliers_count <- function(x){
  otliers <- x[abs(x - mean(x)) > 2 * sd(x)]
  if (length(otliers) > 0){
    return(otliers)
  } else {
    return("There are no otliers")
  }
}

iris_num <- iris[, 1:4]

iris_outliers <- apply(iris_num, 2, outliers_count)
str(iris_outliers)

# step 2 apply(x, 1, mean, ...) 
head(airquality)
?airquality

apply(airquality, 2, mean, na.rm = T)

colMeans()
colSums()
rowMeans()
rowSums()

# step 3 
set.seed(42)

d <- as.data.frame(matrix(rnorm(30), nrow = 5))

my_fun <- function(x) x * 2

d[1, 1] <- NA

my_list <- list()
for (i in seq_along(d)){
  temp_col <- d[, i]
  neg_numbers <- temp_col[temp_col < 0]
  my_list[[i]] <- neg_numbers
}
names(my_list) <- colnames(d)

my_list <- apply(d, 2, function(x) x[x < 0])


find_negative <- function(x){
  x[x < 0]
}
find_positive <- function(x){
  x[x > 0]
}

apply(d, 2, find_positive)


# step 4
apply(array, margin, ...)
head(iris)

aov_result <- apply(iris[, 1:4], 2, function(x) aov(x ~ iris$Species))

norm_test <- apply(iris[, 1:4], 2, 
                   function(x) shapiro.test(x))

norm_test_p <- apply(iris[, 1:4], 2, 
                     function(x) shapiro.test(x)$p.value)


str(aov_result)
aov_result$Petal.Length
norm_test$Petal.Width

# step 1 lapply 
apply(array, margin, ...)

lapply(list, function)

my_list <- list(x = c(rnorm(30), NA), y = rnorm(10))
str(my_list)

lapply(my_list, mean)
lapply(my_list, mean, na.rm = T)
lapply(my_list, function(x) x * 2)

sapply(my_list, range, na.rm = T, simplify = F)

# step 2

cars <- c("Mazda", "Volga", "Merc")
car <- "Mazda RX4"  

sapply(cars, function(x) grepl(x, car))
lapply(cars, function(x) grepl(x, car))

# step 3 by tapply
tapply(mtcars$mpg, mtcars$am, function(x) mean(x))
aggregate(mpg ~ am, mtcars, function(x) mean(x))

by(iris[1:4], iris$Species, 
   function(x) sapply(x, 
                      function(col) shapiro.test(col)$p.value))

aggregate(. ~ Species, iris, function(x) shapiro.test(x)$p.value)


# step 4 vapply, 

vapply(list, function, FUN.VALUE = type, ...)
vapply(mtcars, mean, FUN.VALUE = numeric(1))
sapply(mtcars, mean)

mapply(rep, c(1, 2, 3, 4), c(1, 2, 3, 4))

rep(1, 3)
x <- c(20, 25, 13)
m <- c(0, 1, 2)
s <- c(3, 5, 6)
mapply(rnorm, x, m, s)


