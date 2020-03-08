library(help = "datasets")
mtcars
data(mtcars)
help(mtcars)
my_data <- mtcars

my_data$even_gear <- abs(my_data$gear%%2 - 1)
my_data

mtcars$mpg_4 <- subset(mtcars, cyl == 4)$mpg

subset(mtcars, cyl == 4)$mpg_4 <- subset(mtcars, cyl == 4)$mpg

mtcars$mpg_4 <- 0
mtcars$mpg_4[mtcars$cyl == 4] <- mtcars$mpg[mtcars$cyl == 4]

mpg_4 <- mtcars$mpg[mtcars$cyl == 4]

mini_mtcars <- mtcars[c(3,7,10,12,nrow(mtcars)),]

new_data <- subset(mtcars, cyl != 3 & qsec > mean(qsec))