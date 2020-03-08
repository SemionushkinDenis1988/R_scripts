# steps 3 - 4 data_frame

install.packages("dplyr")
library(dplyr)

my_data <- data_frame(x = rnorm(10000), y = rnorm(10000), 
                      f = factor(rep(1:2, 5000)))

my.data <- data.frame(x = rnorm(10000), y = rnorm(10000), 
                      f = factor(rep(1:2, 5000)))
library(ggplot2)

diamonds <- as_data_frame(diamonds)
diamonds
glimpse(diamonds)

my_data_2 <- data_frame(x = rnorm(10), y = abs(x))
my.data.2 <- data.frame(x = rnorm(10), y = abs(x))

# step 5 select columns
select(diamonds, 1, 2, 3)
diamonds[c("cut", "price", "color")]

select(diamonds, contains("t"))


# step 6 slice rows
slice(diamonds, c(1, 4, 5))
diamonds[c(1, 4, 5)]


# step 7 filter observations
filter(diamonds, carat > 0.3 | color == "J")
diamonds[diamonds$carat > 0.3 & diamonds$color == "J", ]
subset(diamonds, carat > 0.3 & color == "J")


# steps 8 - 9 arrange and mutate
arrange(diamonds, desc(price))
diamonds[order(diamonds$price, diamonds$depth), ]

m <- mutate(diamonds, 
            sqrt_price = sqrt(price), 
            log_carat = log(carat))

mutate(mtcars, am = factor(am), vs = factor(vs))

# step 2 mutate_each
library(ggplot2)
library(dplyr)

d <- as_data_frame(matrix(rnorm(30), ncol = 5))

mutate_each(d, funs(ifelse(. < 0, 0, .)))

col_1 <- d$V1
col_2 <- d$V2

ifelse(col_1 < 0, 0, col_1)
ifelse(col_2 < 0, 0, col_2)

my_fun <- function(x) ifelse(x < 0, 0, x)
sapply(d, function(z) abs(z))


# step 6 group_by 
diamonds <- as_data_frame(diamonds)

gr_diamonds <-  group_by(diamonds, cut)

sample_n(diamonds, 2)
slice(diamonds, 1)

sample_n(gr_diamonds, 2)

slice(gr_diamonds, 1)


# step 7 group_by and summarise
?summarise()

summarise(gr_diamonds,
          numbers = n(),
          mean_price = mean(price), 
          mean_x = mean(x), 
          median_y = median(y), 
          min_y = min(y))

# step 8
gr_diamonds <-  group_by(diamonds, cut, color)
summarise(gr_diamonds,
          numbers = n(),
          mean_price = mean(price), 
          mean_x = mean(x), 
          median_y = median(y), 
          min_y = min(y))

summarise(gr_diamonds,
          numbers = n(),
          mean_price = mean(price), 
          mean_x = mean(x), 
          median_y = median(y), 
          min_y = min(y), 
          great_price = sum(price > 5000))

# step 9
gr_mtcars <- group_by(mtcars, am, vs)
my_means <- summarise_all(gr_mtcars, funs(mean))
