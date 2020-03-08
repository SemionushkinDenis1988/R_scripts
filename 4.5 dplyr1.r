install.packages("dplyr")

library(dplyr)

my_data <- data_frame(x = rnorm(10000), y = rnorm(10000), 
                      f = factor(rep(1:2, 5000)))

my.data <- data.frame(x = rnorm(10000), y = rnorm(10000), 
                      f = factor(rep(1:2, 5000)))

library(ggplot2)

diamonds <- as_data_frame(diamonds)

my_data_2 <- data_frame("My var" = rnorm(10))

my.data.2 <- data.frame("My var" = rnorm(10))

my_data_3 <- data_frame(x = rnorm(10), y = abs(x))

my.data.3 <- data.frame(x = rnorm(10), y = abs(x))

select(diamonds, cut, price) #выбор стобцов
diamonds[, c("price", "cut")]

select(diamonds, cut:price) #все столбцы от cut до price
select(diamonds, -cut)

select(diamonds, starts_with("c"), ends_with("t"), contains("c"))

slice(diamonds, 2:10) #выбор строчек

filter(diamonds, carat >0.3, color == "J") #фильтрует дата фреймо по условиям
filter(diamonds, carat >0.3| color == "J")

arrange(diamonds, price, depth) #сортировка датафрейма по переменным

arrange(diamonds, desc(price, depth)) #сортировка в обратном направлении

order(c(1,-2,3,-5,-1,0)) # возвращает индексы элементов по позрастанию

sort(c(1,-2,3,-5,-1,0))  #сортирует вектор

rename(diamonds, new_cut = cut) #переименовывает cut в new_cut

names(mtcars)[c(1,4)] <- c("new_mpg", "new_hp")

m <- mutate(diamonds, 
            sqrt_price = sqrt(price), 
            log_carat = log(carat))

mutate(mtcars, am = factor(am), vs = factor(vs))

str(mutate(mtcars, am = factor(am), vs = factor(vs)))

library(ggplot2)

ggplot(diamonds)+
  geom_point(aes(x = price, y = carat))

ggplot(m)+
  geom_point(aes(x = sqrt_price, y = carat))

?seq

seq(0, 1, length.out = 11)

select(arrange(filter(iris, Petal.Length > 1.7), Sepal.Length), Sepal.Length, Petal.Length)

filtered_iris <- filter(iris, Petal.Length > 1.7)
arranged_iris <- arrange(filtered_iris, Sepal.Length)
selected_iris <- select(arranged_iris, Sepal.Length, Sepal.Width)

#Оператор %>% (Ctrl + Shift + m)

iris %>% 
  filter(Petal.Length > 1.7) %>% 
  arrange(Sepal.Length) %>% 
  select(Sepal.Length, Sepal.Width)

#задача 1
library(ggplot2)
d <- slice(diamonds, seq(1,length(diamonds[[1]]),2))

seq(1,length(diamonds[[1]]),2)

d <- diamonds[c(T,F), ]

diamonds[c(T,F), ]

d <- filter(diamonds, row_number() %% 2 != 0)

#задача 2
my_df <- mtcars %>% 
  select(mpg, hp, am, vs) %>% 
  filter( mpg > 14, hp > 100) %>% 
  arrange(desc(mpg)) %>% 
  slice(1:10) %>% 
  rename('Miles per gallon' = mpg, 'Gross horsepower'= hp)

d <- as_data_frame(matrix(rnorm(30), ncol = 5))

mutate_each(d, funs(.*3))

mutate_each(d, funs(ifelse(.<0, 0, .)))

#задача 3
all_to_factor <- function(x){
  mutate_each(x, funs(factor))
}

d <- all_to_factor(d)
str(d)
summary(d)

#задача 4
log_transform <- function(x){
  mutate_all(x, funs(ifelse(sapply(.,is.numeric),log(1 + (. - min(.))/(max(.)-min(.))), .)))
}

log_transform <- function(test_data){
mutate_if(test_data, is.numeric, funs(log( ((.)-min(.))/(max(.)- min(.))+1 )  ) )
}

test_data <- as.data.frame(list(V1 = c(-1.6, 0.6, 0.5, 0.8, -0.7), V2 = c(-0.6, 0.3, 2, 0.6, 0), V3 = c(0, -0.2, 0.8, 0.8, 3.1), V4 = c("B", "A", "B", "B", "B")))

test_data1 <- log_transform(test_data)
?mutate_all
#scales -- еще один пакет от Hadley Wickham.
#rescale -- функция, которая "rescale numeric vector to have specified minimum and maximum"

diamonds <- as_data_frame(diamonds)

group_by(diamonds, cut)

gr_diamonds <- group_by(diamonds, cut, color)

sample_n(diamonds, 2)

slice(diamonds, 1)

sample_n(gr_diamonds, 2)

summarise(mtcars, mean_disp = mean(disp), sd_disp = sd(disp))

summarise(gr_diamonds, 
          numbers = n(),
          mean_price = mean(price),
          mean_x = mean(x), 
          median_y = median(y),
          min_y = min(y),
          great_price = sum(price > 5000))

sum(filter(diamonds, cut == "Fair", color == "D")$price>5000)

gr_mtcars <- group_by(mtcars, am, vs)
summarise_all(gr_mtcars, funs(mean))

summarise_all(gr_mtcars, funs(sum(.>10)))


group_by(iris, Species) %>% 
  summarise_all(funs(sd, mean))

select(mtcars, am, hp)
select_(mtcars, interp(~c("am", "hp")))

slice_()
filter_()
group_by_()
mutate_()

var_to_select <- c("hp", "mpg")
c <- select_(mtcars, .dots = var_to_select)
?select_
mtcars$am <-factor(mtcars$am)
mtcars$vs <-factor(mtcars$vs)

factor_vars <- names(which(sapply(mtcars, is.factor))) # сохраним имена факторов в вектор
mtcars %>% 
  group_by_(.dots = factor_vars) %>% 
  summarise(n = n())

mutate(mtcars, new_var = (hp - mean(hp)) / sd(hp))

mini_mtcars <- select(mtcars, hp, am, vs)
mini_mtcars <- mini_mtcars %>% mutate(am = factor(am), 
                                      vs = factor(vs))

mutate_(mini_mtcars, new_var = "(hp - mean(hp)) / sd(hp)")

mutate_(mini_mtcars, new_var = ~ (hp - mean(hp)) / sd(hp))

mutate_(mini_mtcars, new_var =  quote((hp - mean(hp)) / sd(hp)))

install.packages(lazyeval)

library(lazyeval)
interp("(var - mean(var)) / sd(var)", var = as.name(num_var))
interp(quote((var - mean(var)) / sd(var)), var = as.name(num_var))

num_var <- names(which(sapply(mini_mtcars, is.numeric)))
mutate_(mini_mtcars, new_var = interp(~(var - mean(var)) / sd(var), var = as.name(num_var)))

library(lazyeval)
var_for_group <- c("am", "vs")
var_for_filter <- "hp"
var_for_arrange <- "mpg"
var_for_mutate <- "qsec"
var_for_summirise <- "cyl"
group_by_(mtcars, .dots = var_for_group) %>% 
  filter_(interp(~var > 100, var = as.name(var_for_filter))) %>% 
  arrange_(var_for_arrange) %>% 
  mutate_(new_var = interp(~ifelse(var > mean(var), 1, 0), 
                           var = as.name(var_for_mutate))) %>% 
  summarise_(max = interp(~max(var), var = as.name(var_for_summirise)))

summarise_(mtcars, ~shapiro.test(mpg)$p.value)

#задача 5
library(dplyr)
library(lazyeval)
descriptive_stats <- function(dataframe){
  factor_vars <- names(which(sapply(dataframe, is.factor)))
  num_var <- names(which(sapply(dataframe, is.numeric)))
  gr_dataframe <- group_by_(dataframe, .dots = factor_vars)
  summarise_(gr_dataframe,
            n = interp(~n(), var = as.name(num_var)),
            mean = interp(~mean(var, na.rm = T), var = as.name(num_var)),
            sd = interp(~sd(var, na.rm = T), var = as.name(num_var)), 
            median = interp(~median(var, na.rm = T), var = as.name(num_var)),
            first_quartile = interp(~quantile(var, 0.25, na.rm = T), var = as.name(num_var)),
            third_quartile = interp(~quantile(var, 0.75, na.rm = T), var = as.name(num_var)),
            na_values = interp(~sum(ifelse(is.na(var), 1, 0)), var = as.name(num_var))
            )
}

descriptive_stats <- function(df){
  df %>% 
    group_by_(.dots = names(which(sapply(df, is.factor)))) %>%
    summarise_if(is.numeric, funs(
      n=n(), 
      mean = mean(., na.rm=T),
      sd = sd(., na.rm=T),
      median = median(., na.rm=T),
      first_quartile = quantile(., probs=0.25, na.rm=T),
      third_quartile = quantile(., probs=0.75, na.rm=T),
      na_values = sum(is.na(.))
    ))
}

?quantile
quantile(dataframe$salary, na.rm = T)[2]

descriptive_stats(test_data)

test_data <- read.csv("https://stepic.org/media/attachments/course/724/salary.csv")

dataframe <- test_data

#задача 6
to_factors <- function(dataframe, factors){
    mutate_at(dataframe, .vars = factors, funs(as.factor(ifelse(.>mean(.), 1, 0))))
}

to_factors <- function(test_data, factors){    
  test_data[factors] <- mutate_each(test_data[factors], funs(factor(ifelse(. > mean(.), 1, 0))))    
  return(test_data)}

to_factors(mtcars[1:4], factors = c(1, 3))

#задача 7
high_price <-
  diamonds %>%
  group_by(color)%>%
  arrange(desc(price))%>%
  select(color, price)%>%
  slice(1:10)
