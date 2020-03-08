##Пакеты dplyr, tidyr

#Разработчик Harley Wickham: dplyr, tidyr, ggplot, ggvis(визуализация)

#Пакет tidyr нужен для "очистки данных" (концепция tidy data)

set.seed(1122)

set.seed(1122)
df <- data.frame(Name = c("John", "Peter", "Mary", "Caroline"),
                 DrugA_T1 = runif(4, 35, 36),
                 DrugA_T2 = runif(4, 36, 39),
                 DrugB_T1 = runif(4, 36, 36.6),
                 DrugB_T2 = runif(4, 37, 38.5)
)


##Пакет tidyr
library(tidyr)

df1 <- gather(df, Variable, Temperature, -Name) #Группирует и создает новый датафрейм 
df2 <- separate(df1, Variable, c("DrugType", "Time"), "_") #разделяет переменную

##Пакет dplyr
library(dplyr)

select(df2, Time, Temperature) #выбор по столбцам

select(df2, 3:4)             #выбор по столбцам
select(df2, starts_with("T")) #выбор по столбцам
select(df2, -Name, -DrugType) #выбор по столбцам

filter(df2, Temperature>37, Name %in% c("John", "Mary")) #Выбор по строкам

arrange(df2, Name, -Temperature) #сортируем по Name и в обратном порядке по температуре

mutate(df2, DrugType = gsub("Drug", "", DrugType)) #изменение переменной
#transmute - то же что и mutate, но с удалением исходных колонок

summarise(group_by(df2, Time),         #вычисление нового датафрейма с нужными результатами
          AvgTemp = mean(Temperature)
          )
select(df, -c(3:4))

#inner_join Объединение двух дата фреймов по ключу; берутся только значения по ключам, встречающимся в обоих дата фреймах

#sample_n Дата фрейм, состоящий из случайных рядов исходного

#Оператор %>% конвейерный оператор передающий то что слева в то что справа
#x %>% f == f(x), x %>% f(y) == f(x, y), x %>% f(y, param = .) == f(y, param = x) (точка)

#Пример
df <- data.frame(type = c(1,1,2,2,3,3), value = c(5,10,50,100,7,7))

df %>%
  group_by(df, type)%>% 
  summarise(Total = sum(value))%>%
  arrange(-Total)

#Без использования конвейера %>%

library(stringr)
library(dplyr)
options(stringAsFactors = FALSE)

avian <- read.csv("avianHabitat.csv")

avian <- subset(avian, PDB > 0 & DBHt > 0, c("Site", "Observer", "PDB", "DBHt"))

avian$Site <- factor(str_replace(avian$Site, "[:digit:]+", ""))

subset(
  aggregate(avian$DBHt, list(Site = avian$Site, Observer = avian$Observer), max),
  x >=5
)

#С использованием конвейера %>%

avian <- read.csv("avianHabitat.csv")

avian <- 
  avian %>%
  subset(PDB > 0 & DBHt > 0, c("Site", "Observer", "PDB", "DBHt")) %>% 
  transform(Site = factor(str_replace(.$Site, "[:digit:]+", "")))

aggregate(avian$DBHt, list(Site = avian$Site, Observer = avian$Observer), max) %>% 
  subset(x >=5)

#С использованием %>% и dplyr

avian %>% 
  filter(PDB > 0, DBHt >0) %>%                           #фильтруем
  select(Site, Observer, contains("DB")) %>%             #Выбираем стобцы
  mutate(Site = factor(str_replace(Site, "[:digit:]+", ""))) %>% #ИЗменяем
  group_by(Site, Observer) %>%                                    #Группируем
  summarise(MaxHt = max(DBHt)) %>%                       #суммарайс
  filter(MaxHt >=5)                                       #фильтр суммарайса
  
  
#Задача

avian <- read.csv("avianHabitat.csv")

s <- avian %>%
  mutate(Site = factor(str_replace(Site, "[:digit:]+", "")))%>%
  group_by(Site, Observer) %>%
  select(Site, Observer, contains("Ht")) %>%
  summarise_each(function(x) sum(x>0))

#Tidy data
#library(tidyr): ?gather, ?separate
#library(dplyr):
#  ?select
#?filter
#?arrange
#?mutate
#?group_by, ?summarise
#?"%>%" piping operator  

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