##Объектно-ориентированные системы

#Show Keyboard Shortcut Reference Alt+Shift+K

#Copy-on-modify semantics # копия объекта создается в локальном окружение внутри функции

##Пакет microbenchmark и функция system.time показывает быстродействие кода

#ООП это классы, поля этих классов и их методы
##создание классов в R
##S3-классы: 
#1)Нет формальной декларации класса
#2)Функция может иметь разное поведение(method dispatch) в зависимости от класса
#3)Такие функции(для разных классов) называются generic
##S4-классы:
#1)Строгое определение класса и его полей
#2)Больше возможностей для поведения методов
##Reference classes

##Generic - функции
#length(methods(print)) - 186, Проверка функции на generic, если методов больше 1го то generic

##Функция replicate(n, function(x)) вызывает функцию function(x) n раз

##Функция mapply(fun, from = 1:4, to = 2:5, by = 1/(1+1:4)) - многомерный вариант sapply
#где from - список аргументов from, by - список аргументов by, to - список аргументов to
#возвращает лист

#Функция outer(letters, LETTERS, paste0) перебирает ВСЕ КОМБИНАЦИИ аргументов

#Функция Vectorize(function(x, p), "p") векторизует функцию по аргументу p

#Функция do.call(rbind, list(df1, df2, df3))применяет функцию на списки аргументов принимает
#Например do.call(list.files(), function(file) read.csv(file))

#задача

cat_temper <- sort(c("задиристый", "игривый", "спокойный", "ленивый"))
cat_color <- sort(c("белый", "серый", "чёрный", "рыжий"))
cat_age <- sort(c("кот", "котёнок"))
cat_trait <- sort(c("с умными глазами", "с острыми когтями", "с длинными усами"))

cat_catalogue <- outer(cat_temper, cat_color, cat_age, cat_trait, FUN = expand.grid)

s1 <- outer(X = cat_temper, Y = cat_color, paste)

s2 <- outer(X = cat_age, Y = cat_trait, paste)

s3 <- outer(s1, s2, paste)

sort(s3)[42]




# Random walk with absorption
simulate_walk <- function(lower = -10, upper = 10, n_max = 200, p = 1e-3) {
  current_position <- (lower + upper) / 2                        #текущая позиция
  for (i in 1:n_max) {
    is_absorbed <- rbinom(1, 1, p)     #Возвращает тру или фолс по вероятности p
    if (is_absorbed) return(list(status = "Absorbed", 
                                 position = current_position, 
                                 steps = i))
    current_position <- current_position + rnorm(1)#если не адсорбировалось - смещение
    if (current_position < lower) return(list(status = "Left breach",   #выход влево
                                              position = current_position, 
                                              steps = i))
    if (current_position > upper) return(list(status = "Right breach", #выход вправо
                                              position = current_position, 
                                              steps = i))
  }
  return(list(status = "Max steps reached", #максимальное количество шагов(200) пройдено
              position = current_position,
              steps = n_max))
}

# Simulate results
result <- replicate(1000, simulate_walk(), simplify = FALSE) #1000 раз запускаем программу
result <- data.frame(                                        #приводим лист к датафрейму
  status = sapply(result, function(x) x$status),
  position = sapply(result, function(x) x$position),
  steps = sapply(result, function(x) x$steps)
)

# Inspect results
tapply(result$position, result$status, length) #обработка статистики
tapply(result$steps, result$status, mean) #обработка статистики

#Задача с кругом вместо прямой
simulate_walk <- function(n_max = 100, p = 1e-2) {
  current_position <- c(0, 0)                        #текущая позиция
  for (i in 1:n_max) {
    is_absorbed <-  rbinom(1, 1, p)    #Возвращает тру или фолс по вероятности p
    if (is_absorbed) return(1) #Adsorbed
    current_position <- current_position + c(rnorm(1), rnorm(1))#если не адсорбировалось - смещение
    if ((current_position[1]^2+current_position[2]^2)^0.5 > 6) 
      return(2) #Out
  }
  return(3) #"Max steps reached", #максимальное количество шагов(100) пройдено
}

result <- replicate(100000, simulate_walk(), simplify = TRUE)

result1 <- c(sum(result == 1), sum(result == 2), sum(result == 3))

funs <- c("print","summary","plot")
meths <- lapply(funs, methods)

grepl("matrix", meths)
grepl("function", meths)
grepl("default", meths)

m1 <- function(x, y) {
  m <- matrix(0, length(x), length(y))
  for (i in 1:length(x)) 
    for (j in 1:length(y)) {
      m[i, j] = x[i] * y[j]
    }
  m
}

m2 <- function(x, y) {
  vapply(y, function(i) i * x, numeric(length(x)))
}

m3 <- function(x, y) x %o% y

x <- rnorm(100)
y <- runif(1000)
all.equal(m1(x, y), m2(x, y))
all.equal(m2(x, y), m3(x, y))

library(microbenchmark)
microbenchmark(m1(x, y), m2(x, y), m3(x, y))
