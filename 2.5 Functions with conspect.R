##Функции

#Инкапсуляция - когда одна функция спрятана в другую
#Функции это объект первого уровня(то есть того же уровня что и например вектор)
#Функция sign() возвращает знак каждого элемента или 0 если элемент равено 0
#Функция all.equal(x, y) сравнивает x и y, если равно то тру, если нет то фолс
#Обратиться к исходному коду любой функции можно так - (f <- function(x) x^5), (sd) 
#Eсли в выводе есть .C, .Call, .Fortran, .External, .Internal, .Primitive - это обращение
#к скомпилированному коду, то есть чтобы это найти надо смотреть исх. код R
#например (var) из (sd)
#Eсли в выводе есть UseMethod или standartGeneric, то это method dispatch для классов S3/S4?
#methods(plot)[1:20] (полиморфизм, для каждого объекта свой метод)
#Функция возвращает результат по слову return или последнее значение последней операции

##Разбор аргументов:
#f <- function(arg1, arg2, remove_na = TRUE, ..., optional_arg)
#при выполнении функции f(1, arg2, remove = F, optional_arg = 42, do_magic = TRUE)
#первыми будут взяты аргументы по совпавшему имени
#вторыми будут взяты аргументы, чьё имя частично совпало (remove)
#далее позиционные 1:5 + 1:2аргументы
#некоторые аргументы могут вычисляться из ранее полученных!!
#неразобранные аргументы попадают в "..."(ellipsis)(передача множества аргументов, проброс аргументов)
#проброс аргументов пример: f <- function(x, pow = 2) x^pow
#integrate(f, 0, 1, pow = 3) - pow=3 попадает в "..." функции integrate и пробрасывается в f
#%in% - бинарный оператор вхождения, можно создавать свой собственный оператор
#например %nin% <- function(x, y) {...}
#Пакет stringi имеет множество функций для разворота строк и прочего
#Функция outer(x, y) возвращает все возвожные комбинации векторов x, y и применяет FUN к ним
##Объектно-ориентированные системы

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

##Функция replicate(5, function(x)) вызывает функцию function(x) 5 раз

##Функция mapply(fun, from = 1:4, to = 2:5, by = 1/(1+1:4)) - многомерный вариант sapply
#где from - список аргументов from, by - список аргументов by, to - список аргументов to
#возвращает лист

#Функция outer(letters, LETTERS, paste0) перебирает ВСЕ КОМБИНАЦИИ аргументов

#Функция Vectorize(function(x, p), "p") векторизует функцию по аргументу p

#Функция do.call(rbind, list(df1, df2, df3))применяет функцию на списки аргументов принимает
#Например do.call(list.files(), function(file) read.csv(file))


"%+%" <- function(x, y) {
  length(y) <- length(x) <- max(length(x), length(y))
  x+y
}
y <- c(1:5)
x <- c(1:2)

x %+% y

max(length(x), length(y))

length(y) <- length(x)

y

#Создание колоды карт
values <- c("Ace", 2:10, "Jack", "Queen", "King")
suits <- c("Clubs", "Diamonds", "Hearts", "Spades")
card_deck <- outer(values, suits, paste, sep = " of ")

#Создание рулетки

generator <- function(set) { 
  function(n) sample(set, n, replace = T)
} 

roulette_values <- c("Zero!", 1:36)
rigged_roulette_values <- c("Zero!", "Zero!", 1:36)
fair_roulette <- generator(roulette_values)
rigged_roulette <- generator(rigged_roulette_values)


#Генератор функций

generator <- function(set) function(n) sample(set, n, replace = T)

#варианты генератора

card_generator <- generator(card_deck) #фиксируем card_deck в card_generator

card_generator(n = 2) #вызываем card_generator с аргументом n

decorate_string <- function(pattern, ...){ 
  paste(pattern, paste(...), stringi::stri_reverse(pattern), sep = "")
}

decorate_string(pattern = "123", "abc", "def", sep = "+")

library(stringr)

?matrix

fun_list <- c(mean, max)

sapply(fun_list, function(f) f(1:100)) #функции в первом аргументе, передаются во вторую

apply_f <- function(f, x) f(x)         #создание функции apply_f

sapply(fun_list, apply_f, x = 1:100)  #функции в первом аргументе, передаются во вторую 

apply_f(function(x) sum(x^2), 1:10)       #apply_f передает 2й аргумент в функцию в 1м арг.

square <- function() function(x) x^2 #функция, которая возвращает другую функцию

square()(5) #искользование функции, которая возвращает другую функцию

f <- function(x){                 
  g <- function(y) if (y>0) 1 else if (y < 0) -1 else 0 #функция внутри другой функции
  sapply(x, g)
}
sign(-100:100)

my_calc  <- function(x, y){
  s  <- x + y
  return(s)
}

my_calc(x = 10, y = 15)
result  <- my_calc(10, 15)

my_calc_2  <- function(x, y){
  s  <- x + y
  d  <- x - y
  return(c(s, d))
}

my_calc_2(10, 15)


my_calc_3  <- function(x, y, z){
  s  <- x + y + z
  d  <- x - y - z
  return(c(s, d))
}

my_calc_3  <- function(x, y, z = 10){
  s  <- x + y + z
  d  <- x - y - z
  return(c(s, d))
}



############

distr1  <- rnorm(100)
distr1[1:30]  <- NA
distr1[is.na(distr1)]  <- mean(distr1, na.rm = T)

my_na_rm  <- function(x){
  if (is.numeric(x)){
    stat_test  <- shapiro.test(x)
    if (stat_test$p.value > 0.05){
      x[is.na(x)]  <- mean(x, na.rm = T)
      print("NA values were replaced with mean")
    } else{
      x[is.na(x)]  <- median(x, na.rm = T)
      print("NA values were replaced with median")
    }
    return(x)
  } else{
    print("X is not numeric")
  }
}

d1  <- rnorm(2000)
d2  <- runif(2000)

d1[1:10]  <- NA
d2[1:10]  <- NA

d1  <- my_na_rm(d1)
head(d1)

d2  <- my_na_rm(d2)
head(d2)


######################

read_data  <- function(){
  df  <- data.frame()
  number  <<- 0
  for (i in dir(pattern = "*.csv")){
    temp_df  <- read.csv(i, stringsAsFactors = F)
    df  <- rbind(temp_df, df)
    number <<- number + 1
    }
  print(paste(as.character(number), "files were combined"))
  return(df)
}

# Advanced method without for loop

read_data_advanced <- function(){
    df <- do.call(rbind, lapply(dir(pattern = "*.csv"), 
                                read.csv, stringsAsFactors = F))
    return(df)
}

df  <- data.frame(x = factor(1:5))
df1  <- data.frame(x = factor(7:8))
str(df)
str(df1)

df3  <- rbind(df, df1)
str(df3)
table(df3$x)
