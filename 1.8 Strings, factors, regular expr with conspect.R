##Строки

#строки это объекты типа String, строковые векторы - character
#Функции paste(c("a","b"), "c", sep = "", ), paste0(c("a","b"), "c", sep = "", )
#аргумент sep для paste по умолчанию пробел, для paste0 - ничего
#аргумент collapse схлопывает вектор в одну строку по разделителю в кавычках
#Функция strsplit(s, "sep", fixed = TRUE) разбивает строку на список
#аргумент fixed false(по умолчанию) строка разбиения как шаблон в регулярном выражении
#аргумент fixed true строка разбиения как строка разбиения

##Регулярный выражения
#Функция grep("xxx", s) возвращает элементы строки s в которые входит строка "xxx"
#Функция grepl("xxx", s) возвращает логический вектор строки s куда входит строка "xxx"
#grepl и grep работают с листами!
#Функция gsub("xxx", "####", s) заменяет найденный в строке шаблон на второй аргумент
#[[:punct:]] - знаки припинания в регулярных выражениях
#[[:alpha:]] - буквы
#[:space:] - пробелы
#[[:digit:]]
# "\\b[[:alpha::]]{4,5}\\b" слова состоящие из 4 или 5 букв
#$ - конец строки
#* - любое количество символов
#^ - начало строки
#+ - 1 и больше символов
#tolower(month.name), toupper(month.abb) приведение к нижнему и верхнему регистру

#Пакет для работы со строками - stringr
#str_extract(s, 'н.') для каждого элемента в векторе возвращает первого вхождения шаблона
#str_replace(s, "[иа]", "?") заменяет шаблон на указанный третий аргумент
#[иа] - первая попавшаяся буква, которая или и или а
#str_extract_all(s, "н."), all - ВСЕ вхождения, а не первое
#str_replace_all(s, "[иае], "?") заменить ВСЕ шаблоны на другую строку
#str_length(s) возвращает длину строки

##Форматирование чисел
#formatC(c(pi,exp(pi)), exp(pi), digits = 3, format = "e") оставляет digits знаков в числе

#Функция cat("\tx\tc") преобразует служебные символы в отличии от print()

##Факторы
#Качественные переменные имеют тип factor
#as.numeric(f) для фактора переводит фактор в числа
#Функция levels(f) - возвращает градации
#Функция nlevels(f) - возвращает уровни фактора
#(f <- droplevels(f)) - убирает пустые градации, то есть ту, у которой нет значений
#выражение в скобках сразу отправляется на печать
#упорядоченный фактор ordered(f, x) в порядке вектора x!!, или factor(ordered = TRUE) 
#Функция cut(rnorm(1000), -5:5) разбивает нумерик вектор на фактор
#Функция tapply(warpbreaks$breaks, warpbreaks$wool, max) разбивает массив по факторам

options(stringsAsFactors = FALSE)
avian <- read.csv("avianHabitat.csv")
avian$Observer <- as.factor(avian$Observer)

coverage_variables <- names(avian)[str_detect(names(avian),"^P")]

sapply(coverage_variables, function(name) check_percent_range(avian[[name]]))

check_percent_range <- function(x){
  any(x<0 | x>100)
}

unique(avian$Site)

avian$site_name <- factor(str_replace(avian$Site, "[:digit:]+", ""))

tapply(avian$DBHt, avian$site_name, mean)

tapply(avian$total_coverage, avian$site_name, mean)

tapply(avian$LHt, avian$Observer, max)

quakes$mag <- cut(quakes$mag, seq(min(quakes$mag), ceiling(max(quakes$mag)), 0.5), right = F)

library(dplyr)
library(stringr)

summarise(group_by(quakes, mag), n())

tapply(warpbreaks$breaks, warpbreaks$wool, max)

table(cut(rnorm(1000), -5:5))

f <- factor(sample(LETTERS, 30, replace = T), ordered = T)

temp <- (c("feeezing cold", "cold", "comfortable", "burning hot"))

ft <- ordered(sample(temp, 14, replace = TRUE), temp)

f1 <- c(6,5,4,1,3,0)

ft <- ordered(sample(f1, 14, replace = TRUE), f1)

as.numeric(f)
as.character(f)

cat("\tx\tc")
print("\tx\tc")

formatC(c(pi,exp(pi)), exp(pi), digits = 3, format = "e")

paste(c("a","b"), "c", sep = "_")

paste0(c("a","b"), "c", collapse = "")

strsplit(c('a b', 'c d', 'e f'), " ", fixed = TRUE)

s <- c("Терпение и труд всё перетрут", 
       "Кончил дело - гуляй смело",
       "Без труда не вытащишь и рыбку из пруда",
       "Работа не волк, в лес не убежит")

strsplit(s, "[[:punct:]]")

s <- c("xc xd xa xb", "xxxxxxxxxxx")

str_extract(s, 'о.')

grep("з", s)

str_replace(s, "[иа]", "?")

gsub("xxx", "####", s)

str_replace_all(s, "[иае]", "?")

nchar("Аэрофотосъёмка ландшафта уже выявила земли богачей и процветающих крестьян.")

library(stringr)

hamlet <- "To be, or not to be: that is the question:
Whether 'tis nobler in the mind to suffer
The slings and arrows of outrageous fortune,
Or to take arms against a sea of troubles,
And by opposing end them?"

hamlet <- str_replace_all(hamlet, "[:punct:]", "")
hamlet <- tolower(unlist(str_split(hamlet, "[:space:]")))

unlist(str_extract(hamlet, "to"))

sum(str_count(hamlet, "to"))

sum(grepl("[fqw]", hamlet))

sum(grepl("\\b.{7}\\b", hamlet))

unlist(str_extract(hamlet, "[fqw]"))

sum(str_count(hamlet, "[fqw]"))
