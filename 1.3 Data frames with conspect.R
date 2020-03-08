##Датафрейм

##Чтение датафрейма возможно из файлов следующих форматов:
#Comma sep. values(csv), tab sep. values(tsv)
#Неструктурированный текст - readlines, scan(низкоуровневые функции) (для больших файлов)
#XML, HTML - library(XML), library(httr),...
#JSON, YAML - library(JXON), library(YAML),...
#EXCELE - library(XLConnect), library(readxl)
#SAS, Stats, SPSS, MATLAB - library(foreign), library(sas7bdat)
##Напрямую из Web - library(rvest)
##Из баз данных:
#Реляционные базы данных - library(DBI), library(RSQLite),...
#Нереляционные базы данных - library(rmongodb)

#Датафрейм это структура сочетающая в себе свойства матрицы и листа, именованный лист
#функция df[complete.cases(df),], na.omit(df) удаляют NA значения
#Функция within(df, var1,var2, fun) работает с несколькими переменными
#для распознания дат в файле есть функции as.POSIXct, as.POSIXlt, as.Date 
#Функция data.frame(x=1:4, y = LETTERS[1:4], z=c(T,F)) создает дата фрейм
#Функция str(df) возвращает структуру датафрейма
#Функции rownames(df), colnames(df), dimnames(df) - именя строк, колонок, строк и колонок
#Функции nrow(df), ncol(df), dim(df) - строки, колонки, строки и колонки
#Функция length(df) - возвращает количество столбцов
#Функция names(df) - возвращает имена столбцов
#Функция read.table('file', header, sep, na.strings, colclasses, commet.char, skip) - считывает файл в датафрейм и создает датафрей
#'file' - имя файла
#'header' - наличие или отсутствие заголовка в первой строке
#'sep' - разделитель значений
#'quote' - символ обозначающий кавычки
#'na.strings' - строки кодирующие пропущенные значения
#'colClasses' - типы столбцов
#'commentchar' - символ обозначающий комментарии
#'skip' - количество строк пропускаемых с начала файла
#Фунция write.table(), write.csv(), write.csv2() записывают новый файл
#Функция read.csv(), read.csv2(), read.delim(), read.delim2() - оболочки функции read.table
#Функция head(mydata, 3) - возвращает первый элементы дата фрейм
#Функция tail(mydata) - возвращает последние элементы дата фрейма
#Фунция View(mydata) - просмотр дата фрейма
#Функция summary(mydata) - саммари датафрейма
#Функция subset(df, условие, select) возвращает фильтрованный датафрейм 
#Фунции rbind(), cbind() объединяют датафреймы по строкам и колонкам
#Функция order() упорядочивает датафрейм по условию
#Функция rowSums() возвращает сумму строк

##Комбинирование по ключу
#Функция merge(mydata, mydata_salary, by = "x") Объединяет датафреймы по ключу в аргументе by

##Итерация
#vector
#mtcars$mpg #vector
#mtcars[,1] #vector
#mtcars[[1]] #vector
#mtcars[['mpg']] #vector
#df
#mtcars['mpg'] #df
#mtcars[1] #df
#mtcars[,1,drop=F] #df

#Задача сортировка дата фрейма
s <- head(
  attitude
  [order
    (attitude$learning, decreasing = T), 
    c("complaints", "raises", "advance")
    ], 
  5
  ) 

s$sum_values <- apply(s, 1,  sum)

#Задача сабсетинг

subset(attitude, order(learning, decreasing = T), complaints, raises, advance )

merge(mydata, mydata_salary, by = "x")

##работа с датасетом avian

avian <- read.csv("avianHabitat_sewardPeninsula_McNew_2012.csv")

##1)Проверяем дату

str(avian) 
#все данные числовые
head(avian)
summary(avian)

any(!complete.cases(avian)) #проверка пропущенных значений
any(avian$PDB<0 | avian$PDB>100)

#2)Трансформируем переменные

names(avian)

coverage_variables <- names(avian)[-(1:4)][c(T,F)]
avian$total_coverage <- rowSums(avian[, coverage_variables])

summary(avian$total_coverage)

#Задача объединение массивов

Habitat <- read.csv("avianHabitat.csv")
Habitat2 <- read.csv("avianHabitat2.csv", 
                     skip = 5, 
                     sep = ";", 
                     comment.char = "%",
                     quote = '',
                     na.strings = "Don't remember"
                     )

Habitat2$Observer <- "CL"

Habitat3 <- rbind(Habitat, Habitat2)

coverage_variables <- names(Habitat3)[-(1:4)][c(T,F)]
Habitat3$total_coverage <- rowSums(Habitat3[,coverage_variables])

summary(Habitat3$total_coverage)

#Задача найти высоту растительности в avian

height_variables <- names(avian)[-(1:4)][c(F,T)]

head(avian[order(avian$DBHt, decreasing = T),height_variables], 1) #10
head(avian[order(avian$WHt, decreasing = T),height_variables], 1) #24.5
head(avian[order(avian$EHt, decreasing = T),height_variables], 1) #5.3
head(avian[order(avian$AHt, decreasing = T),height_variables], 1) #31.5
head(avian[order(avian$HHt, decreasing = T),height_variables], 1) #8.2
head(avian[order(avian$LHt, decreasing = T),height_variables], 1) #1.3


##Разные операции с датафреймом
mydata_salary <- data.frame(x=c(3, 2, 6, 1), salary = c(100, 1000, 300, 500))

rownames(mydata) <- c("A", "B", "C", "D")

mydata <- data.frame(x=1:4, y = LETTERS[1:4], z=c(T,F))
rownames(mydata) <- c("A", "B", "C", "D")

mydata[,1,drop = F]                        #аргумент drop отменяет упрощение формата 

subset(attitude, rating < 50, -rating)
subset(sel = -rating, sub = rating < 50, attitude)
attitude[attitude$rating < 50, names(attitude) != "rating"]




summary(quakes)

# Summaries
head(mydata, 3)
tail(mydata, 1)

View(mydata)

str(mydata)

a <- names(mydata)

summary(mydata)

# Variables

b <- mydata$score

mean(mydata$score)

summary(mydata$score)

mydata$score * 2

mydata$ten_point_scale <- mydata$score * 2



summary(mydata$ten_point_scale)

mydata$new_varible <- 0
mydata$number <- 1:nrow(mydata)
summary(mydata$number)

nrow(mydata)
ncol(mydata)


# Subsetting

mydata$score[1:10]

mydata[1,1]
mydata[c(2,193,225),1]
mydata[101:200,1]

mydata[5,]
mydata[,1] == mydata$score

mydata[,2:5]
head(mydata[,2:5])

# Subsetting with condition

mydata$gender
mydata$gender == 'female'
head(mydata[mydata$gender == 'female',1:3])

head(subset(mydata, gender == 'female'))
head(subset(mydata, score > 3.5))



# rbind, cbind

mydata2 <- subset(mydata, gender == 'female')
mydata3 <- subset(mydata, gender == 'male')
mydata4 <- rbind(mydata2, mydata3)

mydata5 <- mydata[,1:10]
mydata6 <- mydata[,11:24]
mydata7 <- cbind(mydata6, mydata5)

?read.table

mydata <- read.csv("evals.csv")

head(mydata, 3)
tail(mydata, 3)

View(mydata)

str(mydata)

summary(mydata)

names(mydata)

a <- names(mydata)

mydata$score

b <- mydata$score

mean(mydata$score)

summary(mydata$score)

mydata$ten_point_scale <- mydata$score*2

mydata$new_variable <- 0

mydata$number <- 1:nrow(mydata)

mydata$score[1:10]

mydata[1,1]

mydata[1:100,1]

mydata$gender == 'female'

mydata[mydata$gender == 'female', 1]

head(mydata[mydata$gender == 'female', 1:3], 3)

head(subset(mydata, gender == 'female'), 3)

head(subset(mydata, score > 3.5), 3)

mydata2 <- subset(mydata, gender == 'female')
mydata3 <- subset(mydata, gender == 'male')

mydata4 <- rbind(mydata2, mydata3)

mydata5 <- mydata[, 1:10]
mydata6 <- mydata[, 11:24]

mydata7 <- cbind(mydata5, mydata6)
