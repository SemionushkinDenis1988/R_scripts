##Циклы и условия

#Условие if(условия){выполняется}else{выполняется}
#Условие ifelse(условия, выполнить если условие верно, выполнить если условие не верно)
#Множественный выбор switch("x", a = 5, b = 7, x = 10, 0) вернет 10
#Цикл repeat{выполняется} break 
#Цикл while(условие){выполняется}
#Цикл for(i in x){выполняется} 
#Ключевое слово next - перезапускает цикл
#Ключевое слово break - завершает цикл
#Функция System.time({цикл}) возвращает время выполнения скрипта

#задача 1
set.seed(1337)
x <- runif(1e6, min = -1, max = 1)
sum((-0.2) < x & x < (0.3))

#задача 2
dice_roll <- function(n){
  round(runif(n, 0.5, 6.5))
}

##Пакеты

#Библиотека - это сборник пакетов
#пакет это часть библиотеки, пакеты хранятся по адресу cran.r-project.org
#Функция installed.packages() возвращает список установленных пакетов
#Функция install.packages("xts", dependencies = T) установка xts и всех пакетов от которых он зависит
#Функция update.packages() - обновление пакета
#Функция sessionInfo() - возвращает версию R, подключенные пакеты


mydata <- read.csv('evals.csv')

a <- -10

if (a > 0) {
  print('positive')
} else {
  print('not positive')
  print(a + 1)
} 

if (a > 0) {
  print('positive')
} else if (a < 0) {
  print('negative')
} else print('zero')

ifelse(a > 0, 'positive', 'not positive')

b <- c(1, -1)

ifelse(b > 0, 'positive', 'not positive')

for (i in 1:100){
  print(i)
}

for (i in 1:nrow(mydata)){
  print(mydata$score[i])
}

for (i in 1:nrow(mydata)){
  if (mydata$gender[i] == 'male'){
    print(mydata$score[i])
  }
}
mydata$qyality <- NULL

mydata$qvality <- rep(NA, nrow(mydata))

for (i in 1:nrow(mydata)){
  if (mydata$score[i] > 4){
    mydata$qvality[i] <- 'good'
  } else mydata$qvality[i] <- 'bad'
}

mydata$qvality2 <-  ifelse(mydata$score > 4, 'good', 'bad')

i <- 1

while (i < 51){
  print(mydata$score[i])
  i <- i+1
}
