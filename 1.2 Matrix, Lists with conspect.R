##Матрица - это двумерный массив данных(вектор уложенный по столбцам)

#Функция matrix(1:6,nrow = 2, ncol = 5, byrow = T) задает матрицу с указанными в арг. параметрами
#Функция dim(m) возвращает количество строк и столбцов матрица
#допускается изменение структуры матрицы через dim(m) <- NULL (превращение в вектор) или другое
#%*% - умножение матриц по правилам линейной алгебры
#Схлопывание размерности можно отключить через аргумент drop В слайсе x[,3, drop = F]
#Функции rownames() colnames(), возвращают имена строк и колонок
#Функция paste0("xxx", 1:5) склеивает вектор из двух векторов в строковый вектор
#Функции cbind() и rbind() соединяет матрицы по колонкам и строкам, можно передавать много матриц
#Функция apply(m, 1:2, function(x)) применяет функцию к строкам или столбцам матрицы
#Функция function(x) в apply называется анонимной
#Функции rowSums, rowMeans, colSums, colMeans возвращают sum и mean по столбцам и строкм
#
m <- matrix(1:6,nrow = 2, ncol = 5, byrow = T)
m1 <- matrix(1:6,nrow = 5, ncol = 2, byrow = T)
x <- dim(m)

dim(m) <- c(5, 2)

rownames(m) <- paste0("xxx", 1:5)
colnames(m) <- paste0("xxx", 1:2)

cbind(m,m1)
rbind(m,m)

f <- function(x) sum(x^2)

apply(m1, 2, f)

apply(m1, 1:2, function(i) if (i>13) i else 13)

rowSums(m)
colSums(m)


mat <- matrix(6, 4, 5)
mat[mat > 5]
mat>5

#задача 1 поиск в векторе v ближайшего числа к n
find_closest <- function(v, n) {
    which(
      abs(v-n) == min(abs(v-n))
    )
}

v <- c(5, 2, 7, 7, 7, 2, 0, 0)
n <- 4
abs(v-n)==1

z <- find_closest(v,n)

#Объединение матриц "по диагонали"

bind_diag <- function(m1, m2, fill){
  m3 <- matrix(fill,
               nrow = nrow(m1)+nrow(m2),
               ncol = ncol(m1)+ncol(m2)
               )
  m3[1:nrow(m1), 1:ncol(m1)] <- m1
  m3[nrow(m1) + 1:nrow(m2), ncol(m1) + 1:ncol(m2)] <- m2
  m3
}

m1 <- matrix(1:12, nrow = 3)
m2 <- matrix(10:15, ncol = 3)
bind_diag(m1, m2, fill = NA)
bind_diag(m2, m1, fill = 0)

#Задача строим зиккурат
build_ziggurat <- function(n) {
  m <- matrix(1, n*2-1, n*2-1)
  if(n>1){
    for (i in c(2:n)){
      m[i:(n*2-i), i:(n*2-i)] <- m[i:(n*2-i), i:(n*2-i)]+1}
  } 
  else {m}
  m
}

build_ziggurat <- function(n) {
  d <- n * 2 - 1
  outer(1:d, 1:d, function(x,y) {         #изучить функцию outer и pmin
    x <- n - abs(n - x)
    y <- n - abs(n - y)
    pmin(x,y)
  }
  )
}

build_ziggurat_mod <- function(n) {
  x <- n - abs(n - seq_len(2*n - 1))
  outer(x, x, pmin)                        #изучить функцию outer и pmin
}

build_ziggurat <- function(n) {
  if (n==1) {return(matrix(1))
  } else {
    x<-c(1:n, (n-1):1)      #можно использовать вектор для построения матрицы! 
    m<-matrix(x, 2*n-1, 2*n-1)
    n<-matrix(x, nrow=2*n-1, ncol=2*n-1, byrow = T)
    l<-(m[]+n[]-abs(m[]-n[]))/2
    return(l)
  }
}
m <- matrix(c(1:4, 3:1), 7, 7)
m1 <- matrix(c(1:4, 3:1), 7, 7, byrow = T)

##Списки
#Списки это индексированная структура
#Список может состоять из объектов разных типов
#Список может быть рекурсивным (элементом списка может быть список)
#Функция list(1:5, "my_data", matrix(0, 2, 2)) создает список
#Функция c(l1, l2) (combine) объединяет так же списки
#Функция list(v) может создать список из v
#Функция unlist(v) может создать вектор из v
#операция l[[3]] <- NULL или l[[1]] <- list(NULL) - удалит элемент из списка
#Функция is.null(l$string) - проверяет наличие элемента в списке
#Функция lapply(l, fun) - применяет функцию к каждому элементу списка, возвращает список
#Функция sapply(l, fun) - применяет функцию к каждому элементу списка, возвращает лист
#Функция lengths - возвращает длины элементов списка
#"..." в аргументах функции означает, что мы можем добавлять любое количество аргументов
#например sum(c(1:4), c(1:4),c(1:4),c(1:4))
#Функция diag() возвращает матрицу с числом/вектором по диагонали, или диагональ матрицы 

##NA,NAN, NULL различия
#NA -- это пропущенное значение ("not available"). Например, респондент не ответил на все вопросы предложенной анкеты, или данные с метеостанции за определённый период потерялись из-за сбоя оборудования. NA в этом случае обозначает, что эти данные существуют и имеют смысл, но их не удалось узнать.
#NaN -- "not-a-number" -- результат недопустимой арифметической операции, например 0/0 или Inf - Inf.
#NULL -- отсутствие объекта, "пустота". Применяется в тех случаях, когда объект действительно не существует, не может иметь осмысленного значения.
#Для проверки значений есть три функции, is.na, is.nan и is.null, соответственно.


diag(c(1,2))

l <- list(1:5, "my_data", "1mat" = matrix(0, 2, 2))

l1 <- list(vec = 1:7, fun = sqrt)

l1$fun(5)

lapply(l, length)

lapply(l, paste, collapse = "|")

l2 <- list(incredibly_long_name= 123)
 
l2$inc        #можно указать часть имени если оно слишком длинное

f <- function(y, x = ridiculous_long_variable) y + ridiculous_long_variable
  
f(3, ridic = 5) #можно указать часть имени, если оно слишком длинное

#Функция get longest
get_longest <- function(l){
  len <- sapply(l, length)
  ind <- which.max(len)
  list(number = ind, element = l[[ind]])
    
}
#Функция generate list
gen_list <- function(n_elements, max_len, seed = 111){ #seed фиксирует генератор
  set.seed(seed)
  len <- sample(1:max_len, n_elements)
  lapply(1:n_elements, function(i) rnorm(len[i]))
}

l3 <- gen_list(4, 10) 
n_elements <- 4
max_len <- 10

len <- sample(1:max_len, n_elements)

gl1 <- get_longest(l3)

gl1$number

#задача 2
count_elements <- function(x) {
  u <- rle(sort(x))                     #rle возвращает лист число вхождений и уник
  matrix(c(u$values, u$lengths), 2, byrow = T)
}

count_elements(x)

x <- c(5, 2, 7, 7, 7, 2, 0, 0)

z <- rle(sort(x))

set.seed(1789)
bastille <- list(
  "La Chapelle Tower" = rbinom(5, 10, 1/2), 
  "Tresor Tower" = rbinom(8, 12, 1/4), 
  "Comte Tower" = rbinom(14, 3, 1/5) + 1,
  "Baziniere Tower" = rbinom(8, 4, 4/5), 
  "Bertaudiere Tower" = rbinom(4, 8, 2/3),
  "Liberte Tower" = rbinom(1, 100, 0.1), 
  "Puits Tower" = rbinom(5, 5, 0.7),
  "Coin Tower" = rbinom(3, 16, 0.4)
)

sum(sapply(bastille, sum))

unique(sort(x))
which(sort(x) == unique(sort(x)))
z

