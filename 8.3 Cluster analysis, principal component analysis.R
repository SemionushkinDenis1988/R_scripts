##########КЛастерный анализ и Анализ главных компонент
#
#1)КЛастерный анализ(K-means или Иерархическая кластеризация:
#С помощью кластерного анализа мы можем проверить нашу выборку на наличие кластеров, 
# и их количество
#2)Анализ главных компонент:
#С помощью анализа главных компонент мы можем сгруппировать наши переменные
#и объединить взаимосвязанные переменные в одну более интегративную переменную
#
#Методы кластерных анализов:
#1)К средних(K means)
#2)Иерархическая кластеризация
#
#На примере данных Iris попробум разделить данные на кластеры по цветкам(Species)
#K-means
#Полядок действий:
#1)Решаем на сколько кластеров будем делить наблюдения
#2)Случайно выбираем начальные позиции центроидов кластера
#3)Выводим центроиды на наилучшие позиции
#

library(ggplot2)
d <- iris[, c("Sepal.Length", "Petal.Width")]

fit <- kmeans(d, 3)
d$clusters <- factor(fit$cluster)

ggplot(d, aes(Sepal.Length, Petal.Width, col = clusters))+
  geom_point(size = 2)+
  theme_bw() 

install.packages("factoextra")
library(ggplot2)
library(factoextra)
theme_set(theme_minimal())
d <- iris[, c("Sepal.Length", "Petal.Width")]
fit <- kmeans(d, 3)
fviz_cluster(fit, d)

#Порядок действий K-means:
#a)Указываем N рандомных центроидов
#б)Ищем ближайшие к ним точки на графике
#в)Перемещам рандомные центроиды в геометрический центр найденных точек
#г)Так как центроиды переместились, то некоторые точки теперь принадлежат другим центроидам
#д)Переопределяем принадлежность точек на графике к другим центроидам(кластерам)
#е)Так как некоторые точки прибавились к другим центроидам, то геометрический центр
#этих точек сместился, тогда нам снова надо переместить центроиды в новый геометрический
#центр
#
####Этот алгоритм повторяется до тех пор, пока точки не перестанут переходить из одного
#кластера в другой.
#
#Остаются 2 основных вопрос:
#1)Правильно ли сошелся алгоритм?
#2)Какое число кластеров оптимальнее всего выбрать?
#
#1)Правильно ли сошелся алгоритм:
#Алгоритм не всегда сходится верно, даже если в наблюдениях есть очевидная кластеризация
#Тот факт, что мы рандомно бросаем точки, может привести к тому, что алгоритм сойдется 
#в неверной позиции
#Способы решения проблем неверного схождения алгоритма:
#a)Задать центроиды более удаленные друг от друга(и другие механические улучшения алгоритма)
#б)Многократное проведение кластерного анализа и если, каждый раз центроид приходит в одно
#и тоже положение, то это не случайно
#
#2)Какое число кластеров оптимальнее всего выбрать:
#a)В качестве оптимальности определяется сумма квадратов внутри каждого кластера
#и общую сумму квадратов.
#Внутригрупповая сумма квадратов(within cluster sum of squares) - сумма квадратов отклонения
#каждого наблюдения от центроида кластера
#Общая внутригрупповая сумма квадратов(total within cluster sum of squares) - сумма всех
#сумм квадратов всех кластеров
#
###Идея: если добавление еще одного кластера в наши данные значительно понижает общую
#сумму квадратов, это означает, что в увеличении числа кластеров есть смысл
#Если график снижения сумму квадратов остатков плавный, это может означать, что явной
#кластеризации в данных нет

install.packages("NbClust")
library(NbClust)
data(iris)
dt <- iris[, 1:4]
N <- NbClust(dt, distance = "euclidean",
             min.nc = 2, max.nc = 15, method = "complete", 
             index = "alllong")

#Если на графике снижения суммы квадратов явно присутствует перегиб, значит в окрестности 
#этой точки возможно лежит оптимальное число кластеров
#
###Иерархическая кластеризация 
#a)Сначала рассчитывается расстояние от каждой точки на графике до других точек
#б)По полученным данным иерархическая кластеризация начинает группировать точки между собой
#Есть множество методов по которым иерархическая кластеризация объединяет данные, один из
#них - метод одиночной связи/ближайшего соседа
#
#метод одиночной связи/ближайшего соседа(есть еще метод дальнего соседа, и прочие):
#a)С самого начала алгоритм считает, что количество кластеров равняется количеству точек
#б)после этого считается расстояние каждой точки до других
#в)далее объединяются две ближайшие точки, и их координаты заменяются на координаты их
#центроида
#г)производится повторный расчет с центроидом вместо двух точек
#д)в конечном итоге получается один большой кластер(дерево кластеров, дендрограмма)
#
#Различия между типами иерархических кластеризация только в принципе, по которому
#объединяются точек на графике
#
#Допускается комбинация Иерархической кластеризации с k-means: 
#сначала по иерархической кластеризации определить количество кластеров, потом 
#использовать к-means

library(ggplot2) 
library(ggrepel) # для симпатичной подписи точек на графике

x <- rnorm(10)
y <- rnorm(10)
test_data <- data.frame(x, y)
test_data$labels <- 1:10

ggplot(test_data, aes(x, y, label = labels))+
  geom_point()+
  geom_text_repel()

d = dist(test_data[1:2])
fit <- hclust(d, method = "single")
plot(fit, labels = test_data$labels)
rect.hclust(fit, 6) # укажите желаемое число кластеров, сейчас стоит 2

library(ape)
set.seed(222)
tr <- rtree(
  20, 
  tip.label = 
  c("B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U")
  ) 

plot.phylo(tr)  #(с учётом расстояний)

plot.phylo(tr, use.edge.length=FALSE)  #(только с учетом порядка кластеризации)

######Анализ главных компонент
#
#Вместо осей X и Y вводится новая ось - регрессионная прямая, а точки на графике описываются
#проекцией на эту ось.
#Регрессионная прямая в данном случае называется - новой главной компонентой.
#По регрессионной модели мы можем восстановить начальные данные
#
#Перпендикулярно регрессионной прямой вводится вторая главная компонента
#
#Мы можем посчитать, какой процент дисперсии объясняет каждая из компонент (Scree plot)
#если одна из компонент объясняет меньше 5% дисперсии, то мы можем отказаться от неё
#и оставить одну переменную вместо двух
#
#График по главным компонентам называется biplot, по горизонтали 1ая компонента, 
#по вертикали вторая компонента
#
#Снижение размерности:
#Вводится новая переменная, например вместо "знаний R" и "знаний статистики" - 
#"Опыт в анализе данных", в которой указать значение по оси регрессионной модели
#
#С помощью анализа данных компонент можно объединить сильно-коррелирующие переменные
#и сократить размерность наших данных

data(swiss)
fit <- prcomp(swiss, center = T)
plot(fit, type = "l")
summary(fit)
#PC1     PC2      PC3     PC4     PC5    PC6
#Standard deviation     43.836 21.6022 12.05342 4.75916 3.65754 2.4882
#Proportion of Variance  0.746  0.1812  0.05641 0.00879 0.00519 0.0024
#Cumulative Proportion   0.746  0.9272  0.98361 0.99240 0.99760 1.0000
#92% данных описываются двумя переменными
biplot(fit)                         #показывает количество объясняемых данных

install.packages("pca3d")
library(pca3d)

dt <- swiss
dt$is_catholic <- ifelse(swiss$Catholic > 50, 1, 0)
dt$is_catholic <- factor(dt$is_catholic)
fit <- prcomp(swiss, center = T)
pca3d(fit, group = dt$is_catholic,              #трехмерный график
      fancy = T, 
      new=T)

install.packages("devtools")
library(devtools)
install_github("ggbiplot", "vqv")       #не устанавливается
library(ggbiplot)                       # все данные из 6 мер, переведены в 2мерный график

dt <- swiss
dt$is_catholic <- ifelse(swiss$Catholic > 50, 1, 0)
dt$is_catholic <- factor(dt$is_catholic)
fit <- prcomp(swiss, center = T)
g <- ggbiplot(fit, obs.scale = 1, var.scale = 1,
              group = dt$is_catholic,
              ellipse = TRUE,
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal',
               legend.position = 'top')
print(g)

library(FactoMineR)
df <- mtcars[,c(1,3:7)]
res.pca <- PCA(df, graph=TRUE)

Scree.Plot <- function(R,main="Scree Plot",sub=NULL){
  roots <- eigen(R)$values
  x <- 1:dim(R)[1]
  plot(x,roots,type="b",col='blue',ylab="Eigenvalue",
       xlab="Component Number",main=main,sub=sub) 
  abline(h=1,lty=2,col="red")
  
}
R <- cor(df)
Scree.Plot(R,main ="Scree Plot (Husson mtcars Data)")

#Факторный анализ - метод понижения размерности данных. Идея метода заключается в том, 
#что возможно некоторые переменные могут быть сгруппированы в факторы. Фактор - 
#это некоторая латентная переменная, которая объединяет исходные переменные в данных.
#Например, если бы в нашей выборке каждый испытуемый характеризовался весом, ростом, 
#силой, знаем английского, знанием немецкого и знанием русского языка. То вероятно, мы 
#смогли бы выделить два фактора: "физическая подготовка" и "знание языков". Разумеется, 
#первые три определяли бы первый фактор, а оставшиеся переменные - второй  фактор.
#Давайте посмотрим на основные результаты факторного анализа, примененного к нашим данным
#swiss для того, чтобы выделить два фактора:
  
#Image: 
#https://ucarecdn.com/0f7b065a-ace4-43bd-941c-0e91ee39324e/-/crop/302x172/0,0/-/preview/
  
#Основные результаты факторного анализа - это факторные нагрузки для каждой переменной. 
#Проводя аналогии с анализом главных компонент, мы можем выяснить какие переменные в 
#большей степени взаимосвязаны между собой и влияют на значения первого фактора, а какие 
#переменные напротив группируются друг с другом и влияют на второй фактор.
#В данной таблице видно, что первый фактор - это низкие значения по переменной Fertility, 
#Agriculture но высокие значения переменных Examination и Education. Второй фактор - 
#это наоборот высокие значения по переменным  Fertility, Agriculture, низкие по 
#переменной Examination и максимальный вклад вносит переменная Catholic - то есть процент 
#католического населения.
#Таким образом, можно предположить, что за факторизацию наших переменных отвечает два 
#фактора - преобладание протестантского или католического населения.

fit <- factanal(swiss, factors = 2, rotation = "varimax")
print(fit)

#Задача 1
smart_hclust<-  function(test_data, cluster_number){
  dist_matrix <- dist(test_data, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)
  fit <- hclust(dist_matrix, method = "complete", members = NULL)
  cluster <- cutree(fit, cluster_number)
  test_data$cluster <- factor(cluster,labels = c(1:cluster_number))
  test_data
}

smart_hclust <- function(test_data, n_cluster){    
  d <- dist(test_data)    
  fit <- hclust(d)    
  test_data$cluster <- factor(cutree(fit, k = n_cluster))    
  return(test_data) 
}
smart_hclust(x, 3)

dist_matrix <- dist(swiss) # расчет матрицы расстояний(расстояние между точками на графике)
fit <- hclust(dist_matrix) # иерархическая кластеризация (дендрограмма)
cluster <- cutree(fit, 3) # (срез дендрограммы)

hclust(d, method = "complete", members = NULL) #для кластеризации данных 
dist(x, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)
#Для расчета матрицы расстояний 
cutree() #выделяет кластеры

test_data <- x
x <- read.csv("https://stepic.org/media/attachments/course/524/test_data_hclust.csv")
#Конец задачи 1
#Задача 2

#Вариант 1(мой)
get_difference<-  function(test_data, n_cluster){
  dist_matrix <- dist(test_data, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)
  fit <- hclust(dist_matrix, method = "complete", members = NULL)
  test_data$cluster <- factor(cutree(fit, n_cluster))
  s <- apply(
    test_data[1:length(test_data)-1], 2, function(x) anova(aov(x~cluster, test_data))$P[1]
    )
  return(names(s)[s<0.05])
}
#вариант 2
get_difference <- function(df, n_clusters){
  fit <- hclust(dist(df))
  cluster <- factor(cutree(fit, n_clusters))
  is.good <- sapply(df, function(x) anova(aov(x ~ cluster))$P[1] < 0.05)
  names(df)[is.good]
}
#вариант 3
get_difference <- function(td, cn) {
  cluster <- factor(cutree(hclust(dist(td)), cn))
  a <- sapply(td, function(x) unlist(summary(aov(x ~ cluster)))) #unlist summary работает
  colnames(td[a['Pr(>F)1',] < 0.05])
}

test_data <- read.csv("https://stepic.org/media/attachments/course/524/cluster_2.csv")
s <- apply(select(test_data, -"cluster"), 2, function(x) anova(aov(x~cluster, test_data)))
get_difference(test_data, 2)
n_cluster <- 2
#конец Задачи 2
#Задача 3
get_pc <- function(d){
  fit <- prcomp(d, center = T)
  d$PC1 <- fit[["x"]][,1]
  d$PC2 <- fit[["x"]][,2]
  return(d)
}

d <- read.csv("https://stepic.org/media/attachments/course/524/pca_test.csv")
#Конец задачи 3

#Задача 4
#вариант 1
get_pca2 <- function(data){
  fit <- prcomp(data, center = T)
  cbind(data,fit[["x"]][,c(1:(sum(summary(fit)[["importance"]][3,]<0.9)+1))])
}
#вариант 2
get_pca2 <- function(test_data){    
  fit <- prcomp(test_data)    
  cum_prop <- summary(fit)$importance['Cumulative Proportion',]    
  test_data <- cbind(test_data, fit$x[,1:min(which(cum_prop>0.9))])    
  return(test_data)    
}

#вариант 3
get_pca2 <- function(data) {
  fit <- prcomp(data)
  n <- which(cumsum(fit$sdev^2 / sum(fit$sdev^2)) > 0.9)[1]
  cbind(data, fit$x[, seq_len(n)])
}
#вариант 4
get_pca2 <- function(df){
  fit <- prcomp(df)
  pcs <- summary(fit)$importance[3, ]
  ind <- which(pcs>0.9)[1]
  cbind(df, fit$x[,1:ind])
}

data <- swiss

get_pca2(swiss)

#конец задачи 4

#задача 5
#вариант 1
is_multicol <- function(d){
  s <- apply(d,2, function(x) apply(d, 2, function(y) cor(x,y)))
  diag(s) <- 0
  ifelse(length(rownames(which(round(abs(s), 3)==1, arr.ind = T)))>0,
         return(rownames(which(round(abs(s), 3)==1, arr.ind = T))),
         return("There is no collinearity in the data"))
}

#вариант 2
is_multicol <- function(d){    
  d <- abs(cor(d))     
  d[lower.tri(d)] <- 0    
  diag(d) <- 0    
  index <- which((1-d) < 1e-10, arr.ind = T)    
  if (length(index) == 0){      
    return('There is no collinearity in the data')    
  } else {      
    return(rownames(d)[index])      
  }      
}


is_multicol(d)

diag(apply(d,2, function(x) apply(d, 2, function(y) cor(x,y)))) <- 0

fit[["rotation"]][1,] %in% fit[["rotation"]]

d <- as.data.frame(list(V1 = c(21, 13, 20, 14, 10), V2 = c(16, 7, 10, -3, 9), V3 = c(17, 5, 16, 11, 19), V4 = c(24, 5, 11, 19, -5), V5 = c(-10, -1, -4, 9, -3), V6 = c(31, 12, 18, 26, 2)))
d <- as.data.frame(list(V1 = c(6, 4, 16, 12, -6), V2 = c(9, 6, 24, 17, 15), V3 = c(6, 9, -3, 14, 14), V4 = c(4, 1, 19, 12, 10), V5 = c(-1, 1, -11, -7, 11)))
d <- read.csv("https://stepic.org/media/attachments/course/524/Norris_2.csv")
d <- read.csv("https://stepic.org/media/attachments/course/524/Norris_3.csv")
#конец задачи 5

#задача 6
d <- dist(swiss)    
fit <- hclust(d)    
swiss$cluster <- factor(cutree(fit, k = 2))    
library(ggplot2)
my_plot <- ggplot(swiss, aes(Education, Catholic, col = cluster))+
  geom_point()+
  geom_smooth(method = "lm")

