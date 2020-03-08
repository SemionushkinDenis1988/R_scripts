#################Логистическая регрессия
#
#Применение логистической регрессии в контексте номинативных данных
#
#pi = b0+b1*x1i + b2*x2i + ... bn*xni, где pi-вероятность наступления положительного исхода
#
#Чтобы приравнять pi[0;1] и правую часть уравнения [-бесконечность; + бесконечность] 
#необходима logit transformation (логит трансформация) или просто логорифмирование
#
#Odds = pi/(1-pi) #где, odds(шанс) -это отношения вероятности успеха к вероятности неудачи
#Odds принадлежит интервалу [0; +бесконечность]
#ln(Odds) принадлежит промежутку [-бесконечность; + бесконечность]
#
#log(p/(1-p)) = b0 + b1*x1
#
#p = exp(b0+b1*x1)/(1+exp(b0+b1))
#
#################### Работа с данными Titanic
library(dplyr)
library(ggplot2)
library(vcd)
# 
titanic <- read.csv("https://stepic.org/media/attachments/course/524/train.csv")
titanic <- na.omit(titanic)
glimpse(titanic)
titanic <- mutate(titanic, 
                  Survived = factor(Survived, labels = c("No", "Yes")), 
                  Pclass = factor(Pclass, labels = c("First", "Second", "Third")), 
                  Sex = factor(Sex, labels = c("Female", "Male")))

mosaic(~ Sex + Survived | Pclass, data=titanic) 
#
#
#Intercept only model, определяем вероятность выжить исходя из одной переменной
#
simple_fit <- glm(Survived ~ 1, titanic, family = "binomial")
coef(simple_fit)           
table(titanic$Survived)
odds <- 290 / 424           #шансы выжить
log(odds)                   #intercept
summary(simple_fit)    
exp(coef(simple_fit))      #шансы выжить
#ВЫВОД: ШАНС выжить равен примерно 0.68, а вероятность выжить статистически значимо меньше
#чем вероятность выжить
#
#Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -0.3799     0.0762  -4.985  6.2e-07 ***
#статистическая значимость определяется по нормальному распределению логорифма вокруг нуля
#
#
#Модель с одним номинативным предиктором
fit1 <- glm(Survived ~ Sex, titanic, family = "binomial")
coef(fit1)
table(titanic$Survived, titanic$Sex)         #строим таблицу

odds_male <- 93 / 360                        #шансы для мужчин
odds_female <- 197 / 64                      #шансы для женщин

log(odds_female)                            #логорифмируем и получаем intercept из glm
log(odds_male)                              

odds_ratio <- odds_male / odds_female      #отношение шансов odds_ratio
log(odds_ratio)                           #логорифмируем и получаем slope из glm
#intercept - натуральный логарифм шансов положительного исхода для женщин
#
#Сравнение моделей (определение статистической значимости ПРЕДИКТОРОВ)
#Проверяем через anova (минимульная сумма квадратов остатков)
#Хи-квадрат составляет таблицу ожидания, и сравнивает с нашими моделями
anova(simple_fit, fit1, test = "Chisq")
anova(fit1, test = "Chisq")
#
#Модель с двумя предикторами и их взаимодействиями
fit2 <- glm(Survived ~ Sex * Pclass, titanic, family = "binomial")
coef(fit2)
summary(fit2)

table(titanic$Survived, titanic$Pclass , titanic$Sex)

# (Intercept) 
female_p1_odds <- 82 / 3
log(female_p1_odds)

# Sexmale  
male_p1_odds <- 40  /  61 
log(male_p1_odds)
log(male_p1_odds / female_p1_odds )

# PclassSecond
female_p2_odds <- 68  /  6 
log(female_p2_odds / female_p1_odds )

# PclassThird
female_p3_odds <- 47  /  55 
log(female_p3_odds / female_p1_odds )

# SexMale:PclassSecond
male_p2_odds <- 15 / 84
log(male_p2_odds / female_p2_odds ) - log(male_p1_odds / female_p1_odds )

#Sexmale:factorThird 
male_p3_odds <- 38 / 215
log(male_p3_odds / female_p3_odds ) - log(male_p1_odds / female_p1_odds )


#Сравнение моделей
fit1 <- glm(Survived ~ Sex, titanic, family = "binomial")
fit2 <- glm(Survived ~ Sex * Pclass, titanic, family = "binomial")

anova(fit1, fit2, test="Chisq")

#модель с количественным предиктором (возраст) и двумя номинативными:
fit3 <- glm(Survived ~ Sex + Pclass + Age, titanic, family = "binomial")
summary(fit3)
anova(fit3, test = "Chisq")
#
####Когда нужно использовать непараметрические методы и почему?
#
#Непараметрическая статистика (в значении статистики над данными, которая определяется как 
#функция над выборками, не зависисящая от параметра), интерпретация которой не зависит 
#от совокупности, соответствующей каким-либо параметризованным распределениям. Порядковая 
#статистика, основанная на рангах наблюдений, является одним из примеров такой статистики, 
#и играет центральную роль во многих непараметрических подходах.
#
#1)Когда мы проверяем гипотезу о взаимосвязи количественных переменных:
#a)Линейная регрессия
#б)Коэффициенты корреляции
#2)Когда мы сравниваем группы:
#a)Дисперсионный анализ
#б)Т-тест (частный вариант дисперсионного анализа)
#3)Когда анализируем номенативные переменные:
#a)Логистическая регрессия
#б)Хи-квадрат
#в)Точный тест фишера
#
#основные требования к данным для Т-теста:
#1)Независимость каждого наблюдения
#
#2)Гомогенность(однородность) дисперсий двух выборок (желательно, но не критично, в R
#по умолчанию рассчитывается t-test для неравных дисперсий) (bartlett.test())
#
#3)Нормальность распределения исследуемого признака в генеральной совокупности
#(shapiro-Wilk test или тест Холмогорова-Смирнова)
#
#4)Объем выборки. При маленьком объеме выборки мощность исследования невелика. (N>30)
#
#Что происходит, когда нарушается идея о нормальности распределения или мы работаем
#с маленькими выборками
#
#Для проверки распределения на нормальность используется shapiro-Wilk test в котором:
#H0 - выборка извлечена из нормально распределенной генеральной совокупности
#H1 - выборка извлечена из не нормально распределенной генеральной совокупности

#Выбирая по три элемента из совокупности, упорядочивая их и строя распределение
#можно заметить, что все они распределяются нормально и образуют своеобразные квантили
#таким образом можно узнать где теоретически будут значения этих трех наблюдений, чтобы
#сравнить распределение нашей совокупнсти с нормальным
#программа:
vals <- c()
facs <- c()
unused <- sapply(1:10000, FUN = function(e) {
  x <- sort(rnorm(3))
  vals <<- c(vals, x[1])
  facs <<- c(facs, 1)
  vals <<- c(vals, x[2])
  facs <<- c(facs, 2)
  vals <<- c(vals, x[3])
  facs <<- c(facs, 3)
  return(e)
})
dt <- data.frame(vals, facs)
str(dt)
dt$facs <- factor(dt$facs, labels = c('MIN', 'MID', 'MAX'))

library(ggplot2)
ggplot(dt, aes(vals, fill = facs, alpha=0.5)) +
  geom_density()
#
#Так же можно проверить нормальность распределения на графике boxplot например, если он 
#симметричен, то скорее всего распределение нормальное
#
#Нормальность важна из-за проблемы с выбросами, если есть выбросы, то они сильно влияют
#на дисперсионный анализ и на т-тест
#
#Т-распределение возможно только ЕСЛИ генеральная совокупность распределена НОРМАЛЬНО
#
#На больших выборках(например более 5000 наблюдений) в силу большой мощности исследования 
#Шапиров-Вилкса тест может отвергнуть гипотезу о нормальнсти распределения генеральной 
#совокупности при небольших отклонениях от нормальности в выборке
#
#В случае отклонения гипотезы о нормальности распределения в генеральной совокупности, 
#при оценке генеральной совокупности можно опереться на значение корреляции, 
#если оно превышает 0.95 тогда это нормальное отклонение.
#
##########Непараметрический U-критерий Манна — Уитни(непараметрический аналог Т-теста).
#Самым популярным непараметрическим критерием для сравнения двух групп является 
#U-критерий Манна — Уитни. Логика данного критерия заключается в том, что вместо сравнения 
#средних значений в двух выборках критерий сравнивает сумму рангов (не медианы, 
#как многие думают). Мы сначала упорядочиваем все данные, затем рассчитываем 
#сумму рангов в каждой из групп.
#
#Затем для каждой из выборок рассчитывается показатель:
#U1=R1???n1???(n1+1)/2 
#U2=R2???n2???(n2+1)/2 
#Где R1,R2 - это сумма рангов в двух группах, а n1,n2 - число наблюдений.
#Наименьшее из полученных значений и выступает в качестве статистики теста. 
#Легко показать, что при условии верности нулевой гипотезы распределение этой 
#статистики подчиняется нормальному распределению, где 
#??=n1???n22 и  ??=sqrt(n1???n2???(n1+n2+1)/12) 
#что и позволяет нам рассчитать вероятность получить наблюдаемые или 
#еще более выраженные различия суммы рангов.
#Разумно применять вместо t - теста если: 
#1)Распределения хотя бы в одной из выборок значительно отличается от нормального. 
#2)Есть заметные выбросы в данных. 
#3)В некоторых задачах мощность теста даже выше, чем t критерия 
#(например, когда обеих выборках наблюдается заметная асимметрия в одинаковом направлении). 
#Неразумно применять если: 
#1)Выборки разного размера, с различным направлением асимметрии.  
#
####Критерий Краскела-Уоллиса(kruskal.test)(Непараметрический аналог дисперсионного анализа)
#Основная статистика критерия Краскела-Уоллиса - это дисперсия средних значений
#рангов в сравниваемых группах. При верности нулевой гипотезы распределение этой 
#статистики можно описать при помощи распределения Х-квадрат
#
#H = 12/N*(N+1)ERi^2/ni - 3*(N+1), где N = Eni
#
#Разумно применять если:
#1)Нарушается требование о нормальности распределения (shapiro.test)
#2)Нарушается требование о гомогенности дисперсий (bartlett.test)
#
#Вывод: в случае нарушений требований для параметрических тестов, нужно использовать
#непараметрические тесты
#
#Задача 1
get_coefficients <- function(dataset){
  fit <- glm(factor(y) ~ factor(x), test_data, family = "binomial")
  exp(fit$coefficients)  
}

test_data <- read.csv("https://stepik.org/media/attachments/course/524/test_data_01.csv")
#Конец задачи 1
#Задача 2
centered <- function(test_data, var_names){
  test_data[var_names] <- apply(test_data[var_names],2, function(x) x-mean(x))
  test_data
}

var_names = c("X4", "X2", "X1")
test_data[var_names]
test_data <- read.csv("https://stepic.org/media/attachments/course/524/cen_data.csv")
#Конец задачи 2
#Задача 3
get_features <- function(dataset){
  fit <- glm(is_prohibited~., test_data, family = "binomial")
  diagnostic <- anova(fit, test = "Chisq")
  ifelse(any(diagnostic$`Pr(>Chi)`[-1]<0.05), 
         return(row.names(diagnostic)[-1][diagnostic$`Pr(>Chi)`[-1]<0.05]),
         return("Prediction makes no sense")
         )
}
fit <- glm(is_prohibited~., test_data, family = "binomial")
fit$coefficients
test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_luggage_1.csv")

summary(fit)
diagnostic <- anova(fit, test = "Chisq")

fit$coefficients[1]
#Конец задачи 2
#Задача 4
most_suspicious <- function(test_data, data_for_predict){
  fit <- glm(is_prohibited~., test_data, family = "binomial")
  probs <- predict.glm(fit, data_for_predict)
  max_probs <- probs==max(probs)
  data_for_predict[which(max_probs),"passangers"]
}

which(predict(fit, x1)==max(predict(fit, x1)))

x <- read.csv("https://stepic.org/media/attachments/course/524/test_data_passangers.csv")
x1 <- read.csv("https://stepic.org/media/attachments/course/524/predict_passangers.csv")

#Конец задачи 4
#Задача 5
normality_test <- function(dataset){
  apply(dataset[sapply(dataset, is.numeric)], 2, function(x) shapiro.test(x)$p.value)
}

test[sapply(test, is.numeric)]

test <- read.csv("https://stepic.org/media/attachments/course/524/test.csv")
#Конец задачи 5

#Задача 6
smart_anova <- function(test_data){
  bar_t_p <- bartlett.test(x~y, test_data)$p.value
  norm_t_p <- by(test_data$x, INDICES = test_data$y, function(x) shapiro.test(x)$p.value)
  ifelse(any(c(bar_t_p,norm_t_p)<0.05),
         return(c("KW" = kruskal.test(x ~ y, test_data)$p.value)),
         return(c("ANOVA" = summary(aov(x ~ y, test_data))[[1]]$'Pr(>F)'[1])
                )
  )
}

fit <- aov(x ~ y, test_data)

bar_t <- bartlett.test(x~y, test_data)$p.value
norm_t <- by(test_data$x, INDICES = test_data$y, function(x) shapiro.test(x)$p.value)

test_data <- read.csv("https://stepic.org/media/attachments/course/524/s_anova_test.csv")
#Конец задачи 6
#Задача 7
library(dplyr)
library(lazyeval)
normality_by <- function(test){
  factor_vars <- names(test)[c(2,3)]
  num_var <- names(test)[1]
  gr_dataframe <- group_by_(test, .dots = factor_vars)
  summarise_(gr_dataframe, 
             p_value = interp(~shapiro.test(var)$p.value, var = as.name(num_var)
                              )
             )
}

normality_by <- function(test){
  colnames(test)[1] <- 'p_value'
  aggregate(p_value ~ . ,test,function(x) shapiro.test(x)$p.value)
}

#Passed. 
#Пример правильного решения:
  
  normality_by <- function(test){    
    grouped_data <- aggregate(test[,1],by=list(test[,2], test[,3]),                                  
                              FUN = function(x) {shapiro.test(x)$p.value})                                  
    names(grouped_data) <- c(colnames(test)[2:3],'p_value')                                  
    return(grouped_data)    
  }


#Используя dplyr (при условии, что мы знаем имена переменных в данных):
  
  library(dplyr)    
normality_by <- function(test_data){    
  result <- test_data %>% group_by(y, z) %>%     
    summarize(p_value = shapiro.test(x)$p.value)     
  return(result)    
}


#Более общее решение с dplyr:
  
  library(dplyr)    
get_p_value <- function(x){      
  shapiro.test(x)$p.value    
}    
normality_by <- function(test){    
  grouped <- test %>%    
    group_by_(.dots = colnames(.)[2:3]) %>%         
    summarise_each(funs(get_p_value))         
  names(grouped)[3] <- 'p_value'         
  return(grouped)         
}
test<- read.csv("https://stepic.org/media/attachments/course/524/test_for_norm.csv")
as.factor(test[2])
as.factor(test[3])

str(test)
#Конец задачи 7
#Задача 8
library(ggplot2)
obj <- ggplot(iris, aes(x=Sepal.Length, fill = Species))+
  geom_density(alpha = 0.2) 

#Конец задачи 8
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
