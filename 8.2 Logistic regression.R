#################������������� ���������
#
#���������� ������������� ��������� � ��������� ������������ ������
#
#pi = b0+b1*x1i + b2*x2i + ... bn*xni, ��� pi-����������� ����������� �������������� ������
#
#����� ���������� pi[0;1] � ������ ����� ��������� [-�������������; + �������������] 
#���������� logit transformation (����� �������������) ��� ������ ����������������
#
#Odds = pi/(1-pi) #���, odds(����) -��� ��������� ����������� ������ � ����������� �������
#Odds ����������� ��������� [0; +�������������]
#ln(Odds) ����������� ���������� [-�������������; + �������������]
#
#log(p/(1-p)) = b0 + b1*x1
#
#p = exp(b0+b1*x1)/(1+exp(b0+b1))
#
#################### ������ � ������� Titanic
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
#Intercept only model, ���������� ����������� ������ ������ �� ����� ����������
#
simple_fit <- glm(Survived ~ 1, titanic, family = "binomial")
coef(simple_fit)           
table(titanic$Survived)
odds <- 290 / 424           #����� ������
log(odds)                   #intercept
summary(simple_fit)    
exp(coef(simple_fit))      #����� ������
#�����: ���� ������ ����� �������� 0.68, � ����������� ������ ������������� ������� ������
#��� ����������� ������
#
#Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -0.3799     0.0762  -4.985  6.2e-07 ***
#�������������� ���������� ������������ �� ����������� ������������� ��������� ������ ����
#
#
#������ � ����� ������������ �����������
fit1 <- glm(Survived ~ Sex, titanic, family = "binomial")
coef(fit1)
table(titanic$Survived, titanic$Sex)         #������ �������

odds_male <- 93 / 360                        #����� ��� ������
odds_female <- 197 / 64                      #����� ��� ������

log(odds_female)                            #������������� � �������� intercept �� glm
log(odds_male)                              

odds_ratio <- odds_male / odds_female      #��������� ������ odds_ratio
log(odds_ratio)                           #������������� � �������� slope �� glm
#intercept - ����������� �������� ������ �������������� ������ ��� ������
#
#��������� ������� (����������� �������������� ���������� �����������)
#��������� ����� anova (����������� ����� ��������� ��������)
#��-������� ���������� ������� ��������, � ���������� � ������ ��������
anova(simple_fit, fit1, test = "Chisq")
anova(fit1, test = "Chisq")
#
#������ � ����� ������������ � �� ����������������
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


#��������� �������
fit1 <- glm(Survived ~ Sex, titanic, family = "binomial")
fit2 <- glm(Survived ~ Sex * Pclass, titanic, family = "binomial")

anova(fit1, fit2, test="Chisq")

#������ � �������������� ����������� (�������) � ����� �������������:
fit3 <- glm(Survived ~ Sex + Pclass + Age, titanic, family = "binomial")
summary(fit3)
anova(fit3, test = "Chisq")
#
####����� ����� ������������ ����������������� ������ � ������?
#
#����������������� ���������� (� �������� ���������� ��� �������, ������� ������������ ��� 
#������� ��� ���������, �� ����������� �� ���������), ������������� ������� �� ������� 
#�� ������������, ��������������� �����-���� ����������������� ��������������. ���������� 
#����������, ���������� �� ������ ����������, �������� ����� �� �������� ����� ����������, 
#� ������ ����������� ���� �� ������ ����������������� ��������.
#
#1)����� �� ��������� �������� � ����������� �������������� ����������:
#a)�������� ���������
#�)������������ ����������
#2)����� �� ���������� ������:
#a)������������� ������
#�)�-���� (������� ������� �������������� �������)
#3)����� ����������� ������������ ����������:
#a)������������� ���������
#�)��-�������
#�)������ ���� ������
#
#�������� ���������� � ������ ��� �-�����:
#1)������������� ������� ����������
#
#2)������������(������������) ��������� ���� ������� (����������, �� �� ��������, � R
#�� ��������� �������������� t-test ��� �������� ���������) (bartlett.test())
#
#3)������������ ������������� ������������ �������� � ����������� ������������
#(shapiro-Wilk test ��� ���� �����������-��������)
#
#4)����� �������. ��� ��������� ������ ������� �������� ������������ ��������. (N>30)
#
#��� ����������, ����� ���������� ���� � ������������ ������������� ��� �� ��������
#� ���������� ���������
#
#��� �������� ������������� �� ������������ ������������ shapiro-Wilk test � �������:
#H0 - ������� ��������� �� ��������� �������������� ����������� ������������
#H1 - ������� ��������� �� �� ��������� �������������� ����������� ������������

#������� �� ��� �������� �� ������������, ������������ �� � ����� �������������
#����� ��������, ��� ��� ��� �������������� ��������� � �������� ������������ ��������
#����� ������� ����� ������ ��� ������������ ����� �������� ���� ���� ����������, �����
#�������� ������������� ����� ����������� � ����������
#���������:
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
#��� �� ����� ��������� ������������ ������������� �� ������� boxplot ��������, ���� �� 
#�����������, �� ������ ����� ������������� ����������
#
#������������ ����� ��-�� �������� � ���������, ���� ���� �������, �� ��� ������ ������
#�� ������������� ������ � �� �-����
#
#�-������������� �������� ������ ���� ����������� ������������ ������������ ���������
#
#�� ������� ��������(�������� ����� 5000 ����������) � ���� ������� �������� ������������ 
#�������-������ ���� ����� ���������� �������� � ����������� ������������� ����������� 
#������������ ��� ��������� ����������� �� ������������ � �������
#
#� ������ ���������� �������� � ������������ ������������� � ����������� ������������, 
#��� ������ ����������� ������������ ����� ��������� �� �������� ����������, 
#���� ��� ��������� 0.95 ����� ��� ���������� ����������.
#
##########����������������� U-�������� ����� � �����(����������������� ������ �-�����).
#����� ���������� ����������������� ��������� ��� ��������� ���� ����� �������� 
#U-�������� ����� � �����. ������ ������� �������� ����������� � ���, ��� ������ ��������� 
#������� �������� � ���� �������� �������� ���������� ����� ������ (�� �������, 
#��� ������ ������). �� ������� ������������� ��� ������, ����� ������������ 
#����� ������ � ������ �� �����.
#
#����� ��� ������ �� ������� �������������� ����������:
#U1=R1???n1???(n1+1)/2 
#U2=R2???n2???(n2+1)/2 
#��� R1,R2 - ��� ����� ������ � ���� �������, � n1,n2 - ����� ����������.
#���������� �� ���������� �������� � ��������� � �������� ���������� �����. 
#����� ��������, ��� ��� ������� �������� ������� �������� ������������� ���� 
#���������� ����������� ����������� �������������, ��� 
#??=n1???n22 �  ??=sqrt(n1???n2???(n1+n2+1)/12) 
#��� � ��������� ��� ���������� ����������� �������� ����������� ��� 
#��� ����� ���������� �������� ����� ������.
#������� ��������� ������ t - ����� ����: 
#1)������������� ���� �� � ����� �� ������� ����������� ���������� �� �����������. 
#2)���� �������� ������� � ������. 
#3)� ��������� ������� �������� ����� ���� ����, ��� t �������� 
#(��������, ����� ����� �������� ����������� �������� ���������� � ���������� �����������). 
#��������� ��������� ����: 
#1)������� ������� �������, � ��������� ������������ ����������.  
#
####�������� ��������-�������(kruskal.test)(����������������� ������ �������������� �������)
#�������� ���������� �������� ��������-������� - ��� ��������� ������� ��������
#������ � ������������ �������. ��� �������� ������� �������� ������������� ���� 
#���������� ����� ������� ��� ������ ������������� �-�������
#
#H = 12/N*(N+1)ERi^2/ni - 3*(N+1), ��� N = Eni
#
#������� ��������� ����:
#1)���������� ���������� � ������������ ������������� (shapiro.test)
#2)���������� ���������� � ������������ ��������� (bartlett.test)
#
#�����: � ������ ��������� ���������� ��� ��������������� ������, ����� ������������
#����������������� �����
#
#������ 1
get_coefficients <- function(dataset){
  fit <- glm(factor(y) ~ factor(x), test_data, family = "binomial")
  exp(fit$coefficients)  
}

test_data <- read.csv("https://stepik.org/media/attachments/course/524/test_data_01.csv")
#����� ������ 1
#������ 2
centered <- function(test_data, var_names){
  test_data[var_names] <- apply(test_data[var_names],2, function(x) x-mean(x))
  test_data
}

var_names = c("X4", "X2", "X1")
test_data[var_names]
test_data <- read.csv("https://stepic.org/media/attachments/course/524/cen_data.csv")
#����� ������ 2
#������ 3
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
#����� ������ 2
#������ 4
most_suspicious <- function(test_data, data_for_predict){
  fit <- glm(is_prohibited~., test_data, family = "binomial")
  probs <- predict.glm(fit, data_for_predict)
  max_probs <- probs==max(probs)
  data_for_predict[which(max_probs),"passangers"]
}

which(predict(fit, x1)==max(predict(fit, x1)))

x <- read.csv("https://stepic.org/media/attachments/course/524/test_data_passangers.csv")
x1 <- read.csv("https://stepic.org/media/attachments/course/524/predict_passangers.csv")

#����� ������ 4
#������ 5
normality_test <- function(dataset){
  apply(dataset[sapply(dataset, is.numeric)], 2, function(x) shapiro.test(x)$p.value)
}

test[sapply(test, is.numeric)]

test <- read.csv("https://stepic.org/media/attachments/course/524/test.csv")
#����� ������ 5

#������ 6
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
#����� ������ 6
#������ 7
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
#������ ����������� �������:
  
  normality_by <- function(test){    
    grouped_data <- aggregate(test[,1],by=list(test[,2], test[,3]),                                  
                              FUN = function(x) {shapiro.test(x)$p.value})                                  
    names(grouped_data) <- c(colnames(test)[2:3],'p_value')                                  
    return(grouped_data)    
  }


#��������� dplyr (��� �������, ��� �� ����� ����� ���������� � ������):
  
  library(dplyr)    
normality_by <- function(test_data){    
  result <- test_data %>% group_by(y, z) %>%     
    summarize(p_value = shapiro.test(x)$p.value)     
  return(result)    
}


#����� ����� ������� � dplyr:
  
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
#����� ������ 7
#������ 8
library(ggplot2)
obj <- ggplot(iris, aes(x=Sepal.Length, fill = Species))+
  geom_density(alpha = 0.2) 

#����� ������ 8
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
