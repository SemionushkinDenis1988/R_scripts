
data(swiss)
str(swiss)

pairs(swiss)

library(ggplot2)
ggplot(swiss, aes(x = Examination, y = Education))+
  geom_point()+
  theme(axis.text = element_text(size = 25),
        axis.title = element_text(size = 25, face="bold"))+
  geom_smooth(method = 'lm')

ggplot(swiss, aes(x = Examination))+
  geom_histogram()

ggplot(swiss, aes(x = log(Education)))+
  geom_histogram()

#Задача 1
my_vector <- c(0.027, 0.079, 0.307, 0.098, 0.021, 0.091, 0.322, 0.211, 0.069, 0.261, 0.241, 0.166, 0.283, 0.041, 0.369, 0.167, 0.001, 0.053, 0.262, 0.033, 0.457, 0.166, 0.344, 0.139, 0.162, 0.152, 0.107, 0.255, 0.037, 0.005, 0.042, 0.220, 0.283, 0.050, 0.194, 0.018, 0.291, 0.037, 0.085, 0.004, 0.265, 0.218, 0.071, 0.213, 0.232, 0.024, 0.049, 0.431, 0.061, 0.523)


hist(sqrt(my_vector))
shapiro.test(1/(my_vector))

#Задача 2
beta.coef <- function(x){
  return(c(lm(scale(x[,1]) ~ scale(x[,2]))$coefficients))
}

typeof(lm(scale(mtcars[,1]) ~ scale(mtcars[,3]))$coefficients)
#То, что вы только что сделали, можно сделать 
#с помощью функции lm.beta из библиотеки QuantPsyc! :)

#задача 3
normality.test <- function(x){
  dt <- unlist(sapply(x, FUN = shapiro.test)[c(2:length(sapply(x, FUN = shapiro.test)))[c(2:length(sapply(x, FUN = shapiro.test)))%%4 == 2]])
  names(dt) <- names(x)
  dt
}

normality.test1 <- function(x){
  sapply(x, shapiro.test)[2,]
}

normality.test <- function(x) {
  apply(x, 2, function (i) shapiro.test(i)$p.value)
}
normality.test  <- function(x){
  sapply(x[],function(i) shapiro.test(i)$p.value)
}

dt <- sapply(x, shapiro.test)
normality.test1(mtcars[,1:6])

ggplot(data = swiss, aes(Examination, Education))+
  geom_point()+
  geom_smooth()

lm1 <- lm(Education ~ Examination, swiss)
summary(lm1)

swiss$Examination_squared <- (swiss$Examination)^2

lm2 <- lm(Education ~ Examination + Examination_squared, swiss)

summary(lm2)

anova(lm2, lm1)

swiss$lm1_fitted <- lm1$fitted
swiss$lm2_fitted <- lm2$fitted
swiss$lm1_resid <- lm1$resid
swiss$lm2_resid <- lm2$resid
swiss$obs_number <- 1:nrow(swiss)
names(swiss)

ggplot(swiss, aes(x = Examination, y = Education)) +
  geom_point(size = 3)+
  geom_line(aes(x = Examination, y = lm1_fitted), col = 'red', lwd = 1)+
  geom_line(aes(x = Examination, y = lm2_fitted), col = 'blue', lwd = 1)

ggplot(swiss, aes(x = lm1_fitted, y = lm1_resid)) +
    geom_point(size = 3) + geom_hline(aes(yintercept = 0), col = 'red', lwd = 1)

ggplot(swiss, aes(x = lm2_fitted, y = lm2_resid)) +
  geom_point(size = 3) + geom_hline(aes(yintercept = 0), col = 'red', lwd = 1)


ggplot(swiss, aes(x = obs_number, y = lm1_resid))+
  geom_point(size = 3) + geom_smooth()

ggplot(swiss, aes(x = obs_number, y = lm2_resid))+
  geom_point(size = 3) + geom_smooth()

ggplot(swiss, aes(x = lm1_fitted, y = lm1_resid))+
  geom_point(size = 3)

ggplot(swiss, aes(x = lm2_fitted, y = lm1_resid))+
  geom_point(size = 3)

#задача 4
library(gvlma)

dt <- read.csv("homosc.csv")
x <- gvlma(DV ~ IV, data = dt)

summary(x)

ggplot(swiss, aes(x = lm1_resid))+
  geom_histogram(binwidth = 4, fill = 'white', col = 'black')

qqnorm(lm1$residuals)
qqline(lm1$residuals)

shapiro.test(lm1$residuals)

ggplot(swiss, aes(x = lm2_resid))+
  geom_histogram(binwidth = 4, fill = 'white', col = 'black')

qqnorm(lm2$residuals)
qqline(lm2$residuals)

shapiro.test(lm2$residuals)

par(mfrow=c(2,2))
plot(lm1)

#Задача 5
resid.norm  <- function(fit){
  ifelse(shapiro.test(fit$residuals)$p.value < 0.05,
    myplot <- ggplot(as.data.frame(fit$residuals), aes(x = fit$residuals))+
      geom_histogram(fill = 'red'),
    myplot <- ggplot(as.data.frame(fit$residuals), aes(x = fit$residuals))+
      geom_histogram(fill = 'green'))
  myplot
}
resid.norm <- function(fit) {    
  resid.norm.pv <- shapiro.test(fit$residuals)$p.value    
  plt <- ggplot(data.frame(fit$model), aes(x = fit$residuals)) +    
    geom_histogram(fill = ifelse(resid.norm.pv < 0.05, 'red', 'green'))    
  return(plt)}

#Задача 6
library(psych)
high.corr <- function(x){
  z <- corr.test(x)$r
  diag(z) <- 0
  return(rownames(which(abs(z) == max(abs(z)), arr.ind = T)))
}

pairs(iris[,-5])
cor(iris$Sepal.Length,  iris$Sepal.Width)
names(iris[,-5])
colnames(iris[,-5])
rownames(iris[,-5])                    
names(iris[,-5])
dim(diag(3))

diag(10, 3, 4)

library(psych)
x <- corr.test(iris[,-5])$r
diag(x) <- 0
x[abs(x) == abs(max(x))][1]
colnames(x)
  
dimnames(x)

rownames(which(abs(x) == abs(max(x)), arr.ind = T))
z1 <- as.data.frame(list(V1 = c(-1.1, 0.4, -0.6, -0.3, 1.6, 1, -0.2, -0.7, -0.9, -0.5, 1, 0.5, -1.9, 0.6, 0.6, 0.4, -0.5, 1.7, 0, -2.1, 0.3, -1.7, 0, 0, 0.4), V2 = c(1.7, -1.1, -0.1, 0, -0.2, 0.6, 1.9, 1.4, 1.8, -0.1, -0.8, -0.8, -0.5, -0.1, 0.9, -1, 1.5, 2.1, 1.2, -0.7, 0, -1.3, -1.2, 2.1, -0.3), V3 = c(-1.7, 1.1, 0.1, 0, 0.2, -0.6, -1.9, -1.4, -1.8, 0.1, 0.8, 0.8, 0.5, 0.1, -0.9, 1, -1.5, -2.1, -1.2, 0.7, 0, 1.3, 1.2, -2.1, 0.3)))

z <- corr.test(z1)$r

rownames(which(abs(z) == max(abs(z)), arr.ind = T))
which(abs(z) == max(abs(z)), arr.ind = T)
diag(z) <- 0
high.corr(z1)
