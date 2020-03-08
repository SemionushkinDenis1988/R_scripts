?swiss

swiss <- data.frame(swiss)

str(swiss)

hist(swiss$Fertility, col ="red")

fit <- lm(Fertility ~ Examination + Catholic, data = swiss)
summary(fit)

fit2 <- lm(Fertility ~ Examination*Catholic, data = swiss)
summary(fit2)

confint(fit2)
library(ggplot2)
ggplot(swiss, aes(x = Catholic, y = Fertility)) + 
  geom_point() + geom_smooth(method = 'lm')

#задача 1
test_data <- read.csv("https://stepic.org/media/attachments/course/129/fill_na_test.csv")
library(psych)
fill_na <- function(test_data){
  fit3 <- lm(y ~ x_1 + x_2, data = test_data)
  test_data$y_full <- test_data$y
  test_data$y_full[is.na(test_data$y_full)] <- 
  predict(fit3, test_data)[which(is.na(test_data$y_full))]
  return(test_data)
}

#задача 2
df <- mtcars[, c(6, 1, 5, 3, 4)]
fit <- lm(wt ~ mpg + disp + drat + hp, data = df)
summary(fit)

fit <- lm(wt ~ mpg + disp + hp, df)
summary(fit)
#задача 3 
fit1 <- lm(rating ~ complaints*critical, attitude)
summary(fit1)


hist(swiss$Catholic, col = 'red')

swiss$religious <- ifelse(swiss$Catholic >60, 'Lots', 'Few')
swiss$religious <- as.factor(swiss$religious)

fit3 <- lm(Fertility ~ Examination+religious, data = swiss)
summary(fit3)

fit4 <- lm(Fertility ~ Examination*religious, data = swiss)
summary(fit4)

ggplot(swiss, aes(x = Examination, y = Fertility)) + 
  geom_point() 

ggplot(swiss, aes(x = Examination, y = Fertility)) + 
  geom_point() + 
  geom_smooth(method = 'lm')

ggplot(swiss, aes(x = Examination, y = Fertility, col = religious)) + 
  geom_point()

ggplot(swiss, aes(x = Examination, y = Fertility, col = religious)) + 
  geom_point()+
  geom_smooth(method = 'lm')

fit5 <- lm(Fertility ~ religious*Infant.Mortality*Examination, data = swiss)
summary(fit5)

#задача 4
mtcars$am <- factor(mtcars$am, labels = c('Automatic', 'Manual'))
fit6 <- lm(mpg ~ wt*am , data = mtcars)
summary(fit6)

#задача 5
ggplot(mtcars, aes(x = wt, y = mpg, col = am)) + 
  geom_point() + 
  geom_smooth(method = 'lm')
rm(df)
