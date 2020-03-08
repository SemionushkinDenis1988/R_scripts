df <- mtcars
df_numeric <- df[, c(1,3:7)]

fit <- lm(mpg ~ hp, df_numeric)

fit
summary(fit)

ggplot(df, aes(hp, mpg, col = factor(am)))+
  geom_point(size = 5)+
  geom_smooth(method = "lm")+
  facet_grid(.~cyl)

ggplot(df, aes(hp, mpg))+
  geom_point(size = 5)+
  geom_smooth(method = "lm")+
  facet_grid(.~cyl)

ggplot(df, aes(hp, mpg, col = factor(am)))+
  geom_point(size = 5)+
  geom_smooth()

ggplot(df, aes(hp, mpg))+
  geom_smooth(method = "lm", se = F)+
  facet_grid(.~cyl)

ggplot(df, aes(hp, mpg))+
  geom_smooth(method = "lm")+
  facet_grid(.~cyl)

fitted_values_mpg <- data.frame(mpg = df$mpg, fitted = fit$fitted.values)

new_hp <- data.frame(hp = c(100, 150, 129, 300))
new_hp$mpg <- predict(fit, new_hp)
predict(fit, new_hp)

my_df <- mtcars
my_df$cyl <- factor(my_df$cyl, labels = c("four", "six", "eight"))
fit1 <- lm(mpg ~ cyl, my_df)
summary(fit1)

ggplot(my_df, aes(cyl, mpg))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme(axis.text = element_text(size = 25),
        axis.title = element_text(size = 25, face = "bold"))


my_df <- mtcars
my_df$cyl <- factor(my_df$cyl, labels = c("four", "six", "eight"))
fit1 <- lm(mpg ~ cyl, my_df)
summary(fit1)

ggplot(my_df, aes(cyl, mpg))+
  geom_point()+
  theme(axis.text = element_text(size = 25),
        axis.title = element_text(size = 25, face = "bold"))

aggregate(mpg ~ cyl, my_df, mean)
?aggregate

#задача 1
new_df1 <- read.table("dataset_11508_12.txt")
fit2 <- lm(V1 ~ V2, new_df1)
summary(fit2)

#задача 2
fit_coef <- lm(price ~ depth, diamonds[diamonds$cut == "Ideal" & diamonds$carat == 0.46, ])$coefficients

#задача 3
library(psych)
regr.calc <- function(dataframe){
  df <- dataframe
  if(corr.test(df)$p[2] < 0.05){
    fit1 <- lm(df[,1] ~ df[,2], df)
    df$fit <- predict(fit1, df[1])
    return(df)
  }
  else{return("There is no sense in prediction")}
}

#задача 4
library(ggplot2)
my_plot <- ggplot(iris, aes(Sepal.Width, Petal.Width, col = Species))+
  geom_point()+
  geom_smooth(method = "lm")
