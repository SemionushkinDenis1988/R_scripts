library(ggplot2)

my_df <- read.csv("train.csv", sep=";")
str(my_df)

ggplot(my_df, aes(read, math, col = gender))+
  geom_point(size = 2)+
  facet_grid(.~hon)+
  theme(axis.text = element_text(size = 25), 
    axis.title = element_text(size = 25, face = 'bold'))

fit <- glm(hon ~ read + math + gender, my_df, family = "binomial")
summary(fit)

exp(fit$coefficients)

head(predict(object = fit))

head(predict(object = fit, type = "response"))

my_df$prob <- predict(object = fit, type = "response")

#Задача 1
fit1 <- glm(am ~ disp + vs + mpg, mtcars, family = "binomial")
log_coef <- fit1$coefficients

#Задача 2

library("ggplot2")

obj <- ggplot(data = ToothGrowth, aes(x = supp, y = len, fill = factor(dose)))+
          geom_boxplot()
obj

library(ROCR)

pred_fit <- prediction(my_df$prob, my_df$hon)
perf_fit <- performance(pred_fit, "tpr", "fpr")

plot(perf_fit, colorize = T, print.cuttofs.at = seq(0,1,by=0.1))

?performance

auc <- performance(pred_fit, measure = "auc")
str(auc)

perf3 <- performance(pred_fit, x.measure = "cutoff", measure = "spec")
perf4 <- performance(pred_fit, x.measure = "cutoff", measure = "sens")
perf5 <- performance(pred_fit, x.measure = "cutoff", measure = "acc")

plot(perf3, col = "red", lwd = 2)
plot(add = T, perf4, col = "green", lwd = 2)
plot(add = T, perf5, lwd = 2)

legend(x = 0.6,y = 0.3, c("spec", "sens", "accur"), 
       lty = 1, col =c('red', 'green', 'black'), bty = 'n', cex = 1, lwd = 2)

abline(v = 0.225, lwd = 2)

my_df$pred_resp <- factor(ifelse(my_df$prob > 0.225, 1, 0), labels = c("N", "Y"))

my_df$correct <- ifelse(my_df$pred_resp == my_df$hon, 1, 0)

ggplot(my_df, aes(prob, fill = factor(correct)))+
  geom_dotplot()+
  theme(axis.text=element_text(size=25),
        axis.title=element_text(size=25,face="bold"))

mean(my_df$correct)

test_df <- read.csv("test.csv", sep =";")
test_df$hon <- NA

test_df$hon <- predict(fit, newdata = test_df, type = "response")
View(test_df)

#Задача 3
library(ROCR)
my_df1 <- read.csv("data.csv")
my_df2 <- my_df1[!is.na(my_df1$admit),]

fit1 <- glm(admit ~ rank * gpa, my_df2, family = "binomial")
summary(fit1)

my_df1$prob <- predict(object = fit1, type = "response")

my_df1$prob[!is.na(my_df1$admit)] <- predict(object = fit1, type = "response")

pred_fit1 <- prediction(my_df1$prob[!is.na(my_df1$prob)], my_df1$admit[!is.na(my_df1$admit)])
perf_fit1 <- performance(pred_fit1, "tpr", "fpr")

my_df1$pred_resp <- factor(ifelse(my_df1$prob > 0.4, 1, 0), labels = c("N", "Y"))

df <- read.csv("data.csv")
df$admit <- factor(df$admit)
df$rank <- factor(df$rank)
#Задача 3
fit_df<- glm(admit ~ rank * gpa, df, family = "binomial")

df_na <- subset(df, is.na(df$admit) == TRUE)

df_na$prob <- predict(fit_df, newdata = df_na, type = "response")

df_na$probresp <- ifelse(df_na$prob > 0.4, 1, 0)
sum(df_na$probresp)
