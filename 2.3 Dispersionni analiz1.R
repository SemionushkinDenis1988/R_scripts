
mydata <- read.csv('shops.csv')

str(mydata)

boxplot(price ~ origin, data = mydata)
library(ggplot2)

ggplot(mydata, aes(x = origin, y = price))+
  geom_boxplot()

fit <- aov(price ~ origin, data = mydata)

summary(fit)

fit1 <- aov(price ~ origin + store, data = mydata)

summary(fit1)

model.tables(fit1, "means")

pd = position_dodge(0.1)
ggplot(mydata, aes(x = store, y = price, color = origin, group = origin))+
  stat_summary(fun.data = mean_cl_boot, geom='errorbar', width=0.1, position = position_dodge(width = 0.2))+
  stat_summary(fun.data = mean_cl_boot, geom='line', size=1,position = position_dodge(width = 0.2))+
  stat_summary(fun.data = mean_cl_boot, geom='point', shape='square', size=3, position = position_dodge(width = 0.2))+
  theme_bw()

fit3 <- aov(price ~ origin + store + origin:store, data = mydata)
summary(fit3)              

fit3 <- aov(price ~ origin*store, data = mydata)
summary(fit3) 

summary(npk)

summary(aov(yield ~ N*P, data = npk))

summary(aov(yield ~ N+P+K, data = npk))

ggplot(mydata, aes(x = food, y = price))+
  geom_boxplot()

fit5 <- aov(price ~ food, data = mydata)
summary(fit5)

TukeyHSD(fit5)

TukeyHSD(aov(Sepal.Width ~ Species, iris))

mydata2 <- read.csv('therapy_data.csv')
str(mydata2)

mydata2$subject <- as.factor(mydata2$subject)

fit6 <- aov(well_being ~ therapy, data = mydata2)
summary(fit6)

fit6b <- aov(well_being ~ therapy + Error(subject/therapy), data = mydata2)
summary(fit6b)

fit7 <- aov(well_being ~ therapy*price, data = mydata2)
summary(fit7)

ggplot(mydata2, aes(x = price, y = well_being))+
  geom_boxplot()

fit7b <- aov(well_being ~ therapy*price + Error(subject/(therapy*price)), data = mydata2)

summary(fit7b)

ggplot(mydata2, aes(x = price, y = well_being))+
  geom_boxplot()+
  facet_grid(~subject)

fit8 <- aov(well_being ~ therapy*price*sex, data = mydata2)
summary(fit8)

fit8b <- aov(well_being ~ therapy*price*sex + Error(subject/(therapy*price)), data = mydata2)
summary(fit8b)


mydata3 <- read.csv('Pillulkin.csv')

mydata3$patient <- as.factor(mydata3$patient)

aov(temperature ~ pill + Error(patient/pill), data = mydata3)
summary(aov(temperature ~ pill + Error(patient/pill), data = mydata3))

summary(aov(temperature ~ doctor*pill + Error(patient/doctor*pill), data = mydata3))

library(ggplot2)
obj <- ggplot(ToothGrowth, aes(x = as.factor(dose), y = len, col = supp, group = supp))+
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.1, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'line', size = 1, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'point', size = 3, position = position_dodge(0.2))
  

ggplot(mydata, aes(x = store, y = price, color = origin, group = origin))+
  stat_summary(fun.data = mean_cl_boot, geom='errorbar', width=0.1, position = position_dodge(width = 0.2))+
  stat_summary(fun.data = mean_cl_boot, geom='line', size=1,position = position_dodge(width = 0.2))+
  stat_summary(fun.data = mean_cl_boot, geom='point', shape='square', size=3, position = position_dodge(width = 0.2))+
  theme_bw()

?aes(group)
