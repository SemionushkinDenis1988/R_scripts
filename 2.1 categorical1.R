df <- read.csv("grants.csv")

str(df)

df$status <- as.factor(df$status)
levels(df$status) <- c("Not funded", "Funded")

df$status <- factor(df$status, labels = c("Not funded", "Funded"))

t1 <- table(df$status)
t1

dim(t1)

t2 <- table(df$status, df$field)
t2

dim(t2)

t2 <- table(status = df$status, field = df$field)
t2

prop.table(t2)

prop.table(t2, 1)

prop.table(t2, 2)


t3 <- table(Years = df$years_in_uni, Fiealds =df$field, Status =df$status)
t3

dim(t3)

HairEyeColor

dim(HairEyeColor)

dimnames(HairEyeColor)

HairEyeColor['Red', 'Blue' ,'Male']

prop.table(HairEyeColor[,,'Male'], 2)


red_men <-prop.table(HairEyeColor[,,'Male'], 2)['Red','Blue']

sum(HairEyeColor[,'Green', 'Female'])

barplot(t1)

barplot(t2, legend.text = T, args.legend = list(x = "topright"))
barplot(t2, legend.text = T, args.legend = list(x = "topright"), beside = T)

mosaicplot(t2)

mydata <- as.data.frame(HairEyeColor)

library("ggplot2")
mydata <- as.data.frame(HairEyeColor[,,'Female'])
obj <- ggplot(mydata, aes(x = Hair, y = Freq, fill = Eye)) + 
geom_bar(stat="identity", position ='dodge' )+ #указывает как устанавливать столбики
scale_fill_manual(values=c("Brown", "Blue", "Darkgrey", "Darkgreen")) #добавляет цвета в функцию fill
obj

mydata

binom.test(x = 1, n = 20, p = 0.5)
binom.test(t1)

chisq.test(t1)

chi <- chisq.test(t1)

chi$exp
chi$obs

chisq.test(t2)



fisher.test(t2)

new_tab <- HairEyeColor['Brown',,'Female']
chisq.test(new_tab)


main_stat <- chisq.test(diamonds$cut, diamonds$color)$statistic

chisq.test(diamonds$price, diamonds$carat)


diamonds$factor_price <-factor(diamonds$price >= mean(diamonds$price), labels = c(0,1))
diamonds$factor_carat <-factor(diamonds$carat >= mean(diamonds$carat), labels = c(0,1))

main_stat <- chisq.test(mydata1$factor_price, mydata1$factor_carat)$statistic

fisher_test <- fisher.test(mtcars$am, mtcars$vs)$p.value
