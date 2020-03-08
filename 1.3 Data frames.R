?read.table

mydata <- read.csv("evals.csv")

head(mydata, 3)
tail(mydata, 3)

View(mydata)

str(mydata)

summary(mydata)

names(mydata)

a <- names(mydata)

mydata$score

b <- mydata$score

mean(mydata$score)

summary(mydata$score)

mydata$ten_point_scale <- mydata$score*2

mydata$new_variable <- 0

mydata$number <- 1:nrow(mydata)

mydata$score[1:10]

mydata[1,1]

mydata[1:100,1]

mydata$gender == 'female'

mydata[mydata$gender == 'female', 1]

head(mydata[mydata$gender == 'female', 1:3], 3)

head(subset(mydata, gender == 'female'), 3)

head(subset(mydata, score > 3.5), 3)

mydata2 <- subset(mydata, gender == 'female')
mydata3 <- subset(mydata, gender == 'male')

mydata4 <- rbind(mydata2, mydata3)

mydata5 <- mydata[, 1:10]
mydata6 <- mydata[, 11:24]

mydata7 <- cbind(mydata5, mydata6)
