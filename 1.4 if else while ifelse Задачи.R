mtcars$new_var <- ifelse(mtcars$carb >= 4 | mtcars$cyl > 6, 1, 0)

my_vector <- 20
result <- ifelse(mean(my_vector) > 20, "My mean is great", "My mean is not so great")

?AirPassengers
str(AirPassengers)

good_months <-

for (i in 2:144){
  good_months <- as.vector(AirPassengers)[as.vector(AirPassengers)[i] > as.vector(AirPassengers)[i-1]]
}

for (i in 2:144){
  print(as.vector(AirPassengers)[as.vector(AirPassengers)[i] > as.vector(AirPassengers)[i-1]])
}

for (i in 2:144){
  if (as.vector(AirPassengers)[i] > as.vector(AirPassengers)[i-1]){
    good_months <- as.vector(AirPassengers)[i]
  }
}

good_months <- c()
  
for (i in 2:144){
  good_months[i] <- as.vector(AirPassengers)[as.vector(AirPassengers)[i] > as.vector(AirPassengers)[i-1]]
}

good_months <- c()    
index <- 1    
for (i in 2:length(AirPassengers)) {    
  if (AirPassengers[i]>AirPassengers[i-1]){    
    good_months[index] <- AirPassengers[i]    
    index <- index + 1    
  }    
}

good_months <- AirPassengers[-1][AirPassengers[-1] > AirPassengers[-144]] 


j <- 1
good_months <- c()
for (i in 2:length(AirPassengers)) {
  if (AirPassengers[i] > AirPassengers[i-1]){
    good_months[j] <- AirPassengers[i]
    j <- j+1
  }
}

AirPassengers[AirPassengers[-1]>AirPassengers[-144]]

i <- 1
j <- 1
moving_average <- c()
while (i <= 135){
  moving_average[j] <- mean(AirPassengers[i:(i+9)])
  j <- j+1
  i <- i+1
}

n <- 10    
d <- AirPassengers    
cx <- c(0, cumsum(d))    
moving_average <- (cx[(n + 1):length(cx)] - cx[1:(length(cx) - n)]) / n

library(zoo)
moving_average <- rollmean(AirPassengers, 10)
str(AirPassengers)
