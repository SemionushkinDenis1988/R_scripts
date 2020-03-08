my_calc <- function(x, y){
  s <- x + y
  return(s)
}

result <- my_calc(x = 10, y = 15)

my_calc <- function(x, y){
  s <- x + y
  d <- x - y
  return(c(s, d))
}

my_calc2 <- function(x, y, z = 10){
  s <- x + y + z
  d <- x - y - z
  return(c(s, d))
}

my_calc2(1,2,3)


distr1 <- rnorm(100)
hist(distr1)

distr1[1:30] <- NA
distr1[is.na(distr1)] <- mean(distr1, na.rm = T)

my_na_rm <- function(x){
  x[is.na(x)] <- mean(x, na.rm = T)
  return(x)
}

distr1 <- my_na_rm(x = distr1)

hist(distr1)


my_na_rm(x = c("2", "3", NA))

my_na_rm <- function(x){
  if(is.numeric(x)){
  x[is.na(x)] <- mean(x, na.rm = T)
  return(x)
  }
  else{
    print("X is not numeric")
  }
}

my_na_rm <- function(x){
  if(is.numeric(x)){
    stat_test <- shapiro.test(x)
    if (stat_test$p.value > 0.05){
      x[is.na(x)] <- mean(x, na.rm = T)
      print("NA values were replaced with mean")
    }
    else{
      x[is.na(x)] <- median(x, na.rm = T)
      print("NA values were replaced with median")
    }
    return(x)
  }
  else{
    print("X is not numeric")
  }
}

  
stat_test <- shapiro.test(rnorm(1000))

distr1 <- rnorm(1000)
distr1[1:30] <- NA

distr1 <- my_na_rm(distr1)

d1 <- rnorm(2000)
d2 <- runif(2000)

d1[1:10] <- NA
d2[1:10] <- NA

d1 <- my_na_rm(d1)
d2 <- my_na_rm(d2)

d1 <- rnorm(1000)
d1[1:10] <- NA

source("2.4 functions1.R")

my_na_rm(d1)


my_vector <- c(1, 2, 3, NA, NA)
my_vector[is.na(my_vector)]
new_vector <- c(1:length(my_vector))[is.na(my_vector)]

NA.position <- function(x){
  return(c(1:length(x))[is.na(x)])
}
NA.position <- function(x){    
  which(is.na(x))}

NA.counter <- function(x){
  return(length(c(1:length(x))[is.na(x)]))
}
NA.counter <- function(x){    
  return(sum(is.na(x)))}

dir(pattern = "*.csv")

grants <- data.frame()

for (i in dir(pattern = "*.csv")){
  temp_df <- read.csv(i)
  grants <- rbind(temp_df, grants)
}

read_data <- function(){
  df <- data.frame()
  number <<- 0
  for (i in dir(pattern = "*.csv")){
    temp_df <<- read.csv(i)
    df <- rbind(temp_df, df)
    number <<- number + 1
  }
  print(paste(as.character(number), "files were combined"))
  return(df)
}

grants2 <- read_data()

write.csv(grants2, 'grants2.csv')

x <- c(1, -2, 3, NA, NA)
filtered.sum <- function(x){
  return(sum(x[!(is.na(x)) & x > 0]))
}

filtered.sum(x)

outliers.rm  <- function(x){
  return(x[x < quantile(x, probs = c(0.25, 0.75))[2]+1.5*IQR(x) & 
            x > quantile(x, probs = c(0.25, 0.75))[1]-1.5*IQR(x)])
}

g <- c(4.15, 0.6, 0.87, 1.36, -0.24, 1.3, -0.21, 0.27, -1.05, 0.83, -0.61, 1.12, -0.97, 0.19, -1.22, 1.16, 0.97, 1.28, -0.4, -0.87, -1.26, 0.28, -0.36, 2.38)

quantile(g, probs = c(0.25, 0.75))[1]
