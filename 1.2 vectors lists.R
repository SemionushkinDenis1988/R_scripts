1:67

my_vector1 <- 1:67

my_vector2 <- c(-32, 45, 67, 12.78, 129, 0, -65)

my_vector1[1]
my_vector1[3]

my_vector2[2]

my_vector2[c(1,2,3)]

my_vector2[1:3]

my_vector2[c(1,4,6,7,10)]

the_best_vector <- c(1:5000,7000:10000)

my_numbers_2 <- my_numbers_1[c(2, 5, 7, 9, 12, 16, 20)]


my_vector1 + 10
my_vector2 + 56

my_vector2 == 0
my_vector1 > 30

x <- 23

my_vector1 > 23
my_vector1 > x

my_vector2[my_vector2 > 0]
my_vector2[my_vector2 < 0]

my_vector1[my_vector1 > 20 & my_vector1 < 30]

my_numbers <- my_vector1[my_vector1 > 20 & my_vector1 < 30]

positive_numbers <- my_vector2[my_vector2 > 0]

v1 <- c(165, 178, 180, 181, 167, 178, 187, 167, 187)

mean_v1 <- mean(v1)

v1[v1 > mean_v1]

grater_than_mean <- v1[v1 > mean_v1]

my_sum <- sum(
  my_vector[my_vector>10]
  )

age <- c(16, 18, 22, 27)
is_maried <- c(F, F, T, T)

name <- c("Olga", "Maria", "Nastya", "Polina")
data <- list(age, is_maried, name)

data[[1]][1]
data[[2]][3]

df <- data.frame(Name = name, Age = age, Status = is_maried)

