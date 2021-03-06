##�������

#������������ - ����� ���� ������� �������� � ������
#������� ��� ������ ������� ������(�� ���� ���� �� ������ ��� � �������� ������)
#������� sign() ���������� ���� ������� �������� ��� 0 ���� ������� ������ 0
#������� all.equal(x, y) ���������� x � y, ���� ����� �� ���, ���� ��� �� ����
#���������� � ��������� ���� ����� ������� ����� ��� - (f <- function(x) x^5), (sd) 
#E��� � ������ ���� .C, .Call, .Fortran, .External, .Internal, .Primitive - ��� ���������
#� ����������������� ����, �� ���� ����� ��� ����� ���� �������� ���. ��� R
#�������� (var) �� (sd)
#E��� � ������ ���� UseMethod ��� standartGeneric, �� ��� method dispatch ��� ������� S3/S4?
#methods(plot)[1:20] (�����������, ��� ������� ������� ���� �����)
#������� ���������� ��������� �� ����� return ��� ��������� �������� ��������� ��������

##������ ����������:
#f <- function(arg1, arg2, remove_na = TRUE, ..., optional_arg)
#��� ���������� ������� f(1, arg2, remove = F, optional_arg = 42, do_magic = TRUE)
#������� ����� ����� ��������� �� ���������� �����
#������� ����� ����� ���������, ��� ��� �������� ������� (remove)
#����� ����������� 1:5 + 1:2���������
#��������� ��������� ����� ����������� �� ����� ����������!!
#������������� ��������� �������� � "..."(ellipsis)(�������� ��������� ����������, ������� ����������)
#������� ���������� ������: f <- function(x, pow = 2) x^pow
#integrate(f, 0, 1, pow = 3) - pow=3 �������� � "..." ������� integrate � �������������� � f
#%in% - �������� �������� ���������, ����� ��������� ���� ����������� ��������
#�������� %nin% <- function(x, y) {...}
#����� stringi ����� ��������� ������� ��� ��������� ����� � �������
#������� outer(x, y) ���������� ��� ��������� ���������� �������� x, y � ��������� FUN � ���
##��������-��������������� �������

#��� ��� ������, ���� ���� ������� � �� ������
##�������� ������� � R
##S3-������: 
#1)��� ���������� ���������� ������
#2)������� ����� ����� ������ ���������(method dispatch) � ����������� �� ������
#3)����� �������(��� ������ �������) ���������� generic
##S4-������:
#1)������� ����������� ������ � ��� �����
#2)������ ������������ ��� ��������� �������
##Reference classes

##Generic - �������
#length(methods(print)) - 186, �������� ������� �� generic, ���� ������� ������ 1�� �� generic

##������� replicate(5, function(x)) �������� ������� function(x) 5 ���

##������� mapply(fun, from = 1:4, to = 2:5, by = 1/(1+1:4)) - ����������� ������� sapply
#��� from - ������ ���������� from, by - ������ ���������� by, to - ������ ���������� to
#���������� ����

#������� outer(letters, LETTERS, paste0) ���������� ��� ���������� ����������

#������� Vectorize(function(x, p), "p") ����������� ������� �� ��������� p

#������� do.call(rbind, list(df1, df2, df3))��������� ������� �� ������ ���������� ���������
#�������� do.call(list.files(), function(file) read.csv(file))


"%+%" <- function(x, y) {
  length(y) <- length(x) <- max(length(x), length(y))
  x+y
}
y <- c(1:5)
x <- c(1:2)

x %+% y

max(length(x), length(y))

length(y) <- length(x)

y

#�������� ������ ����
values <- c("Ace", 2:10, "Jack", "Queen", "King")
suits <- c("Clubs", "Diamonds", "Hearts", "Spades")
card_deck <- outer(values, suits, paste, sep = " of ")

#�������� �������

generator <- function(set) { 
  function(n) sample(set, n, replace = T)
} 

roulette_values <- c("Zero!", 1:36)
rigged_roulette_values <- c("Zero!", "Zero!", 1:36)
fair_roulette <- generator(roulette_values)
rigged_roulette <- generator(rigged_roulette_values)


#��������� �������

generator <- function(set) function(n) sample(set, n, replace = T)

#�������� ����������

card_generator <- generator(card_deck) #��������� card_deck � card_generator

card_generator(n = 2) #�������� card_generator � ���������� n

decorate_string <- function(pattern, ...){ 
  paste(pattern, paste(...), stringi::stri_reverse(pattern), sep = "")
}

decorate_string(pattern = "123", "abc", "def", sep = "+")

library(stringr)

?matrix

fun_list <- c(mean, max)

sapply(fun_list, function(f) f(1:100)) #������� � ������ ���������, ���������� �� ������

apply_f <- function(f, x) f(x)         #�������� ������� apply_f

sapply(fun_list, apply_f, x = 1:100)  #������� � ������ ���������, ���������� �� ������ 

apply_f(function(x) sum(x^2), 1:10)       #apply_f �������� 2� �������� � ������� � 1� ���.

square <- function() function(x) x^2 #�������, ������� ���������� ������ �������

square()(5) #������������� �������, ������� ���������� ������ �������

f <- function(x){                 
  g <- function(y) if (y>0) 1 else if (y < 0) -1 else 0 #������� ������ ������ �������
  sapply(x, g)
}
sign(-100:100)

my_calc  <- function(x, y){
  s  <- x + y
  return(s)
}

my_calc(x = 10, y = 15)
result  <- my_calc(10, 15)

my_calc_2  <- function(x, y){
  s  <- x + y
  d  <- x - y
  return(c(s, d))
}

my_calc_2(10, 15)


my_calc_3  <- function(x, y, z){
  s  <- x + y + z
  d  <- x - y - z
  return(c(s, d))
}

my_calc_3  <- function(x, y, z = 10){
  s  <- x + y + z
  d  <- x - y - z
  return(c(s, d))
}



############

distr1  <- rnorm(100)
distr1[1:30]  <- NA
distr1[is.na(distr1)]  <- mean(distr1, na.rm = T)

my_na_rm  <- function(x){
  if (is.numeric(x)){
    stat_test  <- shapiro.test(x)
    if (stat_test$p.value > 0.05){
      x[is.na(x)]  <- mean(x, na.rm = T)
      print("NA values were replaced with mean")
    } else{
      x[is.na(x)]  <- median(x, na.rm = T)
      print("NA values were replaced with median")
    }
    return(x)
  } else{
    print("X is not numeric")
  }
}

d1  <- rnorm(2000)
d2  <- runif(2000)

d1[1:10]  <- NA
d2[1:10]  <- NA

d1  <- my_na_rm(d1)
head(d1)

d2  <- my_na_rm(d2)
head(d2)


######################

read_data  <- function(){
  df  <- data.frame()
  number  <<- 0
  for (i in dir(pattern = "*.csv")){
    temp_df  <- read.csv(i, stringsAsFactors = F)
    df  <- rbind(temp_df, df)
    number <<- number + 1
    }
  print(paste(as.character(number), "files were combined"))
  return(df)
}

# Advanced method without for loop

read_data_advanced <- function(){
    df <- do.call(rbind, lapply(dir(pattern = "*.csv"), 
                                read.csv, stringsAsFactors = F))
    return(df)
}

df  <- data.frame(x = factor(1:5))
df1  <- data.frame(x = factor(7:8))
str(df)
str(df1)

df3  <- rbind(df, df1)
str(df3)
table(df3$x)
