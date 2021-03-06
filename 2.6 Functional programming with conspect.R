##��������-��������������� �������

#Show Keyboard Shortcut Reference Alt+Shift+K

#Copy-on-modify semantics # ����� ������� ��������� � ��������� ��������� ������ �������

##����� microbenchmark � ������� system.time ���������� �������������� ����

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

##������� replicate(n, function(x)) �������� ������� function(x) n ���

##������� mapply(fun, from = 1:4, to = 2:5, by = 1/(1+1:4)) - ����������� ������� sapply
#��� from - ������ ���������� from, by - ������ ���������� by, to - ������ ���������� to
#���������� ����

#������� outer(letters, LETTERS, paste0) ���������� ��� ���������� ����������

#������� Vectorize(function(x, p), "p") ����������� ������� �� ��������� p

#������� do.call(rbind, list(df1, df2, df3))��������� ������� �� ������ ���������� ���������
#�������� do.call(list.files(), function(file) read.csv(file))

#������

cat_temper <- sort(c("����������", "�������", "���������", "�������"))
cat_color <- sort(c("�����", "�����", "������", "�����"))
cat_age <- sort(c("���", "������"))
cat_trait <- sort(c("� ������ �������", "� ������� �������", "� �������� �����"))

cat_catalogue <- outer(cat_temper, cat_color, cat_age, cat_trait, FUN = expand.grid)

s1 <- outer(X = cat_temper, Y = cat_color, paste)

s2 <- outer(X = cat_age, Y = cat_trait, paste)

s3 <- outer(s1, s2, paste)

sort(s3)[42]




# Random walk with absorption
simulate_walk <- function(lower = -10, upper = 10, n_max = 200, p = 1e-3) {
  current_position <- (lower + upper) / 2                        #������� �������
  for (i in 1:n_max) {
    is_absorbed <- rbinom(1, 1, p)     #���������� ��� ��� ���� �� ����������� p
    if (is_absorbed) return(list(status = "Absorbed", 
                                 position = current_position, 
                                 steps = i))
    current_position <- current_position + rnorm(1)#���� �� ��������������� - ��������
    if (current_position < lower) return(list(status = "Left breach",   #����� �����
                                              position = current_position, 
                                              steps = i))
    if (current_position > upper) return(list(status = "Right breach", #����� ������
                                              position = current_position, 
                                              steps = i))
  }
  return(list(status = "Max steps reached", #������������ ���������� �����(200) ��������
              position = current_position,
              steps = n_max))
}

# Simulate results
result <- replicate(1000, simulate_walk(), simplify = FALSE) #1000 ��� ��������� ���������
result <- data.frame(                                        #�������� ���� � ����������
  status = sapply(result, function(x) x$status),
  position = sapply(result, function(x) x$position),
  steps = sapply(result, function(x) x$steps)
)

# Inspect results
tapply(result$position, result$status, length) #��������� ����������
tapply(result$steps, result$status, mean) #��������� ����������

#������ � ������ ������ ������
simulate_walk <- function(n_max = 100, p = 1e-2) {
  current_position <- c(0, 0)                        #������� �������
  for (i in 1:n_max) {
    is_absorbed <-  rbinom(1, 1, p)    #���������� ��� ��� ���� �� ����������� p
    if (is_absorbed) return(1) #Adsorbed
    current_position <- current_position + c(rnorm(1), rnorm(1))#���� �� ��������������� - ��������
    if ((current_position[1]^2+current_position[2]^2)^0.5 > 6) 
      return(2) #Out
  }
  return(3) #"Max steps reached", #������������ ���������� �����(100) ��������
}

result <- replicate(100000, simulate_walk(), simplify = TRUE)

result1 <- c(sum(result == 1), sum(result == 2), sum(result == 3))

funs <- c("print","summary","plot")
meths <- lapply(funs, methods)

grepl("matrix", meths)
grepl("function", meths)
grepl("default", meths)

m1 <- function(x, y) {
  m <- matrix(0, length(x), length(y))
  for (i in 1:length(x)) 
    for (j in 1:length(y)) {
      m[i, j] = x[i] * y[j]
    }
  m
}

m2 <- function(x, y) {
  vapply(y, function(i) i * x, numeric(length(x)))
}

m3 <- function(x, y) x %o% y

x <- rnorm(100)
y <- runif(1000)
all.equal(m1(x, y), m2(x, y))
all.equal(m2(x, y), m3(x, y))

library(microbenchmark)
microbenchmark(m1(x, y), m2(x, y), m3(x, y))
