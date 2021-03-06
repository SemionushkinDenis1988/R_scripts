##������� - ��� ��������� ������ ������(������ ��������� �� ��������)

#������� matrix(1:6,nrow = 2, ncol = 5, byrow = T) ������ ������� � ���������� � ���. �����������
#������� dim(m) ���������� ���������� ����� � �������� �������
#����������� ��������� ��������� ������� ����� dim(m) <- NULL (����������� � ������) ��� ������
#%*% - ��������� ������ �� �������� �������� �������
#����������� ����������� ����� ��������� ����� �������� drop � ������ x[,3, drop = F]
#������� rownames() colnames(), ���������� ����� ����� � �������
#������� paste0("xxx", 1:5) ��������� ������ �� ���� �������� � ��������� ������
#������� cbind() � rbind() ��������� ������� �� �������� � �������, ����� ���������� ����� ������
#������� apply(m, 1:2, function(x)) ��������� ������� � ������� ��� �������� �������
#������� function(x) � apply ���������� ���������
#������� rowSums, rowMeans, colSums, colMeans ���������� sum � mean �� �������� � ������
#
m <- matrix(1:6,nrow = 2, ncol = 5, byrow = T)
m1 <- matrix(1:6,nrow = 5, ncol = 2, byrow = T)
x <- dim(m)

dim(m) <- c(5, 2)

rownames(m) <- paste0("xxx", 1:5)
colnames(m) <- paste0("xxx", 1:2)

cbind(m,m1)
rbind(m,m)

f <- function(x) sum(x^2)

apply(m1, 2, f)

apply(m1, 1:2, function(i) if (i>13) i else 13)

rowSums(m)
colSums(m)


mat <- matrix(6, 4, 5)
mat[mat > 5]
mat>5

#������ 1 ����� � ������� v ���������� ����� � n
find_closest <- function(v, n) {
    which(
      abs(v-n) == min(abs(v-n))
    )
}

v <- c(5, 2, 7, 7, 7, 2, 0, 0)
n <- 4
abs(v-n)==1

z <- find_closest(v,n)

#����������� ������ "�� ���������"

bind_diag <- function(m1, m2, fill){
  m3 <- matrix(fill,
               nrow = nrow(m1)+nrow(m2),
               ncol = ncol(m1)+ncol(m2)
               )
  m3[1:nrow(m1), 1:ncol(m1)] <- m1
  m3[nrow(m1) + 1:nrow(m2), ncol(m1) + 1:ncol(m2)] <- m2
  m3
}

m1 <- matrix(1:12, nrow = 3)
m2 <- matrix(10:15, ncol = 3)
bind_diag(m1, m2, fill = NA)
bind_diag(m2, m1, fill = 0)

#������ ������ ��������
build_ziggurat <- function(n) {
  m <- matrix(1, n*2-1, n*2-1)
  if(n>1){
    for (i in c(2:n)){
      m[i:(n*2-i), i:(n*2-i)] <- m[i:(n*2-i), i:(n*2-i)]+1}
  } 
  else {m}
  m
}

build_ziggurat <- function(n) {
  d <- n * 2 - 1
  outer(1:d, 1:d, function(x,y) {         #������� ������� outer � pmin
    x <- n - abs(n - x)
    y <- n - abs(n - y)
    pmin(x,y)
  }
  )
}

build_ziggurat_mod <- function(n) {
  x <- n - abs(n - seq_len(2*n - 1))
  outer(x, x, pmin)                        #������� ������� outer � pmin
}

build_ziggurat <- function(n) {
  if (n==1) {return(matrix(1))
  } else {
    x<-c(1:n, (n-1):1)      #����� ������������ ������ ��� ���������� �������! 
    m<-matrix(x, 2*n-1, 2*n-1)
    n<-matrix(x, nrow=2*n-1, ncol=2*n-1, byrow = T)
    l<-(m[]+n[]-abs(m[]-n[]))/2
    return(l)
  }
}
m <- matrix(c(1:4, 3:1), 7, 7)
m1 <- matrix(c(1:4, 3:1), 7, 7, byrow = T)

##������
#������ ��� ��������������� ���������
#������ ����� �������� �� �������� ������ �����
#������ ����� ���� ����������� (��������� ������ ����� ���� ������)
#������� list(1:5, "my_data", matrix(0, 2, 2)) ������� ������
#������� c(l1, l2) (combine) ���������� ��� �� ������
#������� list(v) ����� ������� ������ �� v
#������� unlist(v) ����� ������� ������ �� v
#�������� l[[3]] <- NULL ��� l[[1]] <- list(NULL) - ������ ������� �� ������
#������� is.null(l$string) - ��������� ������� �������� � ������
#������� lapply(l, fun) - ��������� ������� � ������� �������� ������, ���������� ������
#������� sapply(l, fun) - ��������� ������� � ������� �������� ������, ���������� ����
#������� lengths - ���������� ����� ��������� ������
#"..." � ���������� ������� ��������, ��� �� ����� ��������� ����� ���������� ����������
#�������� sum(c(1:4), c(1:4),c(1:4),c(1:4))
#������� diag() ���������� ������� � ������/�������� �� ���������, ��� ��������� ������� 

##NA,NAN, NULL ��������
#NA -- ��� ����������� �������� ("not available"). ��������, ���������� �� ������� �� ��� ������� ������������ ������, ��� ������ � ������������ �� ����������� ������ ���������� ��-�� ���� ������������. NA � ���� ������ ����������, ��� ��� ������ ���������� � ����� �����, �� �� �� ������� ������.
#NaN -- "not-a-number" -- ��������� ������������ �������������� ��������, �������� 0/0 ��� Inf - Inf.
#NULL -- ���������� �������, "�������". ����������� � ��� �������, ����� ������ ������������� �� ����������, �� ����� ����� ������������ ��������.
#��� �������� �������� ���� ��� �������, is.na, is.nan � is.null, ��������������.


diag(c(1,2))

l <- list(1:5, "my_data", "1mat" = matrix(0, 2, 2))

l1 <- list(vec = 1:7, fun = sqrt)

l1$fun(5)

lapply(l, length)

lapply(l, paste, collapse = "|")

l2 <- list(incredibly_long_name= 123)
 
l2$inc        #����� ������� ����� ����� ���� ��� ������� �������

f <- function(y, x = ridiculous_long_variable) y + ridiculous_long_variable
  
f(3, ridic = 5) #����� ������� ����� �����, ���� ��� ������� �������

#������� get longest
get_longest <- function(l){
  len <- sapply(l, length)
  ind <- which.max(len)
  list(number = ind, element = l[[ind]])
    
}
#������� generate list
gen_list <- function(n_elements, max_len, seed = 111){ #seed ��������� ���������
  set.seed(seed)
  len <- sample(1:max_len, n_elements)
  lapply(1:n_elements, function(i) rnorm(len[i]))
}

l3 <- gen_list(4, 10) 
n_elements <- 4
max_len <- 10

len <- sample(1:max_len, n_elements)

gl1 <- get_longest(l3)

gl1$number

#������ 2
count_elements <- function(x) {
  u <- rle(sort(x))                     #rle ���������� ���� ����� ��������� � ����
  matrix(c(u$values, u$lengths), 2, byrow = T)
}

count_elements(x)

x <- c(5, 2, 7, 7, 7, 2, 0, 0)

z <- rle(sort(x))

set.seed(1789)
bastille <- list(
  "La Chapelle Tower" = rbinom(5, 10, 1/2), 
  "Tresor Tower" = rbinom(8, 12, 1/4), 
  "Comte Tower" = rbinom(14, 3, 1/5) + 1,
  "Baziniere Tower" = rbinom(8, 4, 4/5), 
  "Bertaudiere Tower" = rbinom(4, 8, 2/3),
  "Liberte Tower" = rbinom(1, 100, 0.1), 
  "Puits Tower" = rbinom(5, 5, 0.7),
  "Coin Tower" = rbinom(3, 16, 0.4)
)

sum(sapply(bastille, sum))

unique(sort(x))
which(sort(x) == unique(sort(x)))
z

