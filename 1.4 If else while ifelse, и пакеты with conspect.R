##����� � �������

#������� if(�������){�����������}else{�����������}
#������� ifelse(�������, ��������� ���� ������� �����, ��������� ���� ������� �� �����)
#������������� ����� switch("x", a = 5, b = 7, x = 10, 0) ������ 10
#���� repeat{�����������} break 
#���� while(�������){�����������}
#���� for(i in x){�����������} 
#�������� ����� next - ������������� ����
#�������� ����� break - ��������� ����
#������� System.time({����}) ���������� ����� ���������� �������

#������ 1
set.seed(1337)
x <- runif(1e6, min = -1, max = 1)
sum((-0.2) < x & x < (0.3))

#������ 2
dice_roll <- function(n){
  round(runif(n, 0.5, 6.5))
}

##������

#���������� - ��� ������� �������
#����� ��� ����� ����������, ������ �������� �� ������ cran.r-project.org
#������� installed.packages() ���������� ������ ������������� �������
#������� install.packages("xts", dependencies = T) ��������� xts � ���� ������� �� ������� �� �������
#������� update.packages() - ���������� ������
#������� sessionInfo() - ���������� ������ R, ������������ ������


mydata <- read.csv('evals.csv')

a <- -10

if (a > 0) {
  print('positive')
} else {
  print('not positive')
  print(a + 1)
} 

if (a > 0) {
  print('positive')
} else if (a < 0) {
  print('negative')
} else print('zero')

ifelse(a > 0, 'positive', 'not positive')

b <- c(1, -1)

ifelse(b > 0, 'positive', 'not positive')

for (i in 1:100){
  print(i)
}

for (i in 1:nrow(mydata)){
  print(mydata$score[i])
}

for (i in 1:nrow(mydata)){
  if (mydata$gender[i] == 'male'){
    print(mydata$score[i])
  }
}
mydata$qyality <- NULL

mydata$qvality <- rep(NA, nrow(mydata))

for (i in 1:nrow(mydata)){
  if (mydata$score[i] > 4){
    mydata$qvality[i] <- 'good'
  } else mydata$qvality[i] <- 'bad'
}

mydata$qvality2 <-  ifelse(mydata$score > 4, 'good', 'bad')

i <- 1

while (i < 51){
  print(mydata$score[i])
  i <- i+1
}
