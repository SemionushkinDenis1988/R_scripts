##########���������� ������ � ������ ������� ���������
#
#1)���������� ������(K-means ��� ������������� �������������:
#� ������� ����������� ������� �� ����� ��������� ���� ������� �� ������� ���������, 
# � �� ����������
#2)������ ������� ���������:
#� ������� ������� ������� ��������� �� ����� ������������� ���� ����������
#� ���������� ��������������� ���������� � ���� ����� ������������� ����������
#
#������ ���������� ��������:
#1)� �������(K means)
#2)������������� �������������
#
#�� ������� ������ Iris �������� ��������� ������ �� �������� �� �������(Species)
#K-means
#������� ��������:
#1)������ �� ������� ��������� ����� ������ ����������
#2)�������� �������� ��������� ������� ���������� ��������
#3)������� ��������� �� ��������� �������
#

library(ggplot2)
d <- iris[, c("Sepal.Length", "Petal.Width")]

fit <- kmeans(d, 3)
d$clusters <- factor(fit$cluster)

ggplot(d, aes(Sepal.Length, Petal.Width, col = clusters))+
  geom_point(size = 2)+
  theme_bw() 

install.packages("factoextra")
library(ggplot2)
library(factoextra)
theme_set(theme_minimal())
d <- iris[, c("Sepal.Length", "Petal.Width")]
fit <- kmeans(d, 3)
fviz_cluster(fit, d)

#������� �������� K-means:
#a)��������� N ��������� ����������
#�)���� ��������� � ��� ����� �� �������
#�)��������� ��������� ��������� � �������������� ����� ��������� �����
#�)��� ��� ��������� �������������, �� ��������� ����� ������ ����������� ������ ����������
#�)�������������� �������������� ����� �� ������� � ������ ����������(���������)
#�)��� ��� ��������� ����� ����������� � ������ ����������, �� �������������� �����
#���� ����� ���������, ����� ��� ����� ���� ����������� ��������� � ����� ��������������
#�����
#
####���� �������� ����������� �� ��� ���, ���� ����� �� ���������� ���������� �� ������
#�������� � ������.
#
#�������� 2 �������� ������:
#1)��������� �� ������� ��������?
#2)����� ����� ��������� ����������� ����� �������?
#
#1)��������� �� ������� ��������:
#�������� �� ������ �������� �����, ���� ���� � ����������� ���� ��������� �������������
#��� ����, ��� �� �������� ������� �����, ����� �������� � ����, ��� �������� �������� 
#� �������� �������
#������� ������� ������� ��������� ��������� ���������:
#a)������ ��������� ����� ��������� ���� �� �����(� ������ ������������ ��������� ���������)
#�)������������ ���������� ����������� ������� � ����, ������ ��� �������� �������� � ����
#� ���� ���������, �� ��� �� ��������
#
#2)����� ����� ��������� ����������� ����� �������:
#a)� �������� ������������� ������������ ����� ��������� ������ ������� ��������
#� ����� ����� ���������.
#��������������� ����� ���������(within cluster sum of squares) - ����� ��������� ����������
#������� ���������� �� ��������� ��������
#����� ��������������� ����� ���������(total within cluster sum of squares) - ����� ����
#���� ��������� ���� ���������
#
###����: ���� ���������� ��� ������ �������� � ���� ������ ����������� �������� �����
#����� ���������, ��� ��������, ��� � ���������� ����� ��������� ���� �����
#���� ������ �������� ����� ��������� �������� �������, ��� ����� ��������, ��� �����
#������������� � ������ ���

install.packages("NbClust")
library(NbClust)
data(iris)
dt <- iris[, 1:4]
N <- NbClust(dt, distance = "euclidean",
             min.nc = 2, max.nc = 15, method = "complete", 
             index = "alllong")

#���� �� ������� �������� ����� ��������� ���� ������������ �������, ������ � ����������� 
#���� ����� �������� ����� ����������� ����� ���������
#
###������������� ������������� 
#a)������� �������������� ���������� �� ������ ����� �� ������� �� ������ �����
#�)�� ���������� ������ ������������� ������������� �������� ������������ ����� ����� �����
#���� ��������� ������� �� ������� ������������� ������������� ���������� ������, ���� ��
#��� - ����� ��������� �����/���������� ������
#
#����� ��������� �����/���������� ������(���� ��� ����� �������� ������, � ������):
#a)� ������ ������ �������� �������, ��� ���������� ��������� ��������� ���������� �����
#�)����� ����� ��������� ���������� ������ ����� �� ������
#�)����� ������������ ��� ��������� �����, � �� ���������� ���������� �� ���������� ��
#���������
#�)������������ ��������� ������ � ���������� ������ ���� �����
#�)� �������� ����� ���������� ���� ������� �������(������ ���������, ������������)
#
#�������� ����� ������ ������������� ������������� ������ � ��������, �� ��������
#������������ ����� �� �������
#
#����������� ���������� ������������� ������������� � k-means: 
#������� �� ������������� ������������� ���������� ���������� ���������, ����� 
#������������ �-means

library(ggplot2) 
library(ggrepel) # ��� ����������� ������� ����� �� �������

x <- rnorm(10)
y <- rnorm(10)
test_data <- data.frame(x, y)
test_data$labels <- 1:10

ggplot(test_data, aes(x, y, label = labels))+
  geom_point()+
  geom_text_repel()

d = dist(test_data[1:2])
fit <- hclust(d, method = "single")
plot(fit, labels = test_data$labels)
rect.hclust(fit, 6) # ������� �������� ����� ���������, ������ ����� 2

library(ape)
set.seed(222)
tr <- rtree(
  20, 
  tip.label = 
  c("B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U")
  ) 

plot.phylo(tr)  #(� ������ ����������)

plot.phylo(tr, use.edge.length=FALSE)  #(������ � ������ ������� �������������)

######������ ������� ���������
#
#������ ���� X � Y �������� ����� ��� - ������������� ������, � ����� �� ������� �����������
#��������� �� ��� ���.
#������������� ������ � ������ ������ ���������� - ����� ������� �����������.
#�� ������������� ������ �� ����� ������������ ��������� ������
#
#��������������� ������������� ������ �������� ������ ������� ����������
#
#�� ����� ���������, ����� ������� ��������� ��������� ������ �� ��������� (Scree plot)
#���� ���� �� ��������� ��������� ������ 5% ���������, �� �� ����� ���������� �� ��
#� �������� ���� ���������� ������ ����
#
#������ �� ������� ����������� ���������� biplot, �� ����������� 1�� ����������, 
#�� ��������� ������ ����������
#
#�������� �����������:
#�������� ����� ����������, �������� ������ "������ R" � "������ ����������" - 
#"���� � ������� ������", � ������� ������� �������� �� ��� ������������� ������
#
#� ������� ������� ������ ��������� ����� ���������� ������-������������� ����������
#� ��������� ����������� ����� ������

data(swiss)
fit <- prcomp(swiss, center = T)
plot(fit, type = "l")
summary(fit)
#PC1     PC2      PC3     PC4     PC5    PC6
#Standard deviation     43.836 21.6022 12.05342 4.75916 3.65754 2.4882
#Proportion of Variance  0.746  0.1812  0.05641 0.00879 0.00519 0.0024
#Cumulative Proportion   0.746  0.9272  0.98361 0.99240 0.99760 1.0000
#92% ������ ����������� ����� �����������
biplot(fit)                         #���������� ���������� ����������� ������

install.packages("pca3d")
library(pca3d)

dt <- swiss
dt$is_catholic <- ifelse(swiss$Catholic > 50, 1, 0)
dt$is_catholic <- factor(dt$is_catholic)
fit <- prcomp(swiss, center = T)
pca3d(fit, group = dt$is_catholic,              #���������� ������
      fancy = T, 
      new=T)

install.packages("devtools")
library(devtools)
install_github("ggbiplot", "vqv")       #�� ���������������
library(ggbiplot)                       # ��� ������ �� 6 ���, ���������� � 2������ ������

dt <- swiss
dt$is_catholic <- ifelse(swiss$Catholic > 50, 1, 0)
dt$is_catholic <- factor(dt$is_catholic)
fit <- prcomp(swiss, center = T)
g <- ggbiplot(fit, obs.scale = 1, var.scale = 1,
              group = dt$is_catholic,
              ellipse = TRUE,
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal',
               legend.position = 'top')
print(g)

library(FactoMineR)
df <- mtcars[,c(1,3:7)]
res.pca <- PCA(df, graph=TRUE)

Scree.Plot <- function(R,main="Scree Plot",sub=NULL){
  roots <- eigen(R)$values
  x <- 1:dim(R)[1]
  plot(x,roots,type="b",col='blue',ylab="Eigenvalue",
       xlab="Component Number",main=main,sub=sub) 
  abline(h=1,lty=2,col="red")
  
}
R <- cor(df)
Scree.Plot(R,main ="Scree Plot (Husson mtcars Data)")

#��������� ������ - ����� ��������� ����������� ������. ���� ������ ����������� � ���, 
#��� �������� ��������� ���������� ����� ���� ������������� � �������. ������ - 
#��� ��������� ��������� ����������, ������� ���������� �������� ���������� � ������.
#��������, ���� �� � ����� ������� ������ ���������� ���������������� �����, ������, 
#�����, ����� �����������, ������� ��������� � ������� �������� �����. �� ��������, �� 
#������ �� �������� ��� �������: "���������� ����������" � "������ ������". ����������, 
#������ ��� ���������� �� ������ ������, � ���������� ���������� - ������  ������.
#������� ��������� �� �������� ���������� ���������� �������, ������������ � ����� ������
#swiss ��� ����, ����� �������� ��� �������:
  
#Image: 
#https://ucarecdn.com/0f7b065a-ace4-43bd-941c-0e91ee39324e/-/crop/302x172/0,0/-/preview/
  
#�������� ���������� ���������� ������� - ��� ��������� �������� ��� ������ ����������. 
#������� �������� � �������� ������� ���������, �� ����� �������� ����� ���������� � 
#������� ������� ������������� ����� ����� � ������ �� �������� ������� �������, � ����� 
#���������� �������� ������������ ���� � ������ � ������ �� ������ ������.
#� ������ ������� �����, ��� ������ ������ - ��� ������ �������� �� ���������� Fertility, 
#Agriculture �� ������� �������� ���������� Examination � Education. ������ ������ - 
#��� �������� ������� �������� �� ����������  Fertility, Agriculture, ������ �� 
#���������� Examination � ������������ ����� ������ ���������� Catholic - �� ���� ������� 
#������������� ���������.
#����� �������, ����� ������������, ��� �� ������������ ����� ���������� �������� ��� 
#������� - ������������ ��������������� ��� ������������� ���������.

fit <- factanal(swiss, factors = 2, rotation = "varimax")
print(fit)

#������ 1
smart_hclust<-  function(test_data, cluster_number){
  dist_matrix <- dist(test_data, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)
  fit <- hclust(dist_matrix, method = "complete", members = NULL)
  cluster <- cutree(fit, cluster_number)
  test_data$cluster <- factor(cluster,labels = c(1:cluster_number))
  test_data
}

smart_hclust <- function(test_data, n_cluster){    
  d <- dist(test_data)    
  fit <- hclust(d)    
  test_data$cluster <- factor(cutree(fit, k = n_cluster))    
  return(test_data) 
}
smart_hclust(x, 3)

dist_matrix <- dist(swiss) # ������ ������� ����������(���������� ����� ������� �� �������)
fit <- hclust(dist_matrix) # ������������� ������������� (������������)
cluster <- cutree(fit, 3) # (���� ������������)

hclust(d, method = "complete", members = NULL) #��� ������������� ������ 
dist(x, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)
#��� ������� ������� ���������� 
cutree() #�������� ��������

test_data <- x
x <- read.csv("https://stepic.org/media/attachments/course/524/test_data_hclust.csv")
#����� ������ 1
#������ 2

#������� 1(���)
get_difference<-  function(test_data, n_cluster){
  dist_matrix <- dist(test_data, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)
  fit <- hclust(dist_matrix, method = "complete", members = NULL)
  test_data$cluster <- factor(cutree(fit, n_cluster))
  s <- apply(
    test_data[1:length(test_data)-1], 2, function(x) anova(aov(x~cluster, test_data))$P[1]
    )
  return(names(s)[s<0.05])
}
#������� 2
get_difference <- function(df, n_clusters){
  fit <- hclust(dist(df))
  cluster <- factor(cutree(fit, n_clusters))
  is.good <- sapply(df, function(x) anova(aov(x ~ cluster))$P[1] < 0.05)
  names(df)[is.good]
}
#������� 3
get_difference <- function(td, cn) {
  cluster <- factor(cutree(hclust(dist(td)), cn))
  a <- sapply(td, function(x) unlist(summary(aov(x ~ cluster)))) #unlist summary ��������
  colnames(td[a['Pr(>F)1',] < 0.05])
}

test_data <- read.csv("https://stepic.org/media/attachments/course/524/cluster_2.csv")
s <- apply(select(test_data, -"cluster"), 2, function(x) anova(aov(x~cluster, test_data)))
get_difference(test_data, 2)
n_cluster <- 2
#����� ������ 2
#������ 3
get_pc <- function(d){
  fit <- prcomp(d, center = T)
  d$PC1 <- fit[["x"]][,1]
  d$PC2 <- fit[["x"]][,2]
  return(d)
}

d <- read.csv("https://stepic.org/media/attachments/course/524/pca_test.csv")
#����� ������ 3

#������ 4
#������� 1
get_pca2 <- function(data){
  fit <- prcomp(data, center = T)
  cbind(data,fit[["x"]][,c(1:(sum(summary(fit)[["importance"]][3,]<0.9)+1))])
}
#������� 2
get_pca2 <- function(test_data){    
  fit <- prcomp(test_data)    
  cum_prop <- summary(fit)$importance['Cumulative Proportion',]    
  test_data <- cbind(test_data, fit$x[,1:min(which(cum_prop>0.9))])    
  return(test_data)    
}

#������� 3
get_pca2 <- function(data) {
  fit <- prcomp(data)
  n <- which(cumsum(fit$sdev^2 / sum(fit$sdev^2)) > 0.9)[1]
  cbind(data, fit$x[, seq_len(n)])
}
#������� 4
get_pca2 <- function(df){
  fit <- prcomp(df)
  pcs <- summary(fit)$importance[3, ]
  ind <- which(pcs>0.9)[1]
  cbind(df, fit$x[,1:ind])
}

data <- swiss

get_pca2(swiss)

#����� ������ 4

#������ 5
#������� 1
is_multicol <- function(d){
  s <- apply(d,2, function(x) apply(d, 2, function(y) cor(x,y)))
  diag(s) <- 0
  ifelse(length(rownames(which(round(abs(s), 3)==1, arr.ind = T)))>0,
         return(rownames(which(round(abs(s), 3)==1, arr.ind = T))),
         return("There is no collinearity in the data"))
}

#������� 2
is_multicol <- function(d){    
  d <- abs(cor(d))     
  d[lower.tri(d)] <- 0    
  diag(d) <- 0    
  index <- which((1-d) < 1e-10, arr.ind = T)    
  if (length(index) == 0){      
    return('There is no collinearity in the data')    
  } else {      
    return(rownames(d)[index])      
  }      
}


is_multicol(d)

diag(apply(d,2, function(x) apply(d, 2, function(y) cor(x,y)))) <- 0

fit[["rotation"]][1,] %in% fit[["rotation"]]

d <- as.data.frame(list(V1 = c(21, 13, 20, 14, 10), V2 = c(16, 7, 10, -3, 9), V3 = c(17, 5, 16, 11, 19), V4 = c(24, 5, 11, 19, -5), V5 = c(-10, -1, -4, 9, -3), V6 = c(31, 12, 18, 26, 2)))
d <- as.data.frame(list(V1 = c(6, 4, 16, 12, -6), V2 = c(9, 6, 24, 17, 15), V3 = c(6, 9, -3, 14, 14), V4 = c(4, 1, 19, 12, 10), V5 = c(-1, 1, -11, -7, 11)))
d <- read.csv("https://stepic.org/media/attachments/course/524/Norris_2.csv")
d <- read.csv("https://stepic.org/media/attachments/course/524/Norris_3.csv")
#����� ������ 5

#������ 6
d <- dist(swiss)    
fit <- hclust(d)    
swiss$cluster <- factor(cutree(fit, k = 2))    
library(ggplot2)
my_plot <- ggplot(swiss, aes(Education, Catholic, col = cluster))+
  geom_point()+
  geom_smooth(method = "lm")

