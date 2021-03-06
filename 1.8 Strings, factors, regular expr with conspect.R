##������

#������ ��� ������� ���� String, ��������� ������� - character
#������� paste(c("a","b"), "c", sep = "", ), paste0(c("a","b"), "c", sep = "", )
#�������� sep ��� paste �� ��������� ������, ��� paste0 - ������
#�������� collapse ���������� ������ � ���� ������ �� ����������� � ��������
#������� strsplit(s, "sep", fixed = TRUE) ��������� ������ �� ������
#�������� fixed false(�� ���������) ������ ��������� ��� ������ � ���������� ���������
#�������� fixed true ������ ��������� ��� ������ ���������

##���������� ���������
#������� grep("xxx", s) ���������� �������� ������ s � ������� ������ ������ "xxx"
#������� grepl("xxx", s) ���������� ���������� ������ ������ s ���� ������ ������ "xxx"
#grepl � grep �������� � �������!
#������� gsub("xxx", "####", s) �������� ��������� � ������ ������ �� ������ ��������
#[[:punct:]] - ����� ���������� � ���������� ����������
#[[:alpha:]] - �����
#[:space:] - �������
#[[:digit:]]
# "\\b[[:alpha::]]{4,5}\\b" ����� ��������� �� 4 ��� 5 ����
#$ - ����� ������
#* - ����� ���������� ��������
#^ - ������ ������
#+ - 1 � ������ ��������
#tolower(month.name), toupper(month.abb) ���������� � ������� � �������� ��������

#����� ��� ������ �� �������� - stringr
#str_extract(s, '�.') ��� ������� �������� � ������� ���������� ������� ��������� �������
#str_replace(s, "[��]", "?") �������� ������ �� ��������� ������ ��������
#[��] - ������ ���������� �����, ������� ��� � ��� �
#str_extract_all(s, "�."), all - ��� ���������, � �� ������
#str_replace_all(s, "[���], "?") �������� ��� ������� �� ������ ������
#str_length(s) ���������� ����� ������

##�������������� �����
#formatC(c(pi,exp(pi)), exp(pi), digits = 3, format = "e") ��������� digits ������ � �����

#������� cat("\tx\tc") ����������� ��������� ������� � ������� �� print()

##�������
#������������ ���������� ����� ��� factor
#as.numeric(f) ��� ������� ��������� ������ � �����
#������� levels(f) - ���������� ��������
#������� nlevels(f) - ���������� ������ �������
#(f <- droplevels(f)) - ������� ������ ��������, �� ���� ��, � ������� ��� ��������
#��������� � ������� ����� ������������ �� ������
#������������� ������ ordered(f, x) � ������� ������� x!!, ��� factor(ordered = TRUE) 
#������� cut(rnorm(1000), -5:5) ��������� ������� ������ �� ������
#������� tapply(warpbreaks$breaks, warpbreaks$wool, max) ��������� ������ �� ��������

options(stringsAsFactors = FALSE)
avian <- read.csv("avianHabitat.csv")
avian$Observer <- as.factor(avian$Observer)

coverage_variables <- names(avian)[str_detect(names(avian),"^P")]

sapply(coverage_variables, function(name) check_percent_range(avian[[name]]))

check_percent_range <- function(x){
  any(x<0 | x>100)
}

unique(avian$Site)

avian$site_name <- factor(str_replace(avian$Site, "[:digit:]+", ""))

tapply(avian$DBHt, avian$site_name, mean)

tapply(avian$total_coverage, avian$site_name, mean)

tapply(avian$LHt, avian$Observer, max)

quakes$mag <- cut(quakes$mag, seq(min(quakes$mag), ceiling(max(quakes$mag)), 0.5), right = F)

library(dplyr)
library(stringr)

summarise(group_by(quakes, mag), n())

tapply(warpbreaks$breaks, warpbreaks$wool, max)

table(cut(rnorm(1000), -5:5))

f <- factor(sample(LETTERS, 30, replace = T), ordered = T)

temp <- (c("feeezing cold", "cold", "comfortable", "burning hot"))

ft <- ordered(sample(temp, 14, replace = TRUE), temp)

f1 <- c(6,5,4,1,3,0)

ft <- ordered(sample(f1, 14, replace = TRUE), f1)

as.numeric(f)
as.character(f)

cat("\tx\tc")
print("\tx\tc")

formatC(c(pi,exp(pi)), exp(pi), digits = 3, format = "e")

paste(c("a","b"), "c", sep = "_")

paste0(c("a","b"), "c", collapse = "")

strsplit(c('a b', 'c d', 'e f'), " ", fixed = TRUE)

s <- c("�������� � ���� �� ��������", 
       "������ ���� - ����� �����",
       "��� ����� �� �������� � ����� �� �����",
       "������ �� ����, � ��� �� ������")

strsplit(s, "[[:punct:]]")

s <- c("xc xd xa xb", "xxxxxxxxxxx")

str_extract(s, '�.')

grep("�", s)

str_replace(s, "[��]", "?")

gsub("xxx", "####", s)

str_replace_all(s, "[���]", "?")

nchar("�������������� ��������� ��� ������� ����� ������� � ������������ ��������.")

library(stringr)

hamlet <- "To be, or not to be: that is the question:
Whether 'tis nobler in the mind to suffer
The slings and arrows of outrageous fortune,
Or to take arms against a sea of troubles,
And by opposing end them?"

hamlet <- str_replace_all(hamlet, "[:punct:]", "")
hamlet <- tolower(unlist(str_split(hamlet, "[:space:]")))

unlist(str_extract(hamlet, "to"))

sum(str_count(hamlet, "to"))

sum(grepl("[fqw]", hamlet))

sum(grepl("\\b.{7}\\b", hamlet))

unlist(str_extract(hamlet, "[fqw]"))

sum(str_count(hamlet, "[fqw]"))
