#Chunks - ������� � R ����� � rmd ����� 

#```{r}
#head(iris)
#```

#```{r, echo=FALSE} ����� ������� ����������� �����
#(1:10)^3
#```

#```{r}
#glacier <- read.csv("C:\\Users\\Titan\\R scripts\\R markdown\\demos\\glacier.csv", 
#                    na.strings = "..", comment.char = "#")
#```

echo = FALSE # ������� ��� �� ������������ �����

`r ...` #������ ������ ����, ������� ������������ ������ ������ 


library(dplyr)       
Date_dif <- summarise(group_by(glacier, GEO), 
          Date_dif = max(Ref_Date)-min(Ref_Date))
Date_dif$GEO[Date_dif$Date_dif == min(Date_dif$Date_dif)]

median_GEO <- summarise(group_by(glacier, GEO, MEASURE),
                median = median(Value, na.rm = T))
median_GEO$GEO[median_GEO$median == max(median_GEO$median)]

glacier$GEO[is.na(glacier$Value)]

##�������� �������� MarkDown
#**������ �����** 
#*��������� �����*
#`������������ �����`
#^������� ������^
#~������ ������~
#~~����������� �����~~
#\������������� ��������
#���������� ���� --(����) ---(������� ����) 
#######��������� - �� 1 �� 6 # ������ �� ������ � ��������
#<�����������> ��� [����� �����](�����������)
#������ ����� ������ ��� ������ � ������ ���� �� ������
#https://www.rstudio.com/wp-content/uploads/2016/03/rmarkdown-cheatsheet-2.0.pdf
#��������� ������ latex
#��� ������ PDF ����� ��������� "TEX"
#�� ������������ ������� ������� �� ����������� � PDF
#� ������� � ������� ����� ��� ����������� � pdf:
#�������������  MiKTeX. ��������� �� ������� ������� (����� � �������).
#��������� ��������, ��������� � ��������� "UTF-8"
# ���������� R : ������ "�����" (���������� ����� � "Knit") - Output Options - �������� ������ PDF (�� �������� ��������� ������� �� "Include table of contents") , ����� � "Advanced" - LaTeX engine: �������� "xelatex"
#����� ������ ������� (����� "--- Title") ����� "\usepackage[utf8]{inputenc}" (�������� ��� ???�������� ???"\usepackage[english,russian]{babel}???", �� ��� �� �������������)
#profit

library(rstudioapi) 
getVersion()

##����� Chunk'��(����� knitr):
#1)echo (default:TRUE) - ���������� �� �������� ����� ������?
#2)eval (default:TRUE) - ��������� �� ��� �����?
#3)include (default:TRUE) - ���������� �� ��������� ���������� �����?
#4)error (default:FALSE) - ��������� �� � �������� ����� ������?
#5)message (default:TRUE) - ��������� �� � �������� ����� ���������?
#6)warning (default:TRUE) - ��������� �� � �������� ����� ��������������?
#7)comment (default: "##") - ������� ����� ������ ������� ������ ����������?
#results(default:"markup") - ����� ������� �������� ���������� ���������� �����?
#highliter(default:TRUE) - ������������ �� ��������� ����?
#tidy(default:FALSE) - ��������������� �� ���(�������, �������)?
##����� chunk'�� ��������� � ���������:
#1)fig.height, fig.weight - ������� ����������� (� ������)
#2)fig.align (default:'default') ������������ ������� �� �������� (����� ������� �����/����)
#3)fig.cap(default:NULL) - ������ � �������� � �������
##����������� ������
#1)cache = TRUE ����������� ��� ������� ������, ����� ��� �������� �� �����������
#2)dependson - �����, ������� ��������� �� ������ ����, �� �������� ������� ���� ����,
#����� ����������� ������������ �����, ���� �������� ������� ����, �� ��������� 
#��������������� ������ � ���, ����������� ���������� ������ ���� ���������

##����� ������
#{r chunk1, echo = TRUE} ����� chunk1 - ��� �����
#{r chunk2, echo = TRUE, ref.label ='chunk1'} � ���� ������ ������� ����� ����� ������ ������

##���������� �����(������������ � ������ knitr)
#�������� ��� ������� � ������ ���������
#knitr::opts_chunk$set(echo=FALSE, fig.width = 5, fig.height = 5)
#���� ����� ����� ��������� � ��������� ���� � ������� include = FALSE, ����� ��� �� ������

##��� �������� ����� knitr
#1)����� �� ��������� ���� rmd ���������� knitr � ���������� ���������: 
#knitr:.Rmd ->.md (���� ���������������� � ����� ������)
#2)����� ���������� pandoc:.md ->.html, .doc, .tex (���� ���������������� � ����� ������)
#3)���� �� ����������� � .pdf, ����� ���������� latex � latex:.tex -> .pdf

##YAML header (��������� rmd ����� � ������� YAML)
#� ��������� .Rmd ����� ����� ��������������� knitr, pandoc, latex
#��������:output:
#           html_document: 
#             toc_float: TRUE
#��������� ����� ���� ������������� ��������������, ���. ��������� ����������� � params        
#��������: params:
#               n: 100
#               d: !r Sys.Date()
#������������� �������� ���: `r params$n`
#CSS(�����) - ������ ������ ����� ����������� �� ����� �������� ���
#����� �������� �������� ����, ����� ������� ��� � YAML header
#������: output:
#           html_document:
#             css: styles.css

#
#� mark down ����� ������������ HTML ���� ��������

#����� ggvis 
#ggvis - ��� web-friendly ������ ggplot2
#��������� ggvis ����� �� ggplot2

