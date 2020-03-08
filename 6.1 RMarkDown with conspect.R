#Chunks - патерны с R кодом в rmd файле 

#```{r}
#head(iris)
#```

#```{r, echo=FALSE} после запятой указываются опции
#(1:10)^3
#```

#```{r}
#glacier <- read.csv("C:\\Users\\Titan\\R scripts\\R markdown\\demos\\glacier.csv", 
#                    na.strings = "..", comment.char = "#")
#```

echo = FALSE # убирает код из выгружаемого файла

`r ...` #пример инлайн кода, который используется внутри текста 


library(dplyr)       
Date_dif <- summarise(group_by(glacier, GEO), 
          Date_dif = max(Ref_Date)-min(Ref_Date))
Date_dif$GEO[Date_dif$Date_dif == min(Date_dif$Date_dif)]

median_GEO <- summarise(group_by(glacier, GEO, MEASURE),
                median = median(Value, na.rm = T))
median_GEO$GEO[median_GEO$median == max(median_GEO$median)]

glacier$GEO[is.na(glacier$Value)]

##Основные элементы MarkDown
#**жирный шрифт** 
#*курсивный шрифт*
#`моноширинный шрифт`
#^вархний индекс^
#~нижний индекс~
#~~Зачеркнутый текст~~
#\экранирование символов
#автозамена тире --(тире) ---(длинное тире) 
#######Заголовок - от 1 до 6 # подряд от малого к большому
#<гиперссылка> или [любой текст](гиперссылка)
#полный набор команд для работы с тектом есть по адресу
#https://www.rstudio.com/wp-content/uploads/2016/03/rmarkdown-cheatsheet-2.0.pdf
#поддержка пакета latex
#для сборки PDF нужна установка "TEX"
#из комментариев степика справка по конвертации в PDF
#К вопросу о русском языке при конвертации в pdf:
#Устанавливаем  MiKTeX. Прогоняем на предмет пдейтов (проще в консоли).
#Формируем документ, сохраняем в кодировке "UTF-8"
# настройках R : панель "опции" (шестеренка рядом с "Knit") - Output Options - выбираем формат PDF (не забываем поставить галочку на "Include table of contents") , затем в "Advanced" - LaTeX engine: выбираем "xelatex"
#самой первой строкой (ПЕРЕД "--- Title") елаем "\usepackage[utf8]{inputenc}" (советуют еще ???добавить ???"\usepackage[english,russian]{babel}???", но мне не потребовалось)
#profit

library(rstudioapi) 
getVersion()

##Опции Chunk'ов(пакет knitr):
#1)echo (default:TRUE) - отображать ли исходный текст чанков?
#2)eval (default:TRUE) - исполнять ли код чанка?
#3)include (default:TRUE) - отображать ли результат исполнения чанка?
#4)error (default:FALSE) - добавлять ли в документ текст ошибок?
#5)message (default:TRUE) - добавлять ли в документ текст сообщений?
#6)warning (default:TRUE) - добавлять ли в документ текст предупреждений?
#7)comment (default: "##") - префикс перед каждой строкой вывода результата?
#results(default:"markup") - каким образом выводить результаты исполнения чанка?
#highliter(default:TRUE) - подсвечивать ли синтаксис кода?
#tidy(default:FALSE) - отформатировать ли код(отступы, пробелы)?
##Опции chunk'ов связанные с графиками:
#1)fig.height, fig.weight - размеры изображений (в дюймах)
#2)fig.align (default:'default') расположение графика на странице (можно прижать влево/прав)
#3)fig.cap(default:NULL) - строка с подписью к графику
##Кеширование чанков
#1)cache = TRUE кеширование для длинных чанков, чтобы они повторно не выполнялись
#2)dependson - опция, которая указывает на другой чанк, от которого зависит этот чанк,
#чтобы кеширование отрабатывало верно, если меняется старший чанк, то зависимый 
#пересчитывается вместе с ним, кеширование запоминает только один результат

##Метки чанков
#{r chunk1, echo = TRUE} метка chunk1 - имя чанка
#{r chunk2, echo = TRUE, ref.label ='chunk1'} в ходе вызова второго чанка будет вызван первый

##Глобальные опции(определяются в пакете knitr)
#Задаются как правило в начале документа
#knitr::opts_chunk$set(echo=FALSE, fig.width = 5, fig.height = 5)
#Этот вызов можно поместить в отдельный чанк и указать include = FALSE, чтобы его не видеть

##Как работает пакет knitr
#1)Когда мы запускаем файл rmd включается knitr и происходит следующее: 
#knitr:.Rmd ->.md (файл перерабатывается в новый формат)
#2)Далее Включается pandoc:.md ->.html, .doc, .tex (файл перерабатывается в новый формат)
#3)Если мы преобразуем в .pdf, тогда включается latex и latex:.tex -> .pdf

##YAML header (заголовок rmd файла в формате YAML)
#в заголовке .Rmd файла можно конфигурировать knitr, pandoc, latex
#Например:output:
#           html_document: 
#             toc_float: TRUE
#параметры могут быть дополнительно настраиваемыми, доп. параметры указываются в params        
#например: params:
#               n: 100
#               d: !r Sys.Date()
#Использование возможно так: `r params$n`
#CSS(стили) - способ менять стиль отображения не меняя исходный код
#Чтобы добавить стилевой файл, можно указать его в YAML header
#Пример: output:
#           html_document:
#             css: styles.css

#
#В mark down можно использовать HTML теги напрямую

#Пакет ggvis 
#ggvis - это web-friendly аналог ggplot2
#Синтаксис ggvis похож на ggplot2

