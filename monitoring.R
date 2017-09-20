library("XML")
baseurl <- "http://indicators.miccedu.ru/monitoring/_vpo/inst.php?id="
#2017
"http://indicators.miccedu.ru/monitoring/_vpo/inst.php?id=1766"
#2016
"http://indicators.miccedu.ru/monitoring/2016/_vpo/inst.php?id=1766"
#2015
"http://indicators.miccedu.ru/monitoring/2015/inst.php?id=1766"
#2014
"https://miccedu.ru/monitoring/2014/materials/inst_1766.htm"
#2013
"https://miccedu.ru/monitoring/2013/materials/inst_1766.htm"

########################### 2016 ############ 2017 ###########################

library("XML")

mon <- data.frame("id"='', "year"='', "vuz"='', "reg"='', "ind"='', "adr"='', "rec"='', "ved"='', "web"='', "dir"='', "org"='', 
                  "rez"='', "group"='', "domain"='', "ugns"='', "index"='', "variable"='', "value"='', 
                  "unit"='', "threshold"='', "change"='', "last"='', stringsAsFactors = F)
mon <- mon[-1,]

links <- data.frame("year"=c(2016:2017), "link"=c("http://indicators.miccedu.ru/monitoring/2016/_vpo/inst.php?id=",
                                                  "http://indicators.miccedu.ru/monitoring/_vpo/inst.php?id="), 
                    stringsAsFactors = F)
for (i in 1:2){
#LOOP VUZ DATA 2017
for (id in idmon){
  tryCatch({
    
    vuz <- readHTMLTable(paste0(links[i,2], id), stringsAsFactors = FALSE)
    
    if(length(vuz) == "0" ) {stop("ERROR: Образовательная организация не найдена. ID = ", id)}
    else { 
      

#---------------I. Общие сведения--------------
x <- vuz$info
Encoding(names(x)) <- "UTF-8"
Encoding(x[,1]) <- "UTF-8"
Encoding(x[,2]) <- "UTF-8"

s <- strsplit(as.character(x[1,2]), ',')

info <- data.frame("id"=id, "year"=links[i,1], "vuz"=names(x)[2], "reg"=gsub('.*[.]', '', gsub('[0-9]+', '', s[[1]][1])), 
                   "ind"=sapply(s[[1]][1], function(x) as.numeric(gsub("[^\\d]+", "", x, perl=TRUE)))[[1]], 
                   "adr"=trimws(paste(s[[1]][2], s[[1]][3], gsub('на карте', '', s[[1]][4]), sep = ",")), 
                   "rec"=x[2][x[1]=='Ректор'], "ved"=x[2][x[1]=='Ведомственная принадлежность'], 
                   "web"=x[2][x[1]=='web-сайт'], "dir"=x[2][x[1]=='Учредитель(и)'], 
                   "org"=x[2][x[1]=='Профиль организации'], 
                   "rez"=ifelse(length(x[2][x[1]=='Результат оценки эффективности деятельности'])==0, 
                                '',x[2][x[1]=='Результат оценки эффективности деятельности']), 
                   stringsAsFactors = F)

#---------------II. Сведения по показателям мониторинга эффективности деятельности--------------

result <- data.frame("id"='', "group"='', "domain"='', "ugns"='', "index"='', "variable"='',  "value"='', 
                     "unit"='', "threshold"='', "change"='', "last"='', stringsAsFactors = F)
result <- result[-1,]

#E.1-E.8 Основные
x <- na.omit(vuz$result)
Encoding(x[,2]) <- "UTF-8"
Encoding(x[,3]) <- "UTF-8"
Encoding(x[,5]) <- "UTF-8"

###s <- strsplit(x$V5, '\\(')

DF <- data.frame("id"=id, "group"='Основные', "domain"='Основные', "ugns"='', "index"=x$V1, "variable"=x$V2, 
                 "value"=gsub("\\*", "", x$V3), "unit"='', "threshold"=x$V4, "change"=gsub("%.*$", "", x$V5), 
                 "last"=gsub(".*\\(|\\)", "", x$V5), stringsAsFactors = F)
result <- rbind(result, DF)

#1.1-1.15 Образовательная деятельность
x <- vuz[5][[1]]
Encoding(names(x)) <- "UTF-8"
Encoding(x[,2]) <- "UTF-8"
Encoding(x[,3]) <- "UTF-8"

DF <- data.frame("id"=id, "group"='Основные', "domain"='Образовательная деятельность', "ugns"='', "index"=x[,1], 
                 "variable"=x[,2], "value"=gsub(" ", "", x[,4]), "unit"=x[,3], "threshold"='', "change"='', "last"='', 
                 stringsAsFactors = F)
result <- rbind(result, DF)

#2.1-2.16 Научно-исследовательская деятельность
x <- vuz[6][[1]]
Encoding(names(x)) <- "UTF-8"
Encoding(x[,2]) <- "UTF-8"
Encoding(x[,3]) <- "UTF-8"

DF <- data.frame("id"=id, "group"='Основные', "domain"='Научно-исследовательская деятельность', "ugns"='', "index"=x[,1], 
                 "variable"=x[,2], "value"=gsub(" ", "", x[,4]), "unit"=x[,3], "threshold"='', "change"='', "last"='', 
                 stringsAsFactors = F)
result <- rbind(result, DF)

#3.1-3.13 Международная деятельность
x <- vuz[7][[1]]
Encoding(names(x)) <- "UTF-8"
Encoding(x[,2]) <- "UTF-8"
Encoding(x[,3]) <- "UTF-8"

DF <- data.frame("id"=id, "group"='Основные', "domain"='Международная деятельность', "ugns"='', "index"=x[,1], 
                 "variable"=x[,2], "value"=gsub(" ", "", x[,4]), "unit"=x[,3], "threshold"='', "change"='', "last"='', 
                 stringsAsFactors = F)
result <- rbind(result, DF)

#4.1-4.4 Финансово-экономическая деятельность
x <- vuz[8][[1]]
Encoding(names(x)) <- "UTF-8"
Encoding(x[,2]) <- "UTF-8"
Encoding(x[,3]) <- "UTF-8"

DF <- data.frame("id"=id, "group"='Основные', "domain"='Финансово-экономическая деятельность', "ugns"='', "index"=x[,1], 
                 "variable"=x[,2], "value"=gsub(" ", "", x[,4]), "unit"=x[,3], "threshold"='', "change"='', "last"='', 
                 stringsAsFactors = F)
result <- rbind(result, DF)

#5.1-5.8 Инфраструктура
x <- vuz[9][[1]]
Encoding(names(x)) <- "UTF-8"
Encoding(x[,2]) <- "UTF-8"
Encoding(x[,3]) <- "UTF-8"

x[2:5,2] <- paste(sub('в том числе:', '', x[1,2]), x[2:5,2], sep=" ")

DF <- data.frame("id"=id, "group"='Основные', "domain"='Инфраструктура', "ugns"='', "index"=x[,1], 
                 "variable"=x[,2], "value"=gsub(" ", "", x[,4]), "unit"=x[,3], "threshold"='', "change"='', "last"='', 
                 stringsAsFactors = F)
result <- rbind(result, DF)

#6.1 Трудоустройство
x <- vuz[10][[1]]
Encoding(names(x)) <- "UTF-8"
Encoding(x[,2]) <- "UTF-8"
Encoding(x[,3]) <- "UTF-8"

DF <- data.frame("id"=id, "group"='Основные', "domain"='Трудоустройство', "ugns"='', "index"=x[,1], 
                 "variable"=x[,2], "value"=gsub(" ", "", x[,4]), "unit"=x[,3], "threshold"='', "change"='', "last"='', 
                 stringsAsFactors = F)
result <- rbind(result, DF)

#7.1-7.5 Кадровый состав
x <- vuz[11][[1]]
Encoding(names(x)) <- "UTF-8"
Encoding(x[,2]) <- "UTF-8"
Encoding(x[,3]) <- "UTF-8"

DF <- data.frame("id"=id, "group"='Основные', "domain"='Кадровый состав', "ugns"='', "index"=x[,1], 
                 "variable"=x[,2], "value"=gsub(" ", "", x[,4]), "unit"=x[,3], "threshold"='', "change"='', "last"='', 
                 stringsAsFactors = F)
result <- rbind(result, DF)

#---------------V. Дополнительные характеристики образовательной организации--------------
x <- vuz$analis_dop
Encoding(x[,1]) <- "UTF-8"
Encoding(x[,2]) <- "UTF-8"
Encoding(x[,3]) <- "UTF-8"
Encoding(x[,4]) <- "UTF-8"

x[5,1] <- sub('в том числе:', '', x[5,1])
x[5:7,2:4] <- x[5:7,1:3]
x[5:7,2] <- paste(x[4,2], x[5:7,2], sep=" ")
x[5:7,1] <- c('1a', '1b', '1c')

#Образовательная деятельность
DF <- data.frame("id"=id, "group"='Дополнительные', "domain"='Образовательная деятельность', "ugns"='', "index"=x[4:15,1], 
                 "variable"=x[4:15,2], "value"=gsub(" ", "", x[4:15,4]), "unit"=x[4:15,3], "threshold"='', "change"='', "last"='', 
                 stringsAsFactors = F)
result <- rbind(result, DF)
#Научная деятельность*
DF <- data.frame("id"=id, "group"='Дополнительные', "domain"='Научная деятельность', "ugns"='', "index"=x[17:27,1], 
                 "variable"=x[17:27,2], "value"=gsub(" ", "", x[17:27,4]), "unit"=x[17:27,3], "threshold"='', "change"='', "last"='', 
                 stringsAsFactors = F)
result <- rbind(result, DF)
#Кадровый потенциал*
DF <- data.frame("id"=id, "group"='Дополнительные', "domain"='Кадровый потенциал', "ugns"='', "index"=x[29:37,1], 
                 "variable"=x[29:37,2], "value"=gsub(" ", "", x[29:37,4]), "unit"=x[29:37,3], "threshold"='', "change"='', "last"='', 
                 stringsAsFactors = F)
result <- rbind(result, DF)
#Международная деятельность
DF <- data.frame("id"=id, "group"='Дополнительные', "domain"='Международная деятельность', "ugns"='', "index"=x[39:47,1], 
                 "variable"=x[39:47,2], "value"=gsub(" ", "", x[39:47,4]), "unit"=x[39:47,3], "threshold"='', "change"='', "last"='', 
                 stringsAsFactors = F)
result <- rbind(result, DF)
#Инфраструктура
DF <- data.frame("id"=id, "group"='Дополнительные', "domain"='Инфраструктура', "ugns"='', "index"=x[49:57,1], 
                 "variable"=x[49:57,2], "value"=gsub(" ", "", x[49:57,4]), "unit"=x[49:57,3], "threshold"='', "change"='', "last"='', 
                 stringsAsFactors = F)
result <- rbind(result, DF)
#Финансово-экономическая деятельность
DF <- data.frame("id"=id, "group"='Дополнительные', "domain"='Финансово-экономическая деятельность', "ugns"='', "index"=x[59:67,1], 
                 "variable"=x[59:67,2], "value"=gsub(" ", "", x[59:67,4]), "unit"=x[59:67,3], "threshold"='', "change"='', "last"='', 
                 stringsAsFactors = F)
result <- rbind(result, DF)


#---------------Объединение таблиц--------------

micedu <- merge(info, result, by="id")
mon <- rbind(mon, micedu)

    }
    message("Connecting indicators.miccedu.ru/monitoring/ ", paste0('Год ', links[i,1], ' / ID = ', id, ' From ', which(idmon == id)))
  }, error=function(e){cat("ОШИБКА! ",conditionMessage(e), "\n")})
}

  }


########################### 2016 ############ 2017 ###########################


########################### 2015 ############ 2015 ###########################

library("XML")
baseurl <- "http://indicators.miccedu.ru/monitoring/2015/inst.php?id="

#LOOP VUZ DATA 2015
for (id in idmon[!(idmon %in% c(1573,1809,1811,1813))]){
  tryCatch({
    
    vuz <- readHTMLTable(paste0(baseurl, id), stringsAsFactors = FALSE)
    
    if(length(vuz) == "0" ) {stop("ERROR: Образовательная организация не найдена. ID = ", id)}
    else { 


#---------------I. Общие сведения--------------
x <- vuz$info
Encoding(names(x)) <- "UTF-8"
Encoding(x[,1]) <- "UTF-8"
Encoding(x[,2]) <- "UTF-8"

s <- strsplit(as.character(x[1,2]), ',')

info <- data.frame("id"=id, "year"=2015, "vuz"=names(x)[2], "reg"=gsub('.*[.]', '', gsub('[0-9]+', '', s[[1]][1])), 
                   "ind"=sapply(s[[1]][1], function(x) as.numeric(gsub("[^\\d]+", "", x, perl=TRUE)))[[1]], 
                   "adr"=trimws(paste(s[[1]][2], s[[1]][3], gsub('на карте', '', s[[1]][4]), sep = ",")), 
                   "rec"=x[2][x[1]=='Ректор'], "ved"=x[2][x[1]=='Ведомственная принадлежность'], 
                   "web"=x[2][x[1]=='web-сайт'], "dir"=x[2][x[1]=='Учредитель(и)'], 
                   "org"=x[2][x[1]=='Профиль организации'], 
                   "rez"=ifelse(length(x[2][x[1]=='Результат оценки эффективности деятельности'])==0, 
                                '',x[2][x[1]=='Результат оценки эффективности деятельности']), 
                   stringsAsFactors = F)

#---------------II. Сведения по показателям мониторинга эффективности деятельности--------------

result <- data.frame("id"='', "group"='', "domain"='', "ugns"='', "index"='', "variable"='',  "value"='', 
                     "unit"='', "threshold"='', "change"='', "last"='', stringsAsFactors = F)
result <- result[-1,]

#E.1-E.8 Основные
x <- na.omit(vuz$result)
Encoding(x[,2]) <- "UTF-8"
Encoding(x[,3]) <- "UTF-8"
Encoding(x[,5]) <- "UTF-8"

DF <- data.frame("id"=id, "group"='Основные', "domain"='Основные', "ugns"='', "index"=x$V1, "variable"=x$V2, 
                 "value"=gsub("\\*", "", x$V3), "unit"='', "threshold"=x$V4, "change"=gsub("%.*$", "", x$V5), 
                 "last"=gsub(".*\\(|\\)", "", x$V5), stringsAsFactors = F)
result <- rbind(result, DF)

#1.1-1.15 Образовательная деятельность
x <- vuz[3][[1]]
Encoding(names(x)) <- "UTF-8"
Encoding(x[,2]) <- "UTF-8"
Encoding(x[,3]) <- "UTF-8"

DF <- data.frame("id"=id, "group"='Основные', "domain"='Образовательная деятельность', "ugns"='', "index"=x[,1], 
                 "variable"=x[,2], "value"=gsub(" ", "", x[,4]), "unit"=x[,3], "threshold"='', "change"='', "last"='', 
                 stringsAsFactors = F)
result <- rbind(result, DF)

#2.1-2.16 Научно-исследовательская деятельность
x <- vuz[4][[1]]
Encoding(names(x)) <- "UTF-8"
Encoding(x[,2]) <- "UTF-8"
Encoding(x[,3]) <- "UTF-8"

DF <- data.frame("id"=id, "group"='Основные', "domain"='Научно-исследовательская деятельность', "ugns"='', "index"=x[,1], 
                 "variable"=x[,2], "value"=gsub(" ", "", x[,4]), "unit"=x[,3], "threshold"='', "change"='', "last"='', 
                 stringsAsFactors = F)
result <- rbind(result, DF)

#3.1-3.13 Международная деятельность
x <- vuz[5][[1]]
Encoding(names(x)) <- "UTF-8"
Encoding(x[,2]) <- "UTF-8"
Encoding(x[,3]) <- "UTF-8"

DF <- data.frame("id"=id, "group"='Основные', "domain"='Международная деятельность', "ugns"='', "index"=x[,1], 
                 "variable"=x[,2], "value"=gsub(" ", "", x[,4]), "unit"=x[,3], "threshold"='', "change"='', "last"='', 
                 stringsAsFactors = F)
result <- rbind(result, DF)

#4.1-4.4 Финансово-экономическая деятельность
x <- vuz[6][[1]]
Encoding(names(x)) <- "UTF-8"
Encoding(x[,2]) <- "UTF-8"
Encoding(x[,3]) <- "UTF-8"

DF <- data.frame("id"=id, "group"='Основные', "domain"='Финансово-экономическая деятельность', "ugns"='', "index"=x[,1], 
                 "variable"=x[,2], "value"=gsub(" ", "", x[,4]), "unit"=x[,3], "threshold"='', "change"='', "last"='', 
                 stringsAsFactors = F)
result <- rbind(result, DF)

#5.1-5.8 Инфраструктура
x <- vuz[7][[1]]
Encoding(names(x)) <- "UTF-8"
Encoding(x[,2]) <- "UTF-8"
Encoding(x[,3]) <- "UTF-8"

x[2:5,2] <- paste(sub('в том числе:', '', x[1,2]), x[2:5,2], sep=" ")

DF <- data.frame("id"=id, "group"='Основные', "domain"='Инфраструктура', "ugns"='', "index"=x[,1], 
                 "variable"=x[,2], "value"=gsub(" ", "", x[,4]), "unit"=x[,3], "threshold"='', "change"='', "last"='', 
                 stringsAsFactors = F)
result <- rbind(result, DF)

#6.1 Трудоустройство
x <- vuz[8][[1]]
Encoding(names(x)) <- "UTF-8"
Encoding(x[,2]) <- "UTF-8"
Encoding(x[,3]) <- "UTF-8"

DF <- data.frame("id"=id, "group"='Основные', "domain"='Трудоустройство', "ugns"='', "index"=x[,1], 
                 "variable"=x[,2], "value"=gsub(" ", "", x[,4]), "unit"=x[,3], "threshold"='', "change"='', "last"='', 
                 stringsAsFactors = F)
result <- rbind(result, DF)

#7.1-7.5 Кадровый состав
x <- vuz[9][[1]]
Encoding(names(x)) <- "UTF-8"
Encoding(x[,2]) <- "UTF-8"
Encoding(x[,3]) <- "UTF-8"

DF <- data.frame("id"=id, "group"='Основные', "domain"='Кадровый состав', "ugns"='', "index"=x[,1], 
                 "variable"=x[,2], "value"=gsub(" ", "", x[,4]), "unit"=x[,3], "threshold"='', "change"='', "last"='', 
                 stringsAsFactors = F)
result <- rbind(result, DF)

#---------------V. Дополнительные характеристики образовательной организации--------------
x <- vuz$analis_dop
Encoding(x[,1]) <- "UTF-8"
Encoding(x[,2]) <- "UTF-8"
Encoding(x[,3]) <- "UTF-8"
Encoding(x[,4]) <- "UTF-8"

x[5,1] <- sub('в том числе:', '', x[5,1])
x[5:7,2:4] <- x[5:7,1:3]
x[5:7,2] <- paste(x[4,2], x[5:7,2], sep=" ")
x[5:7,1] <- c('1a', '1b', '1c')

#Образовательная деятельность
DF <- data.frame("id"=id, "group"='Дополнительные', "domain"='Образовательная деятельность', "ugns"='', "index"=x[4:15,1], 
                 "variable"=x[4:15,2], "value"=gsub(" ", "", x[4:15,4]), "unit"=x[4:15,3], "threshold"='', "change"='', "last"='', 
                 stringsAsFactors = F)
result <- rbind(result, DF)
#Научная деятельность*
DF <- data.frame("id"=id, "group"='Дополнительные', "domain"='Научная деятельность', "ugns"='', "index"=x[17:27,1], 
                 "variable"=x[17:27,2], "value"=gsub(" ", "", x[17:27,4]), "unit"=x[17:27,3], "threshold"='', "change"='', "last"='', 
                 stringsAsFactors = F)
result <- rbind(result, DF)
#Кадровый потенциал*
DF <- data.frame("id"=id, "group"='Дополнительные', "domain"='Кадровый потенциал', "ugns"='', "index"=x[29:37,1], 
                 "variable"=x[29:37,2], "value"=gsub(" ", "", x[29:37,4]), "unit"=x[29:37,3], "threshold"='', "change"='', "last"='', 
                 stringsAsFactors = F)
result <- rbind(result, DF)
#Международная деятельность
DF <- data.frame("id"=id, "group"='Дополнительные', "domain"='Международная деятельность', "ugns"='', "index"=x[39:47,1], 
                 "variable"=x[39:47,2], "value"=gsub(" ", "", x[39:47,4]), "unit"=x[39:47,3], "threshold"='', "change"='', "last"='', 
                 stringsAsFactors = F)
result <- rbind(result, DF)
#Инфраструктура
DF <- data.frame("id"=id, "group"='Дополнительные', "domain"='Инфраструктура', "ugns"='', "index"=x[49:57,1], 
                 "variable"=x[49:57,2], "value"=gsub(" ", "", x[49:57,4]), "unit"=x[49:57,3], "threshold"='', "change"='', "last"='', 
                 stringsAsFactors = F)
result <- rbind(result, DF)
#Финансово-экономическая деятельность
DF <- data.frame("id"=id, "group"='Дополнительные', "domain"='Финансово-экономическая деятельность', "ugns"='', "index"=x[59:67,1], 
                 "variable"=x[59:67,2], "value"=gsub(" ", "", x[59:67,4]), "unit"=x[59:67,3], "threshold"='', "change"='', "last"='', 
                 stringsAsFactors = F)
result <- rbind(result, DF)

#---------------Объединение таблиц--------------

micedu <- merge(info, result, by="id")
mon <- rbind(mon, micedu)

    }
    message("Connecting indicators.miccedu.ru/monitoring/ ", paste0('ID = ', id))
  }, error=function(e){cat("ОШИБКА! ",conditionMessage(e), "\n")})
}

########################### 2015 ############ 2015 ###########################

#---------------Форматирование--------------

mon$vuz <- gsub("\r?\n|\r", " ", mon$vuz)
mon$vuz <- gsub('"', "", mon$vuz)
mon$dir <- gsub("\r?\n|\r", " ", mon$dir)
mon$vuz <- gsub("\r?\n|\r", " ", mon$vuz)
mon$dir <- gsub('"', "", mon$dir)
mon$adr <- trimws(gsub("\\|.*", " ", gsub("\r?\n|\r", "", mon$adr)))



########################################## 2013-2014 ##########################################

########################################## 2013 ##########################################
library("XML")
baseurl <- "https://miccedu.ru/monitoring/2013/materials/inst_"

#LOOP VUZ DATA 2013
  for (id in idmon){
    tryCatch({
     
      download.file(paste0(baseurl, id, ".htm"), destfile = "data.html")
      vuz <- readHTMLTable("data.html", stringsAsFactors = FALSE)
      
      if(length(vuz) == "0" ) {stop("ERROR: Образовательная организация не найдена. ID = ", id)}
      else { 


#---------------I. Общие сведения--------------
#1-7
x <- vuz[1][[1]]
Encoding(names(x)) <- "UTF-8"
Encoding(x[,1]) <- "UTF-8"
Encoding(x[,2]) <- "UTF-8"
x <- x[1:(nrow(x)-1),]
x[,2] <- gsub('\r?\r\n', '', x[,2])
names(x) <- gsub('\r?\r\n', '', names(x))

s <- strsplit(as.character(x[1,2]), ',')

info <- data.frame("id"=id, "year"=2013, "vuz"=names(x)[2], "reg"=trimws(gsub('.*[.]', '', gsub('[0-9]+', '', s[[1]][1]))), 
                   "ind"=na.omit(sapply(s, function(x) as.numeric(gsub("[^\\d]+", "", x, perl=TRUE))))[[1]], #индекс во втором элементе
                   "adr"=trimws(paste(s[[1]][2], s[[1]][3], gsub('на карте', '', s[[1]][4]), sep = ",")), 
                   "rec"=x[2][x[1]=='Ректор'], "ved"=x[2][x[1]=='Ведомственная принадлежность'], 
                   "web"=ifelse(length(x[2][x[1]=='web-сайт'])==0, '', x[2][x[1]=='web-сайт']), 
                   "dir"=ifelse(length(x[2][x[1]=='Учредитель(и)'])==0, '', x[2][x[1]=='Учредитель(и)']), 
                   "org"=x[2][x[1]=='Профиль организации'], 
                   "rez"=ifelse(length(x[2][x[1]=='Результат оценки эффективности деятельности'])==0, 
                                '',x[2][x[1]=='Результат оценки эффективности деятельности']), 
                   stringsAsFactors = F)
#x[2][x[1]=='Категория организации'] -- 1 - национальный исследовательский университет
#x[2][x[1]=='Организационно-правовая форма'] -- 1 - автономное учреждение

#x <- vuz[2][[1]] -- Список филиалов
#Encoding(names(x)) <- "UTF-8"
#Encoding(x[,1]) <- "UTF-8"

#---------------II. Сведения по показателям мониторинга эффективности деятельности--------------

result <- data.frame("id"='', "group"='', "domain"='', "ugns"='', "index"='', "variable"='',  "value"='', 
                     "unit"='', "threshold"='', "change"='', "last"='', stringsAsFactors = F)
result <- result[-1,]

#1-6 Основные (E.1-E.8)

x <- vuz[3][[1]]
Encoding(names(x)) <- "UTF-8"
Encoding(x[,2]) <- "UTF-8"
Encoding(x[,3]) <- "UTF-8"
x[,3] <- gsub("[[:space:]]", "", x[,3])

DF <- data.frame("id"=id, "group"='Основные', "domain"='Основные', "ugns"='', "index"=x[,1], "variable"=x[,2], 
                 "value"=gsub("\\*", "", x[,3]), "unit"='', "threshold"=x[,4], "change"='', "last"='', 
                 stringsAsFactors = F)
result <- rbind(result, DF)

#I.1-I.8 Образовательная деятельность (1.1-1.15)
x <- vuz[4][[1]]
Encoding(names(x)) <- "UTF-8"
Encoding(x[,2]) <- "UTF-8"
Encoding(x[,3]) <- "UTF-8"

DF <- data.frame("id"=id, "group"='Основные', "domain"='Образовательная деятельность', "ugns"='', "index"=x[,1], 
                 "variable"=x[,2], "value"=gsub(" ", "", x[,4]), "unit"=x[,3], "threshold"='', "change"='', "last"='', 
                 stringsAsFactors = F)
result <- rbind(result, DF)

#II.1-II.8 Научно-исследовательская деятельность (2.1-2.16)
x <- vuz[5][[1]]
Encoding(names(x)) <- "UTF-8"
Encoding(x[,2]) <- "UTF-8"
Encoding(x[,3]) <- "UTF-8"

DF <- data.frame("id"=id, "group"='Основные', "domain"='Образовательная деятельность', "ugns"='', "index"=x[,1], 
                 "variable"=x[,2], "value"=gsub(" ", "", x[,4]), "unit"=x[,3], "threshold"='', "change"='', "last"='', 
                 stringsAsFactors = F)
result <- rbind(result, DF)

#III.1-III.6 Международная деятельность (3.1-3.13)
x <- vuz[6][[1]]
Encoding(names(x)) <- "UTF-8"
Encoding(x[,2]) <- "UTF-8"
Encoding(x[,3]) <- "UTF-8"

DF <- data.frame("id"=id, "group"='Основные', "domain"='Международная деятельность', "ugns"='', "index"=x[,1], 
                 "variable"=x[,2], "value"=gsub(" ", "", x[,4]), "unit"=x[,3], "threshold"='', "change"='', "last"='', 
                 stringsAsFactors = F)
result <- rbind(result, DF)

#IV.1-IV.4 Финансово-экономическая деятельность (4.1-4.4)
x <- vuz[7][[1]]
Encoding(names(x)) <- "UTF-8"
Encoding(x[,2]) <- "UTF-8"
Encoding(x[,3]) <- "UTF-8"

DF <- data.frame("id"=id, "group"='Основные', "domain"='Финансово-экономическая деятельность', "ugns"='', "index"=x[,1], 
                 "variable"=x[,2], "value"=gsub(" ", "", x[,4]), "unit"=x[,3], "threshold"='', "change"='', "last"='', 
                 stringsAsFactors = F)
result <- rbind(result, DF)

#V.1-V.4 Инфраструктура (5.1-5.8)
x <- vuz[8][[1]]
Encoding(names(x)) <- "UTF-8"
Encoding(x[,2]) <- "UTF-8"
Encoding(x[,3]) <- "UTF-8"

DF <- data.frame("id"=id, "group"='Основные', "domain"='Инфраструктура', "ugns"='', "index"=x[,1], 
                 "variable"=x[,2], "value"=gsub(" ", "", x[,4]), "unit"=x[,3], "threshold"='', "change"='', "last"='', 
                 stringsAsFactors = F)
result <- rbind(result, DF)

#VI.1-VI.3 Трудоустройство (6.1)
x <- vuz[9][[1]]
Encoding(names(x)) <- "UTF-8"
Encoding(x[,2]) <- "UTF-8"
Encoding(x[,3]) <- "UTF-8"

DF <- data.frame("id"=id, "group"='Основные', "domain"='Трудоустройство', "ugns"='', "index"=x[,1], 
                 "variable"=x[,2], "value"=gsub(" ", "", x[,4]), "unit"=x[,3], "threshold"='', "change"='', "last"='', 
                 stringsAsFactors = F)
result <- rbind(result, DF)

# Кадровый состав (7.1-7.5)

#---------------V. Дополнительные характеристики образовательной организации--------------

x <- vuz[11][[1]]
Encoding(names(x)) <- "UTF-8"
Encoding(x[,1]) <- "UTF-8"
Encoding(x[,2]) <- "UTF-8"
Encoding(x[,3]) <- "UTF-8"
Encoding(x[,4]) <- "UTF-8"

x[3,2] <- sub('в том числе:', '', x[3,2])
x[3:6,2] <- paste(x[2,2], x[3:6,2], sep=" ")
#x[3:6,1] <- c('1a', '1b', '1c', '1d')

#Образовательная деятельность
DF <- data.frame("id"=id, "group"='Дополнительные', "domain"='Образовательная деятельность', "ugns"='', "index"=x[2:9,1], 
                 "variable"=x[2:9,2], "value"=gsub(" ", "", x[2:9,4]), "unit"=x[2:9,3], "threshold"='', "change"='', "last"='', 
                 stringsAsFactors = F)
result <- rbind(result, DF)
#Научная деятельность

#Кадровый потенциал
DF <- data.frame("id"=id, "group"='Дополнительные', "domain"='Кадровый потенциал', "ugns"='', "index"=x[10:11,1], 
                 "variable"=x[10:11,2], "value"=gsub(" ", "", x[10:11,4]), "unit"=x[10:11,3], "threshold"='', "change"='', "last"='', 
                 stringsAsFactors = F)
result <- rbind(result, DF)
#Международная деятельность

#Инфраструктура
DF <- data.frame("id"=id, "group"='Дополнительные', "domain"='Инфраструктура', "ugns"='', "index"=x[12:17,1], 
                 "variable"=x[12:17,2], "value"=gsub(" ", "", x[12:17,4]), "unit"=x[12:17,3], "threshold"='', "change"='', "last"='', 
                 stringsAsFactors = F)
result <- rbind(result, DF)
#Финансово-экономическая деятельность


#---------------Объединение таблиц--------------

micedu <- merge(info, result, by="id")
mon <- rbind(mon, micedu)

      }
      message("Connecting indicators.miccedu.ru/monitoring/ ", paste0('ID = ', id, 'из ', which(idmon == id)))
  }, error=function(e){cat("ОШИБКА! ",conditionMessage(e), "\n")})
}


########################################## 2013 ##########################################


########################################## 2014 ##########################################
library("XML")
baseurl <- "https://miccedu.ru/monitoring/2014/materials/inst_"

#LOOP VUZ DATA 2014
for (id in idmon){
  tryCatch({
    
    download.file(paste0(baseurl, id, ".htm"), destfile = "data.html")
    vuz <- readHTMLTable("data.html", stringsAsFactors = FALSE)
    
    if(length(vuz) == "0" ) {stop("ERROR: Образовательная организация не найдена. ID = ", id)}
    else { 
      
      
      #---------------I. Общие сведения--------------
      #1-6
      x <- vuz[1][[1]]
      Encoding(names(x)) <- "UTF-8"
      Encoding(x[,1]) <- "UTF-8"
      Encoding(x[,2]) <- "UTF-8"
      
      s <- strsplit(as.character(x[1,2]), ',')
      
      info <- data.frame("id"=id, "year"=2014, "vuz"=mon$vuz[mon$id == id][1], # не загружаются названия вузов
                         "reg"=trimws(gsub('.*[.]', '', gsub('[0-9]+', '', s[[1]][1]))), 
                         "ind"=sapply(s[[1]][1], function(x) as.numeric(gsub("[^\\d]+", "", x, perl=TRUE)))[[1]], 
                         "adr"=trimws(paste(s[[1]][2], s[[1]][3], gsub('на карте', '', s[[1]][4]), sep = ",")), 
                         "rec"=ifelse(length(x[2][x[1]=='Ректор'])==0, '', x[2][x[1]=='Ректор']), 
                         "ved"=x[2][x[1]=='Ведомственная принадлежность'], 
                         "web"=ifelse(length(x[2][x[1]=='web-сайт'])==0, '', x[2][x[1]=='web-сайт']), 
                         "dir"=ifelse(length(x[2][x[1]=='Учредитель(и)'])==0, '', x[2][x[1]=='Учредитель(и)']), 
                         "org"=x[2][x[1]=='Профиль организации'], 
                         "rez"=ifelse(length(x[2][x[1]=='Результат оценки эффективности деятельности'])==0, 
                                      '',x[2][x[1]=='Результат оценки эффективности деятельности']), 
                         stringsAsFactors = F)
      #x[2][x[1]=='Категория организации'] -- 1 - национальный исследовательский университет
      #x[2][x[1]=='Организационно-правовая форма'] -- 1 - автономное учреждение
      
      #x <- vuz[2][[1]] -- Список филиалов
      #Encoding(names(x)) <- "UTF-8"
      #Encoding(x[,1]) <- "UTF-8"
      
      #---------------II. Сведения по показателям мониторинга эффективности деятельности--------------
      
      result <- data.frame("id"='', "group"='', "domain"='', "ugns"='', "index"='', "variable"='',  "value"='', 
                           "unit"='', "threshold"='', "change"='', "last"='', stringsAsFactors = F)
      result <- result[-1,]
      
      #(E.1-E.8) Основные 
      
      x <- vuz[3][[1]]
      Encoding(names(x)) <- "UTF-8"
      Encoding(x[,2]) <- "UTF-8"
      Encoding(x[,3]) <- "UTF-8"
      x[,3] <- gsub("[[:space:]]", "", x[,3])
      
      DF <- data.frame("id"=id, "group"='Основные', "domain"='Основные', "ugns"='', "index"=x[,1], "variable"=x[,2], 
                       "value"=gsub("\\*", "", x[,3]), "unit"='', "threshold"=x[,4], "change"='', "last"='', 
                       stringsAsFactors = F)
      result <- rbind(result, DF)
      
      #I1.1-I1.11 Образовательная деятельность (1.1-1.15)
      x <- vuz[4][[1]]
      Encoding(names(x)) <- "UTF-8"
      Encoding(x[,2]) <- "UTF-8"
      Encoding(x[,3]) <- "UTF-8"
      
      DF <- data.frame("id"=id, "group"='Основные', "domain"='Образовательная деятельность', "ugns"='', "index"=x[,1], 
                       "variable"=x[,2], "value"=gsub(" ", "", x[,4]), "unit"=x[,3], "threshold"='', "change"='', "last"='', 
                       stringsAsFactors = F)
      result <- rbind(result, DF)
      
      #I2.1-I2.16 Научно-исследовательская деятельность (2.1-2.16)
      x <- vuz[5][[1]]
      Encoding(names(x)) <- "UTF-8"
      Encoding(x[,2]) <- "UTF-8"
      Encoding(x[,3]) <- "UTF-8"
      
      DF <- data.frame("id"=id, "group"='Основные', "domain"='Образовательная деятельность', "ugns"='', "index"=x[,1], 
                       "variable"=x[,2], "value"=gsub(" ", "", x[,4]), "unit"=x[,3], "threshold"='', "change"='', "last"='', 
                       stringsAsFactors = F)
      result <- rbind(result, DF)
      
      #I3.1-I3.12 Международная деятельность (3.1-3.13)
      x <- vuz[6][[1]]
      Encoding(names(x)) <- "UTF-8"
      Encoding(x[,2]) <- "UTF-8"
      Encoding(x[,3]) <- "UTF-8"
      
      DF <- data.frame("id"=id, "group"='Основные', "domain"='Международная деятельность', "ugns"='', "index"=x[,1], 
                       "variable"=x[,2], "value"=gsub(" ", "", x[,4]), "unit"=x[,3], "threshold"='', "change"='', "last"='', 
                       stringsAsFactors = F)
      result <- rbind(result, DF)
      
      #I4.1-I4.3 Финансово-экономическая деятельность (4.1-4.4)
      x <- vuz[7][[1]]
      Encoding(names(x)) <- "UTF-8"
      Encoding(x[,2]) <- "UTF-8"
      Encoding(x[,3]) <- "UTF-8"
      
      DF <- data.frame("id"=id, "group"='Основные', "domain"='Финансово-экономическая деятельность', "ugns"='', "index"=x[,1], 
                       "variable"=x[,2], "value"=gsub(" ", "", x[,4]), "unit"=x[,3], "threshold"='', "change"='', "last"='', 
                       stringsAsFactors = F)
      result <- rbind(result, DF)
      
      #I5.1-I5.4 Инфраструктура (5.1-5.8)
      x <- vuz[8][[1]]
      Encoding(names(x)) <- "UTF-8"
      Encoding(x[,2]) <- "UTF-8"
      Encoding(x[,3]) <- "UTF-8"
      
      x[2:5,2] <- paste(sub('в том числе:', '', x[1,2]), x[2:5,2], sep=" ")
      
      DF <- data.frame("id"=id, "group"='Основные', "domain"='Инфраструктура', "ugns"='', "index"=x[,1], 
                       "variable"=x[,2], "value"=gsub(" ", "", x[,4]), "unit"=x[,3], "threshold"='', "change"='', "last"='', 
                       stringsAsFactors = F)
      result <- rbind(result, DF)
      
      #I6.1-I6.3 Трудоустройство (6.1)
      x <- vuz[9][[1]]
      Encoding(names(x)) <- "UTF-8"
      Encoding(x[,2]) <- "UTF-8"
      Encoding(x[,3]) <- "UTF-8"
      
      DF <- data.frame("id"=id, "group"='Основные', "domain"='Трудоустройство', "ugns"='', "index"=x[,1], 
                       "variable"=x[,2], "value"=gsub(" ", "", x[,4]), "unit"=x[,3], "threshold"='', "change"='', "last"='', 
                       stringsAsFactors = F)
      result <- rbind(result, DF)
      
      #I9.1-I9.5 Кадровый состав (7.1-7.5)
      x <- vuz[10][[1]]
      Encoding(names(x)) <- "UTF-8"
      Encoding(x[,2]) <- "UTF-8"
      Encoding(x[,3]) <- "UTF-8"
      
      #---------------V. Дополнительные характеристики образовательной организации--------------
      #1-12
      x <- vuz[12][[1]]
      Encoding(names(x)) <- "UTF-8"
      Encoding(x[,1]) <- "UTF-8"
      Encoding(x[,2]) <- "UTF-8"
      Encoding(x[,3]) <- "UTF-8"
      Encoding(x[,4]) <- "UTF-8"
      
      x[3,2] <- sub('в том числе:', '', x[3,2])
      x[3:6,2] <- paste(x[2,2], x[3:6,2], sep=" ")
      x[,2] <- gsub('\r?\r\n', '', x[,2])
      x[,2] <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", x[,2], perl=TRUE)
      #x[3:6,1] <- c('1a', '1b', '1c', '1d')
      
      #Образовательная деятельность
      DF <- data.frame("id"=id, "group"='Дополнительные', "domain"='Образовательная деятельность', "ugns"='', "index"=x[2:9,1], 
                       "variable"=x[2:9,2], "value"=gsub(" ", "", x[2:9,4]), "unit"=x[2:9,3], "threshold"='', "change"='', "last"='', 
                       stringsAsFactors = F)
      result <- rbind(result, DF)
      #Научная деятельность
      
      #Кадровый потенциал
      DF <- data.frame("id"=id, "group"='Дополнительные', "domain"='Кадровый потенциал', "ugns"='', "index"=x[10:11,1], 
                       "variable"=x[10:11,2], "value"=gsub(" ", "", x[10:11,4]), "unit"=x[10:11,3], "threshold"='', "change"='', "last"='', 
                       stringsAsFactors = F)
      result <- rbind(result, DF)
      #Международная деятельность
      
      #Инфраструктура
      DF <- data.frame("id"=id, "group"='Дополнительные', "domain"='Инфраструктура', "ugns"='', "index"=x[12:17,1], 
                       "variable"=x[12:17,2], "value"=gsub(" ", "", x[12:17,4]), "unit"=x[12:17,3], "threshold"='', "change"='', "last"='', 
                       stringsAsFactors = F)
      result <- rbind(result, DF)
      #Финансово-экономическая деятельность
      
      #---------------Объединение таблиц--------------
      
      micedu <- merge(info, result, by="id")
      mon <- rbind(mon, micedu)
      
    }
    message("Connecting indicators.miccedu.ru/monitoring/ ", paste0('ID = ', id, ' ~ FROM ', which(idmon == id)))
  }, error=function(e){cat("ОШИБКА! ",conditionMessage(e), "\n")})
}


########################################## 2014 ##########################################¦

#---------------Форматирование--------------
mon$variable <- gsub('\r?\r\n', '', mon$variable)
mon$value <- gsub('\r?\r\n', '', mon$value)
mon$variable <- gsub("\r?\n|\r", " ", mon$variable)
mon$variable <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", mon$variable, perl=TRUE)
mon$variable <- gsub('- ', '-', mon$variable)
#
mon$vuz <- gsub("\r?\n|\r", " ", mon$vuz)
mon$vuz <- gsub('"', "", mon$vuz)
mon$dir <- gsub("\r?\n|\r", " ", mon$dir)
mon$vuz <- gsub("\r?\n|\r", " ", mon$vuz)
mon$dir <- gsub('"', "", mon$dir)
mon$reg <- gsub('\r?\r\n', '', mon$reg)
mon$org <- gsub('\r?\r\n', '', mon$org)
mon$ved <- gsub('\r?\r\n', '', mon$ved)
mon$adr <- trimws(gsub("\\|.*", " ", gsub("\r?\n|\r", "", mon$adr)))
#0/1
#length(mon$value[mon$value == 'нет'])
mon$value <- gsub('да', '1', mon$value)
mon$value <- gsub('нет', '0', mon$value)
#Encoding
Encoding(mon$value) <- "UTF-8"

########################################## 2013-2014 ##########################################


#################### V5100 | НИУ | ВУ #######################
#5-100 (21)
mon$V5100 <- ''
mon$V5100[mon$id %in% c('59','161','165','170','178','217','222','234','240','247','251','294','296','302','313','336','1507','1519','1520','1766','1792')] <- '5-100'
#НИУ (29)
mon$Vniu <- ''
mon$Vniu[mon$id %in% c('17','59','82','87','88','127','131','140','144','147','161','162','165','170','178','197','198','219','234','237','240','251','258','294','296','336','1595','1766','1785')] <- 'НИУ'
#Ведущие Университеты (ВУ) (45)
mon$Vvu <- ''
mon$Vvu[mon$id %in% c('17','59','82','87','88','127','131','140','144','147','161','162','165','170','178','197','198','217','219','222','234','237','240','247','251','258','266','294','296','302','313','336','337','1507','1519','1520','1527','1544','1595','1725','1766','1771','1785','1792','113416')] <- 'ВУ'
#################### V5100 | НИУ | ВУ #######################


########################################## NEW ID ##########################################

#---------------Форматирование--------------
mon$variable <- gsub(' +', ' ', mon$variable)
mon$variable <- gsub('НП??????', 'НПР', mon$variable)
mon$variable <- gsub('НИОК??????', 'НИОКР', mon$variable)
mon$variable <- gsub(', в том числе:', '', mon$variable)
mon$variable <- gsub(',в том числе:', '', mon$variable)

#Унификация названий показателей для всех периодов
mon$variable[mon$variable == 'Cредний балл ЕГЭ студентов, принятых на обучение по программам бакалавриата и специалитета, по всем формам обучения'] <- 'Средний балл ЕГЭ студентов, принятых на обучение по программам бакалавриата и специалитета, по всем формам обучения'
mon$variable[mon$variable == 'Доля аспирантов, обучающихся в очной форме'] <- 'Доля аспирантов (адъюнктов), интернов, ординаторов, ассистентов-стажеров, обучающихся в очной форме'
mon$variable[mon$variable == 'Доля нетрудоустроенных в течение года выпускников очной формы обучения, из числа обратившихся за содействием в трудоустройстве'] <- 'Удельный вес нетрудоустроенных в течение года выпускников отчетного года очной формы обучения, из числа обратившихся за содействием в трудоустройстве'
mon$variable[mon$variable == 'Доля стоимости современных (не старше 5 лет) машин и оборудования в вузе в общей стоимости машин и оборудования'] <- 'Удельный вес стоимости машин и оборудования (не старше 5 лет) в общей стоимости машин и оборудования'
mon$variable[mon$variable == 'Удельный вес стоимости машин и оборудования (не старше 5 лет) вуза в общей стоимости машин и оборудования'] <- 'Удельный вес стоимости машин и оборудования (не старше 5 лет) в общей стоимости машин и оборудования'
mon$variable[mon$variable == 'Доходы вуза из средств от приносящей доход деятельности в расчете на одного НПР'] <- 'Доходы образовательной организации из средств от приносящей доход деятельности в расчете на одного НПР'
mon$variable[mon$variable == 'Доходы вуза из всех источников в расчете на численность студентов (приведенный контингент)'] <- 'Доходы образовательной организации из всех источников в расчете на численность студентов (приведенный контингент)'
mon$variable[mon$variable == 'Количество научных журналов, в том числе электронных, издаваемых вузом'] <- 'Количество научных журналов, в том числе электронных, издаваемых образовательной организацией'
mon$variable[mon$variable == 'Количество статей в Scopus, в расчете на 100 НПР'] <- 'Число публикаций организации, индексируемых в информационно-аналитической системе научного цитирования Scopus, в расчете на 100 НПР'
mon$variable[mon$variable == 'Количество статей в Web of Science, в расчете на 100 НПР'] <- 'Число публикаций организации, индексируемых в информационно-аналитической системе научного цитирования Web of Science, в расчете на 100 НПР'
mon$variable[mon$variable == 'Количество статей в РИНЦ, в расчете на 100 НПР'] <- 'Число публикаций организации, индексируемых в информационно-аналитической системе научного цитирования РИНЦ, в расчете на 100 НПР'
mon$variable[mon$variable == 'Количество цитирований статей в индексируемой системе цитирования Scopus в расчете на 100 НПР'] <- 'Количество цитирований публикаций, изданных за последние 5 лет, индексируемых в информационно-аналитической системе научного цитирования Scopus в расчете на 100 НПР'
mon$variable[mon$variable == 'Количество цитирований статей в индексируемой системе цитирования Web of Science в расчете на 100 НПР'] <- 'Количество цитирований публикаций, изданных за последние 5 лет, индексируемых в информационно-аналитической системе научного цитирования Web of Science в расчете на 100 НПР'
mon$variable[mon$variable == 'Количество цитирований статей в Российском индексе научного цитирования (далее – РИНЦ) в расчете на 100 НПР'] <- 'Количество цитирований публикаций, изданных за последние 5 лет, индексируемых в Российском индексе научного цитирования (далее – РИНЦ) в расчете на 100 НПР'
mon$variable[mon$variable == 'Количество экземпляров учебной и учебно-методической литературы из общего количества единиц хранения библиотечного фонда, состоящих на учете, в расчете на одного студента (приведенного контингента)'] <- 'Количество экземпляров печатных учебных изданий (включая учебники и учебные пособия) из общего количества единиц хранения библиотечного фонда, состоящих на учете, в расчете на одного студента (приведенного контингента)'
mon$variable[mon$variable == 'Общая площадь учебно-лабораторных помещений в расчете на одного студента (приведенного контингента), закрепленных за вузом на праве оперативного управления'] <- 'Общая площадь учебно-лабораторных помещений в расчете на одного студента (приведенного контингента), закрепленных на праве оперативного управления'
mon$variable[mon$variable == 'Общая площадь учебно-лабораторных помещений в расчете на одного студента (приведенного контингента), имеющихся у вуза на праве собственности'] <- 'Общая площадь учебно-лабораторных помещений в расчете на одного студента (приведенного контингента), имеющихся на праве собственности'
mon$variable[mon$variable == 'Общая площадь учебно-лабораторных помещений в расчете на одного студента (приведенного контингента), предоставленных вузу в аренду'] <- 'Общая площадь учебно-лабораторных помещений в расчете на одного студента (приведенного контингента), предоставленных в аренду'
mon$variable[mon$variable == 'Общая площадь учебно-лабораторных помещений в расчете на одного студента (приведенного контингента), предоставленных вузу в безвозмездное пользование'] <- 'Общая площадь учебно-лабораторных помещений в расчете на одного студента (приведенного контингента), предоставленных в безвозмездное пользование'
mon$variable[mon$variable == 'Общая численность аспирантов'] <- 'Общая численность аспирантов (адъюнктов), интернов, ординаторов, ассистентов-стажеров'
mon$variable[mon$variable == 'Общая численность иностранных аспирантов'] <- 'Общая численность иностранных аспирантов (адъюнктов), интернов, ординаторов, ассистентов-стажеров'
mon$variable[mon$variable == 'Общая численность обучающихся по программам ВПО'] <- 'Общая численность студентов, обучающихся по программам бакалавриата, специалитета, магистратуры'
mon$variable[mon$variable == 'Общая численность обучающихся по программам ВПО по заочной форме обучения'] <- 'Общая численность студентов, обучающихся по программам бакалавриата, специалитета, магистратуры по заочной форме обучения'
mon$variable[mon$variable == 'Общая численность обучающихся по программам ВПО по очно-заочной (вечерней) форме обучения'] <- 'Общая численность студентов, обучающихся по программам бакалавриата, специалитета, магистратуры по очно-заочной (вечерней) форме обучения'
mon$variable[mon$variable == 'Общая численность обучающихся по программам ВПО по очной форме обучения'] <- 'Общая численность студентов, обучающихся по программам бакалавриата, специалитета, магистратуры по очной форме обучения'
mon$variable[mon$variable == 'Общая численность обучающихся по программам ВПО экстернат'] <- 'Общая численность студентов, обучающихся по программам бакалавриата, специалитета, магистратуры экстернат'
mon$variable[mon$variable == 'Общая численность работников образовательного учреждения (без внешних совместителей и работающих по договорам ГПХ)'] <- 'Общая численность работников образовательной организации (без внешних совместителей и работающих по договорам ГПХ)'
mon$variable[mon$variable == 'Общий объем научно-исследовательских и опытно-конструкторских работ (далее -НИОКР)'] <- 'Общий объем научно-исследовательских и опытно-конструкторских работ (далее – НИОКР)'
mon$variable[mon$variable == 'Общий объем НИОКР, выполненных собственными силами'] <- 'Общий объем средств, поступивших (за отчетный год) от выполнения НИОКР, выполненных собственными силами'
mon$variable[mon$variable == 'Общий объем работ, услуг, связанных с научными, научно-техническими, творческими услугами и разработками, выполненных собственными силами'] <- 'Общий объем средств, поступивших (за отчетный год) от выполнения работ, услуг, связанных с научными, научно-техническими, творческими услугами и разработками, выполненных собственными силами'
mon$variable[mon$variable == 'Объем средств от образовательной деятельности, полученных вузом от иностранных граждан и иностранных юридических лиц'] <- 'Объем средств от образовательной деятельности, полученных образовательной организацией от иностранных граждан и иностранных юридических лиц'
mon$variable[mon$variable == 'Объем средств, полученных вузом на выполнение НИОКР от иностранных граждан и иностранных юридических лиц'] <- 'Объем средств, полученных образовательной организацией от выполнения НИОКР от иностранных граждан и иностранных юридических лиц'
mon$variable[mon$variable == 'Объем средств, полученных образовательной организацией на выполнение НИОКР от иностранных граждан и иностранных юридических лиц'] <- 'Объем средств, полученных образовательной организацией от выполнения НИОКР от иностранных граждан и иностранных юридических лиц'
mon$variable[mon$variable == 'Удельный вес выпускников 2012 года очной формы обучения, обратившихся за содействием в поиске подходящей работы'] <- 'Удельный вес выпускников отчетного года очной формы обучения, обратившихся за содействием в поиске подходящей работы в органы содействия в трудоустройстве'
mon$variable[mon$variable == 'Удельный вес выпускников 2012 года очной формы обучения, обратившихся за содействием в поиске подходящей работы и признанных безработными'] <- 'Удельный вес выпускников отчетного года очной формы обучения, обратившихся за содействием в поиске подходящей работы в органы содействия в трудоустройстве и признанных безработными'
mon$variable[mon$variable == 'Удельный вес научно-педагогических работников, защитивших кандидатские и докторские диссертации за отчетный год в общей численности НПР'] <- 'Удельный вес научно-педагогических работников, защитивших кандидатские и докторские диссертации за отчетный период в общей численности НПР'
mon$variable[mon$variable == 'Удельный вес средств, полученных вузом от управления объектами интеллектуальной собственности, в общих доходах вуза'] <- 'Удельный вес средств, полученных образовательной организацией от использования результатов интеллектуальной деятельности, в общих доходах образовательной организации'
mon$variable[mon$variable == 'Удельный вес численности иностранных граждан (кроме стран СНГ) из числа аспирантов вуза в общей численности аспирантов'] <- 'Удельный вес численности иностранных граждан (кроме стран СНГ) из числа аспирантов (адъюнктов), ординаторов, интернов, ассистентов-стажеров образовательной организации в общей численности аспирантов (адъюнктов), ординаторов, интернов, ассистентов-стажеров'
mon$variable[mon$variable == 'Удельный вес численности иностранных граждан (кроме стран СНГ) из числа аспирантов образовательной организации в общей численности аспирантов'] <- 'Удельный вес численности иностранных граждан (кроме стран СНГ) из числа аспирантов (адъюнктов), ординаторов, интернов, ассистентов-стажеров образовательной организации в общей численности аспирантов (адъюнктов), ординаторов, интернов, ассистентов-стажеров'
mon$variable[mon$variable == 'Удельный вес численности иностранных граждан из стран СНГ из числа аспирантов вуза в общей численности аспирантов'] <- 'Удельный вес численности иностранных граждан из стран СНГ из числа аспирантов (адъюнктов), ординаторов, интернов, ассистентов-стажеров образовательной организации в общей численности аспирантов (адъюнктов), ординаторов, интернов, ассистентов-стажеров'
mon$variable[mon$variable == 'Удельный вес численности иностранных граждан из стран СНГ из числа аспирантов образовательной организации в общей численности аспирантов'] <- 'Удельный вес численности иностранных граждан из стран СНГ из числа аспирантов (адъюнктов), ординаторов, интернов, ассистентов-стажеров образовательной организации в общей численности аспирантов (адъюнктов), ординаторов, интернов, ассистентов-стажеров'
mon$variable[mon$variable == 'Удельный вес численности иностранных студентов (кроме стран СНГ), обучающихся программам бакалавриата, специалитета, магистратуры, в общей численности студентов (приведенный контингент)'] <- 'Удельный вес численности иностранных студентов (кроме стран Содружества Независимых Государств (далее – СНГ)), обучающихся программам бакалавриата, специалитета, магистратуры, в общей численности студентов (приведенный контингент)'
mon$variable[mon$variable == 'Удельный вес численности молодых ученых (без ученой степени – до 30 лет, кандидаты наук – до 35 лет, доктора наук – до 40 лет) в общей численности НПР'] <- 'Удельный вес численности НПР без ученой степени – до 30 лет, кандидатов наук – до 35 лет, докторов наук – до 40 лет, в общей численности НПР'
mon$variable[mon$variable == 'Удельный вес численности НПР без ученой степени -до 30 лет, кандидатов наук -до 35 лет, докторов наук-до 40 лет, в общей численности НПР'] <- 'Удельный вес численности НПР без ученой степени – до 30 лет, кандидатов наук – до 35 лет, докторов наук – до 40 лет, в общей численности НПР'
mon$variable[mon$variable == 'Удельный вес численности обучающихся (приведенного контингента), по программам магистратуры и подготовки научно-педагогических кадров в аспирантуре в общей численности приведенного контингента обучающихся по основным образовательным программам высшего образования'] <- 'Удельный вес численности обучающихся (приведенного контингента), по программам магистратуры, подготовки научно-педагогических кадров в аспирантуре (адъюнктуре), ординатуры, интернатуры, ассистентуры-стажировки в общей численности приведенного контингента обучающихся по основным образовательным программам высшего образования'
mon$variable[mon$variable == 'Удельный вес численности обучающихся по программам магистратуры и подготовки научно-педагогических кадров в аспирантуре, имеющих диплом бакалавра, диплом специалиста или диплом магистра других организаций в общей численности обучающихся по программам магистратуры и подготовки научно-педагогических кадров в аспирантуре'] <- 'Удельный вес численности обучающихся по программам магистратуры, подготовки научно-педагогических кадров в аспирантуре (адъюнктуре), ординатуры, ассистентуры-стажировки, имеющих диплом бакалавра, диплом специалиста или диплом магистра других организаций в общей численности обучающихся по программам магистратуры, подготовки научно-педагогических кадров в аспирантуре (адъюнктуре), ординатуры, ассистентуры-стажировки'
mon$variable[mon$variable == 'Удельный вес численности студентов (приведенного контингента), обучающихся по программам магистратуры, в общей численности приведенного контингента студентов'] <- 'Удельный вес численности студентов (приведенного контингента), обучающихся по программам магистратуры, в общей численности приведенного контингента обучающихся по образовательным программам бакалавриата, специалитета и магистратуры'
mon$variable[mon$variable == 'Удельный вес численности студентов вуза, обучающихся по очной форме обучения по образовательным программам бакалавриата, программам специалитета, программам магистратуры, прошедших обучение за рубежом не менее семестра (триместра), в общей численности студентов, обучающихся по очной форме обучения'] <- 'Удельный вес численности студентов, обучающихся по очной форме обучения по образовательным программам бакалавриата, программам специалитета, программам магистратуры, прошедших обучение за рубежом не менее семестра (триместра), в общей численности студентов, обучающихся по очной форме обучения'
mon$variable[mon$variable == 'Удельный вес численности студентов, принятых на условиях целевого приема на первый курс на очную форму обучения по программам бакалавриата и специалитета в общей численности студентов'] <- 'Удельный вес численности студентов, принятых по результатам целевого приема на первый курс на очную форму обучения по программам бакалавриата и специалитета в общей численности студентов, принятых на первый курс по программам бакалавриата и специалитета на очную форму обучения'
mon$variable[mon$variable == 'Численность студентов иностранных образовательных организаций, прошедших обучение в вузе по образовательным по очной форме обучения по образовательным программам бакалавриата, программам специалитета, программам магистратуры, не менее семестра (триместра) в расчете на 100 студентов, обучающихся по очной форме обучения'] <- 'Численность студентов иностранных образовательных организаций, прошедших обучение в образовательной организации по образовательным по очной форме обучения по образовательным программам бакалавриата, программам специалитета, программам магистратуры, не менее семестра (триместра) в расчете на 100 студентов, обучающихся по очной форме обучения'
mon$variable[mon$variable == 'Численность студентов, победителей и призеров заключительного этапа всероссийской олимпиады школьников, членов сборных команд РФ, учувствовавших в международных олимпиадах, принятых на очную форму обучения на первый курс по программам бакалавриата и специалитета без вступительных испытаний'] <- 'Численность студентов, победителей и призеров заключительного этапа всероссийской олимпиады школьников, членов сборных команд Российской Федерации, участвовавших в международных олимпиадах по общеобразовательным предметам по специальностям и (или) направлениям подготовки, соответствующим профилю всероссийской олимпиады школьников или международной олимпиады, принятых на очную форму обучения на первый курс по программам бакалавриата и специалитета без вступительных испытаний'
mon$variable[mon$variable == 'Численность студентов, принятых на условиях целевого приема на первый курс на очную форму обучения по программам бакалавриата и специалитета'] <- 'Численность студентов, принятых по результатам целевого приема на первый курс на очную форму обучения по программам бакалавриата и специалитета'
#mon$variable[mon$variable == ''] <- ''

#Reshape
monrr <- mon[, !colnames(mon) %in% c('group', 'domain', 'ugns', 'index', 'unit', 'threshold', 'change', 'last')]
  ##Филиалы (-4)
  monrr <- monrr[!monrr$id %in% c('1573', '1809', '1811', '1813'), ]
monrr <- reshape(monrr, idvar = c('id', 'year', 'V5100', 'Vniu', 'Vvu', 'vuz', 'reg', 'ind', 'adr', 'rec', 'ved', 'web', 'dir', 'org', 'rez'), timevar = "variable", direction = "wide")
names(monrr) <- gsub('value.', '', names(monrr))
write.table(monrr, file="D:/R/Monitoring/monitoring.reshape.csv", row.names = F, col.names = T, sep = ";")

########################################## NEW ID ##########################################



########################################## SAVE ##########################################

#SPSS
monrr <- data.frame('id' = mon$id, 'yvariable' = paste(mon$year, mon$variable, sep = '.'), 'value' = mon$value)
  ##Филиалы (-4)
  monrr <- monrr[!monrr$id %in% c('1573', '1809', '1811', '1813'), ]
monrr <- reshape(monrr, idvar = "id", timevar = "yvariable", direction = "wide")
names(monrr) <- gsub('value.', '', names(monrr))
write.table(monrr, file="D:/R/Monitoring/monitoring.spss.csv", row.names = F, col.names = T, sep = ";")
#SPSS

#SAVE
  ##Филиалы (-4)
  mon <- mon[!mon$id %in% c('1573', '1809', '1811', '1813'), ]
write.table(mon, file="D:/R/Monitoring/monitoring.csv", row.names = F, col.names = T, sep = ";", fileEncoding = "UTF-8")

saveRDS(mon, file = "D:/R/Monitoring/mon.Rds")
saveRDS(v, file = "D:/R/Monitoring/v.Rds")

########################################## SAVE ##########################################


#################### Vgroup 2017 ########################################### Vgroup 2017 #######################
options(scipen=999)
#Качество студентов ------------------------------------->
##5-100
x <- mon[mon$V5100 == '5-100' & mon$year == '2017' & mon$index %in% c('E.1','1.1','1.4','1.5','1.6'), 
           c('id','year','index','variable','value', 'unit')]
x$value <- as.numeric(sub(",", ".", x$value))
v <- aggregate(x$value, by = list(index=x$index, variable=x$variable, unit=x$unit), median)
##НИУ
x <- mon[mon$Vniu == 'НИУ' & mon$year == '2017' & mon$index %in% c('E.1','1.1','1.4','1.5','1.6'), 
           c('id','year','index','variable','value')]
x$value <- as.numeric(sub(",", ".", x$value))
v <- merge(v, aggregate(x$value, by = list(index=x$index), median), by = "index")
##ВУ
x <- mon[mon$Vvu == 'ВУ' & mon$year == '2017' & mon$index %in% c('E.1','1.1','1.4','1.5','1.6'), 
           c('id','year','index','variable','value')]
x$value <- as.numeric(sub(",", ".", x$value))
v <- merge(v, aggregate(x$value, by = list(index=x$index), median), by = "index")
##ВШЭ
v <- merge(v, x[x$id == '1766',c('index','value')], by = "index")
##Таблица
colnames(v) <- c('index', 'variable', 'unit', 'medianV5100', 'medianVniu', 'medianVvu', 'hse')
x <- data.frame("index"=c('E.1','1.1','1.4','1.5','1.6'), "Vname"=c('Средний балл ЕГЭ (бюджет/внебюджет)',
                                                                    'Средний балл ЕГЭ (бюджет)', 'Минимальный балл ЕГЭ (бюджет/внебюджет)',
                                                                    'Победители Всероссийской олимпиады в приеме, чел.', 'Победители олимпиад в приеме, чел.') , stringsAsFactors = F)
v <- merge(v, x, by = "index")
v$Vgroup <- 'Качество студентов'

#Контингент -------------------------------------> 
##5-100
x <- mon[mon$V5100 == '5-100' & mon$year == '2017' & mon$index %in% c('1.12','1.13','1.11','3','6'), 
           c('id','year','index','variable','value', 'unit')]
x$value <- as.numeric(sub(",", ".", x$value))
x$value[x$index == '6'] <- x$value[x$index == '6']/1000
DF <- aggregate(x$value, by = list(index=x$index, variable=x$variable, unit=x$unit), median)
##НИУ
x <- mon[mon$Vniu == 'НИУ' & mon$year == '2017' & mon$index %in% c('1.12','1.13','1.11','3','6'), 
           c('id','year','index','variable','value')]
x$value <- as.numeric(sub(",", ".", x$value))
x$value[x$index == '6'] <- x$value[x$index == '6']/1000
DF <- merge(DF, aggregate(x$value, by = list(index=x$index), median), by = "index")
##ВУ
x <- mon[mon$Vvu == 'ВУ' & mon$year == '2017' & mon$index %in% c('1.12','1.13','1.11','3','6'), 
           c('id','year','index','variable','value')]
x$value <- as.numeric(sub(",", ".", x$value))
x$value[x$index == '6'] <- x$value[x$index == '6']/1000
DF <- merge(DF, aggregate(x$value, by = list(index=x$index), median), by = "index")
##ВШЭ
DF <- merge(DF, x[x$id == '1766',c('index','value')], by = "index")
##Таблица
colnames(DF) <- c('index', 'variable', 'unit', 'medianV5100', 'medianVniu', 'medianVvu', 'hse')
x <- data.frame("index"=c('1.12','1.13','1.11','3','6'), "Vname"=c('Доля магистрантов и аспирантов, %',
                                                                   'Число аспирантов на 100 студентов, чел.', 'Доля магистрантов и аспирантов из других вузов, %',
                                                                   'Доля студентов очной формы, %', 'Слушатели ДПО, тыс. чел.'), stringsAsFactors = F)
DF <- merge(DF, x, by = "index")
DF$Vgroup <- 'Контингент'
v <- rbind(v, DF)

#НИОКР -------------------------------------> *
##5-100
x <- mon[mon$V5100 == '5-100' & mon$year == '2017' & mon$index %in% c('E.2', '2.10','2.7','2.8','2.16'), 
           c('id','year','index','variable','value', 'unit')]
x$value <- as.numeric(sub(",", ".", x$value))
x$value[x$index == '2.7'] <- x$value[x$index == '2.7']/1000
DF <- aggregate(x$value, by = list(index=x$index, variable=x$variable, unit=x$unit), median)
##НИУ
x <- mon[mon$Vniu == 'НИУ' & mon$year == '2017' & mon$index %in% c('E.2', '2.10','2.7','2.8','2.16'), 
           c('id','year','index','variable','value')]
x$value <- as.numeric(sub(",", ".", x$value))
x$value[x$index == '2.7'] <- x$value[x$index == '2.7']/1000
DF <- merge(DF, aggregate(x$value, by = list(index=x$index), median), by = "index")
##ВУ
x <- mon[mon$Vvu == 'ВУ' & mon$year == '2017' & mon$index %in% c('E.2', '2.10','2.7','2.8','2.16'), 
           c('id','year','index','variable','value')]
x$value <- as.numeric(sub(",", ".", x$value))
x$value[x$index == '2.7'] <- x$value[x$index == '2.7']/1000
DF <- merge(DF, aggregate(x$value, by = list(index=x$index), median), by = "index")
##ВШЭ
DF <- merge(DF, x[x$id == '1766',c('index','value')], by = "index")
##Таблица
colnames(DF) <- c('index', 'variable', 'unit', 'medianV5100', 'medianVniu', 'medianVvu', 'hse')
x <- data.frame("index"=c('E.2', '2.10','2.7','2.8','2.16'), "Vname"=c('Объем НИОКР на 1 НПР, тыс. руб.', 'Внебюджетные доходы от НИОКР на 1 НПР, тыс. руб.',
                                                                       'Общий объем НИОКР, млн руб.', 'Доля доходов от НИОКР в общих доходах, %', 'Количество грантов на 100 НПР, ед.'), 
                stringsAsFactors = F)
DF <- merge(DF, x, by = "index")
DF$Vgroup <- 'НИОКР'
v <- rbind(v, DF)

##Расчетный показатель !! >>

#Публикации -------------------------------------> 
##5-100
x <- mon[mon$V5100 == '5-100' & mon$year == '2017' & mon$index %in% c('2.4','2.5','2.6','2.1','2.2'), 
           c('id','year','index','variable','value', 'unit')]
x$value <- as.numeric(sub(",", ".", x$value))
DF <- aggregate(x$value, by = list(index=x$index, variable=x$variable, unit=x$unit), median)
##НИУ
x <- mon[mon$Vniu == 'НИУ' & mon$year == '2017' & mon$index %in% c('2.4','2.5','2.6','2.1','2.2'), 
           c('id','year','index','variable','value')]
x$value <- as.numeric(sub(",", ".", x$value))
DF <- merge(DF, aggregate(x$value, by = list(index=x$index), median), by = "index")
##ВУ
x <- mon[mon$Vvu == 'ВУ' & mon$year == '2017' & mon$index %in% c('2.4','2.5','2.6','2.1','2.2'), 
           c('id','year','index','variable','value')]
x$value <- as.numeric(sub(",", ".", x$value))
DF <- merge(DF, aggregate(x$value, by = list(index=x$index), median), by = "index")
##ВШЭ
DF <- merge(DF, x[x$id == '1766',c('index','value')], by = "index")
##Таблица
colnames(DF) <- c('index', 'variable', 'unit', 'medianV5100', 'medianVniu', 'medianVvu', 'hse')
x <- data.frame("index"=c('2.4','2.5','2.6','2.1','2.2'), "Vname"=c('Статьи в WoS за год на 100 НПР, ед.',
                                                                    'Статьи в Scopus за год на 100 НПР, ед.', 'Статьи за год в РИНЦ на 100 НПР, ед.', 
                                                                    'Цитирования в WoS за 5 лет на 100 НПР, ед.', 'Цитирования в Scopus за 5 лет на 100 НПР, ед.'), stringsAsFactors = F)
DF <- merge(DF, x, by = "index")
DF$Vgroup <- 'Публикации'
v <- rbind(v, DF)

#Интернационализация -------------------------------------> 
##5-100
x <- mon[mon$V5100 == '5-100' & mon$year == '2017' & mon$index %in% c('E.3','3.1','3.6','3.7'), 
           c('id','year','index','variable','value', 'unit')]
x$value <- as.numeric(sub(",", ".", x$value))
DF <- aggregate(x$value, by = list(index=x$index, variable=x$variable, unit=x$unit), median)
##НИУ
x <- mon[mon$Vniu == 'НИУ' & mon$year == '2017' & mon$index %in% c('E.3','3.1','3.6','3.7'), 
           c('id','year','index','variable','value')]
x$value <- as.numeric(sub(",", ".", x$value))
DF <- merge(DF, aggregate(x$value, by = list(index=x$index), median), by = "index")
##ВУ
x <- mon[mon$Vvu == 'ВУ' & mon$year == '2017' & mon$index %in% c('E.3','3.1','3.6','3.7'), 
           c('id','year','index','variable','value')]
x$value <- as.numeric(sub(",", ".", x$value))
DF <- merge(DF, aggregate(x$value, by = list(index=x$index), median), by = "index")
##ВШЭ
DF <- merge(DF, x[x$id == '1766',c('index','value')], by = "index")
##Таблица
colnames(DF) <- c('index', 'variable', 'unit', 'medianV5100', 'medianVniu', 'medianVvu', 'hse')
x <- data.frame("index"=c('E.3','3.1','3.6','3.7'), "Vname"=c('Доля иностранных студентов, %',
                                                              'Доля иностранных студентов (кроме СНГ), %', 'Студенты-очники, обучавшиеся за рубежом, %', 
                                                              'Иностранные студенты-очники, обучавшиеся не менее триместра, на 100 студентов, чел.'), stringsAsFactors = F)
DF <- merge(DF, x, by = "index")
DF$Vgroup <- 'Интернационализация'
v <- rbind(v, DF)

##Расчетный показатель !! >>
v[nrow(v)+1,] <- NA
##5-100
data <- mon[mon$V5100 == '5-100' & mon$year == '2017' & mon$index %in% c('3.10','3.11'), 
              c('id','year','index','variable','value')]
data$value <- as.numeric(sub(",", ".", data$value))
x <- aggregate(value ~ id, data=data, FUN=function(x) x[data$index == '3.10']+x[data$index == '3.11'])
x$value <- x$value[,1]
v$medianV5100[nrow(v)] <- median(x$value)
##НИУ
data <- mon[mon$Vniu == 'НИУ' & mon$year == '2017' & mon$index %in% c('3.10','3.11'), 
              c('id','year','index','variable','value')]
data$value <- as.numeric(sub(",", ".", data$value))
x <- aggregate(value ~ id, data=data, FUN=function(x) x[data$index == '3.10']+x[data$index == '3.11'])
x$value <- x$value[,1]
v$medianVniu[nrow(v)] <- median(x$value)
##ВУ
data <- mon[mon$Vvu == 'ВУ' & mon$year == '2017' & mon$index %in% c('3.10','3.11'), 
              c('id','year','index','variable','value')]
data$value <- as.numeric(sub(",", ".", data$value))
x <- aggregate(value ~ id, data=data, FUN=function(x) x[data$index == '3.10']+x[data$index == '3.11'])
x$value <- x$value[,1]
v$medianVvu[nrow(v)] <- median(x$value)
##ВШЭ
v$hse[nrow(v)] <- x$value[x$id == '1766']
##Таблица
v$Vname[nrow(v)] <- 'Доля иностранных аспирантов, %'
v$Vgroup[nrow(v)] <- 'Интернационализация'

#Интернационализация+ -------------------------------------> 
##5-100
x <- mon[mon$V5100 == '5-100' & mon$year == '2017' & mon$index %in% c('3.8','3.9','35','36','3.13'), 
           c('id','year','index','variable','value', 'unit')]
x$value <- as.numeric(sub(",", ".", x$value))
x$value[x$index == '36'] <- x$value[x$index == '36']/1000
x$value[x$index == '3.13'] <- x$value[x$index == '3.13']/1000
DF <- aggregate(x$value, by = list(index=x$index, variable=x$variable, unit=x$unit), median)
##НИУ
x <- mon[mon$Vniu == 'НИУ' & mon$year == '2017' & mon$index %in% c('3.8','3.9','35','36','3.13'), 
           c('id','year','index','variable','value')]
x$value <- as.numeric(sub(",", ".", x$value))
x$value[x$index == '36'] <- x$value[x$index == '36']/1000
x$value[x$index == '3.13'] <- x$value[x$index == '3.13']/1000
DF <- merge(DF, aggregate(x$value, by = list(index=x$index), median), by = "index")
##ВУ
x <- mon[mon$Vvu == 'ВУ' & mon$year == '2017' & mon$index %in% c('3.8','3.9','35','36','3.13'), 
           c('id','year','index','variable','value')]
x$value <- as.numeric(sub(",", ".", x$value))
x$value[x$index == '36'] <- x$value[x$index == '36']/1000
x$value[x$index == '3.13'] <- x$value[x$index == '3.13']/1000
DF <- merge(DF, aggregate(x$value, by = list(index=x$index), median), by = "index")
##ВШЭ
DF <- merge(DF, x[x$id == '1766',c('index','value')], by = "index")
##Таблица
colnames(DF) <- c('index', 'variable', 'unit', 'medianV5100', 'medianVniu', 'medianVvu', 'hse')
x <- data.frame("index"=c('3.8','3.9','35','36','3.13'), "Vname"=c('Доля иностранных НПР, %',
                                                                   'Число иностранных НПР, проработавших не менее семестра, чел.',
                                                                   'Число статей совместно с иностранными организациями, ед.',
                                                                   'Доходы от НИОКР из иностранных источников, млн. руб.',
                                                                   'Доходы от образовательной деятельности из иностранных источников, млн. руб.'), stringsAsFactors = F)
DF <- merge(DF, x, by = "index")
DF$Vgroup <- 'Интернационализация+'
v <- rbind(v, DF)

#Кадры -------------------------------------> *
##5-100
x <- mon[mon$V5100 == '5-100' & mon$year == '2017' & mon$index %in% c('7.3','2.13','28','29'), 
           c('id','year','index','variable','value', 'unit')]
x$value <- as.numeric(sub(",", ".", x$value))
DF <- aggregate(x$value, by = list(index=x$index, variable=x$variable, unit=x$unit), median)
##НИУ
x <- mon[mon$Vniu == 'НИУ' & mon$year == '2017' & mon$index %in% c('7.3','2.13','28','29'), 
           c('id','year','index','variable','value')]
x$value <- as.numeric(sub(",", ".", x$value))
DF <- merge(DF, aggregate(x$value, by = list(index=x$index), median), by = "index")
##ВУ
x <- mon[mon$Vvu == 'ВУ' & mon$year == '2017' & mon$index %in% c('7.3','2.13','28','29'), 
           c('id','year','index','variable','value')]
x$value <- as.numeric(sub(",", ".", x$value))
DF <- merge(DF, aggregate(x$value, by = list(index=x$index), median), by = "index")
##ВШЭ
DF <- merge(DF, x[x$id == '1766',c('index','value')], by = "index")
##Таблица
colnames(DF) <- c('index', 'variable', 'unit', 'medianV5100', 'medianVniu', 'medianVvu', 'hse')
x <- data.frame("index"=c('7.3','2.13','28','29'), "Vname"=c('Доля штатных НПР с учеными степенями, %', 'Доля молодых НПР, %',
                                                             'Средняя зарплата ППС, тыс. руб.','Средняя зарплата НР, тыс. руб.'), 
                stringsAsFactors = F)
DF <- merge(DF, x, by = "index")
DF$Vgroup <- 'Кадры'
v <- rbind(v, DF)

##Расчетный показатель !! >>
v[nrow(v)+1,] <- NA
##5-100
data <- mon[mon$V5100 == '5-100' & mon$year == '2017' & mon$index %in% c('1a','1b','1c','22','23'), 
              c('id','year','index','variable','value')]
data$value <- as.numeric(sub(",", ".", data$value))
x <- aggregate(value ~ id, data=data, FUN=function(x) (x[data$index == '1a']+0.25*x[data$index == '1b']+0.1*x[data$index == '1c'])/(x[data$index == '22']+x[data$index == '23']))
x$value <- x$value[,1]
v$medianV5100[nrow(v)] <- median(x$value)
##НИУ
data <- mon[mon$Vniu == 'НИУ' & mon$year == '2017' & mon$index %in% c('1a','1b','1c','22','23'), 
              c('id','year','index','variable','value')]
data$value <- as.numeric(sub(",", ".", data$value))
x <- aggregate(value ~ id, data=data, FUN=function(x) (x[data$index == '1a']+0.25*x[data$index == '1b']+0.1*x[data$index == '1c'])/(x[data$index == '22']+x[data$index == '23']))
x$value <- x$value[,1]
v$medianVniu[nrow(v)] <- median(x$value)
##ВУ
data <- mon[mon$Vvu == 'ВУ' & mon$year == '2017' & mon$index %in% c('1a','1b','1c','22','23'), 
              c('id','year','index','variable','value')]
data$value <- as.numeric(sub(",", ".", data$value))
x <- aggregate(value ~ id, data=data, FUN=function(x) (x[data$index == '1a']+0.25*x[data$index == '1b']+0.1*x[data$index == '1c'])/(x[data$index == '22']+x[data$index == '23']))
x$value <- x$value[,1]
v$medianVvu[nrow(v)] <- median(x$value)
##ВШЭ
v$hse[nrow(v)] <- x$value[x$id == '1766']
##Таблица
v$Vname[nrow(v)] <- 'Число студентов на 1 НПР, чел.'
v$Vgroup[nrow(v)] <- 'Кадры'

#Финансы и инфраструктура -------------------------------------> /
##5-100
x <- mon[mon$V5100 == '5-100' & mon$year == '2017' & mon$index %in% c('E.4','48','4.1','4.2','5.1'), 
           c('id','year','index','variable','value', 'unit')]
x$value <- as.numeric(sub(",", ".", x$value))
x$value[x$index == 'E.4'] <- x$value[x$index == 'E.4']/1000
x$value[x$index == '4.1'] <- x$value[x$index == '4.1']/1000
x$value[x$index == '48'] <- x$value[x$index == '48']/1000000
DF <- aggregate(x$value, by = list(index=x$index, variable=x$variable, unit=x$unit), median)
##НИУ
x <- mon[mon$Vniu == 'НИУ' & mon$year == '2017' & mon$index %in% c('E.4','48','4.1','4.2','5.1'), 
           c('id','year','index','variable','value', 'unit')]
x$value <- as.numeric(sub(",", ".", x$value))
x$value[x$index == 'E.4'] <- x$value[x$index == 'E.4']/1000
x$value[x$index == '4.1'] <- x$value[x$index == '4.1']/1000
x$value[x$index == '48'] <- x$value[x$index == '48']/1000000
DF <- merge(DF, aggregate(x$value, by = list(index=x$index), median), by = "index")
##ВУ
x <- mon[mon$Vvu == 'ВУ' & mon$year == '2017' & mon$index %in% c('E.4','48','4.1','4.2','5.1'), 
           c('id','year','index','variable','value', 'unit')]
x$value <- as.numeric(sub(",", ".", x$value))
x$value[x$index == 'E.4'] <- x$value[x$index == 'E.4']/1000
x$value[x$index == '4.1'] <- x$value[x$index == '4.1']/1000
x$value[x$index == '48'] <- x$value[x$index == '48']/1000000
DF <- merge(DF, aggregate(x$value, by = list(index=x$index), median), by = "index")
##ВШЭ
DF <- merge(DF, x[x$id == '1766',c('index','value')], by = "index")
##Таблица
colnames(DF) <- c('index', 'variable', 'unit', 'medianV5100', 'medianVniu', 'medianVvu', 'hse')
x <- data.frame("index"=c('E.4','48','4.1','4.2','5.1'), "Vname"=c('Доходы из всех источников на 1 НПР, млн руб.',
                                                                   'Доходы из всех источников, млрд руб.', 'Доходы из внебюджетных источников на 1 НПР, млн руб.', 
                                                                   'Доля доходов из внебюджетных источников, %', 'Учебно-лабораторные площади на 1 студента, м2'), stringsAsFactors = F)
DF <- merge(DF, x, by = "index")
DF$Vgroup <- 'Финансы и инфраструктура'
v <- rbind(v, DF)

##Округление
v[,4:7] <- round(v[,4:7], 2)

#################### Vgroup 2017 ########################################### Vgroup 2017 #######################

write.table(v, file="D:/R/Monitoring/monitoring.itog.csv", row.names = F, col.names = T, sep = ";")


#################### PLOTS #######################

#charts api https://google-developers.appspot.com/chart/image/docs/gallery/radar_charts
for (i in 4:6){
  for (j in 1:8){
    chartapi <- paste0("http://chart.apis.google.com/chart?",
                       "chs=535x560", "&", ## Размер диаграммы (< 300000) 470x625 с заголовками
                       #"chtt=", gsub(" ", "+", unique(v$Vgroup)[j]), "|", ## Заголовок
                       #paste(paste0(v[v$Vgroup == unique(v$Vgroup)[j],]$index, ".+", gsub(" ", "+", v[v$Vgroup == unique(v$Vgroup)[j],]$Vname)), 
                       #      collapse = "|"), "&", ## Заголовок
                       #"chts=404040,13,l", "&", ## Формат заголовка
                       "cht=rs", "&", ## Тип графика (r-прямые соединительные линии, rs-кривые соединительные линии)
                       "chco=005DAB,", ifelse(i==4,'FF9900',ifelse(i==5,'F20F76','06CC9F')), "&", ## Цвета в HEX-формате FF9900-оранж.(5-100) F20F76-розов.(НИУ) 06CC9F-зел.(ВУ)
                       "chds=0,", max(v[v$Vgroup == unique(v$Vgroup)[j],][,c(i, 7)]), "&", ## Масштаб
                       "chd=t:", paste(v[v$Vgroup == unique(v$Vgroup)[j],]$hse, collapse = ","), ",", v[v$Vgroup == unique(v$Vgroup)[j],]$hse[1], 
                       "|", paste(v[v$Vgroup == unique(v$Vgroup)[j],][,i], collapse = ","), ",", v[v$Vgroup == unique(v$Vgroup)[j],][,i][1], "&", ## Данные с повтором первой/последней точки
                       "chls=5|0", "&", ## Стиль линии (<line_n_thickness> толщина,<opt_dash_length> ширина штриха,<opt_space_length> ширина пробела)
                       "chxl=0:|", paste(paste0(v[v$Vgroup == unique(v$Vgroup)[j],]$index, "."), collapse = "|"), "&", ## Названия осей
                       "chxs=0,404040,18,0,t|1,404040,14,0.95,t", "&", ## Формат подписей осей
                       "chxt=x,y", "&", ## Оси x-Bottom x-axis, t-Top x-axis, y-Left y-axis, r-Right y-axis
                       "chxr=1,0,", ## Шкала (<axis_index> ось,<start_val> мин,<end_val> макс,<opt_step> шаг)
                       max(v[v$Vgroup == unique(v$Vgroup)[j],4:7]), ",", ## Шкала
                       ifelse(max(v[v$Vgroup == unique(v$Vgroup)[j],4:7])/5 >= 100, ## Шкала
                              round(max(v[v$Vgroup == unique(v$Vgroup)[j],4:7])/5, -2), ## Шкала
                              round(max(v[v$Vgroup == unique(v$Vgroup)[j],4:7])/5, 0)),  "&", ## Шкала
                       "chm=B,", ifelse(i==4,'FF9900',ifelse(i==5,'F20F76','06CC9F')), ",1,0,0","&", ## Заливка (<b_or_B>,<color>,<start_line_index>,<end_line_index>,<0>) | #FF990050 - прозрачность 50%
                       #"chdl=ВШЭ|", ifelse(i==4,'Медиана+5-100',ifelse(i==5,'Медиана+НИУ','Медиана+ВУ')),"&", ## Легенда HSE|Median
                       #"chdls=404040,13", "&", ## Формат подписей легенды
                       "chma=0,0,70,0", "&", ## Отступы
                       "chdlp=b") ## Положение легенды ## "b"(bottom),"t"(top),"l"(left),"r"(right),"bv"/"tv"(vertical order) "bvs"-пропуск пустой легенды
    download.file(chartapi, paste0("D:/R/Monitoring/chartapi.", i, '-', j,  ".png"), mode = 'wb')
    message("Google Chart API for ", unique(v$Vgroup)[j], " - ", ifelse(i==4,'5-100',ifelse(i==5,'НИУ','ВУ')))
  }
}


##############################################################################################################################
##############################################################################################################################
