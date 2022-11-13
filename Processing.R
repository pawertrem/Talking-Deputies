library(dplyr)
library(readxl)
library(stringr)
library(udpipe)
library(wordcloud2)
library(stopwords)
library(tidytext)
library(LDAvis)
library(topicmodels)
library(ggplot2)
library(tidyr)
library(tidytext)
library(writexl)
library(DT)
library(ggwordcloud)
library(stringi)
library(ggplot2)
library(quanteda)
library(writexl)
library(quanteda.textstats)

df <- read.csv('C:/Users/User/Desktop/df.csv')
df$Дата <- as.Date(df$Дата)
df <- df%>%filter(Дата>'2022-01-01')
nrow(df)
sum(nchar(df$Текст))

#Projects
projects <- df%>%group_by(Название)%>%count()%>%arrange(-n)
projects <- na.omit(projects)
projects$is_law <- str_detect(projects$Название, 'О проекте|Об отчёте|О Федеральномзаконе|Информация')
projects <- projects%>%filter(is_law==TRUE)%>%select(Название, n)

projects <- head(projects, 5)

DT::datatable(head(projects, 100), options = list(
  bPaginate = TRUE
))
write_xlsx(projects, 'C:/Users/User/Desktop/projects.xlsx')

#Deputies
deps <- df
deps <- deps%>%group_by(Текст)%>%filter(row_number()==1) 
deps$n <- ifelse(is.na(deps$Текст), 0, 1)
deps <- deps%>%ungroup()%>%group_by(file.list)%>%summarise(n=sum(n))%>%arrange(-n)
deps2 <- read_excel('C:/Users/User/Desktop/GosDuma/deps4.xlsx', sheet = 4)
deps <- deps%>%left_join(deps2, by=c('file.list' = 'Депутат'))%>%select(file.list, Фракция, n)

deps$file.list <- str_c('. ', deps$file.list)
deps$file.list <- str_c(rownames(deps), deps$file.list)

deps10 <- head(deps, 10)
ggplot(data = deps10)+
  geom_col(aes(y=reorder(file.list, +n), x=n, fill = Фракция))+
  xlab('Кол-во высказываний')+
  ylab('Депутат')

write_xlsx(deps, 'C:/Users/User/Desktop/deputies.xlsx')

#WordCloud

df <- df%>%group_by(Текст)%>%filter(row_number()==1) 
lemmas <- read.csv('C:/Users/User/Desktop/lemmatized2.csv', sep=',', encoding = 'UTF-8')
lemmas$Текст <- gsub('\\[', '', lemmas$Текст)
lemmas$Текст <- gsub(']', '', lemmas$Текст)
lemmas$Текст <- gsub(',', '', lemmas$Текст)
lemmas$Текст <- gsub("'", '', lemmas$Текст)
data <- df
lemmas$X = lemmas$X+1
colnames(lemmas) <- c('X', 'Леммы')
data <- data%>%left_join(lemmas, by='X')
data <- data%>%ungroup()%>%select(X, file.list, id, Леммы)
data <- unnest_tokens(data, token, Леммы)
stop_words <- read.delim('C:/Users/User/Desktop/stopwords-ru.txt', encoding = 'UTF-8')
data <- anti_join(data, stop_words, by=c('token'='c'))

pos <- read.csv('C:/Users/User/Desktop/pos.csv', sep=',', encoding = 'UTF-8')
data <- data%>%inner_join(pos, by='token')

cloud = data %>% filter(upos=='NOUN')%>%
  dplyr::count(token, sort=TRUE) %>% 
  top_n(50, n)
write_xlsx(cloud, 'C:/Users/User/Desktop/cloud1.xlsx')
wordcloud2(data = cloud, shuffle = FALSE, color='random-light', rotateRatio=0)

cloud = data%>% filter(upos=='NOUN')%>%
  dplyr::count(token, sort=TRUE) %>% 
  filter(n < quantile(n ,0.975)) %>%
  top_n(50, n)
set.seed(42)
#write_xlsx(cloud, 'C:/Users/User/Desktop/cloud2.xlsx')
ggplot(cloud, aes(label = token, size = n, color=factor(sample.int(10, nrow(cloud), replace = TRUE)))) +
  geom_text_wordcloud_area(rm_outside = TRUE, eccentricity = 1, shape = 'diamond')  +
  scale_size_area(max_size = 10) +
  theme_minimal()

#Parties WC

deps2 <- read_excel('C:/Users/User/Desktop/GosDuma/deps4.xlsx', sheet = 4)

lemmaparty <- data%>%left_join(deps2, by=c('file.list' = 'Депутат'))%>%select(Фракция, token)

tf_idf = lemmaparty %>% group_by(Фракция, token) %>% summarize(count = n())
tf_idf = tf_idf%>%filter(!token %in% c('п', 'м', 'нил', 'н', 'лдпр', 'кпрф', 'справедливый', 'аксаков', 'анатолиевич', 'генрихович', 'владимир', 'вольфович', 'коломейцев', 'макаров', 'анатолиевич', 'вяткин', 'юридико', 'оксана', 'дмитрий'))
tf_idf_filter = tf_idf %>% group_by(token) %>% summarise(count = n())
tf_idf_filter = tf_idf_filter %>% filter(count > 2) 
tf_idf = tf_idf %>% filter(token %in% tf_idf_filter$token)
tf_idf = tf_idf %>% bind_tf_idf(token, Фракция, count)
tf_idf = tf_idf%>%filter(token!='му')

tf_idf10 <- tf_idf %>% 
  arrange(desc(tf_idf)) %>% 
  group_by(Фракция) %>% slice(1:50)

edro <- tf_idf10%>%filter(Фракция=='ЕР')%>%ungroup()%>%select(token, tf_idf)
edro <- wordcloud2(data = edro, shuffle = FALSE, color='random-light', rotateRatio=0, size = 0.75)
kprf <- tf_idf10%>%filter(Фракция=='КПРФ')%>%ungroup()%>%select(token, tf_idf)
kprf <- wordcloud2(data = kprf, shuffle = FALSE, color='random-light', rotateRatio=0)
ldpr <- tf_idf10%>%filter(Фракция=='ЛДПР')%>%ungroup()%>%select(token, tf_idf)
ldpr <- wordcloud2(data = ldpr, shuffle = FALSE, color='random-light', rotateRatio=0, size = 0.75)
NL <- tf_idf10%>%filter(Фракция=='Новые люди')%>%ungroup()%>%select(token, tf_idf)
NL <- wordcloud2(data = NL, shuffle = FALSE, color='random-light', rotateRatio=0, size = 0.75)
SR <- tf_idf10%>%filter(Фракция=='СРЗП')%>%ungroup()%>%select(token, tf_idf)
SR <- wordcloud2(data = SR, shuffle = FALSE, color='random-light', rotateRatio=0, size = 0.75) 
withoutparties <- tf_idf10%>%filter(Фракция=='Депутаты, не входящие во фракции')%>%ungroup()%>%select(token, tf_idf)
withoutparties <- wordcloud2(data = withoutparties, shuffle = FALSE, color='random-light', rotateRatio=0, size = 0.75)

edro
kprf
ldpr
NL
SR

partiesWC <- cbind(edro, kprf, ldpr, NL, SR, withoutparties)

require(openxlsx)
list_of_datasets <- list("ЕР" = edro, "КПРФ" = kprf, "ЛДПР" = ldpr, 'НЛ' = NL, 'СРЗП' = SR, 'Депутаты, не входящие во фракции' = withoutparties)
write.xlsx(list_of_datasets, file = "writeXLSX2.xlsx")


#Context

lemmas <- read.csv('C:/Users/User/Desktop/lemmatized.csv', sep=',', encoding = 'UTF-8')
lemmas$Текст <- stri_replace_all_regex(lemmas$Текст,
                                       pattern=c('\\[', '\\]', ',', "'"),
                                       replacement=c(''),
                                       vectorize=FALSE)
colnames(lemmas) <- c('doc_id', 'lemma')

lemmas$doc_id <- as.integer(lemmas$doc_id)
lemmas$doc_id <- lemmas$doc_id+1
data <- df
lemmas <- data%>%left_join(lemmas, by=c('X'='doc_id'))


lemmas <- lemmas%>%ungroup()%>%select(lemma)

lem.bigrams = lemmas %>% 
  unnest_tokens(bigram, lemma, token = "ngrams", n = 2)

lem.bigrams <- lem.bigrams %>% filter(!bigram %in% c('уважаемый коллега', 'государственный дума', 'российский федерация', 'единый россия', 'фракция единый', 'фракция новый', 'новый люди', 'фракция справедливый') )

rustopwords = data.frame(words=c(stopwords::stopwords("ru"), "это", "в", "с", "на", "и", "который"), stringsAsFactors=FALSE)
lem.bigrams = lem.bigrams %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  dplyr::filter(!word1 %in% rustopwords$words) %>% 
  dplyr::filter(!word2 %in% rustopwords$words) 

lem.bigrams.n = lem.bigrams %>% 
  dplyr::count(word1, word2, sort=TRUE) %>%
  dplyr::select(word1, word2, n)
lem.bigrams.n = na.omit(lem.bigrams.n)
lem.bigrams.n = lem.bigrams.n%>%filter(!word1 %in% c('уважаемый', 'фракция', 'кпрф', 'справедливый', 'вячеслав'))%>%filter(!word2 %in% c('уважаемый', 'фракция', 'кпрф', 'справедливый', 'вячеслав'))

context <- function(x){
  lem.bigrams.n %>% 
    filter(word1 == x | word2 == x) %>% 
    dplyr::count(word1, word2, sort=TRUE) %>%
    dplyr::select(word1, word2, n)%>%
    top_n(30)
}

word1 <- as.data.frame(unique(lem.bigrams.n$word1))
colnames(word1) <- 'word'
word2 <- as.data.frame(unique(lem.bigrams.n$word2))
colnames(word2) <- 'word'
words <- unique(rbind(word1, word2))
words1 <- words%>%left_join(lem.bigrams.n, by=c('word'='word1'))
colnames(words1) <- c('word1', 'word2', 'n')
words2 <- words%>%left_join(lem.bigrams.n, by=c('word'='word2'))
colnames(words2) <- c('word1', 'word2', 'n')
words <- rbind(words1, words2)
words <- words%>%group_by(word1, word2)%>%summarise(nsum = sum(n))

words_upd <- na.omit(words)
words_upd <- words_upd%>%filter(nsum < 500 & nsum > 10)

DT::datatable(context('война'), options = list(
  bPaginate = TRUE
))


write_xlsx(words_upd, 'C:/Users/User/Desktop/bigrams.xlsx')

#Putin

lemmas <- read.csv('C:/Users/User/Desktop/lemmatized2.csv', sep=',', encoding = 'UTF-8')
lemmas$Текст <- gsub('\\[', '', lemmas$Текст)
lemmas$Текст <- gsub(']', '', lemmas$Текст)
lemmas$Текст <- gsub(',', '', lemmas$Текст)
lemmas$Текст <- gsub("'", '', lemmas$Текст)
#lemmas <- lemmas%>%group_by(Текст)%>%
data <- df
lemmas$X = lemmas$X+1
colnames(lemmas) <- c('X', 'Леммы')
data <- data%>%ungroup()%>%left_join(lemmas, by='X')
data$Леммы <- gsub('владимир владимирович', 'владимирвладимирович', data$Леммы)
data <- unnest_tokens(data, token, Леммы)

putin <- c('путин', 'президент', 'главнокомандующий', 'владимирвладимирович')
df1 <- data%>%select(file.list, token)
df1put <- df1%>%filter(token %in% putin)%>%group_by(file.list)%>%count()%>%arrange(-n)

deps2 <- read_excel('C:/Users/User/Desktop/GosDuma/deps4.xlsx', sheet = 4)
df1put <- df1put%>%left_join(deps2, by=c('file.list' = 'Депутат'))%>%select(file.list, Фракция, n)

df1putn <- df1put%>%group_by(token)%>%count()
ggplot(data = df1putn)+
  geom_col(aes(y=token, x=n))

df1put$file.list <- str_c('. ', df1put$file.list)
df1put$file.list <- str_c(rownames(df1put), df1put$file.list)

write_xlsx(df1put, 'C:/Users/User/Desktop/putindeps.xlsx')

df1put10 <- head(df1put, 10)
ggplot(data = df1put10)+
  geom_col(aes(y=reorder(file.list, +n), x=n, fill = Фракция))+
  xlab('Кол-во упоминаний')+
  ylab('Депутат')

df1put <- df1put%>%group_by(Фракция)%>%summarise(n=sum(n))
write_xlsx(df1put, 'C:/Users/User/Desktop/putinpartiesabsolute.xlsx')

ggplot(data = df1put)+
  geom_col(aes(y=reorder(Фракция, +n), x=n, fill = Фракция))+
  xlab('Кол-во упоминаний')+
  ylab('Партия')

df1 <- data%>%select(file.list, token)
df1 <- df1%>%left_join(deps2, by=c('file.list' = 'Депутат'))%>%select(Фракция, token)
df1put <- df1%>%filter(token %in% putin)%>%group_by(Фракция)%>%count()
df1 <- df1%>%group_by(Фракция)%>%count()
df1put <- df1put%>%left_join(df1, by='Фракция')
colnames(df1put) <- c('Фракция', 'Упоминание', 'Всего')
df1put$Share <- df1put$Упоминание/df1put$Всего

write_xlsx(df1put, 'C:/Users/User/Desktop/putinparties.xlsx')

ggplot(data = df1put)+
  geom_col(aes(y=reorder(Фракция, +Share), x=Share, fill = Фракция))+
  xlab('Доля упоминаний Президента')+
  ylab('Партия')

#Applauses

df1 <- df
df1$Смех <- str_detect(df1$Текст, c("Аплодисменты", 'аплодисменты'))
df_laught <- df1%>%filter(Смех == TRUE)

text <- df_laught$Текст
keywords <- c('\\(Аплодисменты', '\\(аплодисменты')


cont <- df_laught %>%
  separate_rows(Текст, sep = '\\.\\s*') %>%separate_rows(Текст, sep = '\\!\\s*')%>%
  slice({
    tmp <- grep(keywords, Текст, ignore.case = TRUE)
    sort(unique(c(tmp-4, tmp-3, tmp-2, tmp-1, tmp)))
  })

em <- cont %>% select(X, Текст)
em <- cont %>% group_by(X) %>% summarise(Текст = paste0(Текст, collapse = '.'))

em$Текст <- gsub(')\\s*', '', em$Текст)
em$Текст <- gsub('\\(Аплодисменты', '(Аплодисменты)', em$Текст)

em <- em%>%left_join(df_laught, by='X')%>%select(Текст.x, Название)
colnames(em) <- c('Текст', 'Проект')

DT::datatable(em, options = list(
  bPaginate = TRUE
))

df_laught[3, 6]

#write_xlsx(em, 'C:/Users/User/Desktop/applauses.xlsx')

#Lexical Richness

lemmas <- read.csv('C:/Users/User/Desktop/lemmatized.csv', sep=',', encoding = 'UTF-8')
lemmas$Текст <- gsub('\\[', '', lemmas$Текст)
lemmas$Текст <- gsub(']', '', lemmas$Текст)
lemmas$Текст <- gsub(',', '', lemmas$Текст)
lemmas$Текст <- gsub("'", '', lemmas$Текст)
lemmas$X <- lemmas$X+1
needX <- df$X
lemmas <- lemmas%>%filter(X %in% needX)
dfmtok <- dfm(lemmas$Текст)
ttr <- textstat_lexdiv(dfmtok, measure='TTR')
ttr$document <- needX

ttr <- ttr%>%inner_join(df, by=c('document'='X'))
ttr$na <- is.na(ttr$TTR)
ttr <- ttr%>%filter(na==FALSE)
ttr <- ttr%>%group_by(file.list)%>%summarise(TTR=mean(TTR))

deps <- df
deps <- deps%>%group_by(Текст)%>%filter(row_number()==1) 
deps$n <- ifelse(is.na(deps$Текст), 0, 1)
deps <- deps%>%ungroup()%>%group_by(file.list)%>%summarise(n=sum(n))%>%arrange(-n)
deps2 <- read_excel('C:/Users/User/Desktop/GosDuma/deps4.xlsx', sheet = 4)
deps <- deps%>%left_join(deps2, by=c('file.list' = 'Депутат'))%>%select(file.list, Фракция, n)

ttr <- ttr%>%arrange(-TTR)
ttr <- ttr%>%inner_join(deps, by='file.list')%>%select(file.list, Фракция, TTR, n)

colnames(ttr)<-c('Депутат', 'Фракция', 'TTR', 'Кол-во высказываний')

ttr <- ttr%>%filter(ttr[,4]>=5)

ttr$Депутат <- str_c('. ', ttr$Депутат)
ttr$Депутат <- str_c(rownames(ttr), ttr$Депутат)

DT::datatable(ttr, options = list(
  bPaginate = TRUE
))

write_xlsx(ttr, 'C:/Users/User/Desktop/ttr.xlsx')

#SW

df1 <- data%>%left_join(deps2, by=c('file.list' = 'Депутат'))%>%select(file.list, Фракция, token)
df2 <- df1%>%inner_join(stop_words, by=c('token'='c'))
df1<-na.omit(df1)
df1 <- df1%>%group_by(file.list, Фракция)%>%count()
df2 <- df2%>%group_by(file.list, Фракция)%>%count()
dfstop <- df1%>%inner_join(df2, by='file.list')
dfstop$perc <- dfstop$n.y/dfstop$n.x
dfstop <- dfstop%>%select(file.list, Фракция.x, perc)%>%arrange(perc)
colnames(dfstop)<-c('Депутат', 'Фракция', 'Доля "шумовых слов"')
DT::datatable(dfstop, options = list(
  bPaginate = TRUE
))
dfstop <- dfstop%>%left_join(deps, by=c('Депутат'='file.list'))
dfstop <- dfstop%>%select(Депутат, Фракция.x, 'Доля "шумовых слов"', n)
dfstop <- dfstop%>%filter(n>=5)

dfstop$Депутат <- str_c('. ', dfstop$Депутат)
dfstop$Депутат <- str_c(rownames(dfstop), dfstop$Депутат)

write_xlsx(dfstop, 'C:/Users/User/Desktop/dfstop.xlsx')