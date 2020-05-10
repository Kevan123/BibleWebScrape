#1 - READ IN TABLE WE WILL USE FOR FOR LOOP
require(stringi)
require(rvest)
require(stringr)
require(tidyr)
require(dplyr)
setwd("C:/Users/krajaram/Desktop/Personal/AIM BLOG")
oldtestdata = read.csv("OldTestData.csv", header = TRUE, sep= ",")
##View(oldtestdata)


#webscape old testament
oldtestdata1 = read_html("https://simple.wikipedia.org/wiki/Old_Testament")%>% 
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/ul') %>% 
  html_text() #Read in old testament summarized data

#data clean up
oldtestdata1 <- str_replace_all(oldtestdata1, "[\n]" , "") 
oldtestdata1 <- str_replace_all(oldtestdata1, c("Chapters") , "") 
oldtestdata1 <- str_replace_all(oldtestdata1, c("Chapter") , "") 
oldtestdata1 <- data.frame(oldtestdata1)
oldtestdata1 <- separate_rows(oldtestdata1,1,sep = "[)]")
oldtestdata1 <- data.frame(do.call(rbind, str_split(oldtestdata1$oldtestdata1, '[(]')))
oldtestdata1 <- oldtestdata1[!apply(is.na(oldtestdata1) | oldtestdata1 == "", 1, all),]
oldtestdata1 <- oldtestdata1[-c(16),]
oldtestdata1$Code <- toupper(substr(gsub(" ", "", oldtestdata1$X1),1,3))
oldtestdata1 <- oldtestdata1 %>%rename('Book' = X1,'Chapters' = X2)

#replacing codes for Song of Solomon and Ezekiel
oldtestdata1$Code<-gsub("THE", "SNG", oldtestdata1$Code)
oldtestdata1$Code<-gsub("EZE", "EZK", oldtestdata1$Code)

#2 - READ IN BIBLE DATA
library('xml2')
library('rvest')
library('stringr')
library('tidyr')
library('dplyr')
library('writexl')
library('tidyverse')

#2.1 - TEST AN EXAMPLE
webbible = read_html("https://my.bible.com/bible/111/GEN.1.niv")%>%
            html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "ch1", " " ))]')%>% 
              html_text()

  
#3 - CREATE FOR LOOP TO GET ALL TEXT IN OLD TESTAMENT  - CORRECT FOR EZEKIEL - EZK NOT EZE
chapters = as.numeric(trimws(oldtestdata1$Chapters))
code = as.vector(oldtestdata1$Code)
mat = cbind(code, chapters)
mat = matrix(mat,39,2)

remove(textdata)
textdata = c()
k=1

for (i in code[] ){
  for (j in 1:mat[k,2]){  
    webbible = tryCatch(read_html(paste0("https://my.bible.com/bible/111/", i, ".",j,".NIV")), error = function(e){NA}) 
    bibletext = webbible%>%html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "reader", " " ))]')%>% html_text()
    bibletext = str_replace_all(bibletext, "[\n]" , "") 
    textdata = rbind(textdata, bibletext)
  }
  k=k+1
}

#Save output if needed with some cleaning
library('stringr')
bibletextdata = textdata
bibletextdata = str_replace_all(bibletextdata, "'", "") 
bibletextdata = str_replace_all(bibletextdata, """, "") 
bibletextdata = str_replace_all(bibletextdata, """, "") 
bibletextdata = data.frame(bibletextdata)
write.csv(bibletextdata, "bibletextdata.csv")

#4 NLP
library(tm)
biblecorpus <- VCorpus(VectorSource(textdata1))
biblecorpusclean <- tm_map(biblecorpus,content_transformer(tolower)) #converting to lower case letters
biblecorpusclean <- tm_map(biblecorpusclean,removeNumbers) #removing numbers
biblecorpusclean <- tm_map(biblecorpusclean,removeWords,c(stopwords('english'))) #remvoing stop words
biblecorpusclean <- tm_map(biblecorpusclean,removePunctuation)

#biblecorpuscleancopy <- biblecorpusclean

inspect(biblecorpus[1])
inspect(biblecorpusclean[1])

#Create a word cloud for frequency
library(wordcloud)
wordcloud(biblecorpusclean ,min.freq = 1000,random.order = FALSE)

bibletdm <- TermDocumentMatrix(biblecorpusclean, control = list(wordLengths = c(1, Inf)))
freq.terms <- findFreqTerms(bibletdm, lowfreq=500)

assoc = findAssocs(bibletdm, freq.terms, 0.7)
assoc = assoc$will
findAssocs(bibletdm, "will", 0.3)
View(assoc)

#Create a plot to show word associations
install.packages("BiocManager")
BiocManager::install("Rgraphviz")

plot(bibletdm, term = freq.terms , corThreshold = 0.4, weighting = F)

#Create a dendrogram
bibletdm2 <- as.matrix(removeSparseTerms(bibletdm, sparse = 0.95))
# cluster terms
distMatrix <- dist((bibletdm2))
fit <- hclust(distMatrix, method = 'ward.D' )
plot(fit)
rect.hclust(fit, k = 6) # cut tree into 6 clusters 

#Perform LDA on top models
library(topicmodels)
lda <- LDA(biblefreqtdm, k = 3)
terms(lda, 3)

#Lex Rank based on graph based centrality
library(lexRankr)
test = textdata[1:50,]

#perform lexrank for top 3 sentences
top_3 = lexRankr::lexRank(test,
                          #only 1 article; repeat same docid for all of input vector
                          docId = rep(1, length(test)),
                          #return 3 sentences to mimick /u/autotldr's output
                          n = 3,
                          continuous = TRUE)

#reorder the top 3 sentences to be in order of appearance in article
order_of_appearance = order(as.integer(gsub("_","",top_3$sentenceId)))
#extract sentences in order of appearance
ordered_top_3 = top_3[order_of_appearance, "sentence"]
ordered_top_3

#n-grams
library(corpus)
term_stats(biblecorpusclean, ngrams = 2:3, types = TRUE,
           subset = type1 == "god" & !type2 %in% stopwords_en)
