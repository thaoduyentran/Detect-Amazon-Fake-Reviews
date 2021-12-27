library(tidyverse)
library(dplyr)
library(e1071)
library(ggplot)
#### loading & cleaning data ####

df2008 <- read.csv("~/Downloads/2008_data.csv", sep = "|")

df2018 <- read.csv("~/Downloads/2018_data.csv", sep = "|")

summary(df2008)
summary(df2018)

df2008$reviewTime <- as.Date(df2008$reviewTime)
df2018$reviewTime <- as.Date(df2018$reviewTime)

df2008$verified <- as.factor(df2008$verified)
df2018$verified <- as.factor(df2018$verified)

df2008$asin <- as.factor(df2008$asin)
df2018$asin <- as.factor(df2018$asin)

df2008 <- df2008[,-1]
df2018 <- df2018[,-1]
#### put data into subset with june to september ####

df2008 <- df2008 %>% arrange(reviewTime) %>%
  filter(reviewTime >= "2008-06-01" & reviewTime <= "2008-09-30")
df2018 <- df2018 %>% arrange(reviewTime) %>%
  filter(reviewTime >= "2018-06-01" & reviewTime <= "2018-09-30")

dim(df2008)
dim(df2018)

#also making sure they're unique data
unique2008 <- df2008 %>% distinct(reviewText, .keep_all = T)
unique2018 <- df2018 %>% distinct(reviewText, .keep_all = T)

library(stringr)


#remove unwanted strings
str <- str_extract(unique2008$reviewText, "\\<div+[:print:]+\\>")
str <- as.factor(str)
summary(str)


unique2008$reviewText <- str_replace(unique2008$reviewText, "\\<a+[:print:]+\\</a\\>",
                                     " *INCLUDES_LINK* ")
unique2008$reviewText <- str_replace(unique2008$reviewText, "\\<a+[:print:]+\\</a\\>$",
                                     " *INCLUDES_LINK* ")
unique2008$reviewText <- str_replace(unique2008$reviewText, "\\&nbsp\\;","")
unique2008$reviewText <- str_replace(unique2008$reviewText, "\\<div+[:print:]+\\>",
                                     " *INCLUDES_VIDEO* ")


unique2018 <- subset2018 %>% distinct(reviewText, .keep_all = T)

unique2018$reviewText <- str_replace(unique2018$reviewText, "\\<a+[:print:]+\\</a\\>",
                                     " *INCLUDES_LINK* ")
unique2018$reviewText <- str_replace(unique2018$reviewText, "\\&nbsp\\;","")
unique2018$reviewText <- str_replace(unique2018$reviewText, "\\<div+[:print:]+\\>",
                                     " *INCLUDES_VIDEO* ")

View(as.data.frame(table(unique2018$reviewText)))
# Create month column
unique2008$month = as.factor(ifelse(unique2008$reviewTime>='2008-06-01' & unique2008$reviewTime <= '2008-06-30', "Jun",
       ifelse(unique2008$reviewTime>='2008-07-01' & unique2008$reviewTime <= '2008-07-31', "Jul",
              ifelse(unique2008$reviewTime>="2008-08-01" & unique2008$reviewTime <= "2008-08-31", "Aug",
                     ifelse(unique2008$reviewTime>="2008-09-01" & unique2008$reviewTime <= "2008-09-30", "Sep",       
                            "NULL")))))
unique2018$month = as.factor(ifelse(unique2018$reviewTime>='2018-06-01' & unique2018$reviewTime <= '2018-06-30', "Jun",
       ifelse(unique2018$reviewTime>='2018-07-01' & unique2018$reviewTime <= '2018-07-31', "Jul",
              ifelse(unique2018$reviewTime>="2018-08-01" & unique2018$reviewTime <= "2018-08-31", "Aug",
                     ifelse(unique2018$reviewTime>="2018-09-01" & unique2018$reviewTime <= "2018-09-30", "Sep",       
                            "NULL")))))
unique2008$monthnum = ifelse(unique2008$month == 'Jun', '6',
                  ifelse(unique2008$month == 'Jul', '7',
                  ifelse(unique2008$month == 'Aug', '8',
                 ifelse(unique2008$month == 'Sep', '9', 'null'))))
  
unique2018$monthnum = ifelse(unique2018$month == 'Jun', '6',
                         ifelse(unique2018$month == 'Jul', '7',
                         ifelse(unique2018$month == 'Aug', '8',
                         ifelse(unique2018$month == 'Sep', '9', 'null'))))

# filter 5 star and 1 star reviews
fivestar2008 <- unique2008 %>%  filter(overall == "5")
fivestar2018 <- unique2018 %>% filter(overall == "5")
onestar2008 = unique2008 %>% filter(overall == "1")
onestar2018 = unique2018 %>% filter(overall == "1")

table(fivestar2008$verified)
table(fivestar2018$verified)
table(onestar2008$verified)
table(onestar2018$verified)

# Look at monthly patterns of verified/unverified reviews (5 star vs 1 star)
f18 = fivestar2018  %>%group_by(month, verified) %>% tally() %>% arrange(month)
o18 = onestar2018  %>%group_by(month, verified) %>% tally() %>% arrange(month)
f08 = fivestar2008  %>%group_by(month, verified) %>% tally() %>% arrange(month)
o08 = onestar2008  %>%group_by(month, verified) %>% tally() %>% arrange(month)

f18 = data.frame(f18) 
f18$Rating = '5 stars'
o08 = data.frame(o08)
o08$Rating = '1 star'
o18 = data.frame(o18)
o18$Rating = '1 star'
f08 = data.frame(f08)
f08$Rating = '5 stars'

View(f18)
View(o18)
View(f08)
View(o08)




amzn_cols <- c("#EDC57E","#FF9900",
               "#45ACCB", "#00A8E1", "#008DFF", "#4C8A9F", "#146EB4", "#0F3765",
               "#232F3E",rep("#313335",2), rep("#37475A",2), "#131A22")

#the colors go from lightest to darkest and yellows, blues, blacks




# Monthly 5* in 2018
fivestar2018 %>% ggplot(aes(x = monthnum)) +  
  scale_x_discrete(labels = c('Jun','Jul','Aug','Sep')) +
  geom_bar(aes(fill=verified), position="dodge") +
  labs(title="Number of 5* Reviews in 2018 (Jun-Sep)",
       x="Month", y="Number of Review",
       fill="Verified Review") +  
  theme_classic()+
  scale_fill_manual(values=c("#FF9900","#146EB4"))+
  geom_text(stat="count", aes(label=..count..,group=verified), cex=2.5, 
            vjust=-0.5, hjust=0.5, position=position_dodge(width=1))+
  theme(axis.text = element_text(size=10),
        axis.title = element_text(size=12),
        legend.text = element_text(size=10),
        plot.title = element_text(size=13, face = "bold"),
        legend.background = element_rect(color="black", fill="white"))

# Monthly 5* in 2008 
fivestar2008 %>% ggplot(aes(x=monthnum)) + 
  scale_x_discrete(labels = c('Jun','Jul','Aug','Sep')) +
  geom_bar(aes(fill=verified), position="dodge") +
  labs(title="Number of 5* Reviews in 2008 (Jun-Sep)",
       x="Month", y="Number of Review",
       fill="Verified Review") + 
  theme_classic()+
  scale_fill_manual(values=c("#FF9900","#146EB4"))+
  geom_text(stat="count", aes(label=..count..,group=verified), cex=2.5, 
            vjust=-0.5, hjust=0.5, position=position_dodge(width=1))+
  theme(axis.text = element_text(size=10),
        axis.title = element_text(size=12),
        legend.text = element_text(size=10),
        plot.title = element_text(size=13, face = "bold"),
        legend.background = element_rect(color="black", fill="white"))

# Monthly 1* in 2018 
onestar2018 %>% ggplot(aes(x=monthnum)) + 
  scale_x_discrete(labels = c('Jun','Jul','Aug','Sep')) +
  geom_bar(aes(fill=verified), position="dodge") +
  labs(title="Number of 1* Reviews in 2018 (Jun-Sep)",
       x="Month", y="Number of Review",
       fill="Verified Review") +
  theme_classic()+
  scale_fill_manual(values=c("#FF9900","#146EB4"))+
  geom_text(stat="count", aes(label=..count..,group=verified), cex=2.5, 
            vjust=-0.5, hjust=0.5, position=position_dodge(width=1))+
  theme(axis.text = element_text(size=10),
        axis.title = element_text(size=12),
        legend.text = element_text(size=10),
        plot.title = element_text(size=13, face = "bold"),
        legend.background = element_rect(color="black", fill="white"))

# Monthly 1* in 2008 
onestar2008 %>% ggplot(aes(x=monthnum)) + 
  scale_x_discrete(labels = c('Jun','Jul','Aug','Sep')) +
  geom_bar(aes(fill=verified), position="dodge") +
  labs(title="Number of 1* Reviews in 2008 (Jun-Sep)",
       x="Month", y="Number of Review",
       fill="Verified Review") +
  theme_classic()+
  scale_fill_manual(values=c("#FF9900","#146EB4"))+
  geom_text(stat="count", aes(label=..count..,group=verified), cex=2.5, 
            vjust=-0.5, hjust=0.5, position=position_dodge(width=1))+
  theme(axis.text = element_text(size=10),
        axis.title = element_text(size=12),
        legend.text = element_text(size=10),
        plot.title = element_text(size=13, face = "bold"),
        legend.background = element_rect(color="black", fill="white"))

# Monthly overall in 2008
unique2008 %>% ggplot(aes(x=monthnum)) + 
  scale_x_discrete(labels = c('Jun','Jul','Aug','Sep')) +
  geom_bar(aes(fill=verified), position="dodge") +
  labs(title="Number of All Rating Reviews in 2008 (Jun-Sep)",
       x="Month", y="Number of Review",
       fill="Verified Review") +
  theme_classic()+
  scale_fill_manual(values=c("#FF9900","#146EB4"))+
  geom_text(stat="count", aes(label=..count..,group=verified), cex=2.5, 
            vjust=-0.5, hjust=0.5, position=position_dodge(width=1))+
  theme(axis.text = element_text(size=10),
        axis.title = element_text(size=12),
        legend.text = element_text(size=10),
        plot.title = element_text(size=13, face = "bold"),
        legend.background = element_rect(color="black", fill="white"))


# Monthly overall in 2018 
unique2018 %>% ggplot(aes(x=monthnum)) + 
  scale_x_discrete(labels = c('Jun','Jul','Aug','Sep')) +
  geom_bar(aes(fill=verified), position="dodge") +
  labs(title="Number of All Rating Reviews in 2018 (Jun-Sep)",
       x="Month", y="Number of Review",
       fill="Verified Review") +
  theme_classic()+
  scale_fill_manual(values=c("#FF9900","#146EB4"))+
  geom_text(stat="count", aes(label=..count..,group=verified), cex=2.5, 
            vjust=-0.5, hjust=0.5, position=position_dodge(width=1))+
  theme(axis.text = element_text(size=10),
        axis.title = element_text(size=12),
        legend.text = element_text(size=10),
        plot.title = element_text(size=13, face = "bold"),
        legend.background = element_rect(color="black", fill="white"))

# Create time series variable t 
dim(fivestar2008)
dim(fivestar2018)
dim(onestar2008)
dim(onestar2018)
fivestar2008$t = 1:11666
onestar2008$t = 1:1680
fivestar2018$t = 1:50406
onestar2018$t = 1:7161

# Create seasonal dummy variables 
df2008$d1 <- ifelse(df2008$month  == "Jun", 1, 0)
df2008$d2 <- ifelse(df2008$month  == "Jul", 1, 0)
df2008$d3 <- ifelse(df2008$month  == "Aug", 1, 0)

df2018$d1 <- ifelse(df2018$month  == "Jun", 1, 0)
df2018$d2 <- ifelse(df2018$month  == "Jul", 1, 0)
df2018$d3 <- ifelse(df2018$month  == "Aug", 1, 0)


# Run logistics regression model 
fit = glm(verified~t + d1 + d2 +d3, fivestar2008, family = 'binomial')
fit2 = glm(verified~t + d1 + d2 +d3, onestar2008, family = 'binomial')
fit3 = glm(verified~t + d1 + d2 +d3, fivestar2018, family = 'binomial')
fit4 = glm(verified~t + d1 + d2 +d3, onestar2018, family = 'binomial')
summary(fit)
summary(fit2)
summary(fit3)
summary(fit4)

options(scipen = 999)

fit.tot08 = glm(verified~t + d1 + d2 +d3, df2008, family = 'binomial')
fit.tot18 = glm(verified~t + d1 + d2 +d3, df2008, family = 'binomial')
summary(fit.tot08)
summary(fit.tot18)

# Verified reviews are not affected by time trend in the whole dataset in both 2008 & 2018 

# 1* datasets in 2008 & 2018 have 3-4 significant predictors and negative coefficient
# meaning that 1* verified reviews decrease continuously over the time horizon and seasonality 
# 5* dataset in 2018 has one significant predictor (d1), it increases in Jun but
# does not increase over time horizon but is affected by seasonality 
# 5* dataset in 2008 doesn't increase or decrease over the time horizon 

## We can use the confint() function to obtain confidence intervals for the 
#coefficient estimates. Note that for logistic models, confidence intervals 
# are based on the profiled log-likelihood function. We can also get CIs based 
# on just the standard errors by using the default method.

### CIs using profiled log-likelihood
confint(fit)
confint(fit2)
confint(fit3)
confint(fit4)

### CIs using standard errors
confint.default(fit)
confint.default(fit2)
confint.default(fit3)
confint.default(fit4)

## Investigate similar reviews from different users
#### Subset duplicated reviews 

n_occur <- data.frame(table(df2008$reviewText))
n_occur[df2008$reviewText >=2 ,]
duplicated2008 = df2008[df2008$reviewText %in% n_occur$Var1[n_occur$Freq >=2],]
View(duplicated2008)

n_occur <- data.frame(table(df2018$reviewText))
n_occur[df2018$reviewText >=2 ,]
duplicated2018 = df2018[df2018$reviewText %in% n_occur$Var1[n_occur$Freq >=2],]
View(duplicated2018)

 
# Duplicated reviews from different users 
dup2008 <- duplicated2008 %>% distinct(reviewerName, .keep_all = T)
dup2018 <- duplicated2018 %>% distinct(reviewerName, .keep_all = T)

# Verified reviews in dup2008 
table(dup2008$verified)
table(dup2018$verified)
table(df2008$verified)
table(df2018$verified)
View(dup2008)
View(dup2018)

d = as.data.frame(table(dup2008$reviewText))
View(d)
d2 = as.data.frame(table(dup2018$reviewText))
View(d2)
# Plot 
dup2008 %>% ggplot(aes(x=monthnum)) + 
  scale_x_discrete(labels = c('Jun','Jul','Aug','Sep')) +
  geom_bar(aes(fill=verified), position="dodge") +
  labs(title="Number of Similar Reviews in 2008 (Jun-Sep)",
       x="Month", y="Number of Review",
       fill="Verified Review") +
  theme_classic()+
  scale_fill_manual(values=c("#FF9900","#146EB4"))+
  geom_text(stat="count", aes(label=..count..,group=verified), cex=2.5, 
            vjust=-0.5, hjust=0.5, position=position_dodge(width=1))+
  theme(axis.text = element_text(size=10),
        axis.title = element_text(size=12),
        legend.text = element_text(size=10),
        plot.title = element_text(size=13, face = "bold"),
        legend.background = element_rect(color="black", fill="white"))

dup2018 %>% ggplot(aes(x=monthnum)) + 
  scale_x_discrete(labels = c('Jun','Jul','Aug','Sep')) +
  geom_bar(aes(fill=verified), position="dodge") +
  labs(title="Number of Similar Reviews in 2018 (Jun-Sep)",
       x="Month", y="Number of Review",
       fill="Verified Review") +
  theme_classic()+
  scale_fill_manual(values=c("#FF9900","#146EB4"))+
  geom_text(stat="count", aes(label=..count..,group=verified), cex=2.5, 
            vjust=-0.5, hjust=0.5, position=position_dodge(width=1))+
  theme(axis.text = element_text(size=10),
        axis.title = element_text(size=12),
        legend.text = element_text(size=10),
        plot.title = element_text(size=13, face = "bold"),
        legend.background = element_rect(color="black", fill="white"))


d2 <- d2 %>% distinct(Var1, .keep_all = T)

# Install
install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes
install.packages("syuzhet") # for sentiment analysis
# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("syuzhet")
install.packages("wordcloud2")
library(wordcloud2)
df2008$reviewText = tolower(df2008$reviewText)
df2018$reviewText = tolower(df2018$reviewText)

dup2018 %>% ggplot(aes(x=monthnum)) + 
  scale_x_discrete(labels = c('Jun','Jul','Aug','Sep')) +
  geom_bar(aes(fill=verified), position="dodge") +
  labs(title="Number of Similar Reviews in 2018 (Jun-Sep)",
       x="Month", y="Number of Review",
       fill="Verified Review") +
  theme_classic()+
  scale_fill_manual(values=c("#FF9900","#146EB4"))+
  geom_text(stat="count", aes(label=..count..,group=verified), cex=2.5, 
            vjust=-0.5, hjust=0.5, position=position_dodge(width=1))+
  theme(axis.text = element_text(size=10),
        axis.title = element_text(size=12),
        legend.text = element_text(size=10),
        plot.title = element_text(size=13, face = "bold"),
        legend.background = element_rect(color="black", fill="white"))


# Create word cloud for similar reviews in 2018 

install.packages("tm")
library(tm)

#### word summary ####
install.packages("tidytext")
library(tidytext)
unique_dup2018 <- dup2018 %>%
  ungroup() %>%
  select(c("overall", "verified", "reviewTime", "reviewerID", "asin", "reviewText")) %>%
  unnest_tokens(word, reviewText) %>%
  filter(!word %in% stop_words$word, str_detect(word, "^[a-z']+$"))

unique_dup2008 <- dup2008 %>%
  ungroup() %>%
  select(c("overall", "verified", "reviewTime", "reviewerID", "asin", "reviewText")) %>%
  unnest_tokens(word, reviewText) %>%
  filter(!word %in% stop_words$word, str_detect(word, "^[a-z']+$"))

install.packages("textdata")
library(textdata)

#using AFINN lexicon
afinn <- get_sentiments("afinn")  

#using NRC lexicon
nrc <- get_sentiments("nrc")

0

summary_unique_dup2018 <- unique_dup2018 %>% ungroup() %>% 
  select(1:6) %>% 
  inner_join(afinn, by="word") %>% group_by(word) %>% 
  summarise(word=word, value=value, word_count=n()) %>%
  distinct() %>% arrange(desc(word_count))
summary_unique_dup2008 <- unique_dup2008 %>% ungroup() %>% 
  select(1:6) %>% 
  inner_join(afinn, by="word") %>% group_by(word) %>% 
  summarise(word=word, value=value, word_count=n()) %>%
  distinct() %>% arrange(desc(word_count))

wordcloud(words = summary_unique_dup2018$word, freq = summary_unique_dup2018$word_count, 
          max.words=100, colors=rep(amzn_cols), scale = c(3,0.5), random.order = F)

wordcloud(words = summary_unique_dup2008$word, freq = summary_unique_dup2008$word_count, 
          max.words=100, colors=rep(amzn_cols), scale = c(3,0.5), random.order = F)


View(unique_dup2008)
#Create a vector containing only the text
tx = unique_dup2008$reviewText
text <- unique_dup2018$reviewText 

# Create a corpus  
docs <- Corpus(VectorSource(text))
dc = Corpus(VectorSource(tx))
# Clean the text
docs <- docs %>% 
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))

dc <- dc %>% 
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))

# Create document term matrix
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

dtm08 <- TermDocumentMatrix(dc) 
matrix08 <- as.matrix(dtm08) 
words08 <- sort(rowSums(matrix08),decreasing=TRUE) 
df08 <- data.frame(word = names(words08),freq=words08)

# Generate word cloud
amzn_cols <- c("#EDC57E","#FF9900",
               "#45ACCB", "#00A8E1", "#008DFF", "#4C8A9F", "#146EB4", "#0F3765",
               "#232F3E",rep("#313335",2), rep("#37475A",2), "#131A22")
set.seed(1234) # for reproducibility 
wordcloud(words = df$word, freq = df$freq, min.freq = 1,           
          max.words=200, random.order=FALSE, rot.per=0.35,            
          colors=rep(amzn_cols))

wordcloud2(df, size = 0.7,color = rep(amzn_cols))

wordcloud2(df08, size = 1.5,color = rep(amzn_cols))


# Count misspelled words
library(qdap)

# which_misspelled
n_misspelled <- sapply(fivestar2018$reviewText, function(x){
  length(which_misspelled(x, suggest = FALSE))
})

n_misspelled <- sapply(fivestar2008$reviewText, function(x){
  length(which_misspelled(x, suggest = FALSE))
})

misspelled = data.frame(fivestar2018$reviewText, n_misspelled, row.names = NULL)
misspelled2 = data.frame(fivestar2008$reviewText, n_misspelled, row.names = NULL)
View(misspelled2)
# check_spelling
misspelled <- misspelled %>% arrange(desc(n_misspelled)) %>%
  filter(n_misspelled >'0')
misspelled2 <- misspelled2 %>% arrange(desc(n_misspelled)) %>%
  filter(n_misspelled >'0')

d