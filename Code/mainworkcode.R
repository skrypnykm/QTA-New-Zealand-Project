#remove all lists
rm(list=ls())

#detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

#set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

lapply(c("tidyverse",
         "ggplot2",
         "quanteda", 
         "lubridate",
         "quanteda.dictionaries",
         "quanteda.textmodels", 
         "quanteda.textstats",
         "quanteda.textplots",
         "dplyr",
         "gridExtra",
         "xtable", # For parallel processing
         "readtext", # for reading in text data
         "stringi", # for working with character strings
         "textstem",
         "stm",
         "gridExtra"), 
       pkgTest)

library(quanteda)
library(stm) # STM
library(wordcloud)
library(ggplot2)
if(!require(devtools)) install.packages("devtools")
library("stmBrowser")

#load data
data <- read.csv("nz20172023.csv")
#(delete url)
data <- data[, !names(data) == "url"]

#NUMBER OF SPEECHES PER YEAR
data$date <- as.Date(data$date)
data$year <- format(data$date, "%Y")

#count the number of speeches per year
speeches_per_year <- table(data$year)
speeches_per_year <- as.data.frame(speeches_per_year)
names(speeches_per_year) <- c("Year", "Speeches")

#plot
per_year <- ggplot(data = speeches_per_year, aes(x = Year, y = Speeches)) +
  geom_bar(stat = "identity", fill = "brown2") +
  geom_text(aes(label = Speeches), vjust = -0.5, size = 4, color = "black") +
  labs(title = "Number of Speeches per Year",
       x = "Year",
       y = "Number of Speeches") +
  theme_minimal()

#CORPUS
corpus <- corpus(data, 
                 text_field = "text")
summary(corpus, 5)

#TEXTUAL STATISTICS 
#n documents
ndoc(corpus)

#mean n sentences 
mean(nsentence(corpus))

#Type-to-Token ratio
meantype <- mean(ntype(corpus))
meantoken <- mean(ntoken(corpus))
ttr <- meantype / meantoken #TTR 0.3443986
ttr


#PREPROCESSING
toks <- quanteda::tokens(corpus,
               include_docvars = TRUE) #keep hold of all METADATA

#lower case
toks <- tokens_tolower(toks)

#cleaning up
toks <- quanteda::tokens(toks,
               remove_numbers = TRUE,
               remove_punct = TRUE,
               remove_symbols = TRUE,
               remove_separators = TRUE,
               remove_url = TRUE)

toks <- tokens_remove(toks, '[\\p{P}\\p{S}]', valuetype = 'regex', padding = TRUE)

#remove stopwords
stop_list <- stopwords("english")
toks <- tokens_remove(toks, stop_list)

toks <- tokens_remove(toks, '[\\p{P}\\p{S}]', valuetype = 'regex', padding = TRUE)

#collocations
unsup_col <- textstat_collocations(toks,
                                   method = "lambda",
                                   size = 2,
                                   min_count = 10,
                                   smoothing = 0.5)

unsup_col <- unsup_col[order(-unsup_col$count),] #sort detected collocations by count (descending)

#identifying z-score to distinguish valuable collocations
mean(unsup_col$z)
toks <- tokens_compound(toks,
                        pattern = unsup_col[unsup_col$z > 14,])

toks <- tokens_remove(toks, "") #remove the whitespace placeholders

#DOCUMENT FEATURE MATRIX (DFM)
dfm <- dfm(toks,
             remove_numbers = TRUE,
             remove_punct = TRUE,
             remove_symbols = TRUE,
             remove_hyphens = TRUE,
             remove_separators = TRUE,
             remove_url = TRUE)

dfm <- dfm_select(dfm, pattern = stopwords("en"), selection = "remove") #remove stop words on cleaned dfm
dfm@docvars$original_text <- as.character(corpus)

dfm <- readRDS('/Data/dfm.rds') #ensuring reproducibility of results

#FEATURE FREQUENCY
#2017
dfm_2017 <- dfm_subset(dfm, year == "2017")
dfm_freq <- textstat_frequency(dfm_2017, n = 30)
#sort features by frequency in descending order
dfm_freq$feature <- with(dfm_freq, reorder(feature, -frequency))
#plot 
ff2017 <- ggplot(dfm_freq, aes(x = feature, y = frequency)) + ggtitle("Feature Frequency of 2017 Speeches") +
  geom_point() + theme_minimal() + theme(axis.text.x = element_text(angle = 75, hjust = 1))

#2018
dfm_2018 <- dfm_subset(dfm, year == "2018")
dfm_freq <- textstat_frequency(dfm_2018, n = 30)
#sort features by frequency in descending order
dfm_freq$feature <- with(dfm_freq, reorder(feature, -frequency))
# plot 
ff2018 <-ggplot(dfm_freq, aes(x = feature, y = frequency)) + ggtitle("Feature Frequency of 2018 Speeches") +
  geom_point() + theme_minimal() + theme(axis.text.x = element_text(angle = 75, hjust = 1))

#2019
dfm_2019 <- dfm_subset(dfm, year == "2019")
dfm_freq <- textstat_frequency(dfm_2019, n = 30)
#sort
dfm_freq$feature <- with(dfm_freq, reorder(feature, -frequency))
#plot
ff2019 <-ggplot(dfm_freq, aes(x = feature, y = frequency)) + ggtitle("Feature Frequency of 2019 Speeches") +
  geom_point() + theme_minimal() + theme(axis.text.x = element_text(angle = 75, hjust = 1))

#2020
dfm_2020 <- dfm_subset(dfm, year == "2020")
dfm_freq <- textstat_frequency(dfm_2020, n = 30)
#sort features by frequency in descending order
dfm_freq$feature <- with(dfm_freq, reorder(feature, -frequency))
#plot
ff2020 <-ggplot(dfm_freq, aes(x = feature, y = frequency)) + ggtitle("Feature Frequency of 2020 Speeches") +
  geom_point() + theme_minimal() + theme(axis.text.x = element_text(angle = 75, hjust = 1))

#2021
dfm_2021 <- dfm_subset(dfm, year == "2021")
dfm_freq <- textstat_frequency(dfm_2021, n = 30)
#sort features by frequency in descending order
dfm_freq$feature <- with(dfm_freq, reorder(feature, -frequency))
#plot 
ff2021 <-ggplot(dfm_freq, aes(x = feature, y = frequency)) + ggtitle("Feature Frequency of 2021 Speeches") +
  geom_point() + theme_minimal() + theme(axis.text.x = element_text(angle = 75, hjust = 1))

#2022
dfm_2022 <- dfm_subset(dfm, year == "2022")
dfm_freq <- textstat_frequency(dfm_2022, n = 30)
#sort features by frequency in descending order
dfm_freq$feature <- with(dfm_freq, reorder(feature, -frequency))
#plot 
ff2022 <-ggplot(dfm_freq, aes(x = feature, y = frequency)) + ggtitle("Feature Frequency of 2022 Speeches") +
  geom_point() + theme_minimal() + theme(axis.text.x = element_text(angle = 75, hjust = 1))

#2023
dfm_2023 <- dfm_subset(dfm, year == "2023")
dfm_freq <- textstat_frequency(dfm_2023, n = 30)
#sort features by frequency in descending order
dfm_freq$feature <- with(dfm_freq, reorder(feature, -frequency))
#plot 
ff2023 <- ggplot(dfm_freq, aes(x = feature, y = frequency)) + ggtitle("Feature Frequency of 2023 Speeches") +
  geom_point() + theme_minimal() + theme(axis.text.x = element_text(angle = 75, hjust = 1))

ff20172019 <- grid.arrange(ff2017, ff2018, ff2019, ncol = 3)
ff20202023 <- grid.arrange(ff2020, ff2021, ff2022, ff2023, ncol = 2)

#KEYNESS

#2018
#subsetting for 2017 and 2018 (to compare two years)
dfm_keyness <- dfm_subset(dfm, year %in% c("2017", "2018"))
dfm_keyness <- dfm_group(dfm_keyness, groups = year)

#calculate keyness and set 2018 as the target year
keyness_stat <- textstat_keyness(dfm_keyness, target = "2018")

#plot
key20172018 <- textplot_keyness(keyness_stat, labelsize = 3, col = c("brown2", "grey"))

#2019
#subsetting for 2018 and 2019 (to compare two years)
dfm_keyness <- dfm_subset(dfm, year %in% c("2018", "2019"))
dfm_keyness <- dfm_group(dfm_keyness, groups = year)

#calculate keyness and set 2019 as the target year
keyness_stat <- textstat_keyness(dfm_keyness, target = "2019")

#plot
key20182019 <- textplot_keyness(keyness_stat, labelsize = 3, col = c("brown2", "grey"))

#2020
#subsetting for 2019 and 2020 (to compare two years)
dfm_keyness <- dfm_subset(dfm, year %in% c("2019", "2020"))
dfm_keyness <- dfm_group(dfm_keyness, groups = year)

#calculate keyness and set 2019 as the target year
keyness_stat <- textstat_keyness(dfm_keyness, target = "2020")

#plot
key20192020 <- textplot_keyness(keyness_stat, labelsize = 3, col = c("brown2", "grey"))

#2021
#subsetting for 2020 and 2021 (to compare two years)
dfm_keyness <- dfm_subset(dfm, year %in% c("2020", "2021"))
dfm_keyness <- dfm_group(dfm_keyness, groups = year)

#calculate keyness and set 2021 as the target year
keyness_stat <- textstat_keyness(dfm_keyness, target = "2021")

#plot
key20202021 <- textplot_keyness(keyness_stat, labelsize = 3, col = c("brown2", "grey"))

#2022

#subsetting for 2021 and 2022 (to compare two years)
dfm_keyness <- dfm_subset(dfm, year %in% c("2021", "2022"))
dfm_keyness <- dfm_group(dfm_keyness, groups = year)

#calculate keyness and set 2022 as the target year
keyness_stat <- textstat_keyness(dfm_keyness, target = "2022")

#plot
key20212022 <- textplot_keyness(keyness_stat, labelsize = 3, col = c("brown2", "grey"))

#2023
#subsetting for 2022 and 2023 (to compare two years)
dfm_keyness <- dfm_subset(dfm, year %in% c("2022", "2023"))
dfm_keyness <- dfm_group(dfm_keyness, groups = year)

#calculate keyness and set 2023 as the target year
keyness_stat <- textstat_keyness(dfm_keyness, target = "2023")

#plot
key20222023 <- textplot_keyness(keyness_stat, labelsize = 3, col = c("brown2", "grey"))

#combined plots
grid.arrange(key20172018, key20182019, ncol = 2)
grid.arrange(key20192020, key20202021, ncol = 2)
grid.arrange(key20212022, key20222023, ncol = 2)


#LEXICAL DIVERSITY
ld <- textstat_lexdiv(dfm, measure = "TTR")
ld
mean(ld$TTR, na.rm = TRUE)

#READABILITY (FLESCH-KINCAID)
rd <- textstat_readability(corpus, measure = "Flesch.Kincaid") 
mean(rd$Flesch.Kincaid, na.rm = TRUE)

#STRUCTURAL TOPIC MODELLING (STM)
stmdfm <- convert(dfm, to = "stm") 
set.seed(123)
modelFit <- stm(documents = stmdfm$documents,
                vocab = stmdfm$vocab,
                K = 30,
                prevalence = ~ s(as.numeric(date)),
                data = stmdfm$meta,
                max.em.its = 500,
                init.type = "Spectral",
                seed = 1234,
                verbose = TRUE)

modelFit <- readRDS('/Data/modelFit.rds') #ensuring reproducibility of results

#topic interpretation with `labelTopics`

labelTopics(modelFit)

#topic interpretation with `plot.STM()`

#plot 1 (FREX)
plot.STM(modelFit, 
         type = "summary", 
         labeltype = "frex",
         text.cex = 0.7,
         main = "Topic prevalence and top terms (FREX)")

#plot 2 (highest probability)
plot.STM(modelFit, 
         type = "summary", 
         labeltype = "prob",
         text.cex = 0.7,
         main = "Topic prevalence and top terms (Highest Probability)")

#plot 3 (lift)
plot.STM(modelFit, 
         type = "summary", 
         labeltype = "lift",
         text.cex = 0.7,
         main = "Topic prevalence and top terms (Lift)")

#ploy 4 (score)
plot.STM(modelFit, 
         type = "summary", 
         labeltype = "score",
         text.cex = 0.7,
         main = "Topic prevalence and top terms (Score)")

#checking texts with foundThoughs function

#check the number of texts and documents modeled
num_texts <- length(dfm@docvars$original_text)
num_documents <- nrow(modelFit$theta)

#ensure that the numbers match
if (num_texts != num_documents) {
  #which texts are missing?
  unmatched_texts <- setdiff(1:num_texts, 1:num_documents)
  #filter the texts to consider only those that have matching documents
  texts <- dfm@docvars$original_text[1:num_documents]
  #read the texts from a given topic 
  findThoughts(modelFit, texts = texts, topics = 20, n = 5)
} else {
  findThoughts(modelFit, texts = dfm@docvars$original_text, topics = 20, n = 5)
}

#WORDCLOUDS 

#china
cloud(modelFit,
      topic = 18,
      scale = c(4, 0.5),
      max.words = 50)

#maori
cloud(modelFit,
      topic = 26,
      scale = c(5, 0.5),
      max.words = 50)

#health
cloud(modelFit,
      topic = 9,
      scale = c(5, 0.5),
      max.words = 50)

#health
cloud(modelFit,
      topic = 11,
      scale = c(5, 0.5),
      max.words = 50)

#covid
cloud(modelFit,
      topic = 29,
      scale = c(4, 0.5),
      max.words = 50)

#nuclear
cloud(modelFit,
      topic = 24,
      scale = c(4, 0.5),
      max.words = 50)

#energy/climate
cloud(modelFit,
      topic = 21,
      scale = c(4, 0.5),
      max.words = 50)

#trade
cloud(modelFit,
      topic = 18,
      scale = c(4, 0.5),
      max.words = 50)

#economy
cloud(modelFit,
      topic = 14,
      scale = c(4, 0.5),
      max.words = 50)

#wellbeing
cloud(modelFit,
      topic = 22,
      scale = c(4, 0.5),
      max.words = 50)

#pacific people
cloud(modelFit,
      topic = 23,
      scale = c(4, 0.5),
      max.words = 50)

#family
cloud(modelFit,
      topic = 7,
      scale = c(4, 0.5),
      max.words = 50)


#COVID PREVALENCE 
#metadata containing dates
dates <- stmdfm$meta$date

#topic prevalence
topicprevalence <- as.data.frame(modelFit$theta)

#combine
topicprevalence <- cbind(dates, topicprevalence)

#convert date format
topicprevalence$date <- as.Date(topicprevalence$date)

#filter for the topic of interest
topicprevalence29 <- topicprevalence[, c("date", "V29")]

#group by year and calculate mean prevalence for the topic of interest
topicprevalence29 <- topicprevalence29 %>%
  mutate(year = lubridate::year(date)) %>%
  group_by(year) %>%
  summarise(meanprevalence29 = mean(V29, na.rm = TRUE))

#plot the prevalence over years
ggplot(topicprevalence29, aes(x = year, y = meanprevalence29)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Mean Prevalence of Topic 29 (COVID-19)") +
  ggtitle("Prevalence of Topic 29 (COVID-19) Over the Years")

#MAORI PREVALENCE 

#filter for the topic of interest
topic_prevalence_topic26 <- topic_prevalence[, c("date", "V26")]

#group by year and calculate mean prevalence for the topic of interest
topic_prevalence_topic26 <- topic_prevalence_topic26 %>%
  mutate(year = lubridate::year(date)) %>%
  group_by(year) %>%
  summarise(mean_prevalence_topic_26 = mean(V26, na.rm = TRUE))

#plot the prevalence over years
ggplot(topic_prevalence_topic26, aes(x = year, y = mean_prevalence_topic_26)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Mean Prevalence of Topic 26 (Maori)") +
  ggtitle("Prevalence of Topic 26 (Maori) Over the Years")
