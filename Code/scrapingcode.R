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
         "rvest",
         "ggplot2",
         "quanteda", 
         "lubridate",
         "quanteda.dictionaries",
         "quanteda.textmodels", 
         "quanteda.textstats",
         "quanteda.textplots",
         "dplyr",
         "gridExtra",
         "xtable", 
         "readtext", 
         "stringi", 
         "textstem",
         "stm",
         "gridExtra",
         "wordcloud"), 
       pkgTest)

#2017-2020

#base url
base_url <- "https://www.beehive.govt.nz/search?f%5B0%5D=content_type_facet%3Aspeech&f%5B1%5D=government_facet%3A6203&page=0"

#initializing empty data frame to store the results
df <- data.frame(url = character(), date = character(), text = character(), stringsAsFactors = FALSE)

#specifying the total number of pages
total_pages <- 20

for (page_number in 0:total_pages) {
  cat("Scraping page:", page_number, "of", total_pages, "\n")
  
  #construct the URL for each page
  page_url <- ifelse(page_number == 0, base_url, paste0(base_url, "&", "page=", page_number))
  
  #scrape the current page
  page <- read_html(page_url)
  
  #hyperlinks
  hyperlinks <- html_attr(html_nodes(page, "h3.field-content a[href^='/speech/']"), "href")
  
  #full hyperlinks
  full_hyperlinks <- paste0("https://www.beehive.govt.nz", hyperlinks)
  
  #scrape dates
  dates <- html_text(html_nodes(page, ".views-field-field-issue-date .field-content time"))
  
  #scrape text for each speech on the current page
  text_list <- lapply(full_hyperlinks, function(link) {
    page <- read_html(link)
    text <- html_text(html_nodes(page, "div.field--name-body p"))
    text <- paste(text, collapse = " ")
    return(text)
  })
  
  #create a temporary df for this page
  temp_df <- data.frame("url" = full_hyperlinks, "date" = dates, "text" = unlist(text_list), stringsAsFactors = FALSE)
  
  #append the results to the main df
  df <- rbind(df, temp_df)
  
}

write.csv(df, "nz20172020.csv", row.names = FALSE)

#2020-2023

#scraping page 1 separately due to the website's specifics

base_url <- "https://www.beehive.govt.nz/search?f%5B0%5D=content_type_facet%3Aspeech&f%5B1%5D=government_facet%3A6203&f%5B2%5D=issue_date_facet%3A2020&page=1"

page <- read_html(base_url)

hyperlinks <- html_attr(html_nodes(page, "h3.field-content a[href^='/speech/']"), "href")

full_hyperlinks <- paste0("https://www.beehive.govt.nz", hyperlinks)

dates <- html_text(html_nodes(page, ".views-field-field-issue-date .field-content time"))

text_list <- lapply(full_hyperlinks, function(link) {
  page <- read_html(link)
  text <- html_text(html_nodes(page, "div.field--name-body p"))
  text <- paste(text, collapse = " ")
  return(text)
})

df <- data.frame(
  url = full_hyperlinks,
  text = unlist(text_list),
  date = dates,
  stringsAsFactors = FALSE
)

write.csv(df, '1nz20172020')

#other pages
base_url <- "https://www.beehive.govt.nz/search?f%5B0%5D=content_type_facet%3Aspeech&f%5B1%5D=government_facet%3A6455"

df <- data.frame(url = character(), date = character(), text = character(), stringsAsFactors = FALSE)

total_pages <- 16
for (page_number in 1:total_pages) {
  cat("Scraping page:", page_number, "of", total_pages, "\n")
  
page_url <- ifelse(page_number == 0, base_url, paste0(base_url, "&", "page=", page_number))
  
page <- read_html(page_url)
  
hyperlinks <- html_attr(html_nodes(page, "h3.field-content a[href^='/speech/']"), "href")
  
full_hyperlinks <- paste0("https://www.beehive.govt.nz", hyperlinks)
  
dates <- html_text(html_nodes(page, ".views-field-field-issue-date .field-content time"))

text_list <- lapply(full_hyperlinks, function(link) {
    page <- read_html(link)
    text <- html_text(html_nodes(page, "div.field--name-body p"))
    text <- paste(text, collapse = " ")
    return(text)
  })
  

if (length(full_hyperlinks) == length(dates) && length(full_hyperlinks) == length(text_list)) {
    temp_df <- data.frame("url" = full_hyperlinks, "date" = dates, "text" = unlist(text_list), stringsAsFactors = FALSE)

    df <- rbind(df, temp_df)
  } else {
    cat("Skipping page", page_number, "due to mismatched lengths.\n")
  }
}

write.csv(df, "nz20202023.csv", row.names = FALSE)

#appending into a final dataframe
nz20172020 <- read.csv('nz20172020')
nz20202023 <- read.csv('nz20202023')
1nz20172020 <- read.csv('1nz20172020.csv')

nz20172023 <- rbind(1nz20172020, nz20172020, nz20202023)
nz20172023$date <- as.Date(nz20172023$date, format = "%d %B %Y")
nz20172023 <- nz20172023[order(nz20172023$date), ]
rownames(nz20172023) <- NULL

if (any(is.na(nz20172023))) {
  print("There are NA values in the dataframe.")
} else {
  print("There are no NA values in the dataframe.")
}

write.csv(nz20172023, "nz20172023.csv", row.names = FALSE)
