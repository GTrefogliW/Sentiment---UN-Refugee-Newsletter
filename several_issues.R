library(tidyverse)
library(rvest)
library(tidytext)
library(here)
library(udpipe)
library(pdftools)
library(tidytext)
library(nametagger)
library(countrycode)

# websites and titles as dataframe
f_web <- "https://www.unhcr.org/refugeebrief/the-refugee-brief-11-february-2022/"
f_web <- read_html(f_web)
list <- html_node(f_web, "#sidebar")
issues <- html_nodes(list, "a")
urls <- html_attr(issues, "href")
titles <- html_text(issues, "href")
issues_df <- data.frame(urls, titles)

#view(issues_df)

# plots and analysis for countries
n <- length(urls)
model <- nametagger_download_model("english-conll-140408", model_dir = tempdir())

one_file <- list()

for (i in 1:n){
  
  # preparing the data
  request <- read_html(issues_df[[i,1]])
  content <- html_node(request, "#left-area")
  paragraphs <- html_nodes(content, "p")
  text_list <- html_text(paragraphs)
  text <- paste(text_list, collapse = "")
  text_df <- tibble(text = text)
  word_tokens_df <- unnest_tokens(text_df, word_tokens,  text, token = "words")
  no_sw_df <- anti_join(word_tokens_df, stop_words, by = c("word_tokens" = "word"))
  write_lines(text_list, file = paste0(issues_df[[i,2]],".txt"))
  one_file[text] <- text
  
  # sentiment analysis options
  sentiment_nrc <-   get_sentiments("nrc")   # manually created via crowdsourcing, ten categories
  sentiment_afinn <- get_sentiments("afinn") # pos/neg integer values
  sentiment_bing <-  get_sentiments("bing")  # built on online reviews, pos/neg only
  
  for (s in c("nrc", "afinn", "bing")) {
    no_sw_df <- no_sw_df %>%
      left_join(get_sentiments(s), by = c("word_tokens" = "word")) %>%
      plyr::rename(replace = c(sentiment = s, value = s), warn_missing = FALSE)
  }
  
  # getting the sentimental analysis plor for nrc
  plot_sentim <- ggplot(data = filter(no_sw_df, !is.na(nrc))) +
    geom_histogram(aes(nrc), stat = "count") +
    scale_x_discrete(guide = guide_axis(angle = 45)) +
    labs(title = issues_df[[i,2]]) +
    theme_bw()
  
  #ggsave(plot_sentim, file = paste0("question2_plot_sentiment_",issues_df[[i,2]],"_.png"), width = 14, height = 10)
  
  # getting the countries
  parsed <- udpipe(text, "english")
  countries_names <- unique(countryname_dict$country.name.en)
  countries_df <- tibble(countries_names = countries_names)
  
  countries_issues <- parsed %>%
    select("lemma", "upos") %>%
    filter(upos == "PROPN",
           lemma %in% countries_df$countries_names) %>% 
    group_by(lemma) %>% 
    summarise(n = n())
  
  countries_issues <- as.data.frame(countries_issues)
  
  plot_country <- countries_issues %>% 
    ggplot(aes(x = reorder(lemma, n), y = n)) + 
    geom_bar(stat = "identity") + 
    coord_flip() +
    labs(title = "Countries Discussed in Article",
         x = "Country",
         y = "# Times in Article") +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme_bw()
  
  ggsave(plot_country, file = paste0("question2_plot_countries_",issues_df[[i,2]],"_.png"), width = 14, height = 10)
  
}

dat <- tibble(content = one_file)
one_file_df <- unnest_tokens(dat, word_tokens, content, token = "words")
dtfs <- anti_join(one_file_df, stop_words, by = c("word_tokens" = "word"))
write.csv(dtfs,"file.csv", row.names = FALSE)

# plots and analysis for the 10 issues

# sentiment analysis options
sentiment_nrc <-   get_sentiments("nrc")   
sentiment_afinn <- get_sentiments("afinn") 
sentiment_bing <-  get_sentiments("bing") 

for (s in c("nrc", "afinn", "bing")) {
  dtfs <- dtfs %>%
    left_join(get_sentiments(s), by = c("word_tokens" = "word")) %>%
    plyr::rename(replace = c(sentiment = s, value = s), warn_missing = FALSE)
}

# getting the sentimental analysis plor for nrc
plot_sentim <- ggplot(data = filter(dtfs, !is.na(nrc))) +
  geom_histogram(aes(nrc), stat = "count") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(title = "Sentiment analysis for the 10 issues") +
  theme_bw()

ggsave(plot_sentim, file = paste0("question2_sentiment_10issues_.png"), width = 14, height = 10)

# takeaways:
# 'negative' is the most prominent sentiment in the 10 issues, followed by 'positive', and by 'fear'. 
# two other relevant sentiments are 'sadness' and 'trust'.

# getting countries
text <- paste(one_file, collapse = "")
parsed <- udpipe(text, "english")
countries_df <- tibble(countries_names = countries_names)

countries_issues <- parsed %>%
  select("lemma", "upos") %>%
  filter(upos == "PROPN",
         lemma %in% countries_df$countries_names) %>% 
  group_by(lemma) %>% 
  summarise(n = n())

countries_issues <- as.data.frame(countries_issues)

plot_country <- countries_issues %>% 
  ggplot(aes(x = reorder(lemma, n), y = n)) + 
  geom_bar(stat = "identity") + 
  coord_flip() +
  labs(title = "Countries Discussed in the 10 Articles",
       x = "Country",
       y = "# Times in the Articles") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw()

#ggsave(plot_country, file = paste0("question2_plot_countries_10issues_.png"), width = 14, height = 10)

# The analysis for the 10 issues is telling us that Ethiopia is the country which appears the most 
# in the newsletter, followed very closed by Afganistan (both more than 15 times). 
# The third country is Belarus, and then Mexico. The complete list can be seen in the plot. 
