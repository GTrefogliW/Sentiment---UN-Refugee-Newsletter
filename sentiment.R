library(tidyverse)
library(rvest)
library(tidytext)
library(nametagger)
library(udpipe)

# setwd("C://Users//guill//OneDrive//Documents//homework-3-GTrefogliW")

url <- "https://www.unhcr.org/refugeebrief/the-refugee-brief-11-february-2022/"
request <- read_html(url)

# preparing the data
content <- html_node(request, "#left-area")
paragraphs <- html_nodes(content, "p")
text_list <- html_text(paragraphs)
text <- paste(text_list, collapse = "")
text_df <- tibble(text = text)
word_tokens_df <- unnest_tokens(text_df, word_tokens,  text, token = "words")
no_sw_df <- anti_join(word_tokens_df, stop_words, by = c("word_tokens" = "word"))

# sentiment analysis options
sentiment_nrc <-   get_sentiments("nrc")   # manually created via crowdsourcing, ten categories, not unique!
sentiment_afinn <- get_sentiments("afinn") # product of one researcher, pos/neg integer values
sentiment_bing <-  get_sentiments("bing")  # built on online reviews, pos/neg only

for (s in c("nrc", "afinn", "bing")) {
  no_sw_df <- no_sw_df %>%
    left_join(get_sentiments(s), by = c("word_tokens" = "word")) %>%
    plyr::rename(replace = c(sentiment = s, value = s), warn_missing = FALSE)
}

# choosing one sentiment analysis
plot_sentim <- ggplot(data = filter(no_sw_df, !is.na(nrc))) +
  geom_histogram(aes(nrc), stat = "count") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(title = "Sentimental Analysis for Last Issue") +
  theme_bw()

#ggsave(plot_sentim, file = paste0("question1_plot_sentiment.png"), width = 14, height = 10)

# We can see that the most prominent sentiment in the article is fear, followed by positive 
# and negative feelings. Other less prominent sentiments are anger, sadness, and trust. 
# By looking at the content of the article this finding makes sense: the issue is reporting 
# news associated with conflicts and other social and economic problems caused by displacements.

# countries analysis
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

#ggsave(plot_country, file = paste0("question1__plot_countries.png"), width = 14, height = 10)

# It is worth noting that the countries that appear the most in the article are Venezuela, 
# Somalia, Uganda, Mexico, Kenya, Greece, Ethippia, Cameron, Albania, and Afganistan.
