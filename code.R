library(tidytext)
library(tidyverse)
library(scales)
library(gridExtra)
library(wordcloud2)
library(treemapify)
library(RColorBrewer)
library(forcats)
library(stringr)

simp <- read.csv('/Users/jazzopardi/Desktop/R/699/Assignment5/simpsons_script_lines.csv')

View(simp)

one <- simp[simp$episode_id == 1, ] # simspons roasting on an open fire

two <- simp[simp$episode_id == 139, ] # marge be not proud

three <- simp[simp$episode_id == 188, ] # miracle on evergreen terrace

four <- simp[simp$episode_id == 235, ] # grift of the magi

five <-simp[simp$episode_id == 256, ] # Skinner's Sense of Snow

six <- simp[simp$episode_id == 275, ] # she of little faith

seven <- simp[simp$episode_id == 320, ] # this the fifteenth season

eight <- simp[simp$episode_id == 365, ] # simpsons christmas story

nine <- simp[simp$episode_id == 387, ] # kill gil

ten <- simp[simp$episode_id == 428, ] # Burns and the Bees 

eleven <- simp[simp$episode_id == 472, ] # the fight before christmas

twelve <- simp[simp$episode_id == 495, ] # holidays of future passed

thirteen <- simp[simp$episode_id == 538, ] # white christmas blues

fourteen <- simp[simp$episode_id == 561, ] # I won't be home for christmas

simpsons_episode <- rbind(one, two, three, four, five,six,seven, eight,nine,ten,eleven,twelve,
                          thirteen,fourteen)

# only goes up to Season 26, episode 9

# characters

simpsons_episode <- simpsons_episode[!(is.na(simpsons_episode$raw_character_text) | simpsons_episode$raw_character_text==""), ]

char <- data.frame(sort(table(simpsons_episode$raw_character_text), decreasing = TRUE)[1:10])


ggplot(char, aes(area = Freq, fill = Var1, label = Var1)) +
  geom_treemap() + geom_treemap_text(place = 'center') + 
  scale_fill_manual(values=c('#107DC0','#FFD521', '#F65132', '#FFFFFF', '#D6E69F',
                             '#D1B271','#424F46','#F7B686','#007C7A','#00947E','#D3AF7F')) + 
  theme(legend.position = 'none')

# locations

loc <- simpsons_episode %>%
  group_by(episode_id, raw_location_text) %>%
  summarise(n = n()) %>%
  filter(n == max(n)) 

simpsons_episode$raw_location_text

loc2 <- simpsons_episode %>%
  group_by(episode_id, raw_location_text) %>%
  summarise(n = n()) %>%
  filter(n == min(n)) 

pie <- data.frame(sort(table(loc$raw_location_text)))

colnames(pie) <- c("Location","Count")

ggplot(pie, aes(x="", y=Count, fill=Location)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y") + theme_void() + scale_fill_manual(values = c('#107DC0','#FFD521'))

# sentiment analysis

tidy_simpsons <- simpsons_episode %>%
  select(episode_id,spoken_words) %>% 
  unnest_tokens(word, spoken_words) %>%
  anti_join(stop_words) %>% # removing stop words
  inner_join(get_sentiments("afinn")) %>%
  inner_join(get_sentiments("bing"))


res <- tidy_simpsons %>%
  group_by(episode_id) %>%
  summarise(afinn_sent = sum(value)) %>%
  mutate(index = 1:14)


res_bing <- tidy_simpsons %>%
  group_by(episode_id,sentiment) %>%
  summarise(n = n()) %>%
  mutate(bing_sent = n - lag(n)) %>%
  na.omit(tidy_simpsons) %>%
  ungroup()%>%
  mutate(index = 1:14)

# sentiment across all episodes according to afinn library

#nb.cols <- 14
#mycolors <- colorRampPalette(brewer.pal(8, "Spectral"))(nb.cols)

#afinn <- ggplot(res, aes(index, afinn_sent, fill = factor(index))) + geom_col() + labs(title = 'Simpsons Christmas Episodes') + 
#  theme(legend.position = "none") + scale_fill_manual(values = mycolors)

#afinn <- ggplot(res, aes(as.numeric(index), afinn_sent)) + geom_point() + labs(title = 'Simpsons Christmas Episodes') + 
#  theme(legend.position = "none") + scale_fill_manual(values = mycolors) + geom_line(color = 'blue') + 
#  geom_hline(yintercept =  0, linetype = 'dotted')

# the same for bing

#bing <- ggplot(res_bing, aes(index, bing_sent, fill = factor(index))) + geom_col() + labs(title = 'Simpsons Christmas Episodes') + 
#  theme(legend.position = "none") + scale_fill_manual(values = mycolors)

#bing <- ggplot(res_bing, aes(as.numeric(index), bing_sent)) + geom_point() + labs(title = 'Simpsons Christmas Episodes') + 
#  theme(legend.position = "none") + scale_fill_manual(values = mycolors) + geom_line(color = 'red') + 
#  geom_hline(yintercept =  0, linetype = 'dotted')


sentiment_analysis <- left_join(res, res_bing, by = 'episode_id')

ggplot(sentiment_analysis, aes(as.numeric(index.x)), label = episode_id) + 
  geom_col(aes(y = afinn_sent, fill = 'Afinn')) +
  geom_col(aes(y = bing_sent, fill = 'Bing')) +
  geom_text(aes(y = bing_sent, label = episode_id),  position = position_dodge(0.9), vjust = 2) +
  scale_fill_manual(values=c('#FED90F','#70D1FE')) +
  labs(title = 'Simpsons Christmas Episodes', x = '', y = 'Count') +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

#grid.arrange(afinn,bing)

sum(res$afinn_sent) #  112 

sum(res_bing$bing_sent) # 6 


# a deeper look into the episodes that diverge

four_christmas_sentiment <- four %>%
  select(spoken_words) %>% 
  unnest_tokens(word, spoken_words) %>%
  anti_join(stop_words) %>%
  mutate(linenumber = row_number()) %>%
  inner_join(get_sentiments("bing")) %>%
  count(index = linenumber %/% 10, sentiment)%>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(sentiment = positive - negative)

afinn_four_christmas_sentiment <- four %>%
  select(spoken_words) %>% 
  unnest_tokens(word, spoken_words) %>%
  anti_join(stop_words) %>%
  mutate(linenumber = row_number()) %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(index = linenumber %/% 10) %>%
  summarise(sentiment = sum(value))

four_bing <- ggplot(four_christmas_sentiment, aes(index, sentiment)) +
  geom_col(show.legend = FALSE, fill = '#FFD521') + labs(title = "Episode 235 (Bing)") + geom_smooth(se = FALSE, color = 'blue')

four_afinn <- ggplot(afinn_four_christmas_sentiment, aes (index, sentiment)) +
  geom_col(fill = '#107DC0') + labs(title = 'Episode 235 (Afinn)', x = '') + geom_smooth(se = FALSE, color = 'yellow')

grid.arrange(four_bing, four_afinn) # can combine into one like above

fourteen_christmas_sentiment <- fourteen %>%
  select(spoken_words) %>% 
  unnest_tokens(word, spoken_words) %>%
  anti_join(stop_words) %>%
  mutate(linenumber = row_number()) %>%
  inner_join(get_sentiments("bing")) %>%
  count(index = linenumber %/% 10, sentiment)%>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(sentiment = positive - negative)

afinn_fourteen_christmas_sentiment <- fourteen %>%
  select(spoken_words) %>% 
  unnest_tokens(word, spoken_words) %>%
  anti_join(stop_words) %>%
  mutate(linenumber = row_number()) %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(index = linenumber %/% 10) %>%
  summarise(sentiment = sum(value))

fourteen_bing <- ggplot(fourteen_christmas_sentiment, aes(index, sentiment)) +
  geom_col(show.legend = FALSE, fill = '#FED439FF') + labs(title = "Episode 561 (Bing)") + geom_smooth(se = FALSE, color = 'blue')

fourteen_afinn <- ggplot(afinn_fourteen_christmas_sentiment, aes (index, sentiment)) +
  geom_col(fill = '#197EC0FF') + labs(title = 'Episode 561 (Afinn)') + geom_smooth(se = FALSE, color = 'yellow')

grid.arrange(fourteen_bing, fourteen_afinn)

# plotting words with most sentiment etc for each episode

bplot_one <- tidy_simpsons %>% # by bing
  select(episode_id, sentiment, word)%>%
  group_by(episode_id) %>%
  count(word, sentiment, sort = TRUE)%>%
  filter(n == max(n)) 

# wordcloud

wc1 <- sort(table(bplot_one$word), decreasing = TRUE)

figPath <- "Simpsons.png" 

wordcloud2(wc1, shape = 'triangle', rotateRatio = 0.9, size = 0.7, color=rep_len(c("#107DC0", '#F65132'), nrow(wc1)))

# same contribution analysis for afinn

bplot_two <- tidy_simpsons %>% # by afinn
  select(episode_id, value, word)%>%
  group_by(episode_id, word) %>%
  summarise(sentiment = sum(value)) %>%
  filter(sentiment == max(abs(sentiment)))

wc2 <- sort(table(bplot_two$word), decreasing = TRUE)

#figPath <- "Simpsons.png" 

wordcloud2(wc2, color=rep_len(c("#107DC0", '#F65132'), nrow(wc1)))

# words per episode

simp_words <- simpsons_episode %>%
  select(episode_id, spoken_words) %>%
  unnest_tokens(word, spoken_words) %>%
  count(episode_id, word, sort = TRUE)

total_words <- simp_words %>% 
  group_by(episode_id) %>% 
  summarize(total = sum(n))

# first episode was 30 mins long, last episode was 22 - less words given shorter episodes

ggplot(total_words, aes(x = episode_id, y = total)) + 
  geom_point(colour = '#F65132') + geom_line(colour = '#107DC0') + 
  labs(title = 'Total words per episode', x = 'Episodes', y = 'Total Words')

# tf-idf

simp_tf_idf <- simp_words %>%
  bind_tf_idf(word, episode_id, n)

#simp_tf_idf %>%
#  select(-total) %>%
#  arrange(desc(tf_idf))

first_tf <- simp_tf_idf %>%
  filter(episode_id == 235) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  head(10) %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = factor(episode_id))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~episode_id, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL) +
  scale_fill_manual(values = '#FFD521')

second_tf <- simp_tf_idf %>%
  filter(episode_id == 275) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  head(10) %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = factor(episode_id))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~episode_id, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL) +
  scale_fill_manual(values = '#107DC0')

third_tf <- simp_tf_idf %>%
  filter(episode_id == 365) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  head(10) %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = factor(episode_id))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~episode_id, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL) +
  scale_fill_manual(values =  '#F65132')

fourth_tf <- simp_tf_idf %>%
  filter(episode_id == 561) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  head(10) %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = factor(episode_id))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~episode_id, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL) +
  scale_fill_manual(values =  '#D6E69F')

grid.arrange(first_tf, second_tf, third_tf, fourth_tf, nrow = 2, ncol = 2)

six %>% 
  filter(str_detect(spoken_words, "padme")) %>%
  select(spoken_words)

# n gram analysis 

# We'll do the same as above, trying to gauge the sentiment of each episode but this time using n-grams to capture
# context that would be missed if using just single word tokens

# Tidy Text refers to this as the "separate/filter/count/unit" principle - which transforms a tidy text df into one with 
# ngrams without stopwords

simpsons_bigrams <- simpsons_episode %>%
  select(episode_id, spoken_words) %>%
  unnest_tokens(bigram, spoken_words, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram))

afinn <- get_sentiments("afinn")

tidy_simpson_bigram <- simpsons_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  unite(bigram, word1, word2, sep = " ") %>%
  group_by(episode_id) %>%
  count(bigram) %>%
  filter(n == max(n)) 

bigram_plt <- ggplot(tidy_simpson_bigram, aes(x = reorder(bigram, -n), y = n, fill = factor(episode_id))) + 
  geom_col() + scale_fill_manual(values = c('#FED439FF', '#709AE1FF','#8A9197FF','#D2AF81FF','#FD7446FF',
                                            '#D5E4A2FF','#197EC0FF','#46732EFF','#71D0F5FF','#370335FF',
                                            '#C80813FF','#91331FFF','#1A9993FF','#FD8CC1FF'), name = 'Episodes')

bigram_plt + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(x = 'Bigram', y = 'Count')

wc_df <- tidy_simpson_bigram[, c('bigram', 'n')] %>%
  group_by(bigram)

wc_df %>%
  group_by(bigram)%>%
  summarise(n = sum(n)) %>%
  wordcloud2(color=rep_len(c("#107DC0", '#F65132', "#075149FF", "#F05C3BFF"), nrow(wc_df)))



# tf- idf in bigram form

simpsons_bigrams$episode_id <- factor(simpsons_bigrams$episode_id)

simps_bigram_tf_idf <- simpsons_bigrams %>%
  count(episode_id, bigram) %>%
  bind_tf_idf(bigram, episode_id, n) %>%
  arrange(desc(tf_idf))

first_bigram_tf <- simps_bigram_tf_idf %>%
  filter(episode_id == 235) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  head(10)%>%
  ggplot(aes(tf_idf, fct_reorder(bigram, tf_idf), fill = episode_id)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~episode_id, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL) +
  scale_fill_manual(values = '#FFD521')

second_bigram_tf <- simps_bigram_tf_idf %>%
  filter(episode_id == 275) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  head(10)%>%
  ggplot(aes(tf_idf, fct_reorder(bigram, tf_idf), fill = episode_id)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~episode_id, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL) +
  scale_fill_manual(values = '#107DC0')

third_bigram_tf <- simps_bigram_tf_idf %>%
  filter(episode_id == 365) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  head(10)%>%
  ggplot(aes(tf_idf, fct_reorder(bigram, tf_idf), fill = episode_id)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~episode_id, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL) +
  scale_fill_manual(values = '#F65132')

fourth_bigram_tf <- simps_bigram_tf_idf %>%
  filter(episode_id == 561) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  head(10)%>%
  ggplot(aes(tf_idf, fct_reorder(bigram, tf_idf), fill = episode_id)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~episode_id, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL) +
  scale_fill_manual(values = '#D6E69F')

grid.arrange(first_bigram_tf, second_bigram_tf, third_bigram_tf, fourth_bigram_tf, nrow = 2, ncol = 2)
