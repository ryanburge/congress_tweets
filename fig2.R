
number <- merged %>% 
  group_by(party, gender) %>% 
  count() %>% 
  filter(party == "Democrat" | party  == "Republican")

number2 <- merged %>% 
  select(screen_name, party, gender) %>% 
  distinct(screen_name, party, gender) %>% 
  group_by(party) %>% 
  count(gender) %>%  
  filter(party == "Democrat" | party  == "Republican")

bars <- bind_cols(number2, number) %>% 
  select(party, gender, n, n1) %>% 
  rename(tweet_n = n1) %>% 
  mutate(avg = tweet_n/n)

bars %>% 
  mutate(avg = round(avg, 0)) %>% 
  filter(gender != "NA") %>% 
  filter(party  == "Republican" | party == "Democrat") %>%
  mutate(gender = str_replace(gender, "M", "Male")) %>% 
  mutate(gender = str_replace(gender, "F", "Female")) %>% 
  mutate(new = paste(party, gender, sep = " - ")) %>% 
  ggplot(., aes(x = new, y = avg, fill = new)) +
  geom_col(color = "black") +
  theme_gg("Poppins") +
  theme(legend.position = "none") +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c("#BFEFFF", "#236B8E", "#ff9999", "#660000")) +
  geom_text(aes(y = avg + 40, label = paste0(avg, ' Tweets')), position = position_dodge(width = .9), size = 5, family = "font") +
  labs(x = "", y = "Average Number of Tweets per Account", title = "Average Number of Tweets per Party and Gender", subtitle = "Between January 2017 and April 2018", caption = "Data: Scraped from Twitter's API") +
  ggsave("D://congress_tweets/images/fig2.png", dpi = 300, width = 8, height =6, type = "cairo")



