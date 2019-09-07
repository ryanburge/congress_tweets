

merged$date <- format(as.Date(strptime(merged$created_at, '%Y-%m-%d %H:%M:%S')), "%m/%d/%Y")
merged$date <- as.Date(merged$date, "%m/%d/%Y")

monthly <- merged %>%
  group_by(month = cut(date, "month"), party, gender) %>% 
  count() %>% 
  ungroup(month) %>% 
  mutate(month = as.Date(month, "%Y-%m-%d")) 


  
monthly %>% 
  filter(gender != "NA") %>% 
  filter(party  == "Republican" | party == "Democrat") %>%
  mutate(gender = str_replace(gender, "M", "Male")) %>% 
  mutate(gender = str_replace(gender, "F", "Female")) %>% 
  mutate(new = paste(party, gender, sep = " - ")) %>% 
  filter(month < "2018-03-01") %>% 
  ggplot(., aes(x= month, y= n, group = new, color = new)) +
  geom_point() +
  geom_line() +
  scale_x_date(breaks = "1 year", labels = date_format("%Y")) +
  theme_gg("Poppins") +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  add_text(x=as.Date("2017-06-01"), y = 21000, word = "Democrat Men") +
  add_text(x=as.Date("2017-08-15"), y = 13750, word = "Republican Men") +
  add_text(x=as.Date("2017-08-15"), y = 10500, word = "Democrat Women") +
  add_text(x=as.Date("2017-08-15"), y = 3200, word = "Republican Women") +
  scale_color_manual(values = c("#BFEFFF", "#236B8E", "#ff9999", "#660000"))  +
  labs(x = "", y = "Total Number of Tweets", title = "Total Tweet Volume by Month", caption = "Data: Scraped from Twitter's API (2017 - 2018)") +
  ggsave("D://congress_tweets/images/total_vol_tweets_party_gender.png", dpi = 300, width = 10, height =6, type = "cairo")
  