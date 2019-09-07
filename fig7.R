merged <- merged %>% 
  filter(screen_name != "PrayerCaucus")

merged$date <- date(merged$created_at)

merged$date2 <- round_date(merged$created_at, "month")

all <- merged %>% 
  group_by(date2) %>% 
  count()

god <- subset(merged, grepl("god|God", ignore.case = TRUE, text)) 

g1 <- god %>% 
  group_by(date2) %>% 
  count() %>% 
  rename(n2 = n)

god <- left_join(g1, all) %>% 
  mutate(pct = n2/n) %>% 
  mutate(term = "God")

faith <- subset(merged, grepl("faith", ignore.case = TRUE, text)) 

g1 <- faith %>% 
  group_by(date2) %>% 
  count() %>% 
  rename(n2 = n)

faith <- left_join(g1, all) %>% 
  mutate(pct = n2/n) %>% 
  mutate(term = "Faith")

pray <- subset(merged, grepl("prayer|pray|praying|prayed", ignore.case = TRUE, text)) 

g1 <- pray %>% 
  group_by(date2) %>% 
  count() %>% 
  rename(n2 = n)

pray <- left_join(g1, all) %>% 
  mutate(pct = n2/n) %>% 
  mutate(term = "Pray*")


jesus <- subset(merged, grepl("jesus|Jesus", ignore.case = TRUE, text)) 

g1 <- jesus %>% 
  group_by(date2) %>% 
  count() %>% 
  rename(n2 = n)

jesus <- left_join(g1, all) %>% 
  mutate(pct = n2/n) %>% 
  mutate(term = "Jesus")


bible <- subset(merged, grepl("bible|Bible", ignore.case = TRUE, text)) 

g1 <- bible %>% 
  group_by(date2) %>% 
  count() %>% 
  rename(n2 = n)

bible <- left_join(g1, all) %>% 
  mutate(pct = n2/n) %>% 
  mutate(term = "Bible")


graph <- bind_rows(god, faith, pray, jesus, bible)

graph %>% 
  filter(term != "Pray*") %>% 
  ggplot(., aes(x=date2, y = pct,  fill = term)) +
  geom_col(color = "black") +
  scale_fill_npg() +
  facet_wrap(~ term, ncol =1) +
  theme_gg("Poppins") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Date", y = "Percent of Tweets per Month", title = "The Usage of Different Religious Language over Time", subtitle = "", caption = "Scraped from Twitter's API (2017 - 2018)") +
  ggsave("D:/congress_tweets/images/fig7.png", width =  8)
 