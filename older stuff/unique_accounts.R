
load('D://rtweets/all_tw.Rdata')

merged$year <- format(as.Date(merged$created_at, format="%Y-%m-%d %h:%m:%s"),"%Y")

merged <- merged %>% 
  mutate(year = as.factor(year))

graph <- merged %>% 
  group_by(year) %>% 
  distinct(screen_name) %>% 
  ct(year)


graph %>% 
  ggplot(., aes(x = year, y = n, fill = n)) +
  geom_col(color = "black") +
  theme_gg("Poppins") +
  scale_fill_gradient(low = "azure3", high = "darkorchid") +
  geom_text(aes(y = n + 13, label = n), position = position_dodge(width = .9), size = 4, family = "font") +
  labs(x = "", y = "Number of Unique Accounts", title = "Number of Unique Accounts Tweeting", caption = "Data: Scraped from Twitter's API (6/2008 - 4/2018)") +
  ggsave("D://congress_tweets/images/count_per_year.png", type = "cairo-png")