
total2 <- merged %>% 
  group_by(party, gender, month = cut(date, "month"))  %>% 
  count() %>% 
  ungroup(month) %>% 
  mutate(month = as.Date(month, "%Y-%m-%d")) %>% 
  rename(tot = n)

aa <- total2 %>% 
  filter(party == "Republican" | party == "Democrat") %>% 
  filter(month == "2018-03-01") %>% 
  mutate(total_pct = tot/47432) %>% 
  mutate(type = "All Tweets")

aa1 <- graph %>% 
  filter(month == "2018-03-01") %>% 
  select(-pct) %>% 
  mutate(total_pct = n/162) %>% 
  mutate(type = "Tweets Containing 'God'")

gg <- bind_rows(aa, aa1) %>% 
  select(-n, -month) %>% 
  mutate(gender = str_replace(gender, "M", "Male   ")) %>% 
  mutate(gender = str_replace(gender, "F", "Female   ")) %>% 
  mutate(new = paste(party, gender, sep = " - "))

gg %>% 
  mutate(total_pct = round(total_pct,3)) %>% 
  ggplot(., aes(x = 1, y = total_pct, fill = new)) +
  geom_col(color = "black") +
  coord_flip() +
  facet_wrap(~type, ncol = 1) +
  theme_gg("Poppins") +
  theme(legend.title = element_blank()) +
  scale_y_continuous(labels = percent) + 
  scale_fill_manual(values = c("#BFEFFF", "#236B8E", "#ff9999", "#660000"))  +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  theme(axis.text.y = element_blank()) +
  theme(axis.ticks.y = element_blank()) + 
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank()) +
  geom_text(aes(label = paste0(total_pct*100, '%')), position = position_stack(vjust = 0.5), size = 5, family = "font") +
  labs(x = "", y = "", title = "Tweets Sent in March of 2018 by Members of Congress", caption = "Data: Scraped from Twitter's API (6/2008 - 4/2018)") +
  ggsave("D://congress_tweets/images/fig4.png", dpi = 300, width = 10, height =4, type = "cairo-png")

