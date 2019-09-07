reps <- lists_members(slug = "u-s-representatives", owner_user = "CSpan")
sen <- lists_members(slug = "senators", owner_user = "CSpan")

profiles <- bind_rows(reps, sen)

demo <- read_csv("congress/party_gender.csv") %>% 
      select(-X1, -X1_1)

xtn <- profiles %>% 
  select(screen_name, description)

xtn <-  left_join(xtn, demo)

write.csv(xtn, "clean_up_descript.csv")

xtn <- read_csv("D://rtweets/congress/profile_clean.csv") 


terms <- 'Christian|christian|christ|Christ|god|God|Jesus|jesus|faith|Faith|catholic|Catholic|Muslim|muslim'
sub <- subset(xtn, grepl(terms, description))

## Only 3 MoCs use one of these terms in their description: one Democrat and two Republicans
## RepValDemings U.S. Representative Florida's 10th Congressional District. Public Servant. Former Orlando Police Chief. Christian. Wife. Mother. Grandmother. Harley Enthusiast. Democrat   F 
## RepCurbelo    Representing Florida's 26th Congressional District, Husband, Dad, Catholic, Son of Cuban exiles, GOP, UM gradx2, Belen Jesuit                                    Republican M    
## RepJeffDuncan Christian, husband, father, small business owner. Lover of football & the outdoors. Employee for the people of SC's Third Congressional District. Call me Jeff.  Republican M     

## Looking for Specific Words ####

cons <- subset(xtn, grepl("conservative", ignore.case = TRUE, description)) 
cons <- cons %>% 
  filter(party != "Remove") %>% 
  mutate(term = "Conservative")

gop <- subset(xtn, grepl("GOP", ignore.case = TRUE, description)) 
gop <- gop %>% 
  filter(party != "Remove") %>% 
  mutate(term = "GOP")

rep <- subset(xtn, grepl("Republican", ignore.case = TRUE, description)) 
rep <- rep %>% 
  filter(party != "Remove") %>% 
  mutate(term = "Republican")

lib<- subset(xtn, grepl("liberal", ignore.case = TRUE, description)) 
lib <- lib %>% 
  filter(party != "Remove") %>% 
  mutate(term = "Liberal")

dem <- subset(xtn, grepl("Democrat", ignore.case = TRUE, description)) 
dem <- dem %>%
  filter(party != "Remove") %>%
  mutate(term = "Democrat")

prog <- subset(xtn, grepl("Progressive", ignore.case = TRUE, description)) 
prog <- prog %>%
  filter(party != "Remove") %>%
  mutate(term = "Progressive")

dad <- subset(xtn, grepl("Dad|Father", ignore.case = TRUE, description)) 
dad <- dad %>%
  filter(party != "Remove") %>%
  mutate(term = "Dad")

mom <- subset(xtn, grepl("Mom|Mother", ignore.case = TRUE, description)) 
mom <- mom %>%
  filter(party != "Remove") %>%
  mutate(term = "Mom")



all <- bind_rows(gop, rep, dem, prog, lib, cons, dad, mom)

aa <- all %>% 
  filter(party == "Republican") %>% 
  count(term) %>% 
  mutate(party = "Republican") %>% 
  mutate(pct = n/ 296)

aa1 <- all %>% 
  filter(party == "Democrat") %>% 
  count(term) %>% 
  mutate(party = "Democrat") %>% 
  mutate(pct = n/ 258) %>% 
  add_row(term = "Liberal", n = 0, party = "Democrat", pct = 0)

graph <- bind_rows(aa, aa1)

tidy <- xtn %>% 
  unnest_tokens(word, description) 

tidy <- tidy %>% anti_join(stop_words)

tidy %>% 
  filter(party == "Democrat") %>% 
  group_by(party) %>% 
  ct(word) %>% 
  arrange(-pct) %>% as.data.frame()

gr1 <- graph %>% 
  filter(party == "Republican") %>% 
  ggplot(., aes(x = reorder(term, pct), y = pct, fill = pct)) +
  geom_col(color = "black") +
  coord_flip() +
  scale_fill_gradient(low = "#FA8072", high = "#7C0A02") +
  theme_gg("Abel") +
  scale_y_continuous(limits = c(0, .15), labels = percent) +
  labs(x = "", y = "                                                                                  Percent of Party's Twitter Descriptions", title = "How Members of Congress Describe Themselves", subtitle = "Republican")

  

gr2 <- graph %>% 
  filter(party == "Democrat") %>% 
  ggplot(., aes(x = reorder(term, pct), y = pct, fill = pct)) +
  geom_col(color = "black") +
  coord_flip() +
  scale_fill_gradient(low = "#73C2FB", high = "#1034A6") +
  theme_gg("Abel") +
  scale_y_continuous(limits = c(0, .15), labels = percent) +
  labs(x = "", y = "", title = "", subtitle = "Democrat")

  
both <- gr1 + gr2  

ggsave("D://rtweets/congress/descript_terms.png", both)


subset(xtn, grepl("mormon", ignore.case = TRUE, description)) 

