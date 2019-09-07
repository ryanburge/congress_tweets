library(socsci)
library(tidytext)
source("D://theme.R")

load('D://congress_tweets/1718only.Rdata')

# 
# load('D://rtweets/all_tw.Rdata')
# 
# merged$year <- format(as.Date(merged$created_at, format="%Y-%m-%d %h:%m:%s"),"%Y")
# 
# merged <- merged %>% 
#   mutate(year = as.numeric(year)) %>% 
#   filter(year >= 2017)
# 
# save(merged, file = "1718only.Rdata")


# 
# ## This is how I built the full data file, for reference. 
# # 
#  load('D://rtweets/congress/rep_tweets.RData')
#  load('D://rtweets/congress/sen_tweets.RData')
#  
#  
# # saveRDS(merged, "all_tw.rds")
#  
#  
#  all <- bind_rows(rep_tweets, sen_tweets)
#  
#  rm(rep_tweets)
#  rm(sen_tweets)
#  
#  demo <- read_csv("congress/party_gender.csv") %>% 
#    select(-X1, -X1_1)
#  
#  merged <- left_join(all, demo)
#  rm(all)
#  
#  merged <- merged %>% 
#    filter(gender != "REMOVE")
# 
# save(merged, file = "all_tw.Rdata")






