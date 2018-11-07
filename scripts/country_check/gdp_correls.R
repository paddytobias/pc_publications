library(tidyverse)
library(ggrepel)
library(fuzzyjoin)
library(DescTools)
library(gganimate)
source("database/db_connect.R")
source("blog/globals.R")



query  = "SELECT abstract_appear, names, search_name, year FROM article_to_country c, country_names, article_to_search s, articles a
where country_id=index and c.article_doi=s.article_doi and a.article_doi=c.article_doi and title_appear<>0"
countries = dbGetQuery(con, query)

countries = countries %>% 
  count(year, names) %>% 
  rename(countries_n=n)

top10_country_mentions = countries %>% 
  group_by(names) %>% 
  summarise(country_pub_sum = sum(countries_n)) %>% 
  top_n(10)

gdp_percap=read.csv("database/database_files/API_NY.GDP.PCAP.CD_DS2_en_csv_v2_10081024/API_NY.GDP.PCAP.CD_DS2_en_csv_v2_10081024.csv", stringsAsFactors = F,skip = 3)


gdp_percap_2017 = gdp_percap %>% 
  filter(Country.Name!="American Samoa") %>%
  select(Country.Name, X2017) %>% 
  rename(gdp_2017 = X2017)


country_mention_gdp = countries %>% 
  regex_inner_join(gdp_percap_2017, by=c("names"="Country.Name")) %>% 
  group_by(names, gdp_2017) %>% 
  summarise(mentions_total = sum(countries_n)) %>% 
  mutate(labels = ifelse(names %in% top10_country_mentions$names, names, ""))


ggplot(country_mention_gdp, aes(gdp_2017, mentions_total))+
  geom_point()+
  labs(x= "GDP per capita ($US), 2017", y= "Abstract mentions", caption = "Publication data from Taylor and Francis Online, 2018\n GDP data from World Bank")+
  geom_text_repel(aes(label=labels))+
  theme_minimal()
ggsave("outputs/publications/pubs_gdp_countries.jpeg")


