library(tidyverse)
library(ggrepel)
library(fuzzyjoin)
library(DescTools)
source("database/db_connect.R")
source("blog/globals.R")


query  = "SELECT abstract_appear, names, search_name, year FROM article_to_country c, country_names, article_to_search s, articles a
where country_id=index and c.article_doi=s.article_doi and a.article_doi=c.article_doi"
countries_db = dbGetQuery(con, query)

summary(countries_db)

countries_db = countries_db %>% 
  filter(search_name %in% pc_cats & year > 1950)

mean_mention = countries_db %>%
  summarise(appear_prop=sum(abstract_appear)/n())

countries_db %>%
  group_by(search_name) %>% 
  summarise(appear_prop=sum(abstract_appear)/n()) %>% 
  rename("Country mentions (%)"=appear_prop)

countries_db %>%
  group_by(year) %>% 
  summarise(appear_prop=(sum(abstract_appear)/n())*100) %>% 
  rename("Country mentions (%)"=appear_prop) %>% 
  ggplot(aes(year, `Country mentions (%)`))+
  geom_line()+
  geom_smooth(se=F)+
  geom_hline(yintercept = mean_mention$appear_prop*100)+
  theme_minimal()+
  labs(x=NULL)

countries_db %>%
  group_by(year,search_name) %>% 
  summarise(appear_prop=sum(abstract_appear)/n()) %>% 
  rename("Country mentions (%)"=appear_prop) %>% 
  ggplot(aes(year, `Country mentions (%)`))+
  geom_line()+
  geom_smooth(se=F)+
  facet_wrap(~search_name)

