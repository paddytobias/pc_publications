library(ggthemes)
library(tidyverse)
library(gridExtra)
source("database/db_connect.R")


query = "select  article_to_search.article_doi, search_name, 
        cited_by_fbwalls_count, cited_by_gplus_count, cited_by_rdts_count, cited_by_wikipedia_count, 
        cited_by_msm_count, cited_by_feeds_count
        from altmetrics, article_to_search
where altmetrics.article_doi=article_to_search.article_doi"
mentions = dbGetQuery(con, query)
dbDisconnect(con)


mentions[3:ncol(mentions)] = sapply(mentions[3:ncol(mentions)],as.numeric)

mentions_gather = mentions %>% 
  gather(key = type, value = count, -c(article_doi, search_name)) %>% 
  na.omit() %>% 
  group_by(search_name, type) %>% 
  summarise(count_sum = sum(count))

ggplot(mentions_gather, aes(sample = count_sum)) +
  stat_qq()

mentions_plot = ggplot(mentions_gather, aes(x = count_sum, color = search_name, fill = search_name))+
  geom_density(alpha=0.1, show.legend = F) +
  labs(title = "Mentions")

query = 'select  article_to_search.article_doi, search_name, citations, "cohorts.doc", "cohorts.sci"
from altmetrics, article_to_search
where altmetrics.article_doi=article_to_search.article_doi'
citations = dbGetQuery(con, query)
dbDisconnect(con)

citations[3:ncol(citations)] = sapply(citations[3:ncol(citations)],as.numeric)

citations_gather = citations %>% 
  gather(key = type, value = count, -c(article_doi, search_name)) %>% 
  na.omit() %>% 
  group_by(search_name, type) %>% 
  summarise(count_sum = sum(count))

ggplot(citations_gather, aes(sample = count_sum)) +
  stat_qq()

citations_plot = ggplot(citations_gather, aes(x = count_sum, color = search_name, fill = search_name))+
  geom_density(alpha=0.1, show.legend = F) +
  labs(title = "Citations")


query = 'select  article_to_search.article_doi, search_name, views
from altmetrics, article_to_search
where altmetrics.article_doi=article_to_search.article_doi'
views = dbGetQuery(con, query)
dbDisconnect(con)

views_gather = views %>% 
  gather(key = type, value = count, -c(article_doi, search_name)) %>% 
  na.omit() %>% 
  group_by(search_name) %>% 
  summarise(count_sum = sum(count))

ggplot(views_gather, aes(sample = count_sum)) +
  stat_qq()

views_plot = ggplot(views_gather, aes(x = count_sum))+
  geom_density()+
  labs(title = "Views")

query = 'select  article_to_search.article_doi, search_name, "cohorts.doc", "cohorts.sci", readers_count, cited_by_peer_review_sites_count, cited_by_book_reviews_count, cited_by_policies_count
from altmetrics, article_to_search
where altmetrics.article_doi=article_to_search.article_doi'
readers = dbGetQuery(con, query)
dbDisconnect(con)

readers[3:ncol(readers)] = sapply(readers[3:ncol(readers)],as.numeric)

readers_gather = readers %>% 
  gather(key = type, value = count, -c(article_doi, search_name)) %>% 
  na.omit() %>% 
  group_by(search_name, type) %>% 
  summarise(count_sum = sum(count))

# ggplot(readers_gather, aes(sample = count_sum)) +
  # stat_qq()

readers_plot = ggplot(readers_gather, aes(x = count_sum))+
  geom_density(alpha=0.1) +
  labs(title = "Readers")

grid.arrange(citations_plot, readers_plot, mentions_plot, views_plot, nrow = 2 )



# readers
readers_gather = readers %>% 
  gather(key = type, value = readers_count, -c(article_doi, search_name)) %>% 
  na.omit() %>% 
  group_by(article_doi) %>% 
  summarise(readers_sum_count = sum(readers_count)) %>% 
  mutate(readers_z = (readers_sum_count-mean(readers_sum_count))/ sd(readers_sum_count))

  
# # views
views_gather = views %>% 
  gather(key = type, value = views_count, -c(article_doi, search_name)) %>% 
  na.omit() %>% 
  group_by(article_doi) %>% 
  summarise(views_sum_count = sum(views_count)) %>% 
  mutate(views_z = (views_sum_count-mean(views_sum_count))/ sd(views_sum_count))
# 
#   mutate(z_views_ungrouped = (views_count - mean(views_count)) / sd(views_count)) %>%  
#   filter((z_views_ungrouped < 3 & z_views_ungrouped > -3)) %>% 
#   mutate(views = (z_views_ungrouped - mean(z_views_ungrouped)) / sd(z_views_ungrouped)) %>% 
#   select(-c(type,z_views_ungrouped) )

# citations
citations_gather = citations %>% 
  gather(key = type, value = citations_count, -c(article_doi, search_name)) %>% 
  na.omit() %>% 
  group_by(article_doi) %>% 
  summarise(citations_sum_count = sum(citations_count)) %>% 
  mutate(citations_z = (citations_sum_count-mean(citations_sum_count))/ sd(citations_sum_count))
  # mutate(views_z = (views_sum_count-mean(views_sum_count))/ sd(views_sum_count))
  # mutate(z_citations_ungrouped = (citations_count - mean(citations_count)) / sd(citations_count)) %>% 
  # filter((z_citations_ungrouped < 3 & z_citations_ungrouped > -3)) %>% 
  # mutate(citations = (z_citations_ungrouped - mean(z_citations_ungrouped)) / sd(z_citations_ungrouped)) %>% 
  # select(-c(type,z_citations_ungrouped) ) 

# mentions
mentions_gather = mentions %>% 
  gather(key = type, value = mentions_count, -c(article_doi, search_name)) %>% 
  na.omit() %>% 
  group_by(article_doi) %>% 
  summarise(mentions_sum_count = sum(mentions_count)) %>% 
  mutate(mentions_z = (mentions_sum_count-mean(mentions_sum_count))/ sd(mentions_sum_count))

  # mutate(z_mentions_ungrouped = (mentions_count - mean(mentions_count)) / sd(mentions_count)) %>% 
  # filter((z_mentions_ungrouped < 3 & z_mentions_ungrouped > -3)) %>% 
  # #select(-type)
  # mutate(mentions = (z_mentions_ungrouped - mean(z_mentions_ungrouped)) / sd(z_mentions_ungrouped)) %>% 
  # select(-c(type,z_mentions_ungrouped) )

  # gather(key = z_type, value = z_score, ends_with("_z")) %>% 
  # gather(key = pop_type, value = pop_count, ends_with("_count")) %>% 
  # filter(z_score < 2 & z_score > -2)

article_popularity = article_pop_type %>% 
  group_by(article_doi) %>% 
  summarise(aca_pop_score = (citations_sum_count*4)+(readers_sum_count*3)+(mentions_sum_count*2)+(views_sum_count*1), 
            pub_pop_score = (mentions_sum_count*4)+(views_sum_count*3)+(readers_sum_count*2)+(citations_sum_count*1), 
            pop_score = mentions_sum_count+views_sum_count+readers_sum_count+citations_sum_count) %>% 
  mutate(aca_pop_z = (aca_pop_score-mean(aca_pop_score))/sd(aca_pop_score), 
         pub_pop_z = (pub_pop_score-mean(pub_pop_score))/sd(pub_pop_score), 
         pop_z = (pop_score-mean(pop_score))/sd(pop_score))

# pub_pop_99.7 = article_popularity %>% 
#   filter(pub_pop_z < 3 & pub_pop_z > -3 ) %>% 
#   select(article_doi, pub_pop_score)
# aca_pop_99.7 = article_popularity %>% 
#   filter(aca_pop_z < 3 & aca_pop_z > -3 ) %>% 
#   select(article_doi, aca_pop_score)
# 
# pop_99.7_aca_pub = pub_pop_99.7 %>% 
#   inner_join(aca_pop_99.7, by = c("article_doi"="article_doi")) %>% 
#   mutate(pub_pop_z = (pub_pop_score-mean(pub_pop_score))/sd(pub_pop_score), 
#          aca_pop_z = (aca_pop_score-mean(aca_pop_score))/sd(aca_pop_score)) %>% 
#   gather(key = pop_z_type, value = z_score, ends_with("_z")) %>% 
#   group_by(pop_z_type)
# 
# ggplot(pop_99.7_aca_pub) +
#   geom_density(aes(z_score, fill = pop_z_type), show.legend = F) + facet_wrap(~pop_z_type)

pop_99.7 = article_popularity %>% 
  filter(pop_z < 3 & pop_z > -3 ) %>% 
  select(article_doi,pop_score, pop_z)

query = "select article_to_search.article_doi, search_name, date_part('year'::text, date_pub) as year
from article_to_search, articles
where articles.article_doi=article_to_search.article_doi"
article_search = dbGetQuery(con, query)
dbDisconnect(con)

pop_99.7_search_join = pop_99.7 %>% 
  inner_join(article_search) #%>% 
  # group_by(search_name) %>% 
  # mutate(search_z = (pop_score-mean(pop_score))/sd(pop_score)) #%>% 
  #filter(search_z > -3 & search_z <3) # making it 99.4% of total sample

ggplot(pop_99.7_search_join, aes(search_z, colour=search_name)) +
  geom_density(show.legend = F) + facet_wrap(~search_name)+
  labs(x = "Popularity per the mean (z = 0)", y = "Density", title = "Popularity of 50%-99.4% of articles", subtitle="Std normal distribution")

pop_99.7_year = pop_99.7_search_join %>% 
  group_by(search_name, year) %>% 
  summarise(sum_pop_year = sum(pop_score)) %>% 
  #mutate(pop_z = (sum_pop_year-mean(sum_pop_year))/sd(sum_pop_year)) %>% 
  filter(year > 2005 & year < 2017) 
ggplot(pop_99.7_year, aes(x= year, y = sum_pop_year))+
  geom_line(show.legend = F, color = "blue")+
  geom_smooth(method = "lm", colour = "grey", size=0.5)+
  facet_wrap(~search_name)+
  labs(x = "Year", y = "Popularity score", title = "Popularity", caption = "Source data from Taylor & Francis Online and Altmetrics, 2018")+
  theme_minimal()
#  scale_y_continuous(labels= c("-2"="-95%", "-1"="-34%", "0"="0%",  "1"="34%", "2"="95%"))
ggsave("../docs/outputs/popularity_norm_year.jpeg")

pop_pub_join = pub_count_mutated %>% 
  inner_join(pop_99.7_year, by = c("query_term"="search_name", "year_published"="year"))
ggplot(pop_pub_join, aes(x= year_published))+
  geom_line(aes(y = sum_pop_year, color = "Popularity"))+
  geom_line(aes(y = publication_count/0.005747561, color = "Publications"))+
  scale_y_continuous(sec.axis = sec_axis(~.*0.005747561, name = "Publication count"), name = "Popularity score") + facet_wrap(~query_term)+
  scale_colour_manual(values = c("blue", "red"))+
  theme_minimal()+
  labs(x = "Year",
      colour = "", title = "Publication trends and their popularity", caption = "Source data from Taylor and Francis Online and Altmetrics, 2018")+
  theme(legend.position = c(0.9, 0.15))
 ggsave("../docs/outputs/pub_v_pop.jpeg")

# ggplot(pop_gather, aes(z_score, fill = z_type, colour = z_type)) +
#   geom_density(alpha=0.1)+
#   facet_wrap(~ search_name) +
#   labs(title="Popularity of 99.7 percent of the articles", subtitle = "Standard normal distribution", x = "Comparing popularities", y = "Density")+
#   guides(color = guide_legend("Search terms"), fill=FALSE)
# ggsave("../docs/outputs/popularity_comparisons_by_pop_type.jpeg")
# 
# ggplot(pop_gather, aes(z_score)) +
#   geom_density(alpha=0.1)+
#   facet_wrap(~ search_name) +
#   labs(title="Popularity of 99.7 percent of the articles", subtitle = "Standard normal distribution", x = "Comparing popularities", y = "Density")+
#   guides(color = guide_legend("Search terms"), fill=FALSE)
# ggsave("../docs/outputs/popularity_comparisons.jpeg")
# 

