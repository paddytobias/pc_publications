library(tidyverse)
library(ggrepel)
source("database/db_connect.R")
source("blog/globals.R")

# publication rates
query = "SELECT a.article_doi AS article_doi, 
a.year AS year, 
s.search_name AS search_name
FROM articles a, article_to_search s
WHERE a.article_doi = s.article_doi"
pub_by_year_search = dbGetQuery(con, query)



pub_count_year_search = pub_by_year_search %>% 
  count(year, search_name) %>% 
  filter(search_name %in% pc_cats) %>% 
  mutate(cats = ifelse(search_name %in% peace_cats, "peace-oriented", "conflict-oriented"), 
         stages = ifelse(search_name %in% long_pc, "long-term", 
                         ifelse(search_name %in% middle_pc, "middle-term", "short-term"))) %>% 
  filter(!is.na(year))

# ggplot(pub_count_year_search, aes(cats, n))+
#   geom_boxplot()
# 
# ggplot(pub_count_year_search %>% 
#          group_by(cats, year) %>% 
#          summarise(sum = sum(n)), aes(year, sum, color = cats))+
#   geom_smooth(method = "loess", se=F)+
#   theme_minimal()+
#   labs(y="Count", x = NULL, color = "Type")

pub_count_year_search$cats = factor(pub_count_year_search$cats)
pub_count_year_search$search_name = factor(pub_count_year_search$search_name)
#pub_count_year_search$year = factor(pub_count_year_search$year)

# # pub rates by pvc
ggplot(pub_count_year_search %>% 
         group_by(cats, year) %>% 
         summarise(sum = sum(n)) %>% 
         filter(year > 1979 & year < 2018), aes(year, sum, color = cats))+
  geom_line(method = "loess", se=F)+
  theme_minimal()+
  labs(y="Sum", x = NULL, color = NULL, title="Publication rates since 1980")+
  theme(legend.position = c(0.2, 0.8))
ggsave("outputs/publications/pub_rates_19802017_pc.jpeg")

ggplot(pub_count_year_search %>% 
         group_by(cats, year) %>% 
         summarise(sum = sum(n)) %>% 
         filter(year > 2005 & year < 2018), aes(year, sum, color = cats))+
  geom_line(method = "loess", se=F)+
  theme_minimal()+
  labs(y="Sum", x = NULL, color = NULL, title="Publication rates 2006-2017")+
  theme(legend.position = c(0.2, 0.8))
ggsave("outputs/publications/pub_rates_20062017_pc.jpeg")

# # pub rates by stages
ggplot(pub_count_year_search %>% 
         group_by(stages, year) %>% 
         summarise(sum = sum(n)) %>% 
         filter(year > 1979 & year < 2018), aes(year, sum, color = stages))+
  geom_line(method = "loess", se=F)+
  theme_minimal()+
  labs(y="Sum", x = NULL, color = NULL, title="Publication rates since 1980")+
  theme(legend.position = c(0.2, 0.8))
ggsave("outputs/publications/pub_rates_19802017_stages.jpeg")

ggplot(pub_count_year_search %>% 
         group_by(stages, year) %>% 
         summarise(sum = sum(n)) %>% 
         filter(year > 2005 & year < 2018), aes(year, sum, color = stages))+
  geom_line(method = "loess", se=F)+
  theme_minimal()+
  labs(y="Sum", x = NULL, color = NULL, title="Publication rates 2006-2017")+
  theme(legend.position = c(0.2, 0.8))+
  scale_x_discrete(limits = c(2006, 2008, 2010, 2012, 2014, 2016))
ggsave("outputs/publications/pub_rates_20062017_stages.jpeg")

# pub rates by search name
ggplot(pub_count_year_search %>% 
         group_by(search_name, year) %>%
         summarise(sum = sum(n)) %>% 
         filter(year > 1979 & year < 2018) %>% 
         mutate(labs = ifelse(year == 2018, search_name, "")), aes(year, sum))+
  geom_line(aes( color = search_name), method = "loess", se=F)+
  theme_minimal()+
  labs(y="Sum", x = NULL, color = NULL, title="Publication rates 1980-2017")+
  theme(legend.position = c(0.2, 0.7))
ggsave("outputs/publications/pub_rates_19802017.jpeg")

ggplot(pub_count_year_search %>% 
         group_by(search_name, year) %>%
         summarise(sum = sum(n)) %>% 
         filter(year > 2005 & year < 2018), aes(year, sum))+
  geom_line(aes( color = search_name), method = "loess", se=F)+
  theme_minimal()+
  labs(y="Sum", x = NULL, color = NULL, title="Publication rates 2006-2017")+
  theme(legend.position = c(0.2, 0.7))+
  scale_x_discrete(limits = c(2006, 2008, 2010, 2012, 2014, 2016))
ggsave("outputs/publications/pub_rates_20052017.jpeg")

# comparing means for search terms
pub_count_year_search$year = factor(pub_count_year_search$year)
aov = lm(pub_count_year_search$n ~ pub_count_year_search$search_name, data = pub_count_year_search)
anova(aov) # p = 0.000861
confint(aov)

aov = lm(pub_count_year_search$n ~ pub_count_year_search$stages, data = pub_count_year_search)
anova(aov)
confint(aov)

# 1990-1999 ~ search
pub_count_year_search %>% 
  filter(year>1990 & year<2000) %>% 
  group_by(search_name) %>% 
  summarise(mean = mean(n), 
            count = sum(n)) %>% 
  ungroup() %>% 
  mutate(pc = count/sum(count))

# 2000-2009 ~ search
pub_count_year_search %>% 
  filter(year>2000 & year < 2010) %>% 
  group_by(search_name) %>% 
  summarise(mean = mean(n), 
            count = sum(n)) %>% 
  ungroup() %>% 
  mutate(pc = count/sum(count))

# since 2010 ~ search
pub_count_year_search %>% 
  filter(year>2009) %>% 
  group_by(search_name) %>% 
  summarise(mean = mean(n), 
            count = sum(n)) %>% 
  ungroup() %>% 
  mutate(pc = count/sum(count))


##---- Popularity
# start building pop score
query = 'select  article_to_search.article_doi, search_name, readers_count
from altmetrics, article_to_search
where altmetrics.article_doi=article_to_search.article_doi'
readers = dbGetQuery(con, query)

readers[3:ncol(readers)] = sapply(readers[3:ncol(readers)],as.numeric)
readers_gather = readers %>% 
  gather(key = type, value = readers_count, -c(article_doi, search_name)) %>% 
  group_by(article_doi) %>% 
  summarise(readers_sum_count = sum(readers_count, na.rm = T))

query = 'select  article_to_search.article_doi, search_name, views
from altmetrics, article_to_search
where altmetrics.article_doi=article_to_search.article_doi'
views = dbGetQuery(con, query)
views_gather = views %>% 
  gather(key = type, value = views_count, -c(article_doi, search_name)) %>% 
  group_by(article_doi) %>% 
  summarise(views_sum_count = sum(views_count, na.rm = T))

query = 'select  article_to_search.article_doi, search_name, citations,"cohorts.doc", "cohorts.sci", "cohorts.doc", "cohorts.sci", cited_by_peer_review_sites_count, cited_by_book_reviews_count, cited_by_policies_count
from altmetrics, article_to_search
where altmetrics.article_doi=article_to_search.article_doi'
citations = dbGetQuery(con, query)
citations[3:ncol(citations)] = sapply(citations[3:ncol(citations)],as.numeric)

citations_gather = citations %>% 
  gather(key = type, value = citations_count, -c(article_doi, search_name)) %>%
  group_by(article_doi) %>% 
  summarise(citations_sum_count = sum(citations_count, na.rm = T))

query = "select  article_to_search.article_doi, search_name, 
cited_by_fbwalls_count, cited_by_gplus_count, cited_by_rdts_count, cited_by_wikipedia_count, 
cited_by_msm_count, cited_by_feeds_count
from altmetrics, article_to_search
where altmetrics.article_doi=article_to_search.article_doi"
mentions = dbGetQuery(con, query)
mentions[3:ncol(mentions)] = sapply(mentions[3:ncol(mentions)],as.numeric)
mentions_gather = mentions %>% 
  gather(key = type, value = mentions_count, -c(article_doi, search_name)) %>% 
  group_by(article_doi) %>% 
  summarise(mentions_sum_count = sum(mentions_count, na.rm = T))

## aggregations of Pop
article_popularity_99pc = readers_gather %>% 
  left_join(views_gather, by = c("article_doi" = "article_doi")) %>% 
  left_join(citations_gather, by = c("article_doi" = "article_doi")) %>% 
  left_join(mentions_gather, by = c("article_doi" = "article_doi")) %>% 
  select(article_doi, ends_with("count"), ends_with("_z")) %>% 
  mutate(mentions = mentions_sum_count,
         readership = readers_sum_count,
         citations = citations_sum_count,
         exposure = readers_sum_count+views_sum_count+citations_sum_count+mentions_sum_count) %>% 
  mutate(popularity = (exposure*0.2)+(readership*0.8)+(mentions)) %>% 
  mutate(pop_z = (popularity-mean(popularity))/sd(popularity)) %>% 
  filter(pop_z > -qnorm(0.99) & pop_z < qnorm(0.99))


## enhance Pop with search name and year
pub_by_year_search_pc = pub_by_year_search %>% 
  filter(search_name %in% pc_cats)


pop_year_search = article_popularity_99pc %>% 
  inner_join(pub_by_year_search_pc) %>% filter(!(search_name %in% meta_cats)) %>% 
  filter(!(search_name %in% meta_cats)) %>% 
  select(article_doi, search_name, year, everything()) 

pop_year_search$search_name = factor(pop_year_search$search_name, levels = reorder_pc)
pop_year_search_sum = pop_year_search %>%
  #add_count(search_name, year) %>% 
  group_by(search_name, year) %>% 
  summarise(pop_sum = sum(popularity),
            readers = sum(readership), 
            mentions = sum(mentions),
            citations = sum(citations),
            pub_count = n()) %>% 
  filter(year > 2000 &year < 2018, 
         search_name %in% pc_cats)

pub_pop_ratio = median(pop_year_search_sum$pub_count)/median(pop_year_search_sum$pop_sum)

pop_year_search_sum_gather = pop_year_search_sum %>% 
  gather(type, value, c(readers, mentions, citations))
# ggplot(pop_year_search_sum, aes(x= year))+
#   geom_line(aes(y = pub_count, color = search_name), show.legend = F)+
#   facet_wrap(~search_name, nrow = 2)+
#   #scale_y_continuous(sec.axis = sec_axis(~.*0.00221342, name = "Publication count"), name = "Popularity score") + facet_wrap(~search_name)+
#   #scale_colour_manual(values = c("blue", "red"))+
#   theme_minimal()+
#   labs(x = "Year",
#        y = "Counts",
#        colour = "", title = "Publication trends", caption = "Source data from Taylor and Francis Online and Altmetrics, 2018")
#   #theme(legend.position = c(0.9, 0.15))
# ggsave("../docs/outputs/pubs_search_year.jpeg")

pub_pop_ratio = median(pop_year_search_sum$pub_count)/median(pop_year_search_sum$pop_sum)
ggplot(pop_year_search_sum, aes(x= year))+
  geom_line(aes(y = pop_sum, color = "Popularity"))+
  geom_line(aes(y = pub_count/pub_pop_ratio, color = "Publications"))+
  scale_y_continuous(sec.axis = sec_axis(~.*pub_pop_ratio, name = "Publication count"), name = "Popularity score") + 
  facet_wrap(~search_name, nrow = 2, scale="free" )+
  scale_colour_manual(values = c("blue", "red"))+
  scale_x_discrete(limits = c(2005, 2010, 2015))+
  theme_minimal()+
  #theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x = "Year",
       colour = "", title = "Publication trends and their popularity", caption = "Source data from Taylor and Francis Online and Altmetrics, 2018")
  #theme(legend.position = c(0.9, 0.15))
ggsave("outputs/popularity/pub_v_pop_by_search.jpeg")

ggplot(pop_year_search_sum_gather, aes(value, pub_count, colour=type))+
  geom_point()+
  labs(x="Popularity aggregate", y = "Publication count", colour="Search terms")

# aov = lm(pop_year_search_sum$pop_sum ~ pop_year_search_sum$search_name, data = pop_year_search_sum)
# anova(aov)
# confint(aov)

year_tot = pop_year_search_sum %>%
  group_by(year, search_name) %>%
  filter(search_name %in% pc_cats) %>%
  summarise(pop = sum(pop_sum),
            readers = sum(readers),
            mentions = sum(mentions),
            citations = sum(citations),
            pub =sum(pub_count),
            pvc = ifelse(search_name %in% peace_cats, "peace-oriented", "conflict-oriented"), 
            stages = ifelse(search_name %in% long_pc, "long-term", ifelse(search_name %in% middle_pc, "medium-term", "short-term"))) %>%
  filter(year > 2000 &year < 2018)
# pub_pop_ratio = median(year_tot$pub_sum_tot)/median(year_tot$pop_sum_tot)
# ggplot(year_tot, aes(x= year))+
#   geom_line(aes(y = pop_sum_tot, color = "Popularity"))+
#   geom_line(aes(y = pub_sum_tot/pub_pop_ratio, color = "Publications"))+
#   scale_y_continuous(sec.axis = sec_axis(~.*pub_pop_ratio, name = "Publication count"), name = "Popularity score") +
#   scale_colour_manual(values = c("blue", "red"))+
#   facet_wrap(~search_name)+
#   theme_minimal()+
#   labs(x = "Year",
#        colour = "", title = "Publication trends and their popularity", caption = "Source data from Taylor and Francis Online and Altmetrics, 2018")+
#   theme(legend.position = c(0.9, 0.15))
# ggsave("../docs/outputs/pub_v_pop.jpeg")

# populary ~ pvc
year_tot_gather = year_tot %>% 
  gather(type, value, c("readers", "mentions", "citations"))


ggplot(year_tot_gather, aes(value, pub, colour=type, shape=stages))+
  geom_point()

pub_pop_ratio = median(year_tot$pub_sum_tot)/median(year_tot$pop_sum_tot)
ggplot(year_tot %>% 
         group_by(year, pvc) %>% 
         summarise(pop_sum_tot = sum(pop_sum_tot), 
                   pub_sum_tot = sum(pub_sum_tot)), aes(x= year))+
  geom_line(aes(y = pop_sum_tot, color = "Popularity"))+
  geom_line(aes(y = pub_sum_tot/pub_pop_ratio, color = "Publications"))+
  facet_wrap(~pvc)+
  scale_y_continuous(sec.axis = sec_axis(~.*pub_pop_ratio, name = "Publication count"), name = "Popularity score") +
  scale_colour_manual(values = c("blue", "red"))+
  theme_minimal()+
  labs(x = "Year",
       colour = "", title = "Publication trends and their popularity", caption = "Source data from Taylor and Francis Online and Altmetrics, 2018")+
  theme(legend.position = c(0.15, 0.8))
ggsave("outputs/popularity/pub_v_pop_pvc.jpeg")

aov = lm(year_tot$pop_sum_tot ~ year_tot$pvc, data = year_tot)
anova(aov)
confint(aov)

year_tot$stages = factor(year_tot$stages, levels = c("short-term", "medium-term", "long-term"))
# popularity ~ stages
ggplot(year_tot %>% 
         group_by(year, stages) %>% 
         summarise(pop_sum_tot = sum(pop_sum_tot), 
                   pub_sum_tot = sum(pub_sum_tot)), aes(x= year))+
  geom_line(aes(y = pop_sum_tot, color = "Popularity"))+
  geom_line(aes(y = pub_sum_tot/pub_pop_ratio, color = "Publications"))+
  facet_wrap(~stages, scales = "free")+
  scale_y_continuous(sec.axis = sec_axis(~.*pub_pop_ratio, name = "Publication count"), name = "Popularity score") +
  scale_colour_manual(values = c("blue", "red"))+
  theme_minimal()+
  labs(x = NULL,
       colour = NULL, title = "Publication rate against popularity", caption = "Publication data from Taylor and Francis Online, 2018\n
       Popularity data from Taylor and Francis and Altmetric, 2018")+
  theme(legend.position = c(0.1, 0.9))
ggsave("outputs/popularity/pub_v_pop_stages.jpeg", width = 10)

aov = lm(year_tot$pop_sum_tot ~ year_tot$stages, data = year_tot)
anova(aov)
confint(aov)

year_tot_2010 = year_tot %>% 
  filter(year > 2009)

ggplot(year_tot_2010 %>% 
         group_by(year, stages) %>% 
         summarise(pop_sum_tot = sum(pop_sum_tot), 
                   pub_sum_tot = sum(pub_sum_tot)), aes(x= year))+
  geom_line(aes(y = pop_sum_tot, color = "Popularity"))+
  geom_line(aes(y = pub_sum_tot/pub_pop_ratio, color = "Publications"))+
  facet_wrap(~stages, scales = "free")+
  scale_y_continuous(sec.axis = sec_axis(~.*pub_pop_ratio, name = "Publication count"), name = "Popularity score") +
  scale_colour_manual(values = c("blue", "red"))+
  theme_minimal()+
  labs(x = NULL,
       colour = NULL, title = "Publication rate against popularity", subtitle = "Since 2010", caption = "Publication data from Taylor and Francis Online, 2018\n
       Popularity data from Taylor and Francis and Altmetric, 2018")+
  theme(legend.position = c(0.1, 0.9))
ggsave("outputs/popularity/pub_v_pop_stages_2010.jpeg", width = 10)


summary_year_tot = year_tot_2010 %>% 
  group_by(stages, year) %>%
  distinct(year, .keep_all = T) %>% 
  summarise(pop_pub_ratio = pop_sum_tot/pub_sum_tot) %>% 
  group_by(stages) %>% 
  summarise(mean= mean(pop_pub_ratio))

aov = lm(summary_year_tot$mean ~ summary_year_tot$stages)
anova(aov)
confint(aov)


## 
aov = lm(year_tot$reader_sum_tot ~ year_tot$stages, data = year_tot)
anova(aov)
confint(aov)

aov = lm(year_tot$follow_sum_tot ~ year_tot$stages, data = year_tot)
anova(aov)
confint(aov)

aov = lm(year_tot$pub_sum_tot ~ year_tot$stages, data = year_tot)
anova(aov)
confint(aov)

read_follow_ratio = median(pop_year_search_sum$reader_sum)/median(pop_year_search_sum$follow_sum)
ggplot(pop_year_search_sum, aes(year))+
  geom_line(aes(y = reader_sum, color = "Readership"))+
  geom_line(aes(y = follow_sum*read_follow_ratio, color = "Followership"))+
  scale_y_continuous(sec.axis = sec_axis(~./read_follow_ratio, name = "Followership"), name = "Readership") + 
  facet_wrap(~search_name, nrow = 2)+
  scale_colour_manual(values = c("darkgreen", "orange"))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x = "Year",
       colour = "", title = "Correlations between readership and followership", caption = "Source data from Taylor and Francis Online and Altmetrics, 2018")
  #theme(legend.position = c(0.9, 0.15))
ggsave("../docs/outputs/reader_v_follower.jpeg")


