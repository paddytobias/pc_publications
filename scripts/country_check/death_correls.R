library(tidyverse)
library(ggrepel)
library(fuzzyjoin)
library(DescTools)
source("database/db_connect.R")
source("blog/globals.R")

# publication rates
query = "SELECT a.article_doi AS article_doi, 
a.year AS year, 
s.search_name AS search_name
FROM articles a, article_to_search s
WHERE a.article_doi = s.article_doi"
pub_by_year_search = dbGetQuery(con, query)

## Uppsala death dat 
death_dat = suppressWarnings(suppressMessages(read_csv("database/database_files/ucdp-brd-conf-181.csv")))


# by conflicts per year
conflict_year = death_dat %>% 
  count(year, location_inc) %>% 
  rename(conflict_n=n)

# by deaths per year
deaths_year = death_dat %>% 
  group_by(year 
           #,location_inc
           ) %>% 
  summarise(deaths_n = sum(bdbest))

# pubs per year
pub_count = pub_by_year_search %>% 
  count(year) 

pub_search_count = pub_by_year_search %>% 
  count(year, search_name) 

# join pubs and conflicts
pub_conflicts = pub_count %>% 
  inner_join(conflict_year, by = c("year"="year")) %>% 
  rename(pub_n=n, country=location_inc) %>% 
  select(year, country, pub_n, conflict_n)

# join pubs and deaths
pub_deaths = deaths_year %>% 
  inner_join(pub_count, by = c("year"="year")) %>% 
  rename(pub_n=n
         #, country = location_inc
         )

pub_search_deaths = deaths_year %>% 
  inner_join(pub_search_count) %>% 
  rename(pub_n=n, country = location_inc)

##---- deaths to publication rates, by year
pub_deaths_by_year = pub_deaths %>% 
  group_by(year, pub_n) %>% 
  summarise(deaths_n = sum(deaths_n)) %>% 
  mutate(deaths_to_pubs = deaths_n/pub_n) %>% 
  ungroup() %>% 
  mutate(pub_death_ratio = mean(pub_n)/mean(deaths_n))

pub_death_ratio = mean(pub_deaths_by_year$pub_n)/mean(pub_deaths_by_year$deaths_n)


p= ggplot(pub_deaths_by_year, aes(year, pub_n, size=deaths_n, text=sprintf("%s<br>Pubs count: %s<br>Death toll: %s", year, pub_n, deaths_n)))+
  geom_point()+
  scale_size_continuous(range=c(0.2,3))+
  theme_minimal()+
  labs(x=NULL, y="Publications count")
ggplotly(p, tooltip = "text")

p= ggplot(pub_deaths_by_year, aes(year, deaths_n, label=paste0("<br><br>", year, "<br>Death toll: ", deaths_n,"<br>Pubs count: ", pub_n)))+
  geom_line()+
  scale_size_continuous(range=c(0.2,3))+
  theme_minimal()+
  labs(x=NULL, y="Death toll")
ggplotly(p, tooltip = "label")


ggplot(pub_deaths_by_year, aes(x = year)) +
  geom_line(aes(y=pub_n, color = "Pub count")) +
  geom_line(aes(y=deaths_n*pub_death_ratio, color = "Death toll")) +
  scale_y_continuous(sec.axis = sec_axis(~./pub_death_ratio, name = "Death toll"), name = "Pub count")+
  theme_minimal()+
  labs(x=NULL, color=NULL, title="Publication rates against deaths from conflict", 
       caption = "Publication data from Taylor and Francis Online, 2018\nDeath toll data from Pettersson, Therése and Kristine Eck, 2018")+
  theme(legend.position = c(0.15, 0.8))
ggsave("outputs/conflict/pub_death.jpeg")

pub_deaths_by_decade = pub_deaths_by_year %>% 
  mutate(decade = ifelse(year>=1990 & year <2000, "90s",
                         ifelse(year>=2000 & year <2010, "00s", 
                                ifelse(year>=2010, "10s", NA)))) %>%
  group_by(decade) %>% 
  summarise(deaths_to_pubs = sum(deaths_to_pubs))


pub_search_deaths_by_year = pub_search_deaths %>% 
  filter(search_name %in% pc_cats) %>% 
  group_by(year, search_name, pub_n) %>% 
  summarise(deaths_n = sum(deaths_n)) %>% 
  mutate(deaths_to_pubs = deaths_n/pub_n) %>% 
  group_by(search_name) %>% 
  mutate(pub_death_prop = mean(pub_n)/mean(deaths_n), 
         pub_death_norm = deaths_n*pub_death_prop)

pub_search_deaths_by_year$search_name = factor(pub_search_deaths_by_year$search_name, levels = reorder_pc)
pub_death_ratio = mean(pub_search_deaths_by_year$pub_n)/mean(pub_search_deaths_by_year$deaths_n)

ggplot(pub_search_deaths_by_year, aes(x = year)) +
  geom_line(aes(y=pub_n, color = "Pub count")) +
  geom_line(aes(y=pub_death_norm, color = "Death toll")) +
  scale_y_continuous(sec.axis = sec_axis(~./pub_death_ratio, name = "Death toll"), name = "Pub count")+
  theme_minimal()+
  facet_wrap(~search_name,scales = "free")+
  labs(x=NULL, color=NULL, title="Publication rates against deaths from conflict", subtitle="Search term",
       caption = "Publication data from Taylor and Francis Online, 2018\nDeath toll data from Pettersson, Therése and Kristine Eck, 2018")+
  theme(legend.position = c(0.1, 0.9))
ggsave("outputs/conflict/pub_search_death.jpeg", width = 10)  


##---- death to county mention correlations
query  = "SELECT abstract_appear, names, search_name, year FROM article_to_country c, country_names, article_to_search s, articles a
where country_id=index and c.article_doi=s.article_doi and a.article_doi=c.article_doi and abstract_appear<>0"
countries = dbGetQuery(con, query)

countries = countries %>% 
  count(year, names) %>% 
  rename(countries_n=n)

top10_country_pub = countries %>% 
  group_by(names) %>% 
  summarise(country_pub_sum = sum(countries_n)) %>% 
  top_n(10)


top10_country_conflict = conflict_year %>% 
  group_by(location_inc) %>% 
  summarise(country_conflicts = sum(conflict_n)) %>% 
  top_n(10)

# country_pub_conflict = countries %>% 
#   regex_inner_join(conflict_year, by=c("year"="year","names"="location_inc")) %>% 
#   group_by(year.x, names) %>% 
#   summarise(country_pub_sum=sum(countries_n), 
#             conflict_sum=sum(conflict_n)) %>% 
#   filter(year.x>1999) %>% 
#   mutate(labels=ifelse((names %in% top10_country_pub$names) & (year.x==2017), names, ""))
# 
# ggplot(country_pub_conflict, aes(year.x, country_pub_sum, color=names))+
#   geom_point(aes(size=conflict_sum)) +
#   #geom_line(show.legend = F)+
#   theme_minimal()+
#   guides(colour=FALSE)+
#   theme(legend.position = c(0.2, 0.7))+
#   labs(y="Publication n", x=NULL, size="Conflict no.", colour="Countries")+
#   geom_text_repel(aes(label=labels), show.legend = F, nudge_x = -5, nudge_y = 5, max.iter = 2000)

## mentions in abstracts against conflict no.
country_pub_conflict = countries %>% 
  regex_inner_join(conflict_year, by=c("names"="location_inc")) %>% 
  group_by(names) %>% 
  summarise(conflict_sum = sum(conflict_n), 
            pubs_sum = sum(countries_n)) %>% 
  mutate(labels=ifelse((names %in% top10_country_pub$names), names, ""))

ggplot(country_pub_conflict, aes(conflict_sum, pubs_sum))+
  geom_point() +
  #geom_smooth(method = "lm", se=F, size=0.4, colour = "tomato")+
  theme_minimal()+
  guides(colour=FALSE)+
  theme(legend.position = c(0.2, 0.7))+
  geom_text_repel(aes(label=labels), show.legend = F, nudge_x = 10, nudge_y = 100)+
  labs(y="Country mentions in publications", x="Conflicts per country", color=NULL,subtitle="1989-2017", title="Country mentions in abstracts and Conflict",
       caption = "Publication data from Taylor and Francis Online, 2018\nDeath toll data from Pettersson, Therése and Kristine Eck, 2018")
ggsave("outputs/conflict/mentions_v_conflicts.jpeg")

country_pub_conflict_year = countries %>% 
  regex_inner_join(conflict_year, by=c("names"="location_inc", "year"="year")) %>% 
  group_by(names, year.x) %>% 
  summarise(conflict_sum = sum(conflict_n), 
            pubs_sum = sum(countries_n)) %>% 
  mutate(labels=ifelse((names %in% top10_country_pub$names), names, ""))

ggplot(country_pub_conflict_year, aes(conflict_sum, pubs_sum))+
  geom_point() +
  #geom_smooth(method = "lm", se=F, size=0.4, colour = "tomato")+
  theme_minimal()+
  guides(colour=FALSE)+
  theme(legend.position = c(0.2, 0.7))+
  geom_text_repel(aes(label=labels), show.legend = F, nudge_x = 10, nudge_y = 100)+
  # Here comes the gganimate specific bits
  labs(title = 'Year: {frame_time}', x = '', y = '') +
  transition_time(year.x) +
  ease_aes('linear')


## by death count
country_pub_deaths = countries %>% 
  regex_inner_join(deaths_year, by=c("names"="location_inc")) %>% 
  group_by(names) %>% 
  summarise(deaths_sum = sum(deaths_n), 
            pubs_sum = sum(countries_n)) %>% 
  mutate(labels=ifelse((names %in% top10_country_pub$names), names, ""))

ggplot(country_pub_deaths, aes(deaths_sum, pubs_sum))+
  geom_point(aes(color=names), show.legend = F) +
  geom_smooth(method = "lm", se=F, size=0.4, colour = "tomato")+
  theme_minimal()+
  guides(colour=FALSE)+
  theme(legend.position = c(0.2, 0.7))+
  geom_text_repel(aes(label=labels), show.legend = F, nudge_x = 10, nudge_y = 100)+
  labs(y="Country mentions in publications", x="Deaths per country", color=NULL, subtitle="1989-2017", title="Country mentions in abstracts and Death toll",
       caption = "Publication data from Taylor and Francis Online, 2018\nDeath toll data from Pettersson, Therése and Kristine Eck, 2018")
ggsave("outputs/conflict/mentions_v_deaths.jpeg")


# country_pub_deaths_year = countries %>% 
#   regex_inner_join(deaths_year, by=c("names"="location_inc")) %>% 
#   group_by(year.x, names) %>% 
#   summarise(deaths_sum = sum(deaths_n), 
#             mentions_sum = sum(countries_n)) %>% 
#   filter(year.x > 1999) %>% 
#   mutate(deaths_to_mentions = deaths_sum/mentions_sum) %>% 
#   group_by(year.x) %>%
#   summarise(deaths_to_mentions_year = sum(deaths_to_mentions))
# 
# #mentions_death_ratio = mean(country_pub_deaths_year$mentions_sum/country_pub_deaths_year$deaths_sum)
# ggplot(country_pub_deaths_year, aes(year.x))+
#   geom_line(aes(y=deaths_to_mentions_year), show.legend = F)
  
#   
#   
# country_pub_deaths = countries %>% 
#   filter(!is.na(year)) %>% 
#   distinct(year, names) %>% 
#   regex_inner_join(deaths_year, by=c("year"="year","names"="location_inc")) %>% 
#   group_by(year.x, names) %>% 
#   summarise(deaths_year=sum(deaths_year)) %>% 
#   filter(year.x>1999) %>% 
#   mutate(labels=ifelse((names %in% top10_country_pub$names) & (year.x==2017), names, ""))
# 
# ggplot(country_pub_deaths, aes(year.x, deaths_year, color=names))+
#   geom_point(aes(size=deaths_year), show.legend = F) +
#   theme_minimal()+
#   theme(legend.position = c(0.3, 0.5))+
#   labs(y="Publication n", x=NULL, size="Deaths", colour="Countries")+
#   geom_text_repel(aes(label=labels), show.legend = F, nudge_x = -5, nudge_y = 10, max.iter = 2000)
# 
# 
# 
