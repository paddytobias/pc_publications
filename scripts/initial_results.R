#token <- gs_auth(cache = FALSE)

#libraries
library(googlesheets)
suppressPackageStartupMessages(library("dplyr"))
library(RPostgreSQL)
library(ggplot2)
library(forcats)
library(directlabels)
library(tidyr)
library(ggthemes)
library(gridExtra)
source("database/db_connect.R")
source("blog/globals.R")


#global variables
caption = "Source data from Taylor and Francis Online, 2018"
#subtitle = "Language in peacebuilding"
output_dir = "../docs/outputs/"


#blog_data = gs_title("blog_data")

##--- search_count to put onto Google Drive
#query = "select * from search_count"
#search_count=dbGetQuery(con, query)

#blog_data %>%
  #gs_edit_cells(ws = "search_count", input = search_count, anchor='A1')

##--- publication counts by year and search
query = "SELECT * FROM pub_count_by_year_search"
pub_count_by_year_search = dbGetQuery(con, query)
pub_count_mutated = pub_count_by_year_search %>%
  filter(query_term != "peace and conflict studies" | query_term !="liberal peacebuilding") %>% 
  group_by(query_term)%>%
  mutate(pub_year_z = (publication_count-mean(publication_count))/sd(publication_count))


pub_count_by_year_search$query_term = factor(pub_count_by_year_search$query_term, levels = reorder_search)
ggplot(pub_count_mutated %>%
         filter(year_published > "2005" & year_published <"2017"), aes(x=year_published, y=publication_count))+
  geom_line(show.legend = FALSE, colour = "red")+
  geom_smooth(method = "lm", colour = "grey", size=0.5)+
  #geom_point(aes(colour=factor(query_term), size = 0.5), show.legend = FALSE)+
  facet_wrap(~factor(query_term), nrow = 2)+
  theme_minimal()+
  labs(x = "Year", y = "Count", title = "Publications", caption = caption)
ggsave(paste0(output_dir, "pub_count_year&search_05-17.jpeg"), device = 'jpeg')

## common countries in titles, limit to top 20
query = "select * from common_countries_title_20"
common_countries_title_20 = dbGetQuery(con, query)

common_countries_title_20$names = factor(common_countries_title_20$names, levels = common_countries_title_20$name[order(common_countries_title_20$sum)])
ggplot(common_countries_title_20, aes(x=names, y=sum))+
  geom_bar(show.legend = FALSE, stat = 'identity', fill = "blue", alpha = 0.5)+
  coord_flip()+
  geom_text(aes(label=sum), vjust=0.5, hjust=1)+
  labs(x = "", y = "", title = "Top 20 countries mentioned in titles", caption = caption)+
  theme(axis.text.x = element_text(angle=20, hjust = 1))  
ggsave(paste0(output_dir, "common_countries_titles_20.jpeg"), device = 'jpeg')


##--- common country in titles by years, limit to top 10 most common countries
query = "select * from common_country_title_years_10"
common_country_title_years_10 = dbGetQuery(con, query)
#common_country_title_years_10$names = factor(common_country_title_years_10$names, levels = common_country_title_years_10$names[unique(order(common_country_title_years_10$sum))])

ggplot(common_country_title_years_10, aes(x=year, y = sum, colour = factor(names)))+
  geom_line(aes(alpha = 0.5), show.legend = FALSE)+
  geom_point()+
  labs(x = "Year", y = "Mentions", title = "Country mentions in Titles", caption=caption)+
  guides(colour = guide_legend(title = "Top 10 mentioned countries"))+
  geom_dl(aes(label= factor(names)), method = list("first.points", rot = 0, hjust=0, vjust=-2, cex = 0.6))#+
  #theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))
ggsave(output_dir, "common_country_title_years_10.jpeg", device = "jpeg")

## common_country_abstract_years_10
query = "select * from common_country_abstract_years_10"
common_country_abstract_years_10 = dbGetQuery(con, query)
#common_country_title_years_10$names = factor(common_country_title_years_10$names, levels = common_country_title_years_10$names[unique(order(common_country_title_years_10$sum))])

ggplot(common_country_abstract_years_10 %>% 
         group_by(names) %>% 
         mutate(names_clean = strsplit(names, split = "\\|")[[1]][1], 
                names_clean = ifelse(year>2006, "", names_clean)), aes(x=year, y = sum, colour = factor(names)))+
  geom_line(aes(alpha = 0.5), show.legend = FALSE)+
  geom_point(show.legend = F)+
  theme_minimal()+
  labs(x = "Year", y = "Mentions", title = "Country mentions in Abstracts", caption=caption)+
  guides(colour = guide_legend(title = "Top 10 mentioned countries"))+
  #geom_dl(aes(label= factor(names)), method = list("first.points", rot = 0, hjust=0, vjust=-2, cex = 0.6))+
  geom_label_repel(aes(label = names_clean), size = 2, show.legend = F, nudge_y = 10, max.iter = 2000)

ggsave(paste0(output_dir,"common_country_abstract_years_10",".jpeg"), device = "jpeg")


##--- common countries in abstracts
query = "SELECT * FROM common_countries_abstract_20"
common_countries_abstract_20 = dbGetQuery(con, query)
common_countries_abstract_20$names = factor(common_countries_abstract_20$names, levels = common_countries_abstract_20$names[order(common_countries_abstract_20$sum)])
common_countries_abstract_20 = common_countries_abstract_20 %>% 
  group_by(names) %>% 
  mutate(names_clean = strsplit(as.character(names), split = "\\|")[[1]][1], 
         sum_df = sum,
         names_clean = reorder(names_clean, sum_df))
common_countries_abstract_20$names_clean = factor(common_countries_abstract_20$names_clean, levels = reorder(common_countries_abstract_20$names_clean, common_countries_abstract_20$sum))

ggplot(common_countries_abstract_20, aes(x=names_clean, y=sum_df))+
  geom_bar(show.legend = FALSE, stat = 'identity', fill = "blue", alpha = 0.5)+
  coord_flip()+
  geom_text(aes(label=sum), vjust=0.5, hjust=1)+
  theme_minimal()+
  labs(x = "", y = "", title = "Top 20 countries mentioned in abstracts", caption = caption)+
  theme(axis.text.x = element_text(angle=20, hjust = 1))  
ggsave(paste0(output_dir, "common_countries_abstract_20.jpeg"), device = 'jpeg')


##--- common countries in abstracts search
query  = "SELECT * FROM common_countries_abstract_search_20"
common_countries_abstract_search_20 = dbGetQuery(con, query)

names_reorder = common_countries_abstract_search_20 %>% 
  group_by(names) %>% 
  summarise(sum = sum(sum)) %>% 
  arrange(sum) 

names_reorder = names_reorder$names

#common_countries_abstract_search_20$name = factor(common_countries_abstract_search_20$name, levels = common_countries_abstract_search_20$names[order(common_countries_abstract_search_20$sum, common_countries_abstract_search_20$search_name)])
ggplot(common_countries_abstract_search_20, aes(x=factor(names, levels = names_reorder), y=sum))+
  geom_bar(aes(color = factor(search_name), fill= factor(search_name)), show.legend = TRUE, stat = 'identity')+
  coord_flip()+
  labs(x = "", y = "", title = "Top 20 common countries in abstracts by search term", caption = caption)+
  theme(axis.text.x = element_text(angle=90, hjust = 1))+
  theme_minimal()+
  guides(color = FALSE, fill = guide_legend(title = "Search terms"))
ggsave(paste0(output_dir, "common_countries_abstract_search_20.jpeg"), device = 'jpeg')

query  = "SELECT title_counts, abstract_counts FROM country_count"
countries_titles_abstract = dbGetQuery(con, query)

ggplot(countries_titles_abstract, aes(title_counts, abstract_counts))+
  geom_point()



meta_cats = c("peace and conflict studies", "liberal peacebuilding")
##--- bibliometrics by search
query = "select * from sum_metrics"
sum_metrics = dbGetQuery(con, query) %>% 
  filter(!(search_name%in% meta_cats))

sum_metrics_ratio = sum_metrics %>%
  mutate(view_ratio = round(sum_views/count_total,2), 
         citation_ratio = round(sum_citations/count_total,2), 
         altmetrics_ratio = round(sum_altmetrics/count_total, 2))%>%
  select(search_name, view_ratio, citation_ratio, altmetrics_ratio) 


views_ratio =sum_metrics_ratio[,1:2]
ggplot(views_ratio, aes(x = search_name, y = view_ratio))+
  geom_bar(stat = 'identity', fill = 'blue', alpha = 0.5)+
  labs(x = "", y = "n views per article", title = "Views per article in search", caption = caption)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(paste0(output_dir, "views_ratio.jpeg"), device = 'jpeg')

cite_ratio = sum_metrics_ratio[,c(1,3)]
ggplot(cite_ratio, aes(x = search_name, y = citation_ratio))+
  geom_bar(stat = 'identity', fill = 'blue', alpha = 0.5)+
  labs(x = "", y = "n cites per article", title = "Cites per article in search", caption = caption)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(paste0(output_dir, "cites_ratio.jpeg"), device = 'jpeg')

alts_ratio = sum_metrics_ratio[,c(1,4)]
ggplot(alts_ratio, aes(x = search_name, y = altmetrics_ratio))+
  geom_bar(stat = 'identity', fill = 'blue', alpha = 0.5)+
  labs(x = "", y = "n metrics per article", title = "Altmetics per article in search", caption = caption)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(paste0(output_dir, "alts_ratio.jpeg"), device = 'jpeg')

sum_metrics_gather = sum_metrics[1:4] %>% 
  gather(key = metric_type, value = sum, -search_name )

sum_metrics_gather = sum_metrics_gather %>%
  group_by(metric_type) %>% 
  mutate(ratio = sum/sum(sum))

sum_metrics_gather$metric_type = factor(sum_metrics_gather$metric_type)

levels(sum_metrics_gather$metric_type) = c("altmetrics", "citations", "views")


ggplot(sum_metrics_gather, aes(x=search_name, y = ratio, fill = metric_type))+
  geom_histogram(position = "dodge", stat = "identity") +
  labs(x = "", y = "%", subtitle = "Bibliometrics by search", title = "Statebuilding dominates in citations and altmetrics", caption = caption)+
  guides(color = FALSE, fill = guide_legend(title = "Search terms"))+
  theme_minimal()+
  scale_color_fivethirtyeight()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(paste0(output_dir, "sum_metrics_gather.jpeg"), device = 'jpeg')

## experimental
query = "select search_name, views, citations, altmetrics from article_to_search"
complete_metrics = dbGetQuery(con, query)

ggplot(complete_metrics, aes(citations, altmetrics))+
  geom_point(alpha = 0.4)
ggsave(paste0(output_dir, "cites_by_alts.jpeg"), device = 'jpeg')

# ggplot(complete_metrics, aes(views, citations, color = search_name))+
#   geom_point(alpha =0.4)+
#   xlim(xlim = c(0,30000))
# 
# 
# 
# ggplot(complete_metrics, aes(views, altmetrics, colour = search_name))+
#   geom_point(alpha = 0.4)+
#   xlim(xlim = c(0,10000))+
#   ylim(ylim=c(0,200))
  

##---- bibliometrics over the years (05-17) by search term
query = "SELECT DISTINCT search_name, year, sum(views) as views, sum(citations) as citations, sum(altmetrics) as altmetrics
        FROM article_to_search s, articles a
WHERE a.article_doi=s.article_doi 
GROUP BY search_name, year order by year"

year_metrics = dbGetQuery(con, query) %>% 
  filter(!(search_name%in% meta_cats) & !(search_name %in% proxy_cats)) %>% 
  group_by(search_name) %>%
  mutate(view_normal = views/max(views), 
         cites_normal = citations/max(citations), 
         alts_normal = altmetrics/max(altmetrics)) 

year_metrics$search_name = factor(year_metrics$search_name, levels=reorder_pc)
ggplot(year_metrics %>% 
         filter(year>2005 & year<2017) %>%
         group_by(year, search_name) %>% 
         mutate(avg = (views+citations+altmetrics)/3), aes(x = year, color = search_name))+
  #geom_smooth(aes(y = view_normal, colour = "Page views"), se = F)+
  #geom_smooth(aes(y = cites_normal, colour = "Citations (CrossRef)"), se=F)+
  #geom_smooth(aes(y = alts_normal, colour = "Altmetric"), se=F)+
  #geom_line(aes(y = avg, colour = "Average"))+
  geom_smooth(aes(y=avg), method = 'loess', se=F, size=0.5)+
  #ylim(ylim = c(-0.4,1.2))+
  labs(x = "Year", y = "Trends score", 
       subtitle = "Bibliometric trends by search name, 2005-17", title= "Conflict and 2012?", 
       caption=caption, alpha = NULL)+
  guides(colour = guide_legend(title = "Search names"))+
  #facet_wrap(~search_name, nrow = 2)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=45))+ annotate("text", x = 2008, y = 1e+05, size=3, label = "Trend lines =\naverage of page views,\ncitation counts and altmetric score)")
  ggsave(paste0(output_dir, "bibmetrics_year&search_05-17.jpeg"), device = 'jpeg')


############# EXPERIMENTAL ########
#query = "select * from views_ratio_year"
#views_ratio_year = dbGetQuery(con, query)

#ggplot(views_ratio_year %>% filter(date_part>2005 & date_part<2017), aes(x = date_part, y = sum))+
#  geom_line(aes(color = factor(search_name)))+
#  facet_wrap(~search_name)

#query = "select * from cites_ratio_year"
#cites_ratio_year = dbGetQuery(con, query)

#ggplot(cites_ratio_year %>% filter(date_part>2005 & date_part<2017), aes(x = date_part, y = sum))+
#  geom_line(aes(color = factor(search_name)))+
#  facet_wrap(~search_name)


query = "select names, sum(views) as views, sum(title_appear) as titles, sum(abstract_appear) as abstracts from country_names, article_to_country, article_to_search where index=country_id and article_to_country.article_doi=article_to_search.article_doi group by names"
country_counts_views = dbGetQuery(con, query)

top_ten_titles = country_counts_views %>% 
  arrange(views, titles) %>% 
  top_n(10, views)

titles_views = ggplot(country_counts_views, aes(titles, views))+
  geom_point()+
  geom_label(aes(label=names), data = top_ten_titles, alpha = 0.5)+
  theme_bw()+
  ylab("Article view no.")+
  xlab("Country count in Titles")


top_ten_abstracts = country_counts_views %>% 
  arrange(views, abstracts) %>% 
  top_n(10, views)
abstracts_views = ggplot(country_counts_views, aes(abstracts, views))+
  geom_point()+
  geom_label(aes(label=names), data = top_ten_abstracts, alpha = 0.5)+
  ylab("")+
  xlab("Country count in Abstracts")+
  theme_bw()
grid.arrange(titles_views, abstracts_views, nrow=1, top="The more mentions of countries the higher the views")
ggsave(paste0(output_dir, "country_count&views.jpeg"), device = 'jpeg')
