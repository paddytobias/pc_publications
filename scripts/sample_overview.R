pb_pk = nrow(intersect(pb, pk))
library(ggthemes)
library(tidyverse)
library(gridExtra)
source("database/db_connect.R")


query = "select * from author_name"
author_name = dbGetQuery(con, query)


query = "SELECT article_doi, search_name from article_to_search"
articles_search = dbGetQuery(con, query)


articles_search = articles_search %>% 
  filter(search_name %in% pc_cats)

# articles_search = articles_search[,2:ncol(articles_search)]
articles_search %>% 
  group_by(search_name) %>% 
  summarise(sum = n())

query = "SELECT * from articles a, article_to_search s where a.article_doi=s.article_doi"
articles = dbGetQuery(con, query)

articles = articles[,2:ncol(articles)]
articles %>% 
  filter(year !="") %>% 
  group_by(search_name) %>% 
  summarise(n = n())

query = "SELECT * from altmetrics a, article_to_search s where a.article_doi=s.article_doi"
altmetrics_search = dbGetQuery(con, query)

altmetrics_search = altmetrics_search[,2:ncol(altmetrics_search)]



query = "SELECT * from article_to_country c, article_to_search s where s.article_doi=c.article_doi"
article_country = dbGetQuery(con, query)

altmetrics_search %>% 
  #select(-1) %>% 
  #filter(!is.na(year)) %>% 
  gather(key = key, value = value, -c(search_name, article_doi)) %>% 
  na.omit() %>% 
  group_by(search_name) %>% 
  summarise(title_count = n())

altmetrics_search %>% 
  filter(search_name %in% pc_cats) %>% 
  group_by(search_name) %>% 
  summarise(n = n())

article_country = article_country[,2:ncol(article_country)]

article_country %>% 
  group_by(search_name) %>% 
  summarise(country_in_title = sum(title_appear), 
            country_in_abstract = sum(abstract_appear))



##---- search overlaps

overlaps = articles_search[duplicated(articles_search$article_doi), ]

uniques = articles %>% 
  filter(search_name %in% pc_cats) %>% 
  anti_join(overlaps) %>% 
  group_by(search_name) %>% 
  summarise(n = n())

overlaps %>% 
  group_by(search_name) %>% 
  summarise(n=n())
  
unique_overlaps = unique(overlaps$article_doi)


overlap_df = overlaps %>% 
  rownames_to_column() %>% 
  spread(search_name, rowname)


pb = articles_search %>% 
  filter(search_name =="peacebuilding") %>% 
  select(article_doi)
pm = articles_search %>% 
  filter(search_name =="peacemaking") %>% 
  select(article_doi)

pk = articles_search %>% 
  filter(search_name =="peacekeeping") %>% 
  select(article_doi)

cp = articles_search %>% 
  filter(search_name =="conflict prevention") %>% 
  select(article_doi)

cr= articles_search %>% 
  filter(search_name =="conflict resolution") %>% 
  select(article_doi)

ct = articles_search %>% 
  filter(search_name =="conflict transformation") %>% 
  select(article_doi)


pb_pb = round(nrow(intersect(pb, pb))/nrow(pb),2)
pb_pm = round(nrow(intersect(pb, pm))/nrow(pb),2)
pb_pk = round(nrow(intersect(pb, pk))/nrow(pb),2)
pb_cp = round(nrow(intersect(pb, cp))/nrow(pb),2)
pb_cr = round(nrow(intersect(pb, cr))/nrow(pb),2)
pb_ct = round(nrow(intersect(pb, ct))/nrow(pb),2)


pm_pb = round(nrow(intersect(pm, pb))/nrow(pm),2)
pm_pm = round(nrow(intersect(pm, pm))/nrow(pm),2)
pm_pk = round(nrow(intersect(pm, pk))/nrow(pm),2)
pm_cp = round(nrow(intersect(pm, cp))/nrow(pm),2)
pm_cr = round(nrow(intersect(pm, cr))/nrow(pm),2)
pm_ct = round(nrow(intersect(pm, ct))/nrow(pm),2)

pk_pb = round(nrow(intersect(pk, pb))/nrow(pk),2)
pk_pm = round(nrow(intersect(pk, pm))/nrow(pk),2)
pk_pk = round(nrow(intersect(pk, pk))/nrow(pk),2)
pk_cp = round(nrow(intersect(pk, cp))/nrow(pk),2)
pk_cr = round(nrow(intersect(pk, cr))/nrow(pk),2)
pk_ct = round(nrow(intersect(pk, ct))/nrow(pk),2)

ct_pb = round(nrow(intersect(ct, pb))/nrow(ct),2)
ct_pm = round(nrow(intersect(ct, pm))/nrow(ct),2)
ct_pk = round(nrow(intersect(ct, pk))/nrow(ct),2)
ct_cp = round(nrow(intersect(ct, cp))/nrow(ct),2)
ct_cr = round(nrow(intersect(ct, cr))/nrow(ct),2)
ct_ct = round(nrow(intersect(ct, ct))/nrow(ct),2)

cr_pb = round(nrow(intersect(cr, pb))/nrow(cr),2)
cr_pm = round(nrow(intersect(cr, pm))/nrow(cr),2)
cr_pk = round(nrow(intersect(cr, pk))/nrow(cr),2)
cr_cp = round(nrow(intersect(cr, cp))/nrow(cr),2)
cr_cr = round(nrow(intersect(cr, cr))/nrow(cr),2)
cr_ct = round(nrow(intersect(cr, ct))/nrow(cr),2)

cp_pb = round(nrow(intersect(cp, pb))/nrow(cp),2)
cp_pm = round(nrow(intersect(cp, pm))/nrow(cp),2)
cp_pk = round(nrow(intersect(cp, pk))/nrow(cp),2)
cp_cp = round(nrow(intersect(cp, cp))/nrow(cp),2)
cp_cr = round(nrow(intersect(cp, cr))/nrow(cp),2)
cp_ct = round(nrow(intersect(cp, ct))/nrow(cp),2)

cp_prop = c(cp_cp, cp_cr, cp_ct, cp_pk, cp_pm, cp_pb)
cr_prop = c(cr_cp, cr_cr, cr_ct, cr_pk, cr_pm, cr_pb)
ct_prop = c(ct_cp, ct_cr, ct_ct, ct_pk, ct_pm, ct_pb)
pk_prop = c(pk_cp, pk_cr, pk_ct, pk_pk, pk_pm, pk_pb)
pm_prop = c(pm_cp, pm_cr, pm_ct, pm_pk, pm_pm, pm_pb)
pb_prop = c(pb_cp, pb_cr, pb_ct, pb_pk, pb_pm, pb_pb)

props = data.frame(pc_cats, cp_prop, cr_prop, ct_prop, pk_prop, pm_prop, pb_prop)

props_gather = props %>% 
  gather(key, value, ends_with("_prop")) %>% 
  mutate(names = ifelse(grepl("cp_", key), "conflict prevention", 
                        ifelse(grepl("cr_", key), "conflict resolution",
                               ifelse(grepl("ct_", key), "conflict transformation",
                                      ifelse(grepl("pk_", key), "peacekeeping",
                                             ifelse(grepl("pm_", key), "peacemaking",
                                                    ifelse(grepl("pb_", key), "peacebuilding","")))))))

props_gather$pc_cats=factor(props_gather$pc_cats, levels = reorder_pc)
props_gather$names=factor(props_gather$names, levels=reorder_pc)
ggplot(props_gather, aes(pc_cats, names, fill = value))+
  geom_tile(alpha=0.8, show.legend = F)+
  geom_text(aes(label=paste0(value*100, "%")), alpha=0.5, size=3)+
  labs(x=NULL, y=NULL, fill="Overlap %")+
  scale_fill_gradient2(high="tomato", label = scales::percent_format()) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))
ggsave("outputs/publications/overlap_search.jpeg")
write_csv(props, "outputs/publications/overlap_search.csv")

pb_pb = round(nrow(intersect(pb, pb))/nrow(articles_search),2)
pb_pm = round(nrow(intersect(pb, pm))/nrow(articles_search),2)
pb_pk = round(nrow(intersect(pb, pk))/nrow(articles_search),2)
pb_cp = round(nrow(intersect(pb, cp))/nrow(articles_search),2)
pb_cr = round(nrow(intersect(pb, cr))/nrow(articles_search),2)
pb_ct = round(nrow(intersect(pb, ct))/nrow(articles_search),2)


pm_pb = round(nrow(intersect(pm, pb))/nrow(articles_search),2)
pm_pm = round(nrow(intersect(pm, pm))/nrow(articles_search),2)
pm_pk = round(nrow(intersect(pm, pk))/nrow(articles_search),2)
pm_cp = round(nrow(intersect(pm, cp))/nrow(articles_search),2)
pm_cr = round(nrow(intersect(pm, cr))/nrow(articles_search),2)
pm_ct = round(nrow(intersect(pm, ct))/nrow(articles_search),2)

pk_pb = round(nrow(intersect(pk, pb))/nrow(articles_search),2)
pk_pm = round(nrow(intersect(pk, pm))/nrow(articles_search),2)
pk_pk = round(nrow(intersect(pk, pk))/nrow(articles_search),2)
pk_cp = round(nrow(intersect(pk, cp))/nrow(articles_search),2)
pk_cr = round(nrow(intersect(pk, cr))/nrow(articles_search),2)
pk_ct = round(nrow(intersect(pk, ct))/nrow(articles_search),2)

ct_pb = round(nrow(intersect(ct, pb))/nrow(articles_search),2)
ct_pm = round(nrow(intersect(ct, pm))/nrow(articles_search),2)
ct_pk = round(nrow(intersect(ct, pk))/nrow(articles_search),2)
ct_cp = round(nrow(intersect(ct, cp))/nrow(articles_search),2)
ct_cr = round(nrow(intersect(ct, cr))/nrow(articles_search),2)
ct_ct = round(nrow(intersect(ct, ct))/nrow(articles_search),2)

cr_pb = round(nrow(intersect(cr, pb))/nrow(articles_search),2)
cr_pm = round(nrow(intersect(cr, pm))/nrow(articles_search),2)
cr_pk = round(nrow(intersect(cr, pk))/nrow(articles_search),2)
cr_cp = round(nrow(intersect(cr, cp))/nrow(articles_search),2)
cr_cr = round(nrow(intersect(cr, cr))/nrow(articles_search),2)
cr_ct = round(nrow(intersect(cr, ct))/nrow(articles_search),2)

cp_pb = round(nrow(intersect(cp, pb))/nrow(articles_search),2)
cp_pm = round(nrow(intersect(cp, pm))/nrow(articles_search),2)
cp_pk = round(nrow(intersect(cp, pk))/nrow(articles_search),2)
cp_cp = round(nrow(intersect(cp, cp))/nrow(articles_search),2)
cp_cr = round(nrow(intersect(cp, cr))/nrow(articles_search),2)
cp_ct = round(nrow(intersect(cp, ct))/nrow(articles_search),2)

cp_prop = c(cp_cp, cp_cr, cp_ct, cp_pk, cp_pm, cp_pb)
cr_prop = c(cr_cp, cr_cr, cr_ct, cr_pk, cr_pm, cr_pb)
ct_prop = c(ct_cp, ct_cr, ct_ct, ct_pk, ct_pm, ct_pb)
pk_prop = c(pk_cp, pk_cr, pk_ct, pk_pk, pk_pm, pk_pb)
pm_prop = c(pm_cp, pm_cr, pm_ct, pm_pk, pm_pm, pm_pb)
pb_prop = c(pb_cp, pb_cr, pb_ct, pb_pk, pb_pm, pb_pb)

props = data.frame(pc_cats, cp_prop, cr_prop, ct_prop, pk_prop, pm_prop, pb_prop)

props_gather = props %>% 
  gather(key, value, ends_with("_prop")) %>% 
  mutate(names = ifelse(grepl("cp_", key), "conflict prevention", 
                        ifelse(grepl("cr_", key), "conflict resolution",
                               ifelse(grepl("ct_", key), "conflict transformation",
                                      ifelse(grepl("pk_", key), "peacekeeping",
                                             ifelse(grepl("pm_", key), "peacemaking",
                                                    ifelse(grepl("pb_", key), "peacebuilding","")))))))

props_gather$pc_cats=factor(props_gather$pc_cats, levels = reorder_pc)
props_gather$names=factor(props_gather$names, levels=reorder_pc)
ggplot(props_gather, aes(pc_cats, names, fill = value))+
  geom_tile(alpha=0.8, show.legend = F)+
  geom_text(aes(label=paste0(value*100, "%")), alpha=0.5, size=3)+
  labs(x=NULL, y=NULL, fill="Overlap %")+
  scale_fill_gradient2(high="tomato", label = scales::percent_format()) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))
ggsave("outputs/publications/overlap_search.jpeg")
write_csv(props, "outputs/publications/overlap_search.csv")
