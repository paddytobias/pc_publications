library(rAltmetric)
library(purrr)
library(tidyverse)
source("database/db_connect.R")
source("login.R")




query = "select article_doi from articles"
article_dois = dbGetQuery(con, query)

article_dois$dois = gsub("(.*)/(.*)/(.*)$", "\\2/\\3", article_dois$article_doi)

dois = article_dois$dois

# Safely allows you to wrap any function around it and capture errors
# In this case we return NULL when the API does not find a DOI
safe_altmetrics <- purrr::safely(altmetrics, otherwise = NULL)
alm <- function(x)  safe_altmetrics(doi = x, apikey = apikey)

start = Sys.time()
# Now we make the API call
requests <- map(dois, alm) 
end = Sys.time()
end-start

# We map the result item from the inner list,
# remove the NULL ones
# then run the altmeric_data on the result objects

results <- requests %>%  
  map("result") %>% 
  compact(.) %>% 
  modify_depth(1, altmetric_data)

dat <- bind_rows(results) %>% select(doi, score, contains("cited"), contains("readers"), contains("cohorts"))

dat = dat %>%
  left_join(article_dois, by = c("doi"="dois"))


names(dat)[1] = "doi"
names(dat)[30] = "article_doi"
dat = dat %>% 
  select(-doi) %>% 
  select(article_doi, score, contains("cited"), contains("readers"), contains("cohorts"))


write_csv(dat, "database/database_files/altmetrics.csv")

# query = "select article_doi from altmetrics"
# alt_doi = dbGetQuery(con, query)
# 
# dat2 = dat %>% 
#   anti_join(alt_doi)

dbWriteTable(con, "altmetrics", value = dat, append = TRUE, row.names = FALSE)

dbDisconnect(con)
# dat_gather = dat %>% 
#   gather(key = 'type', value = 'count', -c(article_doi, score))
#  
# filter_type = 'cited' # 'cited', 'reader', 'cohort'
# dat_gather %>% 
#   filter(type == unique(dat_gather$type)[grepl(filter_type, unique(dat_gather$type))] & count!=0) %>% 
#   ggplot(aes(factor(count, levels = sort(as.numeric(levels(factor(dat_gather$count))))), score))+
#   geom_jitter()



query = "select  search_name, readers_count, title_appear, names from altmetrics, article_to_country, article_to_search, country_names where altmetrics.article_doi=article_to_country.article_doi and country_id=index and article_to_search.article_doi=article_to_country.article_doi"
readers_countries = dbGetQuery(con, query)

readers = readers_countries %>% 
  group_by(names) %>% 
  summarise(Readers = sum(as.numeric(readers_count)))%>%
  top_n(10) %>% 
  arrange(Readers)

readers$names = factor(readers$names, levels = readers$names[order(readers$Readers)])

ggplot(readers, aes(names, Readers)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
