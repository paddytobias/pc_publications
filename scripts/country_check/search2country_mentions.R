## Is there dependence between search names and countries mentioned?
# H0: articles by their search name are independent from the countries they mention
# HA: articles by their search name are dependent from the countries they mention


library(tidyverse)
query = "Select search_name, country_names, count(title_counts) 
from country_count c, article_to_country ac, article_to_search search
where c.country_id=ac.country_id AND ac.article_doi=search.article_doi
group by search_name, country_names"

country_count = dbGetQuery(con, query)

country_count_spread = country_count %>% 
  spread(key = country_names, value = count) %>% 
  mutate_all(funs(ifelse(is.na(.), 0, .))) %>% 
  column_to_rownames("search_name")

chisq.test(country_count_spread)

# p < 0.001

