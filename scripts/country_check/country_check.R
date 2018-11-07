# script to identify country names in titles and abstracts of articles and output
# a join table called article_to_country.csv along with an aggregate of total counts 
# for each search, called country_counts.csv


query_terms = c("peacebuilding", 
  "peace and conflict studies", 
  "conflict resolution", 
  "liberal peacebuilding", 
  "peacemaking"
  ,"conflict transformation"
  , 'state-building'
  , 'nation-building',
  "conflict prevention", 
  "peacekeeping"
)

search_to_country = data.frame(search = as.character(), 
                               search_index = as.character(), 
                               country_index = as.character(), 
                               title_appear = as.character(), 
                               abstract_appear = as.character(), stringsAsFactors = FALSE)

for (search in query_terms){
  country_names = read.csv("../peacebuilding-language/database/database_files/country_names.csv", header = TRUE, stringsAsFactors = FALSE)
  country_names$title_count = 0
  country_names$abstract_count=0
  country_names$search = search
  names(country_names) = c('index','country_names', 'title_counts', 'abstract_count', 'search')
  page_files = list.files(paste0("../data/", search, "/"), pattern = "^page_")
  for (page in page_files){
    dat = read.csv(paste0("../data/",search,"/",page), stringsAsFactors = FALSE)
    print(search)
    for (i in 1:nrow(dat)){

      for (j in 1:nrow(country_names)){

        title_match = grepl(country_names$country_names[j], dat$titles[i])
        abstract_match = grepl(country_names$country_names[j], dat$article_abstract_text[i])
        if (title_match == TRUE & abstract_match == TRUE){
          country_names$title_counts[j] = country_names$title_counts[j] + 1
          country_names$abstract_count[j]=country_names$abstract_count[j]+1
          search_to_country = rbind(search_to_country, cbind(search, as.character(dat$article_doi[i]), country_names$index[j], 1, 1), stringsAsFactors = FALSE)
        } else if (title_match ==TRUE & abstract_match == FALSE){
          country_names$title_counts[j] = country_names$title_counts[j] + 1
            search_to_country = rbind(search_to_country, cbind(search, as.character(dat$article_doi[i]), country_names$index[j], 1, 0), stringsAsFactors = FALSE)
        } else if (title_match== FALSE & abstract_match == TRUE){
          country_names$abstract_count[j]=country_names$abstract_count[j]+1
          search_to_country = rbind(search_to_country, cbind(search, as.character(dat$article_doi[i]), country_names$index[j], 0, 1), stringsAsFactors = FALSE)
        }
      }
    }
  }
  
  
  write.csv(data.frame(country_names), paste0("../data/", search, "/country_count.csv"), row.names=FALSE)
  
}

# search_to_country_CURRENT = read.csv("../peacebuilding-language/database/database_files/article_to_country.csv", header = T, stringsAsFactors = F)

names(search_to_country)=c('search', 'search_index', 'country_index', 'title_appear', 'abstract_appear')
# search_to_country = rbind(search_to_country, search_to_country_CURRENT)
# search_to_country = search_to_country %>% 
#   filter(search == "conflict prevention" | search == "peacekeeping")
write.csv(search_to_country, "../peacebuilding-language/database/database_files/article_to_country.csv", row.names = FALSE)



