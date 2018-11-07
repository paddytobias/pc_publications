library(rvest)
source("login.R")
options(timeout= 4000000) 

query_terms = c(#"peacebuilding", 
  #"peace and conflict studies", 
  #"conflict resolution", 
  #"liberal peacebuilding",
  #"peacemaking", 
  #"conflict transformation",
  #'state-building', 
  #'nation-building',
  "conflict prevention", 
  "peacekeeping"
)

dir.create("../data/")
for (query in query_terms){
  dir.create(paste0("../data/", query))
}

# base urls
repo_url = "https://www-tandfonline-com.ezproxy.une.edu.au"
query_url = "/action/doSearch?AllField="
# limit page to 50 results, order by relevance
pagination_url = "&pageSize=50&subjectTitle=&startPage="


# searchable html
query_count_html = '.search-tab-counts'
titles_html = ".art_title a"
author_html = ".hlFld-ContribAuthor a"
pub_years_html = ".publication-year"
article_type_html = ".article-type"
link_html = 'href'
abstract_html = '.abstractInFull p'
full_text_html = '.NLM_sec_level_1 p'
stats_html = '.value'
references_html = '#references-Section'

get_page = function(url){
  for(run in 1:10){
    website <- tryCatch(jump_to(session, url), error=function(e) e)
    if(inherits(website, "error")){
      print("URL not resolving. Sleeping for 60 seconds, then will try again")
      Sys.sleep(30)
      next
    } 
    
    page <- read_html(website)
    Sys.sleep(30)
    return(page)
  }
}



for (search in query_terms){
  call_url = paste0(repo_url, query_url, gsub(" ", "+", search), pagination_url)
  page_1 = get_page(paste0(call_url, 1))
  # number of returns for query
  query_n_node = html_nodes(page_1, query_count_html)
  query_n = html_text(query_n_node)
  article_n = query_n[1]
  article_n = gsub("\\(|\\)", "", article_n)
  page_n = round(as.numeric(article_n)/50, 0)
  if(page_n>100){
    page_n=100
  }
  
  #prep author authority table for search
  authors_all = data.frame(authors = as.character(), url=as.character(), stringsAsFactors = FALSE)
  page = page_1
  
  
  for (i in 1:page_n){
    print(paste("Working on page number:", i, "\n", search))
    
    # return titles, publish dates and authors
    titles_links = page %>% 
      html_nodes(titles_html) %>%
      html_attr(link_html)
    
    titles = page %>% 
      html_nodes(titles_html) %>%
      html_text()
    
    pub_node = html_nodes(page, pub_years_html)
    pub_dates = as.character(as.Date(gsub(".*: ", "", html_text(pub_node)), "%d %b %Y"))
    
    page_results = cbind(titles, pub_dates, titles_links)
    
    #prep table for article results
    article_pages = data.frame(article_doi = as.character(), abstract = as.character(), article = as.character(), article_references = as.character(), views = as.character(), citations=as.character(), altmetrics = as.character(), stringsAsFactors = FALSE)
    for (j in 1:nrow(page_results)){
      article_doi = page_results[j, 3]
      article_url = paste0(repo_url, article_doi)
      
      message(paste("Collecting text for article:", article_url))
      
      
      #get page
      article_page = get_page(article_url)
      
      # the article's authors
      authors = article_page %>%
        html_nodes(author_html) %>%
        html_text()
      if (length(authors)==0){
        authors=""
      }
      # the articles full text
      article_full_text = article_page %>%
        html_nodes(full_text_html) %>%
        html_text()
      article_full_text = paste(article_full_text, collapse = ' ')
      
      # the articles abstract
      article_abstract_text = article_page %>%
        html_nodes(abstract_html) %>%
        html_text()
      article_abstract_text = paste(article_abstract_text, collapse = ' ')
      
      #article references
      article_references = article_page %>% 
        html_nodes(references_html) %>% 
        html_text()
      article_references = paste(article_references, collapse = ' ')
      
      stats = article_page%>%
        html_nodes(stats_html)%>%
        html_text()
      views = stats[4]
      citations=stats[5]
      altmetrics=stats[6]
      
      article_pages = rbind(article_pages, cbind(article_doi, article_abstract_text, article_full_text, article_references, views, citations,altmetrics), stringsAsFactors = FALSE)
      authors_all = rbind(authors_all, cbind(authors, article_doi))
      
      message(paste(search, "search.\nPage", i, "-", (nrow(page_results)-j), "articles to go for this page!"))
    }
    page_results = cbind(page_results[,1:2], article_pages)
    
    #prep results table per page
    #results = data.frame(links = as.character(), title = as.character(), pub_dates=as.character(), stringsAsFactors = FALSE)
    #results = rbind(results, page_results, stringsAsFactors = FALSE)
    
    # return list of authors; one to many with publications so can't combined with results table
    #author_nodes = html_nodes(page, author_html)
    #authors = html_text(author_nodes)
    
    
    #remove commas and \n
    for (col in 1:ncol(page_results)){
      page_results[,col] = gsub(",|\\n", " ", page_results[,col])}
    
    
    #write page results
    write.csv(page_results, paste0("../data/", search,"/page_",i, ".csv"), row.names = FALSE)
    
    # move to next page
    page = tryCatch(get_page(paste0(call_url, (i+1))), 
                    error=function(e) e)
    
  }
  authors_all[,1]=gsub(",|\\n", " ", authors_all[,1])
  authors_all[,1]=trimws(authors_all[,1])
  authors_all = unique(authors_all)
  for (k in 1:nrow(authors_all)){
    if (is.null(authors_all[k,1])){
      authors_all=authors_all[-k,]
    }
  }
  write.csv(authors_all, paste0("../data/", search,"/authors_", search, ".csv"), row.names = FALSE)
  
}




