

library(rvest)
library(RPostgreSQL)
library(readr)
library(dplyr)
source("UNE_login.R")
#source("database/db_connect.R")
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

repo_url = "https://www-tandfonline-com.ezproxy.une.edu.au"
references_html = '#references-Section'

# get url extensions
# query = "select article_doi from articles"
# url_ext = dbGetQuery(con, query)

# no longer need DB connection
# dbDisconnect(con)

url_ext = read.csv("database/database_files/url_extensions.csv", stringsAsFactors=F, header=T)

url_ext_full = url_ext[grepl("full", url_ext$article_doi),]


#ref_authors_canonical = data.frame(author = as.character(), article_doi = as.character())
references = data.frame(article_doi = as.character(), full_ref = as.character(), 
                        ref_authors = as.character())

#url_ext_full = url_ext_full[grep(i, url_ext_full):length(url_ext_full)]

#last_url_pos=3507
#url_ext_full_sub = url_ext_full[grep(i, url_ext_full):length(url_ext_full)]
#references = read.csv("database/database_files/references_20180709.csv")


for (i in url_ext_full){
  url = paste0(repo_url, i)
  page = get_page(url)
  print(i)
  
  references_txt <- tryCatch(page %>% 
                        html_nodes(references_html) %>% 
                        html_text(), error=function(e) e)
  if(inherits(references_txt, "error")){
    print("No references available")
    next
  }
  
  references_tbl = sub("^References|Bibliography|BIBLIOGRAPHY|REFERENCES", "", references_txt)
  
  if (grepl("\\[.*?\\]", references_txt)==FALSE || references_tbl =="" || length(references_tbl)== 0){
    next
  }
  references_tbl = gsub("\\[.*?\\]", "\n", references_tbl)

  references_tbl = read_delim(references_tbl, delim = "\n", col_names = FALSE)
  references_tbl = references_tbl[grepl(" |^, $", references_tbl$X1),]
  references_tbl = references_tbl[!grepl("^, $", references_tbl$X1),]
  references_tbl$authors = gsub("“", "\"", references_tbl$X1, ignore.case = TRUE)
  #references_tbl$authors = gsub("(.*)(\\..*)", "\\1", references_tbl$authors)
  #references_tbl$authors = sub("(.*)(\\.[[:space:]].*)", "\\1", references_tbl$authors)
  #references_tbl$authors = sub("(.*)(\\.[[:space:]].*)", "\\1", references_tbl$authors)
  #references_tbl$authors = sub("(.*)([[:space:]]\".*)", "\\1", references_tbl$authors)
  #references_tbl$authors = gsub("(.*)(\".*)", "\\1", references_tbl$authors)
  
  for (j in 1:4){
    references_tbl$authors = gsub("(.*)(\".*)", "\\1", references_tbl$authors)
    references_tbl$authors = sub("(.*)([[:space:]]\'.*)", "\\1", references_tbl$authors)
  }
  for (j in 1:4){
    references_tbl$authors = gsub("(.*)(([[:space:]]|\\()[[:digit:]]{4}.*)", "\\1", references_tbl$authors)
  }
  for (j in 1:4){
    references_tbl$authors = gsub("(.*)(([[:digit:]]{4}|n\\.d\\.).*)", "\\1", references_tbl$authors)
  }
  references_tbl$authors = gsub("([a-z|é]\\.)(.*)", "\\1", references_tbl$authors)
  references_tbl$authors = gsub("([a-z])(,)([A-Z])", "\\1\\2 \\3", references_tbl$authors)
  references_tbl$authors = gsub("([[:space:]]{1,}\\,)", ",", references_tbl$authors)
  references_tbl$authors = gsub("([[:space:]]{1,}\\.)", "\\.", references_tbl$authors)
  references_tbl$authors = gsub("([A-Za-z]{2,})(\\.$)", "\\1", references_tbl$authors)
  references_tbl$authors = gsub("(.*[A-Za-z]{1})([[:space:]]|$)", "\\1\\.", references_tbl$authors)
  
  references_tbl= cbind(i, references_tbl)
  names(references_tbl)=c("article_doi", "full_ref", "ref_authors")
  
  references = rbind(references, references_tbl)
  
  message(round((grep(i, url_ext_full)/length(url_ext_full))*100, 2), "% complete")
  
  # random sleep time
  time = round(runif(1, 20, 120))
  Sys.sleep(time)
}

#references$authors = gsub("", "NA", references_tbl$authors)
#references$full_ref = gsub("", "NA", references_tbl$full_ref)

