query_terms = c(#"peacebuilding", 
  #"peace and conflict studies", 
  #"conflict resolution", 
  #"liberal peacebuilding", 
  #"peacemaking"
  #,"conflict transformation"
  #, 'state-building'
  #, 'nation-building'
  "conflict prevention", 
  "peacekeeping"
)

for (search in query_terms){
  page_files = list.files(paste0("../data/", search, "/"), pattern = "^page_")
  
  #add search attr for each page file
  for (i in 1:length(page_files)){
    filename = page_files[i]
    filepath = paste0("../data/", search,"/",filename)
    dat = read.csv(filepath)
    if (!("search" %in% names(dat))){
      dat$search = search
      write.csv(dat, paste0("../data/", search,"/", filename), row.names = FALSE)
    }
  }
  
  #clean up authors table per search by removing rows that have empty author name and adding search attr
  authors = read.csv(paste0("../data/", search, "/authors_", search,".csv"))
  if (!("search" %in% names(authors))){
    authors$search = search
    for (j in 1:nrow(authors)){
      if (authors[j,1]=="" | is.null(authors[j,1])|is.na(authors[j,1])){
        authors = authors[-c(j),]
      }
    }
    write.csv(authors, paste0("../data/", search, "/authors_", search,".csv"), row.names = FALSE)
  }
}



