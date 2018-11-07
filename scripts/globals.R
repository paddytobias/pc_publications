meta_cats = c("liberal peacebuilding", "peace and conflict studies")

proxy_cats = c("nation-building", "state-building")

pc_cats = c("conflict prevention", "conflict resolution", "conflict transformation",  
            "peacekeeping", "peacemaking", "peacebuilding")

all_cats = c(meta_cats,proxy_cats,pc_cats)

reorder_pc = c("conflict prevention", "conflict resolution", "conflict transformation", 
                   "peacekeeping", "peacemaking", "peacebuilding")

conf_cats = c("conflict resolution", "conflict transformation", "conflict prevention")

peace_cats = c("peacekeeping", "peacemaking", "peacebuilding")


long_pc = c("peacebuilding", "conflict transformation")
middle_pc = c("peacemaking", "conflict resolution")
short_pc = c("peacekeeping", "conflict prevention")


other_stop_words = c("scholar", "author", "google", all_cats, "conflict", "peace", "prevention", 
                     "resolution", "transformation",
                     "doi", "view", "views", "journal", "university", "","isbn", "reviewed","de",
                     "article", "study", "research","abstract","pp",
                     "note", "crossref", 2000:2018, "web", "studies", "", ".")
