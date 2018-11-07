get_gamma = function(topic_n){
  load(paste0("text_analysis/lda_outputs/k_", topic_n,"abs_lda.RData"))
  # inter-document topics
  abs_doc_topics <- tidy(abs_lda, matrix = "gamma") %>%
    separate(document, c("article_doi", "search_name", "year"), sep="_", convert=TRUE)
  abs_doc_topics = abs_doc_topics %>% 
    filter((search_name %in% pc_cats) & (year %in% 2000:2018))
  return(abs_doc_topics)
}

get_beta = function(topic_n){
  load(paste0("text_analysis/lda_outputs/k_", topic_n,"abs_lda.RData"))
  # identify words for each topic
  abs_topic_words <- tidy(abs_lda, matrix = "beta") %>% 
    arrange(topic, desc(beta))
  return(abs_topic_words)
}

top_topic = function(gamma) {
  return(gamma %>% 
           group_by(topic) %>% 
           summarise(avg_gamma = mean(gamma)) %>% 
           top_n(1, avg_gamma) %>% 
           select(topic))
}


is_search_dominated_by_topic = function(gamma){
  # calculating top topics by search
  top_topic_search = gamma %>% 
    group_by(search_name, topic) %>% 
    summarise(avg_gamma = mean(gamma)) %>% 
    top_n(1, avg_gamma)
  if (length(unique(factor(top_topic_search$topic))) <= 2){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

is_year_dominated_by_topic = function(gamma){
  # calculating top topics by search
  top_topic_year = gamma %>% 
    group_by(year, topic) %>% 
    summarise(avg_gamma = mean(gamma)) %>% 
    top_n(1, avg_gamma)
  if (length(unique(factor(top_topic_year$topic))) <= 2){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

## trends
plot_trends_with_words = function(gamma, beta, topic_n){
  
  trending = gamma %>% 
    group_by(year, topic) %>% 
    summarise(mean_year = mean(gamma)) %>% 
    group_by(topic) %>% 
    na.omit() %>% 
    do(coef = as.numeric(lm(year ~ mean_year, data = .)$coefficients[2])) %>% 
    mutate(trend = ifelse(coef >= 0, "On the up", "Cooling"))
  
  plot_trend_topic = gamma %>% 
    group_by(year, topic) %>% 
    summarise(mean_year = mean(gamma)) %>% 
    inner_join(trending) %>% 
    ungroup() %>% 
    {ggplot(., aes(factor(year), mean_year, group = factor(topic), color = factor(topic)))+
        geom_smooth(se = FALSE, show.legend = F) +
        labs(x = "Year", y = "Gamma")+
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        geom_label_repel(data = . %>% group_by(topic, trend) %>% 
                           do(augment(loess(mean_year~year, .))) %>% 
                           filter(year == min(year)),
                         aes(year,.fitted, label = topic), show.legend = F, nudge_x = 0, nudge_y = 0)+
        facet_wrap(~trend)}
  
  
  ## top 12 words by topic
  top_terms <- beta %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
  
  plot_top10_terms = top_terms %>%
    mutate(term = reorder(term, beta), 
           topic_name = paste("Topic", topic))
  
  write.csv(plot_top10_terms, "top10_terms.csv")
  
  ggplot(plot_top10_terms, aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic_name, scales = "free") +
    coord_flip()+
    theme_minimal()
  
  
  trend_terms = grid.arrange(plot_trend_topic, plot_top10_terms, nrow = 2, top = "Trends in topics")
  
  #ggsave(paste0("outputs/topic_models/abs/k_", topic_n, "/trending_topics.jpeg"), trend_terms, width = 12, height = 12)
  
}


## boxplot search topics
plot_topics_search = function(gamma, topic_n){
  by_search = gamma %>%
    ggplot(aes(factor(topic), gamma)) +
    geom_boxplot() +
    facet_wrap(~ search_name) +
    theme_minimal()+
    labs(title = "Topics across search names", x = "Topics")
  ggsave(paste0("outputs/topic_models/abs/k_", topic_n,"/box_search_topics.jpeg"), by_search, width = 12, height = 12)
}

plot_topics_year = function(gamma, topic_n){
  by_year = gamma %>%
    ggplot(aes(factor(topic), gamma)) +
    geom_boxplot() +
    facet_wrap(~ year) +
    theme_minimal() +
    labs(title = "Topics across years", x = "Topics")
  ggsave(paste0("outputs/topic_models/abs/k_", topic_n,"/box_year_topics.jpeg"), by_year, width = 12, height = 12)
}

top_words_topic = function(beta, topic_n, top_word_n){
  top_terms <- beta %>%
    group_by(topic) %>%
    top_n(top_word_n, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
  
  top12_topic_words = data.frame(index = 1:top_word_n)
  for (i in levels(factor(top_terms$topic))){
    topic = top_terms %>% 
      filter(topic==as.numeric(i)) %>% 
      head(12) %>% 
      #list(term = formatter("span", style = ~ style(colour = ifelse(beta < 0.05, "red", "green")))) %>% 
      select(term)
    
    colnames(topic) = paste("Topic", i)
    top12_topic_words = cbind(top12_topic_words, topic)
  }
  
  kable_top12_topic_words = top12_topic_words %>% 
    select(-index) %>% 
    kable() %>% 
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
  setwd(paste0("~/ownCloud/GitHub/language-in-peacebuilding/peacebuilding-language/outputs/topic_models/abs/k_",topic_n))
  save_kable(kable_top12_topic_words, file = paste0("k_", topic_n, "_top12_topic_words.html"))
  write_csv(top12_topic_words, paste0("k_", topic_n, "_top12_topic_words.csv"))
  setwd("~/ownCloud/GitHub/language-in-peacebuilding/peacebuilding-language/")
  
}

plot_top_topic_search = function(gamma, beta, topic_n){
  # calculating top topics by search
  top_topic_search = gamma %>% 
    group_by(search_name, topic) %>% 
    summarise(avg_gamma = mean(gamma)) %>% 
    top_n(1, avg_gamma)
  
  ## top 12 words by topic
  top_terms <- beta %>%
    group_by(topic) %>%
    top_n(12, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
  
  top_topicwords_search = top_topic_search %>%
    inner_join(top_terms) %>% 
    mutate(topic_name = paste("Topic", topic))
  
  top_topicwords_search = ggplot(top_topicwords_search, aes(term, beta))+
    geom_bar(stat = "identity")+
    facet_wrap(search_name~topic_name, scales="free") +
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    labs(x = "Words", title = "Best aligned topic by search name", subtitle = paste("Testing for", topic_n, "topics" ))
  ggsave(paste0("outputs/topic_models/abs/k_", topic_n, "/top_topicwords_by_search.jpeg"), top_topicwords_search, width = 12, height = 12)
}

plot_top_topic_year = function(gamma, beta, topic_n){
  ## and by year
  top_topic_year = gamma %>% 
    group_by(year, topic) %>% 
    summarise(avg_gamma = mean(gamma)) %>% 
    top_n(1, avg_gamma)
  
  ## top 12 words by topic
  top_terms <- beta %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
  
  top_topicwords_year = top_topic_year %>%
    inner_join(top_terms) %>% 
    mutate(topic_name = paste("Topic", topic))
  
  top_topicwords_year = ggplot(top_topicwords_year, aes(term, beta))+
    geom_bar(stat = "identity")+
    facet_wrap(year~topic_name, scales="free", nrow = topic_n/3) +
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    labs(x = "Words", title = "Best aligned topic by year", subtitle = paste("Testing for", topic_n, "topics" ))
  ggsave(paste0("outputs/topic_models/abs/k_", topic_n, "/top_topicwords_by_year.jpeg"), top_topicwords_year, width = 12, height = 12)
  
}

plot_search_overlap = function(gamma, topic_n){
  ##---- finding overlap between full texts by classifications
  abs_classifications <- gamma %>%
    group_by(article_doi) %>%
    top_n(1, gamma) %>%
    ungroup()
  
  abs_topics <- abs_classifications %>%
    count(article_doi, topic) %>%
    group_by(article_doi) %>%
    top_n(1, n) %>%
    ungroup() %>%
    transmute(consensus = article_doi, topic)
  
  # By word assignments: augment
  load(paste0("text_analysis/lda_outputs/k_", topic_n,"abs_lda.RData"))
  load("text_analysis/lda_outputs/abs_dtm.RData")
  assignments <- augment(abs_lda, data = abs_dtm)
  
  assignments <- assignments %>%
    inner_join(abs_topics, by = c(".topic" = "topic"))
  
  abs_search  = abs_text %>% 
    distinct(article_doi, search_name) 
  
  overlap_searches = assignments %>%
    separate(document, c("document", "search_name", "year"), sep="_", convert= TRUE) %>% 
    inner_join(abs_search, by = c("consensus" = "article_doi")) %>% 
    filter(search_name.x!="5.x") %>% 
    count(search_name.x, search_name.y, wt = count) %>%
    group_by(search_name.x) %>%
    mutate(percent = n / sum(n)) %>%
    filter(search_name.y %in% pc_cats) %>% 
    ggplot(aes(search_name.x, factor(search_name.y), fill = percent)) +
    geom_tile() +
    scale_fill_gradient2(high = "tomato", label = percent_format()) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          panel.grid = element_blank()) +
    labs(x = "Searches upon which topics were assigned",
         y = "Searches from which topics came",
         fill = "% of overlap", 
         title = "Overlapping topics between searches")
  ggsave(paste0("outputs/topic_models/abs/k_", topic_n, "/overlap_searches.jpeg"), overlap_searches, width = 12, height = 12)
  
}

topic_n = 9

gamma = suppressMessages(read_csv("outputs/topic_models/abs/k_9/gamma_k_9.csv"))
beta = suppressMessages(read_csv("outputs/topic_models/abs/k_9/beta_k_9.csv"))

gamma_year = gamma
gamma_search = gamma

if (is_search_dominated_by_topic(gamma) & topic_n>2){
  topic_1 = top_topic(gamma)$topic  
  gamma_search = gamma %>%
    filter(topic!=topic_1)
} # remove dominant topic if exists

if (is_year_dominated_by_topic(gamma) & topic_n>2){
  topic_1 = top_topic(gamma)$topic
  gamma_year = gamma %>%
    filter(topic!=topic_1)
}
