library(tidyverse)
library(tm)


#Utilty functions

topicmodels_json_ldavis <- function(fitted, dtm){
        
        svd_tsne <- function(x) tsne::tsne(svd(x)$u)
        
        # Find required quantities
        phi <- as.matrix(posterior(fitted)$terms)
        theta <- as.matrix(posterior(fitted)$topics)
        vocab <- colnames(phi)
        term_freq <- slam::col_sums(dtm)
        
        # Convert to json
        json_lda <- LDAvis::createJSON(phi = phi, theta = theta,
                                       vocab = vocab,
                                       mds.method = svd_tsne,
                                       plot.opts = list(xlab="tsne", ylab=""),
                                       doc.length = as.vector(table(dtm$i)),
                                       term.frequency = term_freq)
        
        return(json_lda)
}

get_topic_per_doc<-function(fitted_lda){
        tmResult <- posterior(fitted_lda)
        theta <- tmResult$topics
        beta <- tmResult$terms
        topicNames <- apply(terms(fitted_lda, 5), 2, paste, collapse = " ")  # reset topicnames      
        
        colnames(theta) <- topicNames
        return(theta)
}




df <- read_csv('../data/data_limpia_UNSL.csv')
df <- df %>%
        mutate(decada = ano - ano %% 10)


#df <- read_csv('../data/data_limpia_updt3.csv')
#df <- df %>%
#        mutate(decada = year_cons - year_cons %% 10)

#df$corpus <- str_replace_all(df$corpus, "[\\r\\n\\t]+", " ")
#corpus <- df$corpus
#corpus = 



corpus <- df$letra
corpus <- str_replace_all(df$letra, "[\\r\\n\\t]*", " ")
corpus <- str_replace_all(df$letra, "[:punct:]*", "")
corpus <- Corpus(VectorSource(corpus))


corpus <- tm_map(corpus, content_transformer(tolower))
stp <- c(stopwords(kind = "es"),'pa', 'pa’', 'tambien','mientras','tan','tal','así','asi', 'mas', 'más','aquel', 'áquel')
corpus <- tm_map(corpus, removeWords, stp)

corpus <- tm_map(corpus, removePunctuation, preserve_intra_word_contractions=FALSE)
corpus <- tm_map(corpus, removeNumbers)

DTM <- DocumentTermMatrix(corpus, control = list(minWordLength = 1))

#freq_words <- findMostFreqTerms(DTM,n = 100, 
#                                INDEX = rep(1,nDocs(DTM)))[[1]]
#freq_words <- tibble(word = names(freq_words), 
#                              freq =freq_words)

#library(wordcloud2)
#wordcloud2(freq_words, shuffle = FALSE)

ui = unique(DTM$i)
dtm = DTM[ui,]

library(topicmodels)
library(LDAvis)

lda_list1 <- list()
t0 <- proc.time()

for (i in 10:6){
        #for (d in c(0.06, 0.1, 0.6)){
                #name <- paste('K=', i, 'delta=', d, sep=' ')
                name <- paste('K=', i, 'delta=0.6', sep=' ')
                cat(paste('Fitting LDA', name, sep=' ') , sep='\n')
                lda <- LDA(DTM, 
                             k = i, 
                             method = "Gibbs",
                             control = list(delta=0.6, seed = 1598))
                lda_list1[[name]]<-lda

        #}
}
proc.time() - t0
saveRDS(lda_list1, './models/lda_fits.RDS')


lda_list <- readRDS('./models/lda_fits.RDS')

#topics_terms_06<-map(lda_list, terms, 10)[c(1,4,7,10,13,16)]


lda <- readRDS('./models/20191125_lda_tango_wshop.RDS')
terms(lda, 10)

doc_topics<-get_topic_per_doc(lda)


topic_names<-c('1 Misc', 
               '2 Tango y arrabal', 
               '3 Misc y lunfardo', 
               '4 Campo y gauchesca',
               '5 Ciudad, imagenes urbanas', 
               '6 Emociones positivas',
               '7 Emociones negativas', 
               '8 Imagenes climáticas',
               '9 Misc y familia')


colnames(doc_topics)<-topic_names
topic_proportion_per_decade <- aggregate(doc_topics, 
                                         by = list(decade = df$decada), 
                                         mean)

topic_proportion_per_decade %>% 
        gather("topic", "prop", -decade, factor_key=TRUE) %>%
        
        ggplot(aes(x=decade, y=prop, fill=topic)) + 
                #geom_bar(stat = "identity") + ylab("Proportion") + 
                geom_area(alpha=0.8, size=1, color='black') +
                #scale_fill_discrete(name="Tópico",
                #            #breaks=c("ctrl", "trt1", "trt2"),
                #            labels=c(1:9)) +
                geom_text(aes(label = if_else(decade==1910 | 
                                        decade==2000 | decade==1940 | decade==1960,
                                        round(prop,2),NULL)), 
                          position=position_stack(vjust = 0.5),
                          check_overlap = TRUE) +
                theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
                labs(title='Evolución de los tópicos (media de la composición de las letras de tango) 1900-2010')




        
tangos<-c('barrio reo', 'mi buenos aires querido',
          'garua',
          'chorra', 'malena')

tangos_test <- doc_topics[match(tangos, df$titulo),]
colnames(tangos_test)<-topic_names

tangos_test<- tangos_test %>% 
        as_tibble() %>%
        mutate(tango=tangos) %>%
        select(tango, everything()) %>%
        gather(topic, value,`1 Misc`:`9 Misc y familia`) %>%
        arrange(tango, topic)


ggplot(tangos_test, aes(x=tango, y=value, fill=topic)) + 
        geom_bar(stat = "identity") + ylab("value") +
        geom_text(aes(label = round(value,2)), 
                  position=position_stack(vjust = 0.5),
                  check_overlap = TRUE) +
        labs(title='Composición de tópicos según tangos (media de la composición de las letras de tango) 1900-2010')

        

autores <- df %>% 
        group_by(compositor) %>%
        summarise(n=n()) %>% 
        arrange(desc(n)) %>%
        top_n(8) %>%
        select(compositor)


topic_proportion_per_author <- aggregate(doc_topics, 
                                         by = list(author = df$compositor), 
                                         mean) %>%
        filter(author %in% autores$compositor) %>%
        gather(topic, value,`1 Misc`:`9 Misc y familia`)


topic_proportion_per_author %>%
        ggplot(aes(x=author, y=value, fill=topic)) + 
        geom_bar(stat = "identity") + ylab("value") +
        geom_text(aes(label = round(value,2)), 
                  position=position_stack(vjust = 0.5),
                  check_overlap = TRUE) +
        
        coord_flip()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
        labs(title='Composición de tópicos según autores (media de la composición de las letras de tango) 1900-2010')

