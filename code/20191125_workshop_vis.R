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

#lda <- LDA(DTM, 
#           k = 9, 
#           method = "Gibbs",
#           control = list(delta=0.6, seed = 1598))
#saveRDS(lda, './models/20191125_lda_tango_wshop.RDS')


lda <- readRDS('./models/20191125_lda_tango_wshop.RDS')

# LDA VIS

serVis(topicmodels_json_ldavis(lda, DTM), 
       #out.dir = 'vis', 
       open.browser = TRUE)
