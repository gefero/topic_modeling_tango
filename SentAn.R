setwd('E:/PEN/Datasets_ML/ScrapTango/Topic Modelling Tango/Version R')
library(tidyverse)
library(tidytext)


df <- read_csv('E:/PEN/Datasets_ML/ScrapTango/Topic Modelling Tango/data_limpia_updt.csv')
df <- df %>% 
        select(-'X1') %>%
        mutate(decada = year_cons - year_cons %% 10)

tangos <- separate_rows(df, corpus_stem, sep = "\\\\n | \\|")

tangos <- tangos %>%
        group_by(link_tt) %>% 
        mutate(linea = row_number()) %>% 
        ungroup()

head(tangos)

tangos_tokenizado <- tangos %>% 
        unnest_tokens(word, corpus_stem)



dtm <-tangos_tokenizado %>%
        cast_dtm(tangos_tokenizado, link_tt, word)




head(tangos_tokenizado)

stopwords_es <- read.csv('E:/PEN/Datasets_ML/stpwords/stopwords_complete_no_accents.txt', header=FALSE)

tangos_tokenizado <- tangos_tokenizado %>% 
        anti_join(stopwords_es, by = c("word" = "V1"))



tangos_tokenizado %>% 
        count(word, sort = TRUE)


lexico <- read.csv("https://bitsandbricks.github.io/data/sdal.csv", 
                   stringsAsFactors = FALSE)


sentimiento_clasicos <- tangos_tokenizado %>% 
        #filter(titulo %in% clasicos) %>% 
        inner_join(lexico, by = c("word" = "palabra")) %>% 
        group_by(titulo, linea) %>% 
        summarise(autor = max(compositor_tt),
                  year = max(year_cons),
                  decada = max(decada),
                  sentimiento = mean(media_agrado))


x<-sentimiento_clasicos %>%

        group_by(decada) %>%
        drop_na(decada) %>%
        summarise(med=median(sentimiento))
        #ggplot() +
        #geom_line(aes(x=decada, y=med))
